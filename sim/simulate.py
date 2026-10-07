#!/usr/bin/env python3
"""
Simulate an ICES-wide VMS (Table 1, "VE") and logbook (Table 2, "LE") data lake.

The output mirrors the column layout of the ICES VMS & Logbook data call
(table1Save / table2Save in the national submission workflow), but for *all*
submitting countries and many years -- i.e. what the ICES total could look like.

Storage layout written by this script (the "cold" tier):

    <out>/lake/vms/Year=YYYY/CountryCode=XX/part-0.parquet       Table 1
    <out>/lake/logbook/Year=YYYY/CountryCode=XX/part-0.parquet   Table 2

* Hive partitioning on Year / CountryCode: one folder per national submission,
  so a resubmission replaces one folder, and year/country filters skip files.
* Rows inside each partition are sorted on a Morton (Z-order) key of the c-square
  grid position, and written in small row groups. Parquet keeps min/max stats per
  row group, so a bounding-box query only decompresses the row groups that can
  overlap the box.
* Categorical columns are dictionary encoded by Parquet; numeric grid indices
  (ix, iy) and centroids (lon, lat) are materialised at ingest so nothing has
  to parse C-square strings at query time.

The R side (build_warehouse.R) then builds the "hot" tier: pre-aggregated cube
pyramids in a DuckDB database.

A handful of realistic data-quality problems are injected on purpose (see
INJECTED_ISSUES) so the app's QC Lab has something to find.
"""
import argparse
import json
import math
import os
import time

import duckdb
import numpy as np
import pyarrow as pa
from scipy import ndimage

# --------------------------------------------------------------------------- grid
RES = 0.05
LON0, LON1 = -50.0, 45.0
LAT0, LAT1 = 34.0, 82.0
NX = int(round((LON1 - LON0) / RES))
NY = int(round((LAT1 - LAT0) / RES))

COUNTRIES = ["NO", "IS", "FO", "GB", "IE", "FR", "ES", "PT", "BE", "NL",
             "DE", "DK", "SE", "FI", "PL", "EE", "LV", "LT"]
EU_BOARDERS = {"FR", "NL", "BE", "DE", "DK", "IE", "ES"}

VLEN = ["VL0006", "VL0608", "VL0810", "VL1012", "VL1215", "VL1518", "VL1824", "VL2440", "VL40XX"]
VLEN_LEN = {"VL0006": (4, 6), "VL0608": (6, 8), "VL0810": (8, 10), "VL1012": (10, 12),
            "VL1215": (12, 15), "VL1518": (15, 18), "VL1824": (18, 24),
            "VL2440": (24, 40), "VL40XX": (40, 95)}

# Ping interval (hours) per country -- national VMS systems differ
PING_H = {c: 2.0 for c in COUNTRIES}
PING_H.update({"NO": 1.0, "IS": 1.0, "FO": 1.0, "DK": 1/6, "SE": 0.5, "GB": 2.0, "NL": 2.0})

# VMS threshold: Norway collects VMS for >=15 m only, EU >=12 m.
VMS_MIN_IDX = {c: VLEN.index("VL1215") for c in COUNTRIES}
VMS_MIN_IDX["NO"] = VLEN.index("VL1518")

# --------------------------------------------------------------------------- grounds
# name: (lon, lat, sd_lon, sd_lat)
GROUNDS = {
    "n_northsea":   (1.0, 59.4, 1.4, 0.8),
    "c_northsea":   (2.8, 56.6, 2.0, 1.0),
    "s_northsea":   (3.2, 53.4, 1.3, 0.7),
    "dogger":       (2.2, 54.9, 1.0, 0.4),
    "fladen":       (0.4, 58.3, 0.8, 0.5),
    "farn":         (-0.9, 55.2, 0.5, 0.3),
    "norw_deep":    (4.6, 58.6, 1.0, 0.45),
    "skagerrak":    (9.6, 57.9, 1.2, 0.35),
    "kattegat":     (11.6, 56.8, 0.4, 0.5),
    "wadden":       (6.6, 53.75, 1.6, 0.22),
    "channel_w":    (-3.6, 49.8, 1.5, 0.4),
    "channel_e":    (0.4, 50.3, 0.9, 0.3),
    "celtic":       (-7.0, 50.6, 1.6, 0.8),
    "irish_sea":    (-5.2, 53.9, 0.5, 0.5),
    "west_scot":    (-7.4, 57.1, 1.0, 0.8),
    "porcupine":    (-12.8, 52.0, 1.0, 0.8),
    "w_ireland":    (-11.0, 54.2, 1.0, 1.3),
    "rockall":      (-14.2, 57.4, 0.8, 0.5),
    "hatton":       (-18.0, 58.6, 1.1, 0.7),
    "biscay":       (-4.0, 46.0, 1.6, 1.0),
    "iberia_n":     (-8.0, 43.8, 1.2, 0.3),
    "iberia_w":     (-9.4, 40.0, 0.35, 1.6),
    "gulf_cadiz":   (-7.2, 36.7, 0.6, 0.3),
    "norw_coast":   (4.6, 62.0, 1.0, 1.6),
    "mid_norway":   (9.0, 65.0, 1.1, 1.2),
    "lofoten":      (13.8, 68.4, 1.3, 0.6),
    "finnmark":     (23.0, 71.4, 3.0, 0.5),
    "barents":      (22.0, 73.2, 3.5, 1.2),
    "norw_sea":     (2.0, 66.0, 4.0, 2.0),
    "iceland_s":    (-20.0, 63.4, 4.0, 0.5),
    "iceland_n":    (-18.0, 66.9, 3.0, 0.4),
    "iceland_w":    (-25.0, 65.3, 1.0, 1.0),
    "faroe":        (-7.0, 62.0, 1.0, 0.5),
    "baltic_w":     (13.0, 54.9, 1.0, 0.35),
    "baltic_c":     (18.2, 56.3, 1.5, 1.1),
    "bothnia":      (20.0, 62.0, 1.0, 1.4),
    "gulf_finland": (25.5, 59.8, 1.5, 0.25),
    "gulf_riga":    (23.4, 57.8, 0.5, 0.35),
}

# home ports: fleets fish mostly near home; distant-water gears range further
HOME = {"NO": (5.5, 62.0), "IS": (-21.0, 64.3), "FO": (-6.8, 62.0), "GB": (-2.5, 56.5), "IE": (-8.5, 53.3),
        "FR": (-3.0, 48.0), "ES": (-8.0, 43.4), "PT": (-8.9, 40.2), "BE": (3.0, 51.3), "NL": (4.6, 52.6),
        "DE": (8.4, 54.0), "DK": (9.0, 56.6), "SE": (12.6, 57.8), "FI": (22.5, 61.0), "PL": (18.6, 54.6),
        "EE": (24.2, 59.2), "LV": (22.0, 57.0), "LT": (21.1, 55.7)}
RANGE_KM = {"OTM": 1800, "PS": 1400, "LLS": 1400, "PTM": 700, "OTB": 900, "TBB": 550, "SDN": 500, "SSC": 600,
            "DRB": 450, "GNS": 350, "FPO": 250}


def km(a, b):
    lon1, lat1, lon2, lat2 = map(math.radians, (a[0], a[1], b[0], b[1]))
    h = math.sin((lat2 - lat1) / 2) ** 2 + math.cos(lat1) * math.cos(lat2) * math.sin((lon2 - lon1) / 2) ** 2
    return 6371 * 2 * math.asin(math.sqrt(h))


def country_grounds(grounds, c, l4):
    rng_km = RANGE_KM.get(l4, 800)
    w = {g: v * math.exp(-km(HOME[c], GROUNDS[g][:2]) / rng_km) for g, v in grounds.items()}
    tot = sum(w.values())
    return {g: v / tot for g, v in w.items() if v / tot > 0.01}


# --------------------------------------------------------------------------- gear
# L4: speed mean, speed sd (knots), gear width (km) or None (no seabed contact)
GEAR = {
    "OTB": (3.0, 0.45, 0.075), "TBB": (5.2, 0.6, 0.024), "OTM": (3.8, 0.6, None),
    "PTM": (3.3, 0.5, None),   "PS":  (1.6, 0.9, None),  "SDN": (0.9, 0.45, 0.85),
    "SSC": (1.3, 0.5, 0.95),   "DRB": (2.6, 0.4, 0.0085), "LLS": (1.8, 0.7, None),
    "GNS": (0.6, 0.35, None),  "FPO": (0.5, 0.35, None),
}
BOTTOM_MOBILE = {"OTB", "TBB", "SDN", "SSC", "DRB"}

# Fisheries. weight ~ share of annual VMS effort events.
# season: (peak month, concentration 0=flat .. 3=very seasonal)
# vlen: probabilities over VLEN
def vl(**kw):
    p = np.array([kw.get(k, 0.0) for k in VLEN], dtype=float)
    return p / p.sum()

FISHERIES = [
    dict(l6="OTB_DEF_>=120_0_0", w=16, season=(3, 0.4), cpue=260, price=2.6,
         countries=dict(NO=5, GB=5, DK=2, DE=1.5, FR=2, NL=1, IS=4, FO=1.5, BE=0.3, SE=0.5),
         grounds=dict(n_northsea=4, c_northsea=3, fladen=1, norw_coast=1, mid_norway=1, lofoten=1.5,
                      finnmark=1.5, barents=3, iceland_s=2, iceland_n=1.5, iceland_w=1, faroe=1.2, west_scot=1),
         vlen=vl(VL1518=0.6, VL1824=1.5, VL2440=3, VL40XX=1.6)),
    dict(l6="OTB_CRU_70-99_0_0", w=11, season=(6, 0.5), cpue=55, price=7.5,
         countries=dict(GB=6, IE=3, DK=2, SE=1, FR=1.5, BE=0.5, NL=0.6, NO=0.3),
         grounds=dict(fladen=3, farn=2, irish_sea=2.5, porcupine=0.8, w_ireland=0.6, west_scot=1.5,
                      skagerrak=1.5, kattegat=1, biscay=1.2, celtic=0.8, c_northsea=0.4),
         vlen=vl(VL1215=2, VL1518=2, VL1824=2, VL2440=0.8)),
    dict(l6="OTB_CRU_32-69_0_0", w=4, season=(5, 0.3), cpue=40, price=9.0,
         countries=dict(NO=3, SE=1.5, DK=2, IS=0.6),
         grounds=dict(norw_deep=3, skagerrak=2, barents=1, iceland_n=0.4),
         vlen=vl(VL1215=0.5, VL1518=1, VL1824=1.5, VL2440=1.2)),
    dict(l6="OTB_DEF_>=105_1_120", w=4, season=(2, 0.7), cpue=160, price=2.3,
         countries=dict(DK=2, SE=1.5, PL=2, DE=1.5, LT=0.6, LV=0.6, EE=0.2, FI=0.2),
         grounds=dict(baltic_w=2, baltic_c=3, gulf_riga=0.2),
         vlen=vl(VL1215=1.5, VL1518=1.5, VL1824=1.5, VL2440=0.8)),
    dict(l6="TBB_DEF_80-99_0_0", w=8, season=(5, 0.3), cpue=75, price=5.2,
         countries=dict(NL=6, BE=2.5, GB=1.2, DE=0.6),
         grounds=dict(s_northsea=4, dogger=1.5, channel_e=1, channel_w=1.2, c_northsea=1, celtic=0.6, irish_sea=0.3),
         vlen=vl(VL1824=0.5, VL2440=3.5, VL40XX=0.7)),
    dict(l6="TBB_CRU_16-31_0_0", w=4, season=(9, 0.8), cpue=30, price=4.0,
         countries=dict(NL=3, DE=3, DK=1),
         grounds=dict(wadden=1),
         vlen=vl(VL1215=2, VL1518=2, VL1824=1.5)),
    dict(l6="OTM_SPF_32-69_0_0", w=6, season=(10, 1.2), cpue=4200, price=0.75,
         countries=dict(NO=3, NL=2, DE=1, FR=1, IE=1.2, DK=1.2, GB=1.2, FO=1, IS=1.2),
         grounds=dict(norw_sea=4, n_northsea=1.5, west_scot=1, w_ireland=1, iceland_s=0.6, faroe=0.8, channel_e=0.3),
         vlen=vl(VL40XX=3, VL2440=0.6)),
    dict(l6="PS_SPF_>=0_0_0", w=4, season=(11, 1.4), cpue=9000, price=0.7,
         countries=dict(NO=6, IS=2, FO=1, DK=0.8, GB=0.6),
         grounds=dict(norw_coast=2, mid_norway=1.5, lofoten=1, norw_sea=1.5, n_northsea=1, iceland_n=0.6, iceland_w=0.5, faroe=0.3),
         vlen=vl(VL2440=1, VL40XX=3, VL1824=0.2)),
    dict(l6="PTM_SPF_16-31_0_0", w=5, season=(2, 0.5), cpue=2500, price=0.3,
         countries=dict(PL=2, LV=2, LT=1, EE=1.5, FI=2, SE=1.5, DK=1, DE=0.5),
         grounds=dict(baltic_c=3, bothnia=1.5, gulf_finland=1.2, gulf_riga=0.8, baltic_w=0.5),
         vlen=vl(VL1215=0.4, VL1518=0.8, VL1824=1.2, VL2440=1.8, VL40XX=0.4)),
    dict(l6="SDN_DEM_>=120_0_0", w=3.5, season=(7, 0.4), cpue=230, price=2.6,
         countries=dict(DK=3, NO=1.5, GB=0.6, NL=0.6, SE=0.3),
         grounds=dict(c_northsea=2, n_northsea=1, skagerrak=1.5, kattegat=0.5, norw_coast=0.6),
         vlen=vl(VL1215=1, VL1518=1.5, VL1824=1.5, VL2440=0.7)),
    dict(l6="SSC_DEM_>=120_0_0", w=2.5, season=(6, 0.4), cpue=180, price=2.4,
         countries=dict(GB=2, FR=1, NL=1.2, IE=0.6, BE=0.2, NO=0.3),
         grounds=dict(n_northsea=1.5, channel_e=1.5, channel_w=0.8, celtic=0.5, irish_sea=0.2),
         vlen=vl(VL1824=1.5, VL2440=1.5)),
    dict(l6="DRB_MOL_>=0_0_0", w=3.5, season=(12, 0.6), cpue=130, price=3.1,
         countries=dict(FR=3, GB=3, IE=1, BE=0.2),
         grounds=dict(channel_e=2, channel_w=1.5, irish_sea=1.2, west_scot=0.6, celtic=0.4),
         vlen=vl(VL1215=2, VL1518=1.5, VL1824=1.2, VL2440=0.6)),
    dict(l6="LLS_DEF_0_0_0", w=6, season=(2, 0.5), cpue=140, price=3.4,
         countries=dict(NO=5, IS=3, FO=1.2, ES=1.2, FR=0.4, GB=0.3),
         grounds=dict(lofoten=2, finnmark=2, barents=1.2, norw_coast=1.2, mid_norway=1, iceland_s=1.6,
                      iceland_w=1, faroe=1, rockall=0.6, hatton=0.4, porcupine=0.5, biscay=0.3),
         vlen=vl(VL1215=0.5, VL1518=1, VL1824=1.2, VL2440=1.5, VL40XX=0.6)),
    dict(l6="GNS_DEF_120-219_0_0", w=4.5, season=(3, 0.6), cpue=60, price=3.0,
         countries=dict(NO=3, DK=1.5, PL=1, FR=1, ES=0.8, PT=0.8, DE=0.5, SE=0.4, GB=0.5, IE=0.3, EE=0.3, FI=0.2),
         grounds=dict(norw_coast=1.5, lofoten=1.2, finnmark=1, skagerrak=0.8, baltic_c=1, baltic_w=0.6,
                      channel_w=0.8, celtic=0.4, iberia_n=0.8, iberia_w=0.8, biscay=0.6),
         vlen=vl(VL1215=2, VL1518=1.5, VL1824=1, VL2440=0.3)),
    dict(l6="FPO_CRU_0_0_0", w=2.5, season=(8, 0.6), cpue=22, price=6.5,
         countries=dict(GB=3, IE=1.5, NO=0.6, FR=0.8, SE=0.3, DK=0.3),
         grounds=dict(channel_w=1, west_scot=1.5, irish_sea=0.6, n_northsea=0.6, w_ireland=0.6, norw_coast=0.3, farn=0.4),
         vlen=vl(VL1215=2, VL1518=1.2, VL1824=0.4)),
    dict(l6="OTB_MDD_>=55_0_0", w=6, season=(4, 0.3), cpue=110, price=3.6,
         countries=dict(ES=4, PT=1.5, FR=2),
         grounds=dict(biscay=2.5, iberia_n=2, iberia_w=1.5, gulf_cadiz=1, celtic=0.4, porcupine=0.6),
         vlen=vl(VL1518=0.6, VL1824=1.5, VL2440=2, VL40XX=0.3)),
    dict(l6="OTB_DWS_>=100_0_0", w=1.2, season=(6, 0.2), cpue=90, price=4.5,
         countries=dict(FR=2, ES=1.5, FO=0.4, IS=0.3),
         grounds=dict(hatton=1.5, rockall=1, porcupine=1),
         vlen=vl(VL2440=1, VL40XX=1.5)),
]

# Logbook-only coastal fleet (vessels under the VMS threshold)
SMALL_FISHERIES = [
    dict(l6="GNS_DEF_120-219_0_0", w=4, countries=dict(NO=5, DK=2, FR=1.5, PT=2, ES=1.5, PL=1, SE=1, FI=0.6, EE=0.6, GB=0.8, IE=0.5, DE=0.5, LV=0.4, LT=0.3),
         grounds=dict(norw_coast=2, mid_norway=1.5, lofoten=1, finnmark=1.2, skagerrak=1, kattegat=0.8, baltic_c=1, baltic_w=0.8,
                      gulf_finland=0.5, bothnia=0.6, channel_w=0.6, iberia_n=1, iberia_w=1.2, biscay=0.6), season=(3, 0.6), cpue=25, price=3.2),
    dict(l6="FPO_CRU_0_0_0", w=4, countries=dict(GB=5, IE=2, FR=1.5, NO=1, SE=0.5, DK=0.4),
         grounds=dict(channel_w=1.5, west_scot=1.5, irish_sea=0.8, n_northsea=0.6, w_ireland=1, farn=0.8, norw_coast=0.5, skagerrak=0.4),
         season=(8, 0.6), cpue=12, price=7.0),
    dict(l6="LLS_DEF_0_0_0", w=2.5, countries=dict(NO=5, IS=3, FO=0.5, PT=0.6, ES=0.5),
         grounds=dict(lofoten=1.5, finnmark=1.5, mid_norway=0.8, iceland_s=1, iceland_w=1, iceland_n=0.8, iberia_w=0.4), season=(3, 0.7), cpue=60, price=3.4),
    dict(l6="OTB_CRU_70-99_0_0", w=1.5, countries=dict(GB=3, IE=1, SE=0.5, DK=0.5),
         grounds=dict(west_scot=1.5, irish_sea=1, farn=1, kattegat=0.5), season=(6, 0.5), cpue=25, price=7.5),
]

EVENTS_PER_ROW = 2.3  # effort events generated per target Table 1 row

# preferred depth window (m) per metier (L6) or gear (L4); None = pelagic/no preference
DEPTH_PREF = {
    "OTB_DEF_>=120_0_0": (60, 450), "OTB_CRU_70-99_0_0": (60, 220), "OTB_CRU_32-69_0_0": (140, 520),
    "OTB_DEF_>=105_1_120": (30, 110), "OTB_MDD_>=55_0_0": (90, 550), "OTB_DWS_>=100_0_0": (550, 1600),
    "TBB_DEF_80-99_0_0": (18, 70), "TBB_CRU_16-31_0_0": (4, 22), "SDN": (30, 160), "SSC": (35, 160),
    "DRB": (15, 65), "LLS": (90, 700), "GNS": (10, 160), "FPO": (5, 90),
    "OTM": None, "PTM": None, "PS": None,
}

WIND_FARMS = [  # (lon0, lon1, lat0, lat1, from_year)  -> all fishing excluded
    (1.75, 2.65, 53.72, 54.10, 2019),  # Hornsea-like
    (2.95, 3.30, 51.58, 51.78, 2021),  # Borssele-like
    (1.85, 2.80, 54.80, 55.18, 2024),  # Dogger Bank wind-like
    (7.55, 8.00, 56.20, 56.42, 2024),  # Thor-like
    (2.40, 3.00, 51.95, 52.25, 2023),  # Hollandse Kust-like
]
BOTTOM_CLOSURES = [  # mobile bottom gears excluded
    (1.50, 3.00, 54.40, 55.50, 2022),  # Dogger Bank SAC-like
    (-4.60, -3.90, 54.00, 54.50, 2023),  # small Irish Sea MPA-like
]

INJECTED_ISSUES = [
    {"country": "NO", "years": "all", "field": "AverageGearWidth", "severity": "Critical",
     "issue": "Gear widths for mobile bottom gears reported in metres instead of km (x1000)."},
    {"country": "NO", "years": "2022-2025", "field": "AverageFishingSpeed", "severity": "Major",
     "issue": "~4% of records carry steaming speeds (9-16 kn) misclassified as fishing."},
    {"country": "NO", "years": "all", "field": "AverageFishingSpeed", "severity": "Minor",
     "issue": "SDN/SSC fishing speeds centred at ~3 kn instead of near zero."},
    {"country": "DK", "years": "2021", "field": "kWFishingHour", "severity": "Major",
     "issue": "kW-hours inflated x10 for one year (unit discontinuity)."},
    {"country": "FR", "years": "2019", "field": "TotValue", "severity": "Major",
     "issue": "Value not submitted for one year (all NA)."},
    {"country": "ES", "years": "2024", "field": "C-square", "severity": "Major",
     "issue": "~0.5% of records located outside the ICES area (NAFO, 46-50W)."},
    {"country": "PT", "years": "2023", "field": "AverageVesselLength", "severity": "Minor",
     "issue": "Some vessel lengths reported in feet (x3.28) -> >144 m vessels."},
    {"country": "IE", "years": "2017", "field": "AverageInterval", "severity": "Minor",
     "issue": "VMS interval reported in minutes instead of hours."},
]


# --------------------------------------------------------------------------- helpers
def csquare(lon, lat):
    """0.05 degree C-square codes (e.g. '1500:361:134:3'). Memory-light."""
    lon = np.asarray(lon, dtype=float)
    lat = np.asarray(lat, dtype=float)
    q = np.where(lat >= 0, np.where(lon >= 0, 1, 7), np.where(lon >= 0, 3, 5))
    alat = np.abs(lat) + 1e-9
    alon = np.abs(lon) + 1e-9
    lat10, lon10 = (alat // 10).astype(int), (alon // 10).astype(int)
    lat1, lon1 = (alat % 10 // 1).astype(int), (alon % 10 // 1).astype(int)
    lat01, lon01 = ((alat * 10) % 10 // 1).astype(int), ((alon * 10) % 10 // 1).astype(int)
    q1 = 2 * (lat1 >= 5) + (lon1 >= 5) + 1
    q01 = 2 * (lat01 >= 5) + (lon01 >= 5) + 1
    q005 = 2 * (((alat * 100) % 10) >= 5) + (((alon * 100) % 10) >= 5) + 1
    # pack into one integer per cell, then format once per distinct cell
    code = ((((((q * 10 + lat10) * 100 + lon10) * 1000 + q1 * 100 + lat1 * 10 + lon1) * 1000
             + q01 * 100 + lat01 * 10 + lon01) * 10) + q005).astype(np.int64)
    uniq, inv = np.unique(code, return_inverse=True)
    def f(c):
        s = f"{c:011d}"
        return f"{s[0:4]}:{s[4:7]}:{s[7:10]}:{s[10]}"
    return np.array([f(int(c)) for c in uniq], dtype=object)[inv]


def ices_rect(lon, lat):
    """ICES statistical rectangle codes (e.g. '47F2')."""
    lon = np.asarray(lon, dtype=float)
    lat = np.asarray(lat, dtype=float)
    row = np.floor((lat - 36.0) * 2).astype(int) + 1
    letters = np.array(list("ABCDEFGHJKLM"))
    digit = np.where(lon < -40, np.floor(lon + 44).astype(int) % 10, np.floor(lon).astype(int) % 10)
    li = np.where(lon < -40, 0, np.floor((lon + 40) / 10).astype(int) + 1)
    li = np.clip(li, 0, len(letters) - 1)
    rows = np.char.zfill(np.clip(row, 0, 99).astype(str), 2)
    return np.char.add(np.char.add(rows, letters[li]), digit.astype(str))


def morton(ix, iy):
    def spread(v):
        v = v.astype(np.uint64) & 0xFFFF
        v = (v | (v << 8)) & 0x00FF00FF
        v = (v | (v << 4)) & 0x0F0F0F0F
        v = (v | (v << 2)) & 0x33333333
        v = (v | (v << 1)) & 0x55555555
        return v
    return spread(ix) | (spread(iy) << np.uint64(1))


def build_environment(rng):
    """Land mask, depth (m) and habitat per grid cell."""
    import global_land_mask as glm
    lon_c = LON0 + (np.arange(NX) + 0.5) * RES
    lat_c = LAT0 + (np.arange(NY) + 0.5) * RES
    LON, LAT = np.meshgrid(lon_c, lat_c)
    land = glm.is_land(LAT, LON)
    # distance to land in km (anisotropic sampling at ~60N)
    dist = ndimage.distance_transform_edt(~land, sampling=(5.55, 5.55 * math.cos(math.radians(58))))
    noise = ndimage.gaussian_filter(rng.normal(size=land.shape), 12)
    noise = noise / (noise.std() + 1e-9)
    depth = 12 + 2.6 * dist ** 0.97
    # broad shelf seas
    ns = (LON > -3.5) & (LON < 9.5) & (LAT > 50.8) & (LAT < 59.5)
    depth = np.where(ns, np.minimum(depth, 22 + (LAT - 50.8) * 13 + 8 * noise), depth)
    trench = (LON > 1.5) & (LON < 11) & (LAT > 57.4) & (LAT < 62.5) & (dist < 75)
    depth = np.where(trench & ~((LAT < 58.5) & (dist > 55)), np.maximum(depth, 240 + 70 * noise), depth)
    baltic = (LON > 9.8) & (LAT > 53.3) & (LAT < 66.5)
    depth = np.where(baltic, np.minimum(depth, 18 + 1.4 * dist ** 0.9 + 6 * noise), depth)
    channel = (LON > -6) & (LON < 2.5) & (LAT > 48.5) & (LAT < 51.3)
    depth = np.where(channel, np.minimum(depth, 30 + 0.6 * dist + 10 * noise), depth)
    barents = (LON > 15) & (LAT > 70.5)
    depth = np.where(barents, np.clip(depth, 0, 230 + 60 * noise), depth)
    iceshelf = (LON < -12) & (LON > -28) & (LAT > 62.5) & (LAT < 67.5) & (dist < 120)
    depth = np.where(iceshelf, np.minimum(depth, 40 + 1.4 * dist + 30 * noise), depth)
    depth = np.clip(depth + 5 * noise, 2, 4500)
    depth[land] = 0

    # habitat (MSFD broad benthic habitat type), stored as small integer codes
    sed_field = ndimage.gaussian_filter(rng.normal(size=land.shape), 5)
    q = np.quantile(sed_field[~land], [0.35, 0.6, 0.78, 0.92])
    sed = np.digitize(sed_field, q).astype(np.uint8)  # 0..4
    zone = np.digitize(depth, [10, 60, 200, 1100, 2700]).astype(np.uint8)  # 0..5
    hab = (zone * 5 + sed).astype(np.uint8)
    return land, depth.astype(np.float32), hab


_ZONES = ["Infralittoral", "Circalittoral", "Offshore circalittoral", "Upper bathyal", "Lower bathyal", "Abyssal"]
_SEDS = ["sand", "mud", "coarse sediment", "mixed sediment", "rock and biogenic reef"]
HAB_NAMES = np.array([
    (z + (" rock and biogenic reef" if s_ == "rock and biogenic reef" else " sediment")) if zi >= 3 else f"{z} {s_}"
    for zi, z in enumerate(_ZONES) for s_ in _SEDS])

DEPTH_BINS = [0, 30, 60, 100, 200, 500, 1000, 1e9]
DEPTH_LBL = np.array(["0-30", "30-60", "60-100", "100-200", "200-500", "500-1000", ">1000"])


def season_probs(peak, conc):
    m = np.arange(1, 13)
    p = np.exp(conc * np.cos(2 * np.pi * (m - peak) / 12))
    return p / p.sum()


# --------------------------------------------------------------------------- core
class Sim:
    def __init__(self, years, rows_per_year, seed, out):
        self.rng = np.random.default_rng(seed)
        self.years = years
        self.rows_per_year = rows_per_year
        self.out = out
        t = time.time()
        self.land, self.depth, self.hab = build_environment(self.rng)
        print(f"environment grid {NX}x{NY} in {time.time() - t:.1f}s")
        self._make_trends()
        self._make_fleets()

    # year-to-year multipliers per (fishery, country)
    def _make_trends(self):
        self.trend = {}
        ny = len(self.years)
        for f in FISHERIES + SMALL_FISHERIES:
            for c in f["countries"]:
                walk = np.exp(np.cumsum(self.rng.normal(0, 0.07, ny)))
                walk /= walk.mean()
                self.trend[(f["l6"], c, "small" if f in SMALL_FISHERIES else "vms")] = walk

    def _make_fleets(self):
        """Vessel pools per country x L4 x length class (distinct-vessel counts)."""
        self.fleet = {}
        counter = {c: 0 for c in COUNTRIES}
        for f in FISHERIES + SMALL_FISHERIES:
            l4 = f["l6"].split("_")[0]
            for c, w in f["countries"].items():
                for vi, v in enumerate(VLEN):
                    key = (c, l4, v)
                    if key in self.fleet:
                        continue
                    n = int(max(2, round(w * (12 if vi >= 4 else 30) * self.rng.uniform(0.6, 1.4))))
                    ids = [f"{c}{counter[c] + i:05d}" for i in range(n)]
                    counter[c] += n
                    lo, hi = VLEN_LEN[v]
                    lens = self.rng.uniform(lo, hi, n)
                    kw = np.clip(9.5 * lens ** 1.45 * self.rng.lognormal(0, 0.18, n), 15, 9000)
                    self.fleet[key] = (np.array(ids), lens.astype(np.float32), kw.astype(np.float32))

    # ---- sample positions on the grid for a fishery, rejecting land/closures
    def _patch_fields(self):
        """Fine-scale patchiness: real VMS footprints are streaky, not Gaussian."""
        if hasattr(self, "_patch"):
            return self._patch
        fields = []
        for k in range(4):
            a = ndimage.gaussian_filter(self.rng.normal(size=self.land.shape), 1.6)
            b = ndimage.gaussian_filter(self.rng.normal(size=self.land.shape), 5 + 2 * k)
            f = 0.55 * a / a.std() + 1.0 * b / b.std()
            fields.append(np.exp(0.9 * f).astype(np.float32))
        self._patch = [f / np.quantile(f, 0.98) for f in fields]
        self._mud = (self.hab % 5 == 1) & (self.hab < 15)
        return self._patch

    def _positions(self, grounds, n, year, l6, coastal=False):
        l4 = l6.split("_")[0]
        pref = DEPTH_PREF.get(l6, DEPTH_PREF.get(l4))
        patch = self._patch_fields()[sum(map(ord, l4)) % 4]
        names = list(grounds)
        w = np.array([grounds[g] for g in names], float)
        w /= w.sum()
        out_x, out_y, got = [], [], 0
        for attempt in range(8):
            m = int((n - got) * 3.5) + 200
            g_idx = self.rng.choice(len(names), size=m, p=w)
            gname = np.array(names)[g_idx]
            cx = np.array([GROUNDS[g][0] for g in gname])
            cy = np.array([GROUNDS[g][1] for g in gname])
            sx = np.array([GROUNDS[g][2] for g in gname])
            sy = np.array([GROUNDS[g][3] for g in gname])
            # mackerel/herring drift north-west over time in the Norwegian Sea
            drift = (gname == "norw_sea") & (l4 in ("OTM", "PS"))
            k = year - self.years[0]
            cx = np.where(drift, cx - 0.55 * k, cx)
            cy = np.where(drift, cy + 0.22 * k, cy)
            halo = self.rng.random(m) < 0.3
            lon = cx + self.rng.normal(size=m) * sx * np.where(halo, 2.0, 0.9)
            lat = cy + self.rng.normal(size=m) * sy * np.where(halo, 2.0, 0.9)
            ix = np.floor((lon - LON0) / RES).astype(int)
            iy = np.floor((lat - LAT0) / RES).astype(int)
            ok = (ix >= 0) & (ix < NX) & (iy >= 0) & (iy < NY) & (lon > -44) & (lon < 30) & (lat > 35)
            ix, iy = ix[ok], iy[ok]
            ok = ~self.land[iy, ix]
            ix, iy = ix[ok], iy[ok]
            d = self.depth[iy, ix]
            acc = patch[iy, ix].copy()
            if pref is not None:
                lo, hi = pref
                mid, half = (math.log(lo) + math.log(hi)) / 2, (math.log(hi) - math.log(lo)) / 2
                z = (np.log(np.maximum(d, 1)) - mid) / max(half, 0.2)
                acc *= np.exp(-0.5 * np.maximum(np.abs(z) - 0.6, 0) ** 2 * 4)
            if coastal:
                acc *= np.exp(-d / 60)
            if l6.startswith("OTB_CRU_70"):
                acc *= np.where(self._mud[iy, ix], 1.0, 0.18)
            keep = self.rng.random(len(ix)) < np.clip(acc, 0, 1)
            ix, iy = ix[keep], iy[keep]
            clon = LON0 + (ix + 0.5) * RES
            clat = LAT0 + (iy + 0.5) * RES
            keep = np.ones(len(ix), bool)
            for x0, x1, y0, y1, yr in WIND_FARMS:
                if year >= yr:
                    keep &= ~((clon > x0) & (clon < x1) & (clat > y0) & (clat < y1))
            if l4 in BOTTOM_MOBILE:
                for x0, x1, y0, y1, yr in BOTTOM_CLOSURES:
                    if year >= yr:
                        keep &= ~((clon > x0) & (clon < x1) & (clat > y0) & (clat < y1))
            ix, iy = ix[keep], iy[keep]
            out_x.append(ix); out_y.append(iy)
            got += len(ix)
            if got >= n:
                break
        ix, iy = np.concatenate(out_x)[:n], np.concatenate(out_y)[:n]
        return ix, iy

    def _events(self, fisheries, year, total, small=False):
        """Generate effort events for one year. Returns dict of numpy columns."""
        cols = {k: [] for k in ["country", "l6", "vlen", "vessel", "ix", "iy", "month", "hours",
                                "speed", "kw", "len", "kg", "eur", "width", "interval"]}
        wsum = sum(f["w"] for f in fisheries)
        yi = self.years.index(year)
        for f in fisheries:
            l6 = f["l6"]
            l4 = l6.split("_")[0]
            spd_m, spd_sd, width = GEAR[l4]
            cw = f["countries"]
            for c, w in cw.items():
                tr = self.trend[(l6, c, "small" if small else "vms")][yi]
                # policy shocks
                if l6 == "OTB_DEF_>=105_1_120" and year >= 2020:
                    tr *= 0.12
                if l4 == "TBB" and l6.startswith("TBB_DEF"):
                    tr *= (1 - 0.035 * (year - self.years[0]))
                    if c == "NL" and year >= 2021:
                        tr *= 0.8
                if year >= 2021 and c == "GB":
                    tr *= 1.08
                n = int(total * f["w"] / wsum * w / sum(cw.values()) * tr)
                if n <= 0:
                    continue
                ix, iy = self._positions(country_grounds(f["grounds"], c, l4), n, year, l6, coastal=small)
                n = len(ix)
                if n == 0:
                    continue
                # Effort is heavy-tailed: fleets return to the same tows/cells. Re-use a
                # subset of positions with Zipf-like weights so several vessels share a
                # c-square-month (realistic NoDistinctVessels and hot spots).
                m_uniq = max(1, int(n * 0.12))
                wz = 1.0 / np.arange(1, m_uniq + 1) ** 0.9
                pick = self.rng.choice(m_uniq, size=n, p=wz / wz.sum())
                ix, iy = ix[pick], iy[pick]
                # Brexit-ish: EU vessels fish less in the GB-heavy western/northern grounds
                if year >= 2021 and c in EU_BOARDERS:
                    clon = LON0 + (ix + 0.5) * RES
                    clat = LAT0 + (iy + 0.5) * RES
                    gbz = (clat > 54.5) & (clon < 1.5) & (clon > -10)
                    keep = ~gbz | (self.rng.random(n) > 0.35)
                    ix, iy = ix[keep], iy[keep]
                    n = len(ix)
                months = self.rng.choice(np.arange(1, 13), size=n, p=season_probs(*f["season"]))
                if year == 2020:  # covid dip spring 2020
                    drop = np.isin(months, [3, 4, 5]) & (self.rng.random(n) < 0.3)
                    keepm = ~drop
                    ix, iy, months = ix[keepm], iy[keepm], months[keepm]
                    n = len(ix)
                if small:
                    vprob = np.zeros(len(VLEN))
                    top = VMS_MIN_IDX[c]
                    vprob[:top] = np.linspace(1.6, 0.6, top)
                    vprob /= vprob.sum()
                else:
                    vprob = f["vlen"].copy()
                    vprob[:VMS_MIN_IDX[c]] = 0
                    if vprob.sum() == 0:
                        continue
                    vprob /= vprob.sum()
                vls = self.rng.choice(len(VLEN), size=n, p=vprob)
                vessel = np.empty(n, dtype=object)
                kw = np.empty(n, np.float32)
                ln = np.empty(n, np.float32)
                for vi in np.unique(vls):
                    m = vls == vi
                    ids, lens, kws = self.fleet[(c, l4, VLEN[vi])]
                    pick = self.rng.integers(0, len(ids), m.sum())
                    vessel[m], ln[m], kw[m] = ids[pick], lens[pick], kws[pick]
                hours = self.rng.lognormal(math.log(2.4 if not small else 4.5), 0.85, n).astype(np.float32)
                speed = np.clip(self.rng.normal(spd_m, spd_sd, n), 0.05, None).astype(np.float32)
                depth = self.depth[iy, ix]
                # catch rate: depends on ground richness (smooth field) and depth suitability
                rich = np.exp(0.35 * np.sin(ix / 37.0) * np.cos(iy / 23.0))
                kg = (hours * f["cpue"] * rich * self.rng.lognormal(0, 0.55, n)).astype(np.float32)
                eur = (kg * f["price"] * self.rng.lognormal(0, 0.15, n) * (1 + 0.025 * (year - 2013))).astype(np.float32)
                wd = np.full(n, np.nan, np.float32) if width is None else \
                    (width * self.rng.lognormal(0, 0.15, n)).astype(np.float32)
                cols["country"].append(np.full(n, c))
                cols["l6"].append(np.full(n, l6))
                cols["vlen"].append(np.array(VLEN)[vls])
                cols["vessel"].append(vessel.astype(str))
                cols["ix"].append(ix.astype(np.int32))
                cols["iy"].append(iy.astype(np.int32))
                cols["month"].append(months.astype(np.int8))
                cols["hours"].append(hours)
                cols["speed"].append(speed)
                cols["kw"].append(kw)
                cols["len"].append(ln)
                cols["kg"].append(kg)
                cols["eur"].append(eur)
                cols["width"].append(wd)
                cols["interval"].append(np.full(n, PING_H[c], np.float32))
        return {k: np.concatenate(v) for k, v in cols.items()}

    # ---- inject the QC issues
    def _inject(self, ev, year):
        r = self.rng
        c = ev["country"]
        l4 = np.char.partition(ev["l6"].astype(str), "_")[:, 0]
        no = c == "NO"
        # NO: gear width in metres
        m = no & np.isin(l4, list(BOTTOM_MOBILE))
        ev["width"][m] *= 1000
        # NO: seines centred ~3 kn
        m = no & np.isin(l4, ["SDN", "SSC"])
        ev["speed"][m] = np.clip(r.normal(3.0, 0.8, m.sum()), 0.2, None)
        # NO: steaming speeds in 2022+
        if year >= 2022:
            m = no & (r.random(len(c)) < 0.04)
            ev["speed"][m] = r.uniform(9, 16, m.sum())
        # IE 2017 interval in minutes
        if year == 2017:
            ev["interval"][c == "IE"] *= 60
        # PT 2023 vessel length in feet for some records
        if year == 2023:
            m = (c == "PT") & (r.random(len(c)) < 0.15)
            ev["len"][m] *= 3.28
        # ES 2024 positions in NAFO
        if year == 2024:
            m = np.where((c == "ES"))[0]
            pick = r.choice(m, size=max(1, int(len(m) * 0.005)), replace=False)
            ev["ix"][pick] = r.integers(0, int((-46 - LON0) / RES), len(pick))
            ev["iy"][pick] = r.integers(int((43 - LAT0) / RES), int((47.5 - LAT0) / RES), len(pick))
        return ev

    def run(self):
        import gc
        os.makedirs(self.out, exist_ok=True)
        stats = []
        for year in self.years:
            t0 = time.time()
            gc.collect()
            con = duckdb.connect()
            con.execute(f"SET threads={min(4, os.cpu_count() or 1)}")
            con.execute(f"SET memory_limit='{os.environ.get('SIM_DUCKDB_MEM', '1GB')}'")
            ev = self._events(FISHERIES, year, int(self.rows_per_year * EVENTS_PER_ROW))
            ev = self._inject(ev, year)
            n = len(ev["ix"])
            # ~6% of VMS trips fail to link to logbooks -> logbook-only, VMSEnabled = N
            unlinked = self.rng.random(n) < 0.06
            # ~4% of linked trips lose their landings in Table 1 (no fishing pings
            # after activity filtering) although the logbook says VMSEnabled = Y
            dropped = (~unlinked) & (self.rng.random(n) < 0.04)
            sm = self._events(SMALL_FISHERIES, year, int(self.rows_per_year * EVENTS_PER_ROW * 0.26), small=True)

            def enrich(e):
                ix, iy = e["ix"], e["iy"]
                lon = (LON0 + (ix + 0.5) * RES).astype(np.float32)
                lat = (LAT0 + (iy + 0.5) * RES).astype(np.float32)
                e["lon"], e["lat"] = lon, lat
                inside = (ix >= 0) & (ix < NX) & (iy >= 0) & (iy < NY)
                d = np.where(inside, self.depth[np.clip(iy, 0, NY - 1), np.clip(ix, 0, NX - 1)], 3000)
                e["depth"] = DEPTH_LBL[np.digitize(d, DEPTH_BINS[1:-1])]
                e["habitat"] = HAB_NAMES[np.where(inside, self.hab[np.clip(iy, 0, NY - 1), np.clip(ix, 0, NX - 1)], 25)]
                e["rect"] = ices_rect(lon, lat)
                return e

            ev = enrich(ev)
            sm = enrich(sm)

            # ---------------- Table 1 (VMS) -------------------------------------
            vm = ~unlinked & ~dropped
            t1 = pa.table({
                "CountryCode": ev["country"][vm], "Month": ev["month"][vm], "ix": ev["ix"][vm], "iy": ev["iy"][vm],
                "lon": ev["lon"][vm], "lat": ev["lat"][vm], "MetierL6": ev["l6"][vm],
                "VesselLengthRange": ev["vlen"][vm], "HabitatType": ev["habitat"][vm], "DepthRange": ev["depth"][vm],
                "vessel": ev["vessel"][vm], "hours": ev["hours"][vm], "speed": ev["speed"][vm], "kw": ev["kw"][vm],
                "len": ev["len"][vm], "kg": ev["kg"][vm], "eur": ev["eur"][vm], "width": ev["width"][vm],
                "interval": ev["interval"][vm], "rect": ev["rect"][vm],
            })
            con.register("ev", t1)
            dk_mult = 10.0 if year == 2021 else 1.0
            # Two-step aggregation (per vessel, then per Table 1 key) instead of
            # COUNT(DISTINCT)/STRING_AGG(DISTINCT): cheaper and spill-safe.
            ie_fix = 60 if year == 2017 else 1
            agg = con.execute(f"""
                WITH e AS (
                  SELECT *, CASE WHEN isnan(width) THEN NULL ELSE width END AS w FROM ev
                ), v AS (
                  SELECT CountryCode, Month, ix, iy, lon, lat, rect, MetierL6, VesselLengthRange, HabitatType, DepthRange, vessel,
                         count(*) AS n_ev,
                         sum(greatest(1, round(hours / (interval / CASE WHEN CountryCode = 'IE' THEN {ie_fix} ELSE 1 END)))) AS recs,
                         sum(speed) AS s_speed, sum(hours) AS h, sum(interval) AS s_int, sum(len) AS s_len, sum(kw) AS s_kw,
                         sum(hours * kw) AS kwh, sum(hours * speed * 1.852 * (CASE WHEN w > 20 THEN w / 1000 ELSE w END)) AS sa,
                         sum(kg) AS kg, sum(eur) AS eur, sum(w) AS s_w, count(w) AS n_w
                  FROM e
                  GROUP BY CountryCode, Month, ix, iy, lon, lat, rect, MetierL6, VesselLengthRange, HabitatType, DepthRange, vessel
                )
                SELECT 'VE' AS RecordType, CountryCode, {year}::SMALLINT AS Year, Month::TINYINT AS Month,
                       ix::SMALLINT AS ix, iy::SMALLINT AS iy, lon, lat, rect AS ICESrectangle,
                       split_part(MetierL6,'_',1) AS MetierL4, split_part(MetierL6,'_',2) AS MetierL5, MetierL6,
                       VesselLengthRange, HabitatType, DepthRange,
                       sum(n_ev)::INTEGER AS _n,
                       sum(recs)::INTEGER AS NumberOfRecords,
                       (sum(s_speed) / sum(n_ev))::REAL AS AverageFishingSpeed,
                       sum(h)::REAL AS FishingHour,
                       (sum(s_int) / sum(n_ev))::REAL AS AverageInterval,
                       (sum(s_len) / sum(n_ev))::REAL AS AverageVesselLength,
                       (sum(s_kw) / sum(n_ev))::REAL AS AveragekW,
                       (sum(kwh) * CASE WHEN CountryCode = 'DK' THEN {dk_mult} ELSE 1 END)::REAL AS kWFishingHour,
                       sum(sa)::REAL AS SweptArea,
                       sum(kg)::REAL AS TotWeight,
                       (CASE WHEN CountryCode = 'FR' AND {year} = 2019 THEN NULL ELSE sum(eur) END)::REAL AS TotValue,
                       count(*)::SMALLINT AS NoDistinctVessels,
                       CASE WHEN count(*) < 3 THEN string_agg(vessel, ';' ORDER BY vessel) ELSE 'not_required' END AS AnonymizedVesselID,
                       (sum(s_w) / nullif(sum(n_w), 0))::REAL AS AverageGearWidth
                FROM v
                GROUP BY CountryCode, Month, ix, iy, lon, lat, rect, MetierL6, VesselLengthRange, HabitatType, DepthRange
            """).arrow()
            con.unregister("ev")
            # c-square strings + morton ordering key
            lon = agg.column("lon").to_numpy()
            lat = agg.column("lat").to_numpy()
            agg = agg.append_column("Csquare", pa.array(csquare(lon, lat)))
            mk = morton(agg.column("ix").to_numpy().astype(np.int64) % 65536, agg.column("iy").to_numpy().astype(np.int64))
            agg = agg.append_column("_z", pa.array(mk))
            agg = agg.drop(["_n"])
            con.register("t1", agg)
            # one Parquet file per submission: Year=YYYY/CountryCode=XX/  (a national
            # resubmission replaces exactly one folder), Morton-sorted inside
            for cc in sorted(set(agg.column("CountryCode").to_pylist())):
                d1 = os.path.join(self.out, "lake", "vms", f"Year={year}", f"CountryCode={cc}")
                os.makedirs(d1, exist_ok=True)
                con.execute(f"""
                    COPY (SELECT RecordType, Month, Csquare, ICESrectangle, ix, iy, lon, lat,
                                 MetierL4, MetierL5, MetierL6, VesselLengthRange, HabitatType, DepthRange,
                                 NumberOfRecords, AverageFishingSpeed, FishingHour, AverageInterval, AverageVesselLength,
                                 AveragekW, kWFishingHour, SweptArea, TotWeight, TotValue, NoDistinctVessels,
                                 AnonymizedVesselID, AverageGearWidth
                          FROM t1 WHERE CountryCode = '{cc}' ORDER BY _z)
                    TO '{d1}/part-0.parquet' (FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 8192)
                """)
            n1 = agg.num_rows
            con.unregister("t1")

            # ---------------- Table 2 (logbook) ---------------------------------
            def le_table(e, vms_flag):
                days = np.clip(e["hours"] / 14.0, 0.05, None).astype(np.float32)
                return pa.table({
                    "CountryCode": e["country"], "Month": e["month"], "ICESrectangle": e["rect"],
                    "MetierL6": e["l6"], "VesselLengthRange": e["vlen"], "VMSEnabled": vms_flag,
                    "vessel": e["vessel"], "days": days, "kw": e["kw"], "kg": e["kg"], "eur": e["eur"],
                })
            # logbooks report one rectangle per fishing day; ~14% of VMS effort falls
            # in a neighbouring rectangle to the one written in the logbook
            ev_le = dict(ev)
            mis = self.rng.random(n) < 0.14
            dlon = np.where(self.rng.random(n) < 0.5, 1.0, 0.0) * self.rng.choice([-1, 1], n)
            dlat = np.where(dlon == 0, 0.5, 0.0) * self.rng.choice([-1, 1], n)
            ev_le["rect"] = np.where(mis, ices_rect(ev["lon"] + dlon, ev["lat"] + dlat), ev["rect"])
            le1 = le_table(ev_le, np.where(unlinked, "N", "Y"))
            le2 = le_table(sm, np.full(len(sm["ix"]), "N"))
            con.register("le", pa.concat_tables([le1, le2]))
            con.execute(f"""
                CREATE OR REPLACE TEMP TABLE le_agg AS
                  WITH v AS (
                    SELECT CountryCode, Month, ICESrectangle, MetierL6, VesselLengthRange, VMSEnabled, vessel,
                           sum(days) AS days, sum(days * kw) AS kwd, sum(kg) AS kg, sum(eur) AS eur
                    FROM le GROUP BY CountryCode, Month, ICESrectangle, MetierL6, VesselLengthRange, VMSEnabled, vessel
                  )
                  SELECT 'LE' AS RecordType, CountryCode, Month::TINYINT AS Month, ICESrectangle,
                         split_part(MetierL6,'_',1) AS MetierL4, split_part(MetierL6,'_',2) AS MetierL5, MetierL6,
                         VesselLengthRange, VMSEnabled,
                         sum(days)::REAL AS FishingDays,
                         (sum(kwd) * CASE WHEN CountryCode = 'DK' THEN {dk_mult} ELSE 1 END)::REAL AS kWFishingDays,
                         sum(kg)::REAL AS TotWeight,
                         (CASE WHEN CountryCode = 'FR' AND {year} = 2019 THEN NULL ELSE sum(eur) END)::REAL AS TotValue,
                         count(*)::SMALLINT AS NoDistinctVessels,
                         CASE WHEN count(*) < 3 THEN string_agg(vessel, ';' ORDER BY vessel) ELSE 'not_required' END AS AnonymizedVesselID
                  FROM v GROUP BY CountryCode, Month, ICESrectangle, MetierL6, VesselLengthRange, VMSEnabled
            """)
            for cc in [r[0] for r in con.execute("SELECT DISTINCT CountryCode FROM le_agg ORDER BY 1").fetchall()]:
                d2 = os.path.join(self.out, "lake", "logbook", f"Year={year}", f"CountryCode={cc}")
                os.makedirs(d2, exist_ok=True)
                con.execute(f"""
                    COPY (SELECT * EXCLUDE (CountryCode) FROM le_agg WHERE CountryCode = '{cc}' ORDER BY ICESrectangle, Month)
                    TO '{d2}/part-0.parquet' (FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 16384)
                """)
            n2 = con.execute("SELECT count(*) FROM le_agg").fetchone()[0]
            con.unregister("le")
            con.close()
            del ev, sm, agg, t1, le1, le2, con
            stats.append(dict(year=year, vms_rows=n1, logbook_rows=n2))
            print(f"  {year}: table1 {n1:>9,} rows | table2 {n2:>8,} rows  ({time.time() - t0:.1f}s)", flush=True)

        meta = dict(
            generated=time.strftime("%Y-%m-%d %H:%M:%S"),
            grid=dict(res=RES, lon0=LON0, lat0=LAT0, nx=NX, ny=NY),
            years=self.years, stats=stats, injected_issues=INJECTED_ISSUES,
            wind_farms=WIND_FARMS, bottom_closures=BOTTOM_CLOSURES,
        )
        with open(os.path.join(self.out, "lake", "_meta.json"), "w") as fh:
            json.dump(meta, fh, indent=1)
        tot1 = sum(s["vms_rows"] for s in stats)
        tot2 = sum(s["logbook_rows"] for s in stats)
        print(f"done: {tot1:,} VMS rows, {tot2:,} logbook rows")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default="data")
    ap.add_argument("--years", default="2013-2025")
    ap.add_argument("--rows-per-year", type=int, default=int(os.environ.get("ROWS_PER_YEAR", 600_000)))
    ap.add_argument("--seed", type=int, default=20261006)
    a = ap.parse_args()
    y0, y1 = (int(x) for x in a.years.split("-"))
    Sim(list(range(y0, y1 + 1)), a.rows_per_year, a.seed, a.out).run()
