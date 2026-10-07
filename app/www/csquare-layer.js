/* ============================================================================
 * CSquareLayer — a canvas grid renderer for Leaflet.
 *
 * Why: drawing 100k+ c-squares as Leaflet polygons/SVG is slow (one DOM node or
 * path object per cell, plus GeoJSON parsing). Here the server ships three
 * base64 typed arrays (ix, iy: Int16; v: Float32) and the browser paints them
 * onto a single canvas with fillRect, batched by colour. Colour scale, palette
 * and opacity are applied client-side, so changing them never touches the server.
 * ==========================================================================*/
(function () {
  "use strict";

  // ---------------------------------------------------------------- palettes
  const PALETTES = {
    viridis: ["#440154", "#482878", "#3e4989", "#31688e", "#26828e", "#1f9e89", "#35b779", "#6ece58", "#b5de2b", "#fde725"],
    magma:   ["#000004", "#1c1044", "#4f127b", "#812581", "#b5367a", "#e55064", "#fb8761", "#fec287", "#fcfdbf"],
    inferno: ["#000004", "#1f0c48", "#550f6d", "#88226a", "#ba3655", "#e35933", "#f98e09", "#f9cb35", "#fcffa4"],
    ocean:   ["#0b1d3a", "#123f6b", "#13668f", "#1690a6", "#3bb6a8", "#86d39b", "#d4ea8b", "#fff6a8"],
    heat:    ["#fff5c0", "#ffd36b", "#ff9f40", "#f2622e", "#c92a3a", "#7f0d4a", "#3b0a45"],
    turbo:   ["#30123b", "#4662d7", "#36aaf9", "#1ae4b6", "#72fe5e", "#c8ef34", "#faba39", "#f66b19", "#ca2a04", "#7a0403"],
    div:     ["#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b"]
  };
  const NBIN = 256;

  function hexToRgb(h) {
    const n = parseInt(h.slice(1), 16);
    return [(n >> 16) & 255, (n >> 8) & 255, n & 255];
  }
  function buildLUT(name) {
    const stops = (PALETTES[name] || PALETTES.viridis).map(hexToRgb);
    const lut = new Array(NBIN);
    for (let i = 0; i < NBIN; i++) {
      const t = i / (NBIN - 1) * (stops.length - 1);
      const a = Math.floor(t), b = Math.min(a + 1, stops.length - 1), f = t - a;
      const c = [0, 1, 2].map(k => Math.round(stops[a][k] + (stops[b][k] - stops[a][k]) * f));
      lut[i] = `rgb(${c[0]},${c[1]},${c[2]})`;
    }
    return lut;
  }

  function b64ToTyped(b64, Type) {
    if (!b64) return new Type(0);
    const bin = atob(b64);
    const u8 = new Uint8Array(bin.length);
    for (let i = 0; i < bin.length; i++) u8[i] = bin.charCodeAt(i);
    return new Type(u8.buffer);
  }

  function quantile(sorted, p) {
    if (!sorted.length) return 0;
    const i = Math.min(sorted.length - 1, Math.max(0, Math.floor(p * (sorted.length - 1))));
    return sorted[i];
  }

  function fmt(v, unit) {
    if (v === null || v === undefined || !isFinite(v)) return "–";
    const a = Math.abs(v);
    let s;
    if (a >= 1e9) s = (v / 1e9).toFixed(2) + "B";
    else if (a >= 1e6) s = (v / 1e6).toFixed(2) + "M";
    else if (a >= 1e4) s = (v / 1e3).toFixed(1) + "k";
    else if (a >= 100) s = v.toFixed(0);
    else if (a >= 1) s = v.toFixed(1);
    else s = v.toPrecision(2);
    return unit ? s + " " + unit : s;
  }

  // ICES rectangle code from rectangle grid indices (origin -50E, 36N, 1 x 0.5 deg)
  function rectCode(ix, iy) {
    const lon = -50 + ix, row = iy + 1;
    let letter, digit;
    if (lon < -40) { letter = "A"; digit = ((Math.floor(lon + 44) % 10) + 10) % 10; }
    else { letter = "ABCDEFGHJKLM"[Math.floor((lon + 40) / 10) + 1]; digit = ((Math.floor(lon) % 10) + 10) % 10; }
    return String(row).padStart(2, "0") + letter + digit;
  }

  // ------------------------------------------------------------- the layer
  const CSquareLayer = L.Layer.extend({
    initialize: function (id) {
      this.id = id;
      this.data = null;        // current frame
      this.frames = null;      // time-lapse frames
      this.palette = "viridis";
      this.scale = "log";
      this.opacity = 0.85;
      this.overlays = { rects: false, area: true, closures: false };
      this.closures = [];
      this.selected = null;
      this._lut = buildLUT(this.palette);
      this._divLut = buildLUT("div");
    },

    onAdd: function (map) {
      this._map = map;
      if (!map.getPane("csq")) {
        const p = map.createPane("csq");
        p.style.zIndex = 450;
        p.style.pointerEvents = "none";
      }
      this._canvas = L.DomUtil.create("canvas", "csq-canvas leaflet-zoom-hide");
      map.getPane("csq").appendChild(this._canvas);
      this._ctx = this._canvas.getContext("2d");
      this._tip = L.DomUtil.create("div", "csq-tip", map.getContainer());
      map.on("moveend zoomend resize viewreset", this._reset, this);
      map.on("mousemove", this._hover, this);
      map.on("mouseout", () => { this._tip.style.display = "none"; }, this);
      map.on("click", this._click, this);
      this._reset();
    },

    onRemove: function (map) {
      L.DomUtil.remove(this._canvas);
      L.DomUtil.remove(this._tip);
      map.off("moveend zoomend resize viewreset", this._reset, this);
      map.off("mousemove", this._hover, this);
      map.off("click", this._click, this);
    },

    // ---- data ------------------------------------------------------------
    setData: function (msg) {
      const d = {
        x0: msg.x0, y0: msg.y0, dx: msg.dx, dy: msg.dy, kind: msg.kind || "csq",
        ix: b64ToTyped(msg.ix, Int16Array), iy: b64ToTyped(msg.iy, Int16Array), v: b64ToTyped(msg.v, Float32Array),
        label: msg.label, unit: msg.unit, mode: msg.mode || "seq", center: msg.center || 0,
        meta: msg.meta || {}
      };
      d.n = d.v.length;
      d.lookup = new Map();
      for (let i = 0; i < d.n; i++) d.lookup.set(d.ix[i] * 65536 + d.iy[i], i);
      this.frames = null;
      this.data = d;
      this._computeScale();
      this._reset();
    },

    setFrames: function (msg) {
      const fr = msg.frames.map(f => ({
        frameLabel: f.label, ix: b64ToTyped(f.ix, Int16Array), iy: b64ToTyped(f.iy, Int16Array), v: b64ToTyped(f.v, Float32Array)
      }));
      const common = { x0: msg.x0, y0: msg.y0, dx: msg.dx, dy: msg.dy, kind: msg.kind || "csq",
                       label: msg.label, unit: msg.unit, mode: "seq", center: 0, meta: {} };
      // one colour scale across all frames so change is visible
      let all = [];
      fr.forEach(f => { for (let i = 0; i < f.v.length; i += Math.max(1, Math.floor(f.v.length / 4000))) all.push(f.v[i]); });
      this._frameScaleSample = Float32Array.from(all);
      this.frames = fr.map(f => {
        const d = Object.assign({}, common, f);
        d.n = d.v.length;
        d.lookup = new Map();
        for (let i = 0; i < d.n; i++) d.lookup.set(d.ix[i] * 65536 + d.iy[i], i);
        return d;
      });
      this.showFrame(0);
    },

    showFrame: function (k) {
      if (!this.frames) return;
      this.frameIndex = k;
      this.data = this.frames[k];
      this._computeScale(this._frameScaleSample);
      this._reset();
    },

    // ---- colour scale ------------------------------------------------------
    _computeScale: function (sample) {
      const d = this.data;
      if (!d) return;
      const src = sample || d.v;
      const step = Math.max(1, Math.floor(src.length / 20000));
      const s = [];
      for (let i = 0; i < src.length; i += step) {
        const x = src[i];
        if (isFinite(x)) s.push(d.mode === "div" ? Math.abs(x - d.center) : x);
      }
      s.sort((a, b) => a - b);
      const pos = s.filter(x => x > 0);
      const sc = { sorted: s };
      if (d.mode === "div") {
        sc.m = quantile(s, 0.97) || 1;
        sc.k = sc.m / 12;
      } else if (this.scale === "log") {
        sc.lo = Math.max(quantile(pos, 0.02), 1e-9);
        sc.hi = Math.max(quantile(pos, 0.995), sc.lo * 1.0001);
      } else if (this.scale === "linear") {
        sc.lo = 0;
        sc.hi = quantile(s, 0.99) || 1;
      } else {
        sc.q = s;
      }
      this._sc = sc;
      // colour bin per cell, then bucket cells by bin (counting sort)
      const n = d.n, bins = new Uint8Array(n);
      for (let i = 0; i < n; i++) bins[i] = this._bin(d.v[i]);
      const counts = new Uint32Array(NBIN + 1);
      for (let i = 0; i < n; i++) counts[bins[i] + 1]++;
      for (let b = 0; b < NBIN; b++) counts[b + 1] += counts[b];
      const order = new Uint32Array(n), fill = counts.slice();
      for (let i = 0; i < n; i++) order[fill[bins[i]]++] = i;
      d.bins = bins; d.order = order; d.binStart = counts;
      this._legend();
    },

    _t: function (v) {
      const d = this.data, sc = this._sc;
      if (!isFinite(v)) return null;
      if (d.mode === "div") {
        const x = v - d.center;
        const t = Math.asinh(x / sc.k) / Math.asinh(sc.m / sc.k);
        return 0.5 + 0.5 * Math.max(-1, Math.min(1, t));
      }
      if (this.scale === "log") {
        if (v <= 0) return 0;
        return (Math.log(v) - Math.log(sc.lo)) / (Math.log(sc.hi) - Math.log(sc.lo));
      }
      if (this.scale === "linear") return (v - sc.lo) / (sc.hi - sc.lo);
      // quantile: binary search rank
      const q = sc.q; let lo = 0, hi = q.length - 1;
      while (lo < hi) { const mid = (lo + hi) >> 1; if (q[mid] < v) lo = mid + 1; else hi = mid; }
      return q.length > 1 ? lo / (q.length - 1) : 0.5;
    },
    _bin: function (v) {
      const t = this._t(v);
      if (t === null) return 0;
      return Math.max(0, Math.min(NBIN - 1, Math.round(t * (NBIN - 1))));
    },
    _inv: function (t) { // value at scale position t (for legend ticks)
      const d = this.data, sc = this._sc;
      if (d.mode === "div") return d.center + sc.k * Math.sinh((2 * t - 1) * Math.asinh(sc.m / sc.k));
      if (this.scale === "log") return Math.exp(Math.log(sc.lo) + t * (Math.log(sc.hi) - Math.log(sc.lo)));
      if (this.scale === "linear") return sc.lo + t * (sc.hi - sc.lo);
      return quantile(sc.q, t);
    },

    setStyle: function (o) {
      if (o.palette) { this.palette = o.palette; this._lut = buildLUT(o.palette); }
      if (o.opacity !== undefined) this.opacity = +o.opacity;
      if (o.overlays) Object.assign(this.overlays, o.overlays);
      if (o.scale && o.scale !== this.scale) { this.scale = o.scale; this._computeScale(this.frames ? this._frameScaleSample : null); }
      this._legend();
      this._draw();
    },

    // ---- rendering -----------------------------------------------------------
    _reset: function () {
      const map = this._map;
      if (!map || !this._canvas) return;
      const size = map.getSize(), dpr = window.devicePixelRatio || 1;
      const tl = map.containerPointToLayerPoint([0, 0]);
      L.DomUtil.setPosition(this._canvas, tl);
      this._canvas.style.width = size.x + "px";
      this._canvas.style.height = size.y + "px";
      if (this._canvas.width !== Math.round(size.x * dpr) || this._canvas.height !== Math.round(size.y * dpr)) {
        this._canvas.width = Math.round(size.x * dpr);
        this._canvas.height = Math.round(size.y * dpr);
      }
      this._ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      this._draw();
    },

    _proj: function () {
      // Web Mercator: x is linear in lon, y is cached per latitude row
      const map = this._map;
      const a = map.latLngToContainerPoint([0, 0]).x, b = map.latLngToContainerPoint([0, 1]).x;
      const ycache = new Map();
      return {
        x: lon => a + (b - a) * lon,
        y: lat => {
          let y = ycache.get(lat);
          if (y === undefined) { y = map.latLngToContainerPoint([Math.max(-85, Math.min(85, lat)), 0]).y; ycache.set(lat, y); }
          return y;
        }
      };
    },

    _draw: function () {
      const ctx = this._ctx, map = this._map;
      if (!ctx || !map) return;
      const t0 = performance.now();
      const size = map.getSize();
      ctx.clearRect(0, 0, size.x, size.y);
      const d = this.data;
      const P = this._proj();
      if (d && d.n) {
        ctx.globalAlpha = this.opacity;
        const lut = d.mode === "div" ? this._divLut : this._lut;
        const W = size.x, H = size.y;
        for (let b = 0; b < NBIN; b++) {
          const s = d.binStart[b], e = d.binStart[b + 1];
          if (s === e) continue;
          ctx.fillStyle = lut[b];
          for (let j = s; j < e; j++) {
            const i = d.order[j];
            const lon = d.x0 + d.ix[i] * d.dx, lat = d.y0 + d.iy[i] * d.dy;
            const x1 = P.x(lon), x2 = P.x(lon + d.dx);
            if (x2 < 0 || x1 > W) continue;
            const y1 = P.y(lat + d.dy), y2 = P.y(lat);
            if (y2 < 0 || y1 > H) continue;
            ctx.fillRect(x1, y1, Math.max(1, x2 - x1 + 0.35), Math.max(1, y2 - y1 + 0.35));
          }
        }
        ctx.globalAlpha = 1;
      }
      this._drawOverlays(P, size);
      this.lastDrawMs = performance.now() - t0;
      if (this._statusEl) this._updateStatus();
    },

    _drawOverlays: function (P, size) {
      const ctx = this._ctx, map = this._map, z = map.getZoom();
      const dark = document.documentElement.getAttribute("data-bs-theme") === "dark";
      ctx.save();
      if (this.overlays.rects && z >= 5) {
        const bnd = map.getBounds();
        ctx.strokeStyle = dark ? "rgba(255,255,255,0.18)" : "rgba(10,30,50,0.22)";
        ctx.lineWidth = 1;
        ctx.beginPath();
        for (let lon = Math.floor(bnd.getWest()); lon <= bnd.getEast(); lon++) { const x = Math.round(P.x(lon)) + 0.5; ctx.moveTo(x, 0); ctx.lineTo(x, size.y); }
        for (let lat = Math.floor(bnd.getSouth() * 2) / 2; lat <= bnd.getNorth(); lat += 0.5) { const y = Math.round(P.y(lat)) + 0.5; ctx.moveTo(0, y); ctx.lineTo(size.x, y); }
        ctx.stroke();
        if (z >= 6) {
          ctx.fillStyle = dark ? "rgba(255,255,255,0.45)" : "rgba(10,30,50,0.5)";
          ctx.font = "10px 'JetBrains Mono', ui-monospace, monospace";
          for (let lon = Math.floor(bnd.getWest()); lon <= bnd.getEast(); lon++)
            for (let lat = Math.floor(bnd.getSouth() * 2) / 2; lat <= bnd.getNorth(); lat += 0.5)
              if (lat >= 36) ctx.fillText(rectCode(lon + 50, Math.round((lat - 36) * 2)), P.x(lon) + 3, P.y(lat + 0.5) + 11);
        }
      }
      if (this.overlays.area) { // ICES data call area: 44W-30E, 35-90N
        ctx.setLineDash([6, 5]);
        ctx.strokeStyle = dark ? "rgba(255,200,90,0.75)" : "rgba(200,90,0,0.75)";
        ctx.lineWidth = 1.5;
        const x1 = P.x(-44), x2 = P.x(30), y1 = P.y(85), y2 = P.y(35);
        ctx.strokeRect(x1, y1, x2 - x1, y2 - y1);
        ctx.setLineDash([]);
      }
      if (this.overlays.closures && this.closures.length) {
        ctx.lineWidth = 1.5;
        this.closures.forEach(c => {
          const x1 = P.x(c[0]), x2 = P.x(c[1]), y1 = P.y(c[3]), y2 = P.y(c[2]);
          ctx.strokeStyle = c[5] === "wind" ? "#e040fb" : "#ff5252";
          ctx.fillStyle = c[5] === "wind" ? "rgba(224,64,251,0.10)" : "rgba(255,82,82,0.10)";
          ctx.fillRect(x1, y1, x2 - x1, y2 - y1);
          ctx.strokeRect(x1, y1, x2 - x1, y2 - y1);
          if (z >= 6) {
            ctx.fillStyle = ctx.strokeStyle;
            ctx.font = "11px Inter, sans-serif";
            ctx.fillText((c[5] === "wind" ? "Wind farm " : "Bottom-gear closure ") + c[4] + "+", x1 + 3, y1 - 4);
          }
        });
      }
      if (this.selected && this.data) {
        const s = this.selected;
        const x1 = P.x(s.lon0), x2 = P.x(s.lon0 + s.dx), y1 = P.y(s.lat0 + s.dy), y2 = P.y(s.lat0);
        ctx.strokeStyle = dark ? "#fff" : "#000";
        ctx.lineWidth = 2;
        ctx.strokeRect(x1 - 1, y1 - 1, x2 - x1 + 2, y2 - y1 + 2);
      }
      ctx.restore();
    },

    // ---- interaction ---------------------------------------------------------
    _cellAt: function (latlng) {
      const d = this.data;
      if (!d) return null;
      const ix = Math.floor((latlng.lng - d.x0) / d.dx), iy = Math.floor((latlng.lat - d.y0) / d.dy);
      const i = d.lookup.get(ix * 65536 + iy);
      return { ix, iy, i, lon0: d.x0 + ix * d.dx, lat0: d.y0 + iy * d.dy, dx: d.dx, dy: d.dy };
    },

    _hover: function (e) {
      const c = this._cellAt(e.latlng);
      const tip = this._tip;
      if (!c || c.i === undefined) { tip.style.display = "none"; this._map.getContainer().style.cursor = ""; return; }
      const d = this.data, v = d.v[c.i];
      const name = d.kind === "rect" ? "ICES rect " + rectCode(c.ix, c.iy)
        : `${c.lat0.toFixed(2)}°N ${c.lon0.toFixed(2)}°E · ${d.dx.toFixed(2)}° cell`;
      let val = fmt(v, d.unit);
      if (d.mode === "div") val = (v - d.center > 0 ? "+" : "") + fmt(v - d.center, d.unit);
      tip.innerHTML = `<div class="csq-tip-k">${name}</div><div class="csq-tip-v">${val}</div><div class="csq-tip-l">${d.label || ""}</div>`;
      tip.style.display = "block";
      const p = e.containerPoint;
      tip.style.left = (p.x + 14) + "px";
      tip.style.top = (p.y + 14) + "px";
      this._map.getContainer().style.cursor = "pointer";
    },

    _click: function (e) {
      const c = this._cellAt(e.latlng);
      if (!c || c.i === undefined) return;
      this.selected = c;
      this._draw();
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue(this.id + "_cell", {
          ix: c.ix, iy: c.iy, lon0: c.lon0, lat0: c.lat0, dx: c.dx, dy: c.dy, kind: this.data.kind,
          rect: this.data.kind === "rect" ? rectCode(c.ix, c.iy) : null, nonce: Date.now()
        }, { priority: "event" });
      }
    },

    // ---- legend + status -----------------------------------------------------
    attachUI: function (legendEl, statusEl) { this._legendEl = legendEl; this._statusEl = statusEl; this._legend(); },

    _legend: function () {
      const el = this._legendEl, d = this.data;
      if (!el || !d || !this._sc) return;
      const lut = d.mode === "div" ? this._divLut : this._lut;
      const stops = [];
      for (let i = 0; i <= 10; i++) stops.push(lut[Math.round(i / 10 * (NBIN - 1))] + " " + (i * 10) + "%");
      const ticks = [0, 0.25, 0.5, 0.75, 1].map(t => {
        let v = this._inv(t);
        if (d.mode === "div") v = v - d.center;
        return `<span>${d.mode === "div" && v > 0 ? "+" : ""}${fmt(v)}</span>`;
      }).join("");
      const sc = d.mode === "div" ? "diverging" : this.scale;
      el.innerHTML = `<div class="csq-leg-title">${d.label || ""} <span class="csq-leg-unit">${d.unit || ""} · ${sc}</span></div>
        <div class="csq-leg-bar" style="background:linear-gradient(90deg, ${stops.join(",")})"></div>
        <div class="csq-leg-ticks">${ticks}</div>`;
    },

    _updateStatus: function () {
      const d = this.data;
      if (!d) return;
      const m = d.meta || {};
      const parts = [
        `<b>${d.kind === "rect" ? "ICES rect" : (d.dx.toFixed(2) + "°")}</b>`,
        `${d.n.toLocaleString()} cells`,
        m.ms !== undefined ? `query ${m.cached ? "<span class='csq-hit'>cache</span>" : Math.round(m.ms) + " ms"}` : null,
        m.kb !== undefined ? `${m.kb} kB` : null,
        `draw ${this.lastDrawMs ? this.lastDrawMs.toFixed(0) : "–"} ms`
      ].filter(Boolean);
      if (this.frames) parts.push(`<b>${this.frames[this.frameIndex].frameLabel}</b>`);
      this._statusEl.innerHTML = parts.join(" · ");
    }
  });

  // ------------------------------------------------------- control panel UI
  function buildControls(layer, map, opts) {
    const ctl = L.control({ position: "topright" });
    ctl.onAdd = function () {
      const div = L.DomUtil.create("div", "csq-panel");
      const palOpts = Object.keys(PALETTES).filter(p => p !== "div").map(p => `<option value="${p}" ${p === layer.palette ? "selected" : ""}>${p}</option>`).join("");
      div.innerHTML = `
        <div class="csq-row"><label>Grid</label>
          <select data-k="res"><option value="auto">auto (zoom)</option><option value="1">0.05° c-square</option>
          <option value="2">0.1°</option><option value="5">0.25°</option><option value="10">0.5°</option><option value="20">1°</option></select></div>
        <div class="csq-row"><label>Scale</label>
          <select data-k="scale"><option value="log">log</option><option value="quantile">quantile</option><option value="linear">linear</option></select></div>
        <div class="csq-row"><label>Palette</label><select data-k="palette">${palOpts}</select></div>
        <div class="csq-row"><label>Opacity</label><input data-k="opacity" type="range" min="0.2" max="1" step="0.05" value="${layer.opacity}"></div>
        <div class="csq-row csq-checks">
          <label><input type="checkbox" data-o="area" checked> ICES area</label>
          <label><input type="checkbox" data-o="rects"> ICES rects</label>
          <label><input type="checkbox" data-o="closures"> Closures</label>
        </div>
        ${opts.timelapse ? `<div class="csq-row csq-btns">
          <button class="btn btn-sm btn-outline-primary" data-tl="yr" title="Animate year by year (all frames fetched in one query)">▶ Years</button>
          <button class="btn btn-sm btn-outline-primary" data-tl="mo" title="Animate the seasonal cycle">▶ Seasons</button></div>` : ""}`;
      if (!opts.grid) div.querySelector('[data-k="res"]').closest(".csq-row").remove();
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      div.addEventListener("change", e => {
        const k = e.target.dataset.k, o = e.target.dataset.o;
        if (k === "res") Shiny.setInputValue(layer.id + "_res", e.target.value);
        else if (k === "scale") layer.setStyle({ scale: e.target.value });
        else if (k === "palette") layer.setStyle({ palette: e.target.value });
        else if (o) layer.setStyle({ overlays: { [o]: e.target.checked } });
      });
      div.addEventListener("input", e => { if (e.target.dataset.k === "opacity") layer.setStyle({ opacity: e.target.value }); });
      div.addEventListener("click", e => {
        const by = e.target.dataset.tl;
        if (by) Shiny.setInputValue(layer.id + "_timelapse", { by: by, nonce: Date.now() }, { priority: "event" });
      });
      return div;
    };
    ctl.addTo(map);

    const leg = L.control({ position: "bottomright" });
    leg.onAdd = () => { const d = L.DomUtil.create("div", "csq-legend"); L.DomEvent.disableClickPropagation(d); return d; };
    leg.addTo(map);
    const st = L.control({ position: "bottomleft" });
    st.onAdd = () => L.DomUtil.create("div", "csq-status");
    st.addTo(map);
    layer.attachUI(leg.getContainer(), st.getContainer());

    // time-lapse player
    const player = L.DomUtil.create("div", "csq-player", map.getContainer());
    player.innerHTML = `<button class="btn btn-sm btn-primary" data-p="play">❚❚</button>
      <input type="range" min="0" max="0" value="0" step="1"><span class="csq-frame"></span>
      <button class="btn btn-sm btn-outline-secondary" data-p="close" title="Back to live map">✕</button>`;
    L.DomEvent.disableClickPropagation(player);
    L.DomEvent.disableScrollPropagation(player);
    layer._player = player;
    const rng = player.querySelector("input"), lab = player.querySelector(".csq-frame"), btn = player.querySelector('[data-p="play"]');
    let timer = null;
    const show = k => { layer.showFrame(k); rng.value = k; lab.textContent = layer.frames[k].frameLabel; };
    const stop = () => { clearInterval(timer); timer = null; btn.textContent = "▶"; };
    const play = () => {
      stop(); btn.textContent = "❚❚";
      timer = setInterval(() => { show((layer.frameIndex + 1) % layer.frames.length); }, 850);
    };
    btn.addEventListener("click", () => timer ? stop() : play());
    rng.addEventListener("input", () => { stop(); show(+rng.value); });
    player.querySelector('[data-p="close"]').addEventListener("click", () => {
      stop(); player.style.display = "none"; layer.frames = null;
      Shiny.setInputValue(layer.id + "_timelapse_close", Date.now(), { priority: "event" });
    });
    layer._startPlayer = () => {
      rng.max = layer.frames.length - 1;
      player.style.display = "flex";
      show(0); play();
    };
  }

  // ---------------------------------------------------- Shiny integration
  const layers = {};
  function withMap(id, cb, tries) {
    tries = tries || 0;
    const w = window.HTMLWidgets && HTMLWidgets.find("#" + id);
    const map = w && w.getMap && w.getMap();
    if (map) return cb(map);
    if (tries < 100) setTimeout(() => withMap(id, cb, tries + 1), 100);
  }
  function getLayer(id, opts, cb) {
    withMap(id, map => {
      if (!layers[id]) {
        const lyr = new CSquareLayer(id);
        lyr.addTo(map);
        buildControls(lyr, map, opts || {});
        layers[id] = lyr;
        // keep basemap in sync with dark mode unless the user picked one
        new MutationObserver(() => lyr._draw()).observe(document.documentElement, { attributes: true, attributeFilter: ["data-bs-theme"] });
      }
      cb(layers[id], map);
    });
  }
  window.CSquare = { layers, getLayer, fmt, rectCode };

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("csq-init", msg => getLayer(msg.id, msg, lyr => {
      lyr.closures = msg.closures || [];
    }));
    Shiny.addCustomMessageHandler("csq-data", msg => getLayer(msg.id, msg.opts, lyr => {
      if (lyr._player) lyr._player.style.display = "none";
      lyr.setData(msg);
    }));
    Shiny.addCustomMessageHandler("csq-frames", msg => getLayer(msg.id, msg.opts, lyr => {
      lyr.setFrames(msg);
      lyr._startPlayer();
    }));
    Shiny.addCustomMessageHandler("csq-style", msg => getLayer(msg.id, {}, lyr => {
      lyr.setStyle(msg);
      const sel = lyr._map.getContainer().querySelector('.csq-panel [data-k="scale"]');
      if (sel && msg.scale) sel.value = msg.scale;
    }));
    Shiny.addCustomMessageHandler("csq-select", msg => {
      const lyr = layers[msg.id]; if (lyr) { lyr.selected = null; lyr._draw(); }
    });
    Shiny.addCustomMessageHandler("perf", msg => {
      const el = document.getElementById("perf-hud");
      if (el) el.innerHTML = msg.html;
    });
    Shiny.addCustomMessageHandler("flyto", msg => withMap(msg.id, map => map.flyToBounds(msg.bounds, { duration: 0.8 })));
  }

  // Leaflet maps in hidden tabs have zero size until shown
  document.addEventListener("shown.bs.tab", () => {
    Object.values(layers).forEach(l => { if (l._map) { l._map.invalidateSize(); l._reset(); } });
  });
})();
