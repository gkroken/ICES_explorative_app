# Robust CRAN/PPM install: long download timeout, retries for anything that
# failed (PPM binaries occasionally time out), and a hard failure listing what
# is still missing so the build never "succeeds" with packages absent.
#
#   Rscript install_packages.R pkg1 pkg2 ...
pkgs <- commandArgs(trailingOnly = TRUE)
options(timeout = max(1800, getOption("timeout")), Ncpus = 2L)

missing_pkgs <- function() pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

for (attempt in 1:4) {
  todo <- missing_pkgs()
  if (!length(todo)) break
  message(sprintf("== attempt %d: installing %s", attempt, paste(todo, collapse = ", ")))
  try(install.packages(todo, dependencies = c("Depends", "Imports", "LinkingTo")))
  if (length(missing_pkgs())) Sys.sleep(5 * attempt)
}

left <- missing_pkgs()
if (length(left)) stop("Packages still missing after retries: ", paste(left, collapse = ", "))
message("== all packages installed: ", paste(pkgs, collapse = ", "))
