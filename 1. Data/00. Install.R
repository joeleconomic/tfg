# Lista de paquetes CRAN
packages <- c(
  "tidyverse", "skimr", "openxlsx", "fixest", "lmtest", "dynlm",
  "tseries", "broom", "spdep", "spatialreg", "splm", "sf", "geodata",
  "terra", "plm", "car", "systemfit", "modelsummary", "stargazer", "pandoc", "nlme"
)

# Instalar los paquetes si no están instalados
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

invisible(lapply(packages, install_if_missing))

# Instalar desde GitHub (solo si no está ya instalado)
if (!requireNamespace("ineptR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("c-matos/ineptR")
}
