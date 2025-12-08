if (!require(remotes)) {
  install.packages("remotes")
  library(remotes)
}

# Packages and versions required
packages <- list(
  sp = "2.1-4",
  spbabel = "0.6.0",
  spdplyr = "0.4.0",
  terra = "1.7-78",
  raster = "3.6-26",
  rsconnect = "0.8.29",
  shinydashboard = "0.7.2",
  rgdal = "1.6-6"
)

# Install each package from CRAN or CRAN Archive
for (pkg in names(packages)) {
  version <- packages[[pkg]]
  message("Installing ", pkg, " version ", version, " ...")
  
  remotes::install_version(pkg, version = version, repos = "https://cran.r-project.org")
  
  message("✔ Installed ", pkg, " ", version)
}

message("All specified package versions installed successfully!")
