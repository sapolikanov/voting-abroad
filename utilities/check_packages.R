# Packages needed for the whole project

packages = c(
  # Data management
  "here", "readxl", "conflicted", 
  # Wrangling
  "tidyverse", "lubridate", 
  "countrycode", "reshape2",
  # Vizualisations
  "ggrepel", "ggmice", "qqplotr", "GGally", "marginaleffects",
  "kableExtra", "stargazer", "summarytools", "modelsummary",
  "estimatr", "patchwork",
  # Geospatial
  "rnaturalearth", "rnaturalearthdata", "sf", "classInt",
  # Modelling
  "nnet", "nestedLogit", "performance", "see", "DHARMa",
  "lme4", "nlopt", "optimx", "numDeriv", "equatiomatic",
  # Imputations
  "mice", "miceadds", "micemd",
  "finalfit"
)

# Installing and loading packages
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    renv::install(x, dependencies = "most")
    library(x, character.only = TRUE)
  }
})
