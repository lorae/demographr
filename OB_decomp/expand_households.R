source("R/config.R")
source("R/expand_households_functions.R")

households <- readRDS(SF_PATH)
adults <- expand_households_to_adults(households)
saveRDS(adults, AD_PATH)