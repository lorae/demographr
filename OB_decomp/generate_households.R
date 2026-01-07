setwd("OB_decomp")
source("R/config.R")
source("R/generate_households_functions.R")

set.seed(SEED)
dir.create(DATA_DIR, showWarnings = FALSE)

synthetic_households <- generate_households(
  n_households = 1000,
  mean_hoh_age = DEFAULT_MEAN_HOH_AGE
)

saveRDS(synthetic_households, SF_PATH)
