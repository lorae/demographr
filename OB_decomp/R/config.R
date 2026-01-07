# Shared config used across scripts

SEED <- 123

DATA_DIR <- "data"
SF_PATH  <- file.path(DATA_DIR, "synthetic_households_sf.rds")
AD_PATH  <- file.path(DATA_DIR, "synthetic_adults.rds")

AGE_MIN <- 22
AGE_MAX <- 100

DEFAULT_MEAN_HOH_AGE <- 45
DEFAULT_SD_AGE <- 30

PARAMS <- list(
  children = list(b0 = 2.5, b1 = -0.04, sd_e = 1.0),
  spouse   = list(b0 = 1.22, b1 = -0.01, sd_e = 0.4, max = 1),
  other_sf = list(b0 = 1.8, b1 = -0.04, sd_e = 0.4)
)
