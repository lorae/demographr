# The purpose of this file is to provide examples of the results of Oaxaca-Blinder
# decompositions of household size when
# 1. Just immigrant status is involved
# 2. just age is involved
# 3. Age and immigration status are involved.


# ------------------------------------------------------------
# Step 0: Setup
# ------------------------------------------------------------

source("R/config.R")
source("R/generate_households_functions.R")
source("R/expand_households_functions.R")

N_HH <- 10000   # not too big, otherwise code takes forever

# ------------------------------------------------------------
# Step 1: Generate 2000-style population (younger age distribution)
# ------------------------------------------------------------


set.seed(SEED)

hh_2000 <- generate_households(
  n_households = N_HH,
  mean_hoh_age = 30   # younger baseline
)

person_2000 <- expand_households_to_adults(hh_2000)

# ------------------------------------------------------------
# Step 2: Generate 2019-style population (older age distribution)
# ------------------------------------------------------------

set.seed(SEED)

hh_2019 <- generate_households(
  n_households = N_HH,
  mean_hoh_age = 50   # older baseline
)

person_2019 <- expand_households_to_adults(hh_2019)

# ----- Step 4: Apply KOB ---- #

hhsize_2000 <- person_2000$hh_size |> mean()
hhsize_2019 <- person_2019$hh_size |> mean()

age_2000 <- person_2000$age |> mean()
age_2019 <- person_2019$age |> mean()

children_2000 <- person_2000$n_children |> mean()
children_2019 <- person_2019$n_children |> mean()

spouse_2000 <- person_2000$n_spouses |> mean()
spouse_2019 <- person_2019$n_spouses |> mean()

nonsf_2000 <- person_2000$n_nonsf |> mean()
nonsf_2019 <- person_2019$n_nonsf |> mean()

hhsize_2019 - hhsize_2000

# ----- Step 4a: Component regressions by year ----- #

# Children regressions
reg_children_2000 <- lm(n_children ~ age, data = person_2000)
reg_children_2019 <- lm(n_children ~ age, data = person_2019)

# Spouse regressions
reg_spouse_2000 <- lm(n_spouses ~ age, data = person_2000)
reg_spouse_2019 <- lm(n_spouses ~ age, data = person_2019)

# Non-subfamily regressions
reg_nonsf_2000 <- lm(n_nonsf ~ age, data = person_2000)
reg_nonsf_2019 <- lm(n_nonsf ~ age, data = person_2019)


# --- Results

make_coef_table <- function(reg_2000, reg_2019,
                            year_names = c("2000", "2019")) {
  coefs_2000 <- coef(reg_2000)
  coefs_2019 <- coef(reg_2019)
  
  coef_names <- union(names(coefs_2000), names(coefs_2019))
  
  out <- cbind(
    `2000` = coefs_2000[coef_names],
    `2019` = coefs_2019[coef_names]
  )
  
  rownames(out) <- coef_names
  out
}

# Children coefficients
coef_children <- make_coef_table(
  reg_children_2000,
  reg_children_2019
)

# Spouse coefficients
coef_spouse <- make_coef_table(
  reg_spouse_2000,
  reg_spouse_2019
)

# Non-subfamily coefficients
coef_nonsf <- make_coef_table(
  reg_nonsf_2000,
  reg_nonsf_2019
)

# Children
coef_children
i_children <- coef_children["(Intercept)", "2019"] - coef_children["(Intercept)", "2000"]
e_children <- (age_2019 - age_2000)*coef_children["age", "2019"]
c_children <- (coef_children["age", "2019"] - coef_children["age", "2000"])*age_2000

i_children + c_children + e_children
children_diff <- children_2019 - children_2000
# it works!

i_children / children_diff
e_children / children_diff
c_children / children_diff

# --- Spouse decomposition --- #
coef_spouse

spouse_2000 <- person_2000$n_spouses |> mean()
spouse_2019 <- person_2019$n_spouses |> mean()

i_spouse <- coef_spouse["(Intercept)", "2019"] -
  coef_spouse["(Intercept)", "2000"]

e_spouse <- (age_2019 - age_2000) *
  coef_spouse["age", "2019"]

c_spouse <- (coef_spouse["age", "2019"] -
               coef_spouse["age", "2000"]) *
  age_2000

# Check decomposition identity
i_spouse + c_spouse + e_spouse

spouse_diff <- spouse_2019 - spouse_2000

# Shares
i_spouse / spouse_diff
e_spouse / spouse_diff
c_spouse / spouse_diff

# --- Non-subfamily decomposition --- #

nonsf_2000 <- person_2000$n_nonsf |> mean()
nonsf_2019 <- person_2019$n_nonsf |> mean()

i_nonsf <- coef_nonsf["(Intercept)", "2019"] -
  coef_nonsf["(Intercept)", "2000"]

e_nonsf <- (age_2019 - age_2000) *
  coef_nonsf["age", "2019"]

c_nonsf <- (coef_nonsf["age", "2019"] -
              coef_nonsf["age", "2000"]) *
  age_2000

# Check decomposition identity
i_nonsf + c_nonsf + e_nonsf

nonsf_diff <- nonsf_2019 - nonsf_2000

# Shares
i_nonsf / nonsf_diff
e_nonsf / nonsf_diff
c_nonsf / nonsf_diff

nonsf_diff + spouse_diff + children_diff
# --- Non-subfamily decomposition --- #

nonsf_2000 <- person_2000$n_nonsf |> mean()
nonsf_2019 <- person_2019$n_nonsf |> mean()

i_nonsf <- coef_nonsf["(Intercept)", "2019"] -
           coef_nonsf["(Intercept)", "2000"]

e_nonsf <- (age_2019 - age_2000) *
           coef_nonsf["age", "2019"]

c_nonsf <- (coef_nonsf["age", "2019"] -
            coef_nonsf["age", "2000"]) *
            age_2000

# Check decomposition identity
i_nonsf + c_nonsf + e_nonsf

nonsf_diff <- nonsf_2019 - nonsf_2000

# Shares
i_nonsf / nonsf_diff
e_nonsf / nonsf_diff
c_nonsf / nonsf_diff

# ---
spouse_diff + children_diff + nonsf_diff
hhsize_2019 - hhsize_2000
