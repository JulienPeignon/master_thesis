#####################################
##### ADDRESSING SELECTION BIAS #####
#####################################

# Load preprocessing module
suppressPackageStartupMessages({
  preprocess <- modules::use("modules/data_import_and_preprocess.R")
})

# Use the detectCores() function to find the number of CPU cores
n_cores <- parallel::detectCores()

# Define the selection equation formula
Z_employment <-
  "age + sex + race + education + experience +
  I(experience^2) + I(experience^3) + I(experience^4) +
  education:experience + education_category:experience_category +
  marital_status + citizenship_status + birthplace_parents + nb_children +
  nb_children_under_5"

# Create the full formula with treatment and fixed effects
formula_selection_employment <- as.formula(paste("employment ~ treatment +", Z_employment, "| state + YEAR"))

# Fit a fixed effects GLM with a probit link
twfe_selection_employment <- fixest::feglm(
  formula_selection_employment,
  cluster = ~state,
  family = binomial(link = "probit"),
  data = DT_employment,
  nthreads = as.integer(n_cores / 4)
)

fixest::etable(twfe_selection_employment)

# Identify rows without NA
na_indices <- twfe_selection_employment$obs_selection$obsRemoved * -1
non_na_indices <- setdiff(1:nrow(DT_employment), na_indices)

# Predict linear predictors for non-NA rows
predicted_values <- predict(twfe_selection_employment, newdata = DT_employment[non_na_indices, ], type = "link")

# Add predicted employment to the data table
DT_employment$predicted_employment <- NA
DT_employment$predicted_employment[non_na_indices] <- predicted_values

# Compute the CDF and PDF of a standard normal for predicted employment
DT_employment$cdf <- pnorm(DT_employment$predicted_employment)
DT_employment$pdf <- dnorm(DT_employment$predicted_employment)

# Calculate the Inverse Mills' Ratio
DT_employment$IMR <- DT_employment$pdf / DT_employment$cdf

# Union dataset
DT_union <- DT_employment[employment == 1]

# Wage dataset
DT_wage <- preprocess$wage_dataset(DT_employment)
