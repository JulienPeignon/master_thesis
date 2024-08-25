#####################################################
##### SENSITIVITY TO PARALLEL TRENDS VIOLATIONS #####
#####################################################

# Load functions
suppressPackageStartupMessages({
  reporting_results <- modules::use("modules/estimating_and_reporting_results.R")
})

# UNIONIZATION RATE ============================================================
betahat_union <- summary(twfe_dynamic_union)$coefficients[1:10]
sigma_union <- summary(twfe_dynamic_union)$cov.scaled[1:10, 1:10]

# Sensitivity analysis using relative magnitudes restrictions
delta_rm_results_union <-
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat_union,
    sigma = sigma_union,
    numPrePeriods = 4,
    numPostPeriods = 6,
    Mbarvec = seq(0.1, 0.5, by = 0.1),
    l_vec = rep(1 / 6, 6)
  )

delta_rm_results_union

originalResults_union <- HonestDiD::constructOriginalCS(
  betahat = betahat_union,
  sigma = sigma_union,
  numPrePeriods = 4,
  numPostPeriods = 6,
  l_vec = rep(1 / 6, 6)
)

# Plot results
reporting_results$plot_honestdid(originalResults_union, delta_rm_results_union, color = "#808000")

# EMPLOYMENT ===================================================================
betahat_employment <- summary(twfe_dynamic_employment)$coefficients[1:10]
sigma_employment <- summary(twfe_dynamic_employment)$cov.scaled[1:10, 1:10]

# Sensitivity analysis using relative magnitudes restrictions
delta_rm_results_employment <-
  HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat_employment,
    sigma = sigma_employment,
    numPrePeriods = 4,
    numPostPeriods = 6,
    Mbarvec = seq(0.1, 0.5, by = 0.1),
    l_vec = rep(1 / 6, 6)
  )

delta_rm_results_employment

originalResults_employment <- HonestDiD::constructOriginalCS(
  betahat = betahat_employment,
  sigma = sigma_employment,
  numPrePeriods = 4,
  numPostPeriods = 6,
  l_vec = rep(1 / 6, 6)
)

# Plot results
reporting_results$plot_honestdid(originalResults_employment, delta_rm_results_employment, color = "#FF7F50")
