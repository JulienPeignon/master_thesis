###########################################
##### HETEROGENEITY-ROBUST ESTIMATORS #####
###########################################

# Load functions
suppressPackageStartupMessages({
  reporting_results <- modules::use("modules/estimating_and_reporting_results.R")
})

# Use the detectCores() function to find the number of CPU cores
n_cores <- parallel::detectCores()

# First stage Borusyak, Jaravel, Spiess (2024)
first_stage_borusyak <- stats::as.formula("~ 1 + i(YEAR) | state")

# Chaisemartin and D'Hautfeuille (2020) ========================================
TwoWayFEWeights::twowayfeweights(
  DT_wage_frisch_waugh,
  "residuals_wage",
  "state",
  "YEAR",
  "treatment"
)

TwoWayFEWeights::twowayfeweights(
  DT_union_frisch_waugh,
  "residuals_union",
  "state",
  "YEAR",
  "treatment"
)

TwoWayFEWeights::twowayfeweights(
  DT_employment_frisch_waugh,
  "residuals_employment",
  "state",
  "YEAR",
  "treatment"
)

# UNIONIZATION RATE ============================================================

### Callaway and Sant'Anna (2021)
twfe_union_callaway <- did::att_gt(
  yname = "residuals_union",
  tname = "YEAR",
  idname = "state_numeric",
  gname = "first_treat",
  data = DT_union_frisch_waugh[first_treat_5 == 1],
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "nevertreated",
  clustervars = "state",
  base_period = "universal",
  print_details = TRUE,
  pl = TRUE,
  cores = as.integer(n_cores / 4)
)

# Aggregate into event study
twfe_union_callaway_es <- did::aggte(twfe_union_callaway,
  type = "dynamic",
  na.rm = TRUE
)
summary(twfe_union_callaway_es)

# Aggregate into state-level ATT
twfe_union_callaway_group <- did::aggte(twfe_union_callaway, type = "group", na.rm = TRUE)
summary(twfe_union_callaway_group)
reporting_results$plot_group_effects(twfe_union_callaway_group, "unionization rate", "#808000")

### Borusyak, Jaravel, Spiess (2024)
twfe_union_borusyak <- didimputation::did_imputation(
  data = DT_union_frisch_waugh[first_treat_5 == 1],
  yname = "residuals_union",
  gname = "first_treat",
  tname = "YEAR",
  idname = "state",
  first_stage = first_stage_borusyak,
  horizon = TRUE,
  pretrends = TRUE,
  cluster_var = "state"
)

### Plot results
twfe_union <- reporting_results$preprocessing_robust_twfe(DT_union_frisch_waugh[first_treat_5 == 1], "union", twfe_union_borusyak, twfe_union_callaway_es)

reporting_results$plot_robust_twfe(twfe_union, "#808000", "unionization rate", separate = TRUE)
reporting_results$plot_robust_twfe(twfe_union, "#808000", "unionization rate", separate = FALSE)

# LOG HOURLY WAGES =============================================================

### Callaway and Sant'Anna (2021)
twfe_wage_callaway <- did::att_gt(
  yname = "residuals_wage",
  tname = "YEAR",
  idname = "state_numeric",
  gname = "first_treat",
  data = DT_wage_frisch_waugh[first_treat_5 == 1],
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "nevertreated",
  clustervars = "state",
  base_period = "universal",
  print_details = TRUE,
  pl = TRUE,
  cores = as.integer(n_cores / 4)
)

# Aggregate into event study
twfe_wage_callaway_es <- did::aggte(twfe_wage_callaway,
  type = "dynamic",
  na.rm = TRUE
)
summary(twfe_wage_callaway_es)

# Aggregate into state-level ATT
twfe_wage_callaway_group <- did::aggte(twfe_wage_callaway, type = "group", na.rm = TRUE)
summary(twfe_wage_callaway_group)
reporting_results$plot_group_effects(twfe_wage_callaway_group, "log hourly wages", "#008080")

### Borusyak, Jaravel, Spiess (2024)
twfe_wage_borusyak <- didimputation::did_imputation(
  data = DT_wage_frisch_waugh[first_treat_5 == 1],
  yname = "residuals_wage",
  gname = "first_treat",
  tname = "YEAR",
  idname = "state",
  first_stage = first_stage_borusyak,
  horizon = TRUE,
  pretrends = TRUE,
  cluster_var = "state"
)

### Plot results
twfe_wage <- reporting_results$preprocessing_robust_twfe(DT_wage_frisch_waugh[first_treat_5 == 1], "wage", twfe_wage_borusyak, twfe_wage_callaway_es)

reporting_results$plot_robust_twfe(twfe_wage, "#008080", "log hourly wages", separate = TRUE)
reporting_results$plot_robust_twfe(twfe_wage, "#008080", "log hourly wages", separate = FALSE)

# EMPLOYMENT ===================================================================

### Callaway and Sant'Anna (2021)
twfe_employment_callaway <- did::att_gt(
  yname = "residuals_employment",
  tname = "YEAR",
  idname = "state_numeric",
  gname = "first_treat",
  data = DT_employment_frisch_waugh[first_treat_5 == 1],
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "nevertreated",
  clustervars = "state",
  base_period = "universal",
  print_details = TRUE,
  pl = TRUE,
  cores = as.integer(n_cores / 4)
)

# Aggregate into event study
twfe_employment_callaway_es <- did::aggte(twfe_employment_callaway,
  type = "dynamic",
  na.rm = TRUE
)
summary(twfe_employment_callaway_es)

# Aggregate into state-level ATT
twfe_employment_callaway_group <- did::aggte(twfe_employment_callaway, type = "group", na.rm = TRUE)
summary(twfe_employment_callaway_group)
reporting_results$plot_group_effects(twfe_employment_callaway_group, "employment", "#FF7F50")

### Borusyak, Jaravel, Spiess (2024)
twfe_employment_borusyak <- didimputation::did_imputation(
  data = DT_employment_frisch_waugh[first_treat_5 == 1],
  yname = "residuals_employment",
  gname = "first_treat",
  tname = "YEAR",
  idname = "state",
  first_stage = first_stage_borusyak,
  horizon = TRUE,
  pretrends = TRUE,
  cluster_var = "state"
)

### Plot results
twfe_employment <- reporting_results$preprocessing_robust_twfe(DT_employment_frisch_waugh[first_treat_5 == 1], "employment", twfe_employment_borusyak, twfe_employment_callaway_es)

reporting_results$plot_robust_twfe(twfe_employment, "#FF7F50", "employment", separate = TRUE)
reporting_results$plot_robust_twfe(twfe_employment, "#FF7F50", "employment", separate = FALSE)
