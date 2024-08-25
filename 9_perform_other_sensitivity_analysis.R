##############################################################
##### COMPARATIVE IMPACT ON DIVERSE SOCIOECONOMIC GROUPS #####
##############################################################

# Load functions
suppressPackageStartupMessages({
  reporting_results <- modules::use("modules/estimating_and_reporting_results.R")
})

# Graphic parameters
par(
  cex.lab = 2.5, # Increase the size of axis labels
  cex.axis = 2, # Increase the size of axis numbers
  par(mar = c(8, 12, 2, 2) + 0.1) # Bottom, left, top, right margins
)

# Use the detectCores() function to find the number of CPU cores
n_cores <- parallel::detectCores()

# Store the independent variables as a string
X_wage <-
  "age + sex + race + education + experience + union_member +
  union_cover + I(experience^2) + I(experience^3) + I(experience^4) +
  education:experience + education_category:experience_category +
  industry_category + industry_category:factor(YEAR) + fulltime_status +
  marital_status + citizenship_status + birthplace_parents + nb_children"

X_union <-
  "age + sex + race + education + experience + union_cover +
  I(experience^2) + I(experience^3) + I(experience^4) +
  education:experience + education_category:experience_category +
  industry_category + industry_category:factor(YEAR) + fulltime_status +
  marital_status + citizenship_status + birthplace_parents + nb_children"

X_employment <-
  "age + sex + race + education + experience +
  I(experience^2) + I(experience^3) + I(experience^4) +
  education:experience + education_category:experience_category +
  marital_status + citizenship_status + birthplace_parents + nb_children +
  nb_children_under_5"

# Create a formula object for the dynamic regression model, adding state and year fixed effects
formula_dynamic_wage <-
  as.formula(
    paste(
      "log_hourly_wage ~ i(time_to_treat_5, rwl_state, ref = -1) +",
      X_wage,
      "+ IMR",
      "| state + YEAR"
    )
  )

formula_dynamic_union <-
  as.formula(
    paste(
      "union_member ~ i(time_to_treat_5, rwl_state, ref = -1) +",
      X_union,
      "+ IMR",
      "| state + YEAR"
    )
  )

formula_dynamic_employment <-
  as.formula(
    paste(
      "employment ~ i(time_to_treat_5, rwl_state, ref = -1) +",
      X_employment,
      "| state + YEAR"
    )
  )

high_unionization_sectors <- c(
  "Construction",
  "Manufacturing",
  "Transportation and utilities",
  "Welfare and education"
)

low_unionization_sectors <- c(
  "FIRE",
  "Trade",
  "Business and personal services"
)

# EDUCATED / NON-EDUCATED WORKERS ==============================================

### UNIONIZATION RATE

# Educated
twfe_dynamic_union_educated <-
  fixest::feols(
    formula_dynamic_union,
    cluster = ~state,
    data = DT_union[education > 12],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_union_educated)

# Non-educated
twfe_dynamic_union_non_educated <-
  fixest::feols(
    formula_dynamic_union,
    cluster = ~state,
    data = DT_union[education <= 12],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_union_non_educated)

# Plot results
reporting_results$plot_side_by_side(twfe_dynamic_union_educated, twfe_dynamic_union_non_educated, twfe_dynamic_union, c("Educated", "Non educated"), "unionization rate", "#808000")

### LOG HOURLY WAGES

# Educated
twfe_dynamic_wage_educated <-
  fixest::feols(
    formula_dynamic_wage,
    cluster = ~state,
    data = DT_wage[education > 12],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_wage_educated)

# Non-educated
twfe_dynamic_wage_non_educated <-
  fixest::feols(
    formula_dynamic_wage,
    cluster = ~state,
    data = DT_wage[education <= 12],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_wage_non_educated)

### EMPLOYMENT

# Educated
twfe_dynamic_employment_educated <-
  fixest::feols(
    formula_dynamic_employment,
    cluster = ~state,
    data = DT_employment[education > 12],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_employment_educated)

# Non-educated
twfe_dynamic_employment_non_educated <-
  fixest::feols(
    formula_dynamic_employment,
    cluster = ~state,
    data = DT_employment[education <= 12],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_employment_non_educated)

# Plot results
reporting_results$plot_side_by_side(twfe_dynamic_employment_educated, twfe_dynamic_employment_non_educated, twfe_dynamic_employment, c("Educated", "Non educated"), "employment", "#FF7F50", "top left")

# HIGH / LOW UNIONIZATION SECTORS  =============================================

### UNIONIZATION RATE

# High unionization
twfe_dynamic_union_high_unionization <-
  fixest::feols(
    formula_dynamic_union,
    cluster = ~state,
    data = DT_union[industry_category %in% high_unionization_sectors],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_union_high_unionization)

# Low unionization
twfe_dynamic_union_low_unionization <-
  fixest::feols(
    formula_dynamic_union,
    cluster = ~state,
    data = DT_union[industry_category %in% low_unionization_sectors],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_union_low_unionization)

# Plot results
reporting_results$plot_side_by_side(twfe_dynamic_union_low_unionization, twfe_dynamic_union_high_unionization, twfe_dynamic_union, c("Low unionization", "High unionization"), "unionization rate", "#808000")

### LOG HOURLY WAGES

# High unionization
twfe_dynamic_wage_high_unionization <-
  fixest::feols(
    formula_dynamic_wage,
    cluster = ~state,
    data = DT_wage[industry_category %in% high_unionization_sectors],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_wage_high_unionization)

# Low unionization
twfe_dynamic_wage_low_unionization <-
  fixest::feols(
    formula_dynamic_wage,
    cluster = ~state,
    data = DT_wage[industry_category %in% low_unionization_sectors],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_wage_low_unionization)

# HIGHLY UNIONIZED SECTORS AND NON-EDUCATED WORKERS ============================

### UNIONIZATION RATE
twfe_dynamic_union_high_unionization_non_educated <-
  fixest::feols(
    formula_dynamic_union,
    cluster = ~state,
    data = DT_union[education <= 12 & (industry_category %in% high_unionization_sectors)],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_union_high_unionization_non_educated)

fixest::iplot(
  twfe_dynamic_union_high_unionization_non_educated,
  xlab = "\n\n Event time",
  ylab = "Change in unionization rate \n\n",
  main = "",
  pt.join = TRUE,
  ci.col = "#808000",
  lwd = 3,
  grid.par = list(lty = 1, lwd = 0.5)
)

# LOG HOURLY WAGES
twfe_dynamic_wage_high_unionization_non_educated <-
  fixest::feols(
    formula_dynamic_wage,
    cluster = ~state,
    data = DT_wage[education <= 12 & (industry_category %in% high_unionization_sectors)],
    nthreads = as.integer(n_cores / 4)
  )

fixest::etable(twfe_dynamic_wage_high_unionization_non_educated)

fixest::iplot(
  twfe_dynamic_wage_high_unionization_non_educated,
  xlab = "\n\n Event time",
  ylab = "Change in log hourly wages \n\n",
  main = "",
  pt.join = TRUE,
  ci.col = "#008080",
  lwd = 3,
  grid.par = list(lty = 1, lwd = 0.5)
)
