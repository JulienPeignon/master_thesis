################################
##### FRISCH WAUGH THEOREM #####
################################

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

# UNIONIZATION RATE ============================================================

# Create a formula object for the dynamic regression model, adding state and year fixed effects
formula_frisch_waugh_union <-
  as.formula(paste(
    "union_member ~ ",
    X_union,
    "+ IMR",
    "| state + YEAR"
  ))

# Run a dynamic TWFE regression and cluster standard errors by state
twfe_frisch_waugh_union <-
  fixest::feols(
    formula_frisch_waugh_union,
    cluster = ~state,
    data = DT_union,
    nthreads = as.integer(n_cores / 4)
  )

# Add residuals to union dataset
residuals_twfe_frisch_waugh_union <-
  residuals(twfe_frisch_waugh_union)

DT_union_frisch_waugh <- DT_union
DT_union_frisch_waugh$residuals_union <-
  residuals_twfe_frisch_waugh_union

# LOG HOURLY WAGES =============================================================

# Create a formula object for the dynamic regression model, adding state and year fixed effects
formula_frisch_waugh_wage <-
  as.formula(paste(
    "log_hourly_wage ~ ",
    X_wage,
    "+ IMR",
    "| state + YEAR"
  ))

# Run a dynamic TWFE regression and cluster standard errors by state
twfe_frisch_waugh_wage <-
  fixest::feols(
    formula_frisch_waugh_wage,
    cluster = ~state,
    data = DT_wage,
    nthreads = as.integer(n_cores / 4)
  )

# Add residuals to wage dataset
residuals_twfe_frisch_waugh_wage <-
  residuals(twfe_frisch_waugh_wage)

DT_wage_frisch_waugh <- DT_wage
DT_wage_frisch_waugh$residuals_wage <- residuals_twfe_frisch_waugh_wage

# EMPLOYMENT ===================================================================

# Create a formula object for the dynamic regression model, adding state and year fixed effects
formula_frisch_waugh_employment <-
  as.formula(paste(
    "employment ~ ",
    X_employment,
    "| state + YEAR"
  ))

# Run a dynamic TWFE regression and cluster standard errors by state
twfe_frisch_waugh_employment <-
  fixest::feols(
    formula_frisch_waugh_employment,
    cluster = ~state,
    data = DT_employment,
    nthreads = as.integer(n_cores / 4)
  )

# Add residuals to employment dataset
residuals_twfe_frisch_waugh_employment <-
  residuals(twfe_frisch_waugh_employment)

DT_employment_frisch_waugh <- DT_employment
DT_employment_frisch_waugh$residuals_employment <-
  residuals_twfe_frisch_waugh_employment
