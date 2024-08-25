#######################
##### TWFE MODELS #####
#######################

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

# UNIONIZATION RATE ============================================================

### Static TWFE

# Create a formula object for the static regression model, adding state and year fixed effects
formula_static_union <-
  as.formula(paste(
    "union_member ~ treatment +",
    X_union,
    "+ IMR",
    "| state + YEAR"
  ))

# Run a static TWFE regression and cluster standard errors by state
twfe_static_union <-
  fixest::feols(
    formula_static_union,
    cluster = ~state,
    data = DT_union,
    nthreads = as.integer(n_cores / 4)
  )

# Display a summary table of the static TWFE regression results
fixest::etable(twfe_static_union)

### Dynamic TWFE

# Create a formula object for the dynamic regression model, adding state and year fixed effects
formula_dynamic_union <-
  as.formula(
    paste(
      "union_member ~ i(time_to_treat_5, rwl_state, ref = -1) +",
      X_union,
      "+ IMR",
      "| state + YEAR"
    )
  )

# Run a dynamic TWFE regression and cluster standard errors by state
twfe_dynamic_union <-
  fixest::feols(
    formula_dynamic_union,
    cluster = ~state,
    data = DT_union,
    nthreads = as.integer(n_cores / 4)
  )

# Display a summary table of the dynamic TWFE regression results
fixest::etable(twfe_dynamic_union)

# Create an event-study plot to visualize the dynamic treatment effects
fixest::iplot(
  twfe_dynamic_union,
  xlab = "\n\n Event time",
  ylab = "Change in unionization rate \n\n",
  main = "",
  pt.join = TRUE,
  ci.col = "#808000",
  lwd = 3,
  grid.par = list(lty = 1, lwd = 0.5)
)

# LOG HOURLY WAGES =============================================================

### Static TWFE

# Create a formula object for the static regression model, adding state and year fixed effects
formula_static_wage <-
  as.formula(paste(
    "log_hourly_wage ~ treatment +",
    X_wage,
    "+ IMR",
    "| state + YEAR"
  ))

# Run a static TWFE regression and cluster standard errors by state
twfe_static_wage <-
  fixest::feols(
    formula_static_wage,
    cluster = ~state,
    data = DT_wage,
    nthreads = as.integer(n_cores / 4)
  )

# Display a summary table of the static TWFE regression results
fixest::etable(twfe_static_wage)

### Dynamic TWFE

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

# Run a dynamic TWFE regression and cluster standard errors by state
twfe_dynamic_wage <-
  fixest::feols(
    formula_dynamic_wage,
    cluster = ~state,
    data = DT_wage,
    nthreads = as.integer(n_cores / 4)
  )

# Display a summary table of the dynamic TWFE regression results
fixest::etable(twfe_dynamic_wage)

# Create an event-study plot to visualize the dynamic treatment effects
fixest::iplot(
  twfe_dynamic_wage,
  xlab = "\n\n Event time",
  ylab = "Change in log hourly wages \n\n",
  main = "",
  pt.join = TRUE,
  ci.col = "#008080",
  lwd = 3,
  grid.par = list(lty = 1, lwd = 0.5)
)

# EMPLOYMENT ===================================================================

### Static TWFE

# Create a formula object for the static regression model, adding state and year fixed effects
formula_static_employment <-
  as.formula(paste("employment ~ treatment +", X_employment, "| state + YEAR"))

# Run a static TWFE regression and cluster standard errors by state
twfe_static_employment <-
  fixest::feols(
    formula_static_employment,
    cluster = ~state,
    data = DT_employment,
    nthreads = as.integer(n_cores / 4)
  )

# Display a summary table of the static TWFE regression results
fixest::etable(twfe_static_employment)

### Dynamic TWFE

# Create a formula object for the dynamic regression model, adding state and year fixed effects
formula_dynamic_employment <-
  as.formula(
    paste(
      "employment ~ i(time_to_treat_5, rwl_state, ref = -1) +",
      X_employment,
      "| state + YEAR"
    )
  )

# Run a dynamic TWFE regression and cluster standard errors by state
twfe_dynamic_employment <-
  fixest::feols(
    formula_dynamic_employment,
    cluster = ~state,
    data = DT_employment,
    nthreads = as.integer(n_cores / 4)
  )

# Display a summary table of the dynamic TWFE regression results
fixest::etable(twfe_dynamic_employment)

# Create an event-study plot to visualize the dynamic treatment effects
fixest::iplot(
  twfe_dynamic_employment,
  xlab = "\n\n Event time",
  ylab = "Change in employment \n\n",
  main = "",
  pt.join = TRUE,
  ci.col = "#FF7F50",
  lwd = 3,
  grid.par = list(lty = 1, lwd = 0.5)
)
