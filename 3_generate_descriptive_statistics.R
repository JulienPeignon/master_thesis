##################################
##### DESCRIPTIVE STATISTICS #####
##################################

# Load the descriptive statistics module
suppressPackageStartupMessages({
  desc_stats <- modules::use("modules/generate_descriptive_stats.R")
})

# Generate and print descriptive statistics for numerical variables
desc_stats$get_descriptive_stats(DT_employment, output_type = "text")
desc_stats$get_descriptive_stats(DT_union, output_type = "text")
desc_stats$get_descriptive_stats(DT_wage, output_type = "text")

# T-tests to compare the three datasets for numeric variables
desc_stats$perform_t_tests(
  DT_employment,
  DT_union,
  DT_wage,
  desc_stats$variables_to_describe_numeric
)

# Generate and print descriptive statistics for factor variables
desc_stats$factor_summary(DT_employment, output_type = "text")
desc_stats$factor_summary(DT_union, output_type = "text")
desc_stats$factor_summary(DT_wage, output_type = "text")

# T-tests to compare the three datasets for factor variables
desc_stats$perform_t_tests(
  DT_employment,
  DT_union,
  DT_wage,
  desc_stats$variables_to_describe_factor
)

# Generate and print descriptive statistics for unionization rates
desc_stats$unionization_rates(DT_employment, output_type = "text")
desc_stats$unionization_rates(DT_union, output_type = "text")
desc_stats$unionization_rates(DT_wage, output_type = "text")

# Unionization rates for treated states before adoption of RTW law
desc_stats$unionization_rates(DT_union[(state == "Indiana" & YEAR < 2012) |
  (state == "Michigan" & YEAR < 2013) |
  (state == "Wisconsin" & YEAR < 2015) |
  (state == "West Virginia" & YEAR < 2016) |
  (state == "Kentucky" & YEAR < 2017)], output_type = "text")

# Unionization rates for never treated states
desc_stats$unionization_rates(DT_union[!(state %in% c("Indiana", "Michigan", "Wisconsin", "West Virginia", "Kentucky"))],
  output_type = "text"
)

# Display the count of observations for treated states within the data table
desc_stats$count_observations_treatment(DT_employment, output_type = "text")
desc_stats$count_observations_treatment(DT_union, output_type = "text")
desc_stats$count_observations_treatment(DT_wage, output_type = "text")

# Plot USA map highlighting RWL states
state_names <- names(desc_stats$states_and_years_treatment)
already_voted <- preprocess$already_voted_states

maps <- desc_stats$generate_usa_maps(state_names, already_voted)
maps$continental_usa_map
maps$alaska_map
maps$hawaii_map
maps$puerto_rico_map
