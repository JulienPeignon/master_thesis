# PARAMETERS ===================================================================

# Package needed
import(dplyr)

# List of CPS to download
suppressPackageStartupMessages({
  cps <- modules::use("modules/list_of_cps.R")
})

# Define dictionaries for column names and labels
dictionnary_column_names <- list(
  # Define a list of variable names
  list_of_variables = c(
    "STATEFIP",
    "AGE",
    "SEX",
    "RACE",
    "MARST",
    "CITIZEN",
    "NATIVITY",
    "WKSTAT",
    "EDUC",
    "UNION",
    "EMPSTAT",
    "CLASSWKR",
    "NCHILD",
    "NCHLT5",
    "EARNWEEK",
    "UHRSWORKORG",
    "HOURWAGE",
    "INCWAGE",
    "WKSWORK1",
    "UHRSWORKLY",
    "CPI99",
    "EARNWT",
    "IND1990",
    "PAIDHOUR"
  ),
  # Corresponding column names
  column_names = c(
    "state",
    "age",
    "sex",
    "race",
    "marital_status",
    "citizenship_status",
    "birthplace_parents",
    "fulltime_status",
    "education",
    "union_status",
    "labor_force_status",
    "class_worker",
    "nb_children",
    "nb_children_under_5",
    "weekly_earnings_org",
    "hours_worked_per_week_org",
    "hourly_wage_org",
    "annual_wage_asec",
    "weeks_worked_asec",
    "hours_worked_per_week_asec",
    "deflation_ratio",
    "ORG_weight",
    "industry",
    "paid_hour_flag"
  )
)

# Define dictionary for new labels
dictionnary_new_labels <- list(
  education = c(
    0,
    NA,
    0,
    4,
    1,
    2,
    3,
    4,
    6,
    5,
    6,
    8,
    7,
    8,
    9,
    10,
    11,
    12,
    12,
    12,
    12,
    13,
    13,
    14,
    14,
    14,
    15,
    16,
    16,
    17,
    17,
    18,
    17,
    17,
    20,
    NA
  ),
  sex = c(0, 1, NA)
)

# Define variables to be considered as factors
factor_variables <-
  c(
    "state",
    "race",
    "citizenship_status",
    "marital_status",
    "birthplace_parents",
    "union_status",
    "class_worker",
    "fulltime_status",
    "labor_force_status"
  )

# Define variable to be considered as numeric
numeric_variables <-
  c(
    "age",
    "sex",
    "education",
    "annual_wage_asec",
    "weeks_worked_asec",
    "hours_worked_per_week_asec",
    "hours_worked_per_week_org",
    "hourly_wage_org",
    "weekly_earnings_org",
    "nb_children",
    "nb_children_under_5",
    "industry",
    "paid_hour_flag",
    "weekly_earnings_org_flag",
    "hourly_wage_org_flag",
    "weeks_worked_asec_flag",
    "hours_worked_per_week_asec_flag"
  )

# Define columns and values to be replaced with NA
dictionnary_create_na <- list(
  columns = c(
    "race",
    "state",
    "citizenship_status",
    "marital_status",
    "birthplace_parents",
    "annual_wage_asec",
    "hours_worked_per_week_asec",
    "weekly_earnings_org",
    "hourly_wage_org",
    "hours_worked_per_week_org",
    "hours_worked_per_week_org",
    "fulltime_status",
    "labor_force_status",
    "class_worker",
    "class_worker",
    "paid_hour_flag"
  ),
  values = c(
    "Blank",
    "State not identified",
    "NIU",
    "NIU",
    "Unknown",
    99999999,
    999,
    9999.99,
    999.99,
    998,
    999,
    "NIU, blank, or not in labor force",
    "NIU",
    "Missing/Unknown",
    "NIU",
    0
  )
)

# Define columns to drop
columns_to_drop <-
  c(
    "HWTFINL",
    "SERIAL",
    "HFLAG",
    "ASECWTH",
    "PERNUM",
    "CPSID",
    "union_status",
    "CPSIDV",
    "ASECWT",
    "ORG_weight",
    "annual_wage_asec",
    "weekly_earnings_org",
    "hourly_wage_org",
    "deflation_ratio",
    "weeks_worked_asec",
    "hours_worked_per_week_org",
    "hours_worked_per_week_asec",
    "industry",
    "topcode_hourly_wage",
    "hourly_wage",
    "paid_hour_flag",
    "weekly_earnings_org_flag",
    "hourly_wage_org_flag",
    "weeks_worked_asec_flag",
    "hours_worked_per_week_asec_flag",
    "ASECFLAG",
    "WTFINL"
  )

# Create a list that maps specific states to their corresponding treatment years
states_and_years_treatment <-
  list(
    "Kentucky" = 2017,
    # Treatment year for Kentucky
    "Indiana" = 2012,
    # Treatment year for Indiana
    "Michigan" = 2013,
    # Treatment year for Michigan
    "Wisconsin" = 2015,
    # Treatment year for Wisconsin
    "West Virginia" = 2016
    # Treatment year for West Virginia
  )

# States that adopted RWL before 2007
already_voted_states <- c(
  "Alabama",
  "Arizona",
  "Arkansas",
  "Florida",
  "Georgia",
  "Idaho",
  "Iowa",
  "Kansas",
  "Louisiana",
  "Mississippi",
  "Nebraska",
  "Nevada",
  "North Carolina",
  "North Dakota",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Virginia",
  "Wyoming",
  "Oklahoma"
)

# IMPORTING DATA ===============================================================

#' Extract CPS Data
#'
#' This function extracts CPS (Current Population Survey) data using an API key,
#' downloads the data, and reads it into a data table with specified column names.
#'
#' @param api_key Your IPUMS API key (default is set_ipums_api_key).
#' @param name_of_extract Name for the CPS data extract.
#' @param list_of_variables A list of variables to be extracted.
#' @param column_names New column names to replace the default variable names.
#' @return A data table containing the extracted CPS data with renamed columns.
#' @export
extract_cps <- function(api_key = set_ipums_api_key,
                        name_of_extract,
                        list_of_variables,
                        column_names) {
  list_of_cps <- cps$list_of_cps

  # Define the extract using specified parameters
  extract <- ipumsr::define_extract_cps(name_of_extract, list_of_cps, list_of_variables)

  # Submit the extract and wait for it to be ready
  submitted_extract <- ipumsr::submit_extract(extract, api_key = api_key)
  downloadable_extract <- ipumsr::wait_for_extract(submitted_extract, api_key = api_key)

  # Download the extract and read it into a data frame
  path_to_ddi_file <- ipumsr::download_extract(
    downloadable_extract,
    download_dir = getwd(),
    overwrite = FALSE,
    api_key = api_key
  )

  # Read and process the downloaded data
  ipums_cps <- ipumsr::read_ipums_micro(path_to_ddi_file)

  # Convert data frame to data table and rename columns
  DT <- data.table::as.data.table(ipums_cps)
  DT <- DT %>% dplyr::rename_with(~column_names, dplyr::all_of(list_of_variables))

  return(DT)
}

# PREPROCESSING DATA ===========================================================

#' Add Quality Flags to CPS Data
#'
#' This function adds quality flags from an external parquet file to the given CPS data table.
#' It merges the flags based on year, month, and CPSIDP, and appends relevant columns to the original data.
#'
#' @param dt A data table containing CPS data.
#' @return A data table with added quality flag columns.
#' @export
add_flag <- function(dt) {
  # Load and filter quality flags data
  flags <- data.table::setDT(arrow::read_parquet("data/quality_flags.parquet")) %>%
    dplyr::filter(CPSIDP != 0) %>%
    dplyr::select(CPSIDP, YEAR, MONTH, QEARNWEE, QHOURWAG, QWKSWORK, QUHRSWORKLY)

  # Rename columns in the flags data table
  data.table::setnames(
    flags,
    old = c("QEARNWEE", "QHOURWAG", "QWKSWORK", "QUHRSWORKLY"),
    new = c(
      "weekly_earnings_org_flag",
      "hourly_wage_org_flag",
      "weeks_worked_asec_flag",
      "hours_worked_per_week_asec_flag"
    )
  )

  # Convert 'MONTH' from haven_labelled to integer in the input data table
  dt$MONTH <- as.integer(haven::as_factor(dt$MONTH))

  # Perform a left join on the filtered data
  dt_merged <- dt[CPSIDP != 0] %>%
    dplyr::left_join(flags, by = c("YEAR", "MONTH", "CPSIDP"))

  # Combine merged data with the original data where CPSIDP is zero
  dt <- dplyr::bind_rows(dt_merged, dt[CPSIDP == 0]) %>%
    dplyr::select(dplyr::everything(), dplyr::matches("_flag$"))

  return(dt)
}


#' Change Labels of Variables
#'
#' This function updates the labels of variables in a data table based on a provided dictionary
#' and converts specified variables to factors.
#'
#' @param data A data table containing the variables to relabel.
#' @param dict A list or named vector that provides the new labels for each variable.
#' @param var_factor A vector of variable names that need to be converted to factors.
#' @return The data table with updated variable labels.
#' @export
change_labels <- function(data, dict, var_factor) {
  # Update labels for variables based on the provided dictionary
  for (variable in attributes(dict)$names) {
    name_variable <- variable
    levels <- attributes(data[[name_variable]])$labels
    labels <- dict[[name_variable]]
    data[, (name_variable) := factor(data[[name_variable]], levels = levels, labels = labels)]
  }

  # Convert specified variables to factors
  for (variable in var_factor) {
    name_variable <- variable
    levels <- attributes(data[[name_variable]])$labels
    labels <- attributes(levels)$names
    data[, (name_variable) := factor(data[[name_variable]], levels = levels, labels = labels)]
  }

  return(data)
}


#' Replace Specified Values with NA
#'
#' This function replaces specified values in given columns with `NA` based on a dictionary.
#'
#' @param data A data table where values need to be replaced.
#' @param dict A list containing two elements: `columns`, a vector of column names, and `values`,
#' a vector of corresponding values to be replaced with `NA`.
#' @return The data table with specified values replaced by `NA`.
#' @export
remove_blank <- function(data, dict) {
  # Loop through each column and replace specified values with NA
  for (i in seq_along(dict$columns)) {
    column <- dict$columns[i]
    value <- dict$values[i]
    data[eval(parse(text = paste0(column, " == '", value, "'"))), eval(parse(text = paste0(column, " := NA")))]
  }

  return(data)
}


#' Convert Specified Variables to Numeric
#'
#' This function converts specified variables in a data table to numeric.
#'
#' @param data A data table containing the variables to be converted.
#' @param list A vector of variable names to be converted to numeric.
#' @return The data table with specified variables converted to numeric.
#' @export
make_numeric <- function(data, list) {
  # Loop through each variable in the list and convert it to numeric
  for (var in list) {
    data[, (var) := as.numeric(as.character(get(var)))]
  }

  return(data)
}


#' Add 'treatment' and 'first_treat' Variables to a Data Table
#'
#' This function adds treatment-related variables based on states and their respective treatment years.
#' It initializes, calculates, and updates several columns including `treatment`, `first_treat`, and `time_to_treat`.
#'
#' @param DT A data table containing state and year data.
#' @param states_and_years A named list where names are states and values are the corresponding treatment years.
#' Defaults to `states_and_years_treatment`.
#' @return The data table with added treatment variables.
#' @export
get_treatment_variable <- function(DT, states_and_years = states_and_years_treatment) {
  # Initialize the 'treatment' and 'first_treat' columns in DT with zeros
  DT[, `:=`(treatment = 0, first_treat = 0)]

  # Loop through each state and year to update the 'treatment' column
  for (state in names(states_and_years)) {
    year <- states_and_years[[state]]
    state_variable <- state

    # Update 'treatment' to 1 for rows where the state matches and the YEAR is greater than the treatment year
    DT[state == state_variable & YEAR > year, treatment := 1]
  }

  # Loop through each state to populate the 'first_treat' column
  for (state in names(states_and_years)) {
    state_variable <- state
    DT[state == state_variable, first_treat := states_and_years[[state_variable]]]
  }

  # Create a binary variable indicating if 'first_treat' is within 5 years of 'YEAR'
  DT[, first_treat_5 := ifelse(first_treat == 0 | (first_treat - YEAR >= -5 & first_treat - YEAR <= 5), 1, 0)]

  # Create a binary indicator 'rwl_state' based on whether 'first_treat' is 0 or not
  DT[, rwl_state := ifelse(first_treat == 0, 0, 1)]

  # Calculate the time difference between 'YEAR' and 'first_treat' if 'rwl_state' is 1, otherwise set it to 0
  DT[, time_to_treat := ifelse(rwl_state == 1, YEAR - first_treat, 0)]

  # Adjust 'time_to_treat' to NA if it is outside the range of -5 to 5
  DT[, time_to_treat_5 := ifelse(time_to_treat >= -5 & time_to_treat <= 5, time_to_treat, NA)]

  # Assign treatment year for states in the treatment list; otherwise, set it to 1000
  DT[, treatment_year := ifelse(state %in% names(states_and_years), YEAR - time_to_treat, 1000)]

  return(DT)
}


#' Adjust Topcoded Wages
#'
#' This function adjusts topcoded wages in a data table for hourly, annual, and weekly wages.
#' It applies specific adjustments based on the year and other conditions.
#'
#' @param dt A data table containing wage data with potential topcoding issues.
#' @return The data table with adjusted wage variables.
#' @export
adjust_topcoded_wages <- function(dt) {
  # Adjust hourly wage based on year and hours worked
  dt$topcode_hourly_wage <- ifelse(
    dt$YEAR <= 2002,
    ifelse(
      dt$hours_worked_per_week_org < 20,
      99.99,
      round(1923.07 / dt$hours_worked_per_week_org, 2)
    ),
    ifelse(
      dt$YEAR >= 2003,
      ifelse(
        dt$hours_worked_per_week_org < 29,
        99.99,
        round(2885.07 / dt$hours_worked_per_week_org, 2)
      ),
      NA
    )
  )

  # Apply inflation adjustment to topcoded hourly wage
  dt$hourly_wage_org <- ifelse(
    dt$hourly_wage_org == dt$topcode_hourly_wage,
    dt$hourly_wage_org * 1.4,
    dt$hourly_wage_org
  )

  # Adjust annual wage based on topcoded values by year range
  dt$annual_wage_asec <- ifelse(
    (dt$YEAR >= 1998 & dt$YEAR <= 2010 & dt$annual_wage_asec == 999999) |
      (dt$YEAR >= 2011 & dt$annual_wage_asec == 9999999),
    dt$annual_wage_asec * 1.4,
    dt$annual_wage_asec
  )

  # Adjust weekly wage if it matches topcoded values
  dt$weekly_earnings_org <- ifelse(
    dt$weekly_earnings_org == 2885 | dt$weekly_earnings_org == 2884.61,
    dt$weekly_earnings_org * 1.4,
    dt$weekly_earnings_org
  )

  return(dt)
}


#' Complete Preprocessing of CPS Data
#'
#' This function performs a comprehensive series of preprocessing steps on a CPS dataset.
#' It handles label changes, NA values, numeric conversions, wage adjustments,
#' variable transformations, and filtering based on user-defined parameters.
#'
#' @param data_table A data table containing the raw CPS data.
#' @param states_to_drop A vector of states to be dropped from the analysis (default: `already_voted_states`).
#' @param dict_labels A dictionary for updating variable labels (default: `dictionnary_new_labels`).
#' @param factor_var A vector of variables to be converted to factors (default: `factor_variables`).
#' @param dict_na A dictionary for replacing specific values with NA (default: `dictionnary_create_na`).
#' @param list_numeric A vector of variables to be converted to numeric (default: `numeric_variables`).
#' @param drop A vector of columns to be dropped from the final dataset (default: `columns_to_drop`).
#' @return A fully preprocessed data table ready for analysis.
#' @export
complete_preprocess <- function(data_table,
                                states_to_drop = already_voted_states,
                                dict_labels = dictionnary_new_labels,
                                factor_var = factor_variables,
                                dict_na = dictionnary_create_na,
                                list_numeric = numeric_variables,
                                drop = columns_to_drop) {
  # Add earnings flag variables
  data_table <- add_flag(data_table)

  # Update variable labels and convert specified variables to factors
  change_labels(
    data = data_table,
    dict = dict_labels,
    var_factor = factor_var
  )

  # Convert specified variables to numeric
  make_numeric(data = data_table, list = list_numeric)

  # Create numeric encoding for states
  data_table[, state_numeric := as.numeric(as.factor(state))]

  # Replace specified values with NA
  remove_blank(data = data_table, dict = dict_na)

  # Create union-related variables
  data_table[union_status == "Member of labor union", union_member := 1][
    union_status != "Member of labor union", union_member := 0
  ]

  data_table[union_status == "Covered by union but not a member", union_cover := 1][
    union_status != "Covered by union but not a member", union_cover := 0
  ]

  data_table[, union := as.integer(union_member == 1 | union_cover == 1)]

  # Create employment status variable
  data_table[labor_force_status %in% c("At work", "Has job, not at work last week"), employment := 1]
  data_table[!labor_force_status %in% c("At work", "Has job, not at work last week"), employment := 0]

  # Create experience variable based on age and education
  data_table[, experience := pmax(age - 6 - education, 0)]

  # Exclude certain industries
  data_table <- data_table[!industry %in% c(400, 421)]

  # Create industry category dummies
  data_table <- data_table[, industry_category := factor(
    data.table::fcase(
      industry == 0, NA_character_,
      industry >= 10 & industry <= 39, "Agriculture, forestry, and fisheries",
      industry >= 40 & industry <= 50, "Manufacturing",
      industry == 60, "Construction",
      industry >= 100 & industry <= 392, "Manufacturing",
      industry >= 400 & industry <= 472, "Transportation and utilities",
      industry >= 500 & industry <= 691, "Trade",
      industry >= 700 & industry <= 712, "FIRE",
      industry >= 721 & industry <= 810, "Business and personal services",
      industry >= 812 & industry <= 893, "Welfare and education",
      industry >= 900 & industry <= 932, "Public administration",
      industry >= 940 & industry <= 968, "Armed forces",
      default = "Other"
    )
  )]

  # Exclude specific worker categories and industries
  data_table <- data_table[!class_worker %in% c(
    "Self-employed", "Self-employed, not incorporated", "Self-employed, incorporated",
    "Armed forces", "Unpaid family worker", "Federal government employee",
    "State government employee", "Local government employee"
  ) & !industry_category %in% c("Agriculture, forestry, and fisheries", "Armed forces")]

  # Exclude individuals not in the labor force
  data_table <- data_table[!grepl("^NILF", labor_force_status)]

  # Remove missing values in birthplace_parents
  data_table <- data_table[!is.na(birthplace_parents)]

  # Create education category dummies
  data_table <- data_table[, education_category := factor(
    data.table::fcase(
      education >= 0 & education <= 5, "None to Primary Education",
      education >= 6 & education <= 10, "Secondary Education",
      education >= 11 & education <= 15, "Higher Education (Undergraduate)",
      education >= 16 & education <= 20, "Graduate Studies (Postgraduate)",
      default = NA_character_
    )
  )]

  # Create experience category dummies
  data_table <- data_table[, experience_category := factor(
    data.table::fcase(
      between(experience, 0, 9), "0-10 Years",
      between(experience, 10, 19), "10-20 Years",
      between(experience, 20, 29), "20-30 Years",
      between(experience, 30, 40), "30-40 Years",
      default = NA_character_
    )
  )]

  # Adjust topcoded wages
  data_table <- adjust_topcoded_wages(data_table)

  # Initialize hourly_wage variable with NA
  data_table[, hourly_wage := NA_real_]

  # Assign hourly wage based on conditions
  data_table[
    paid_hour_flag == 2 & (hourly_wage_org_flag != 4 | is.na(hourly_wage_org_flag)),
    hourly_wage := hourly_wage_org
  ]

  # Calculate hourly wage from weekly earnings if not available
  data_table[
    is.na(hourly_wage) & paid_hour_flag == 1 & !is.na(hours_worked_per_week_org) &
      hours_worked_per_week_org != 0 & (weekly_earnings_org_flag != 4 | is.na(weekly_earnings_org_flag)),
    hourly_wage := weekly_earnings_org / hours_worked_per_week_org
  ]

  # Calculate hourly wage from annual earnings if not available
  data_table[
    is.na(hourly_wage) & YEAR != 1998 & annual_wage_asec != 0 & weeks_worked_asec > 0 &
      hours_worked_per_week_asec > 0 & weeks_worked_asec_flag != 1 & hours_worked_per_week_asec_flag != 1,
    `:=`(YEAR = YEAR - 1, experience = experience - 1, age = age - 1)
  ]

  data_table[
    is.na(hourly_wage) & YEAR != 1998 & annual_wage_asec != 0 & weeks_worked_asec > 0 &
      hours_worked_per_week_asec > 0 & weeks_worked_asec_flag != 1 & hours_worked_per_week_asec_flag != 1,
    hourly_wage := annual_wage_asec / (weeks_worked_asec * hours_worked_per_week_asec)
  ]

  # Update deflation_ratio based on unique values by year
  unique_ratios <- data_table[!is.na(deflation_ratio), .(deflation_ratio = unique(deflation_ratio)), by = YEAR]
  data_table <- data_table[unique_ratios, deflation_ratio := i.deflation_ratio, on = .(YEAR)]

  # Calculate deflated hourly wage and its logarithm
  data_table[, deflated_hourly_wage := hourly_wage * deflation_ratio]
  data_table[, log_hourly_wage := log(deflated_hourly_wage)]

  # Drop specified columns
  data_table[, (drop) := NULL]

  # Keep only potential workers within age and experience ranges
  data_table <- data_table[age >= 15 & age <= 65 & experience > 0 & experience <= 40]
  data_table[experience == 40, experience_category := "30-40 Years"]

  # Add treatment variables
  data_table_final <- get_treatment_variable(data_table)

  # Remove unused factor levels
  cols_to_droplevels <- c(
    "race", "marital_status", "citizenship_status", "birthplace_parents",
    "class_worker", "fulltime_status", "industry_category", "labor_force_status"
  )
  data_table_final[, (cols_to_droplevels) := lapply(.SD, droplevels), .SDcols = cols_to_droplevels]

  # Drop states that adopted RWL prior to 2007
  data_table_final <- data_table_final[!state %in% states_to_drop]

  return(data_table_final)
}


#' Create Wage Dataset for Analysis
#'
#' This function filters the data table to include only rows with valid wage data
#' and employed individuals. It restricts the wage range, removes unemployed individuals,
#' and drops unused factor levels.
#'
#' @param DT A data table containing the full dataset.
#' @return A data table filtered for wage analysis.
#' @export
wage_dataset <- function(DT) {
  # Filter rows where deflated_hourly_wage is between 1 and 200
  DT_wage <- DT[deflated_hourly_wage >= 1 & deflated_hourly_wage <= 200]

  # Remove unemployed individuals
  DT_wage <- DT_wage[
    labor_force_status != "No, not in the labor force" &
      !fulltime_status %in% c(
        "Not at work, usually full - time",
        "Not at work, usually part - time",
        "Unemployed, seeking full - time work",
        "Unemployed, seeking part - time work"
      )
  ]

  # Remove unused factor levels in specified columns
  cols_to_droplevels <- c("fulltime_status", "labor_force_status")
  DT_wage[, (cols_to_droplevels) := lapply(.SD, droplevels), .SDcols = cols_to_droplevels]

  # Keep only rows where the individual is employed
  DT_wage <- DT_wage[employment == 1]

  return(DT_wage)
}
