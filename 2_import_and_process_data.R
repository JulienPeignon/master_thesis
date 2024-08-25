##########################################
##### IMPORTING & PREPROCESSING DATA #####
##########################################

# Load preprocessing module
suppressPackageStartupMessages({
  preprocess <- modules::use("modules/data_import_and_preprocess.R")
})

# Set API key and extract data from IPUMS
set_ipums_api_key <-
  rstudioapi::showPrompt(title = "IPUMS API KEY", message = "Your API key")

DT_raw <- preprocess$extract_cps(
  api_key = set_ipums_api_key,
  name_of_extract = "RTW_laws",
  list_of_variables = preprocess$dictionnary_column_names$list_of_variables,
  column_names = preprocess$dictionnary_column_names$column_names
)

# At this stage, ensure that you have downloaded the quality_flag.parquet file using Git LFS as instructed in the README.md.
# This file is critical for the next steps, as it contains the necessary quality flag data for processing.

# Preprocess the data
DT_employment <- preprocess$complete_preprocess(DT_raw)

# Union dataset
DT_union <- DT_employment[employment == 1]

# Wage dataset
DT_wage <- preprocess$wage_dataset(DT_employment)
