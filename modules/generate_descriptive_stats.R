# PARAMETERS ===================================================================

# Package needed
import(dplyr)

# Define the list of variables for which to generate descriptive statistics
variables_to_describe_numeric <-
  c(
    "age",
    "sex",
    "education",
    "deflated_hourly_wage",
    "log_hourly_wage",
    "union_member",
    "union_cover",
    "experience",
    "employment",
    "nb_children",
    "nb_children_under_5"
  )

variables_to_describe_factor <-
  c(
    "race",
    "marital_status",
    "citizenship_status",
    "birthplace_parents",
    "labor_force_status",
    "fulltime_status",
    "industry_category",
    "education_category",
    "experience_category"
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
    "West Virginia" = 2016 # Treatment year for West Virginia
  )

# DESCRIPTIVE STATISTICS =======================================================

#' Generate Descriptive Statistics for a List of Variables
#'
#' This function calculates and displays descriptive statistics (mean, min, max, number of missing values, and total observations)
#' for a list of numeric variables in a data table. The results can be output as plain text or LaTeX using `stargazer`.
#'
#' @param DT A data table containing the variables to be described.
#' @param var_list A character vector of variable names to be described (default: `variables_to_describe_numeric`).
#' @param output_type The type of output to be generated: "text", "latex", etc. (default: "text").
#' @return A stargazer table with descriptive statistics.
#' @export
get_descriptive_stats <- function(DT,
                                  var_list = variables_to_describe_numeric,
                                  output_type = "text") {
  # Create a data table with descriptive statistics for each variable
  stats_DT <- data.table::data.table(
    Variable = var_list,
    Mean = sapply(var_list, function(v) {
      # Mean, rounded and formatted
      mean(DT[[v]], na.rm = TRUE)
    }),
    Min = sapply(var_list, function(v) {
      # Minimum value
      min(DT[[v]], na.rm = TRUE)
    }),
    Max = sapply(var_list, function(v) {
      # Maximum value
      max(DT[[v]], na.rm = TRUE)
    }),
    Nb_NA = sapply(var_list, function(v) {
      # Number of missing (NA) values
      sum(is.na(DT[[v]]))
    }),
    Observations = sapply(var_list, function(v) {
      # Total number of observations
      nrow(DT)
    })
  )

  # Generate a table of the statistics with stargazer
  stargazer::stargazer(stats_DT,
    title = "Descriptive Statistics",
    summary = FALSE,
    type = output_type # Set the output type (e.g. "text", "latex")
  )
}


#' Summarize Factor Variables
#'
#' This function generates a summary of factor variables, showing the frequency and percentage of each level.
#' The results can be output as plain text or LaTeX using `stargazer`.
#'
#' @param dt A data table containing the factor variables to summarize.
#' @param cols A character vector of column names to summarize (default: `variables_to_describe_factor`).
#' @param output_type The type of output to be generated: "text", "latex", etc. (default: "text").
#' @return The function prints summary tables for each specified factor variable using `stargazer`.
#' @export
factor_summary <- function(dt,
                           cols = variables_to_describe_factor,
                           output_type = "text") {
  # Placeholder list for stargazer output
  tables_list <- list()

  # Loop through each column specified
  for (col in cols) {
    # If the column isn't a factor, skip it
    if (!is.factor(dt[[col]])) {
      next
    }

    # Calculate frequencies and percentages
    tab <- table(dt[[col]])
    data_table <- data.table::as.data.table(tab)
    names(data_table) <- c("Label", "Frequency")
    data_table$Percentage <- formatC(round((data_table$Frequency / sum(data_table$Frequency)) * 100, 2), format = "f", digits = 2)

    # We only keep the Label and Percentage columns, dropping Frequency
    data_table <- data_table[, .(Label, Percentage)]

    # Add the table to the list
    tables_list[[col]] <- data_table
  }

  # Print stargazer tables for each factor variable
  for (name in names(tables_list)) {
    cat(paste("\n", "Descriptive Statistics for", name, "\n"))
    stargazer::stargazer(tables_list[[name]],
      title = name,
      summary = FALSE,
      type = output_type
    )
  }
}


#' Count Observations for Specific States and Years
#'
#' This function counts the number of observations before and after a specified treatment year for each state in the dataset.
#' It outputs a summary table using the `stargazer` package.
#'
#' @param DT A data table containing state and year data.
#' @param states_and_years A named list mapping states to their treatment years (default: `states_and_years_treatment`).
#' @param output_type The type of output to be generated: "text", "latex", etc. (default: "text").
#' @return The function prints a table summarizing the number of observations before and after the treatment year for each state.
#' @export
count_observations_treatment <- function(DT, states_and_years = states_and_years_treatment, output_type = "text") {
  # Create an empty data.table to store the results
  result <- data.table::data.table(State = character(), Before_Treatment = integer(), After_Treatment = integer(), Total = integer())

  # Loop through the states and years dictionary, counting observations for each state and year
  for (state in names(states_and_years)) {
    year <- states_and_years[[state]] # Get the year for the current state
    state_variable <- state # Store the current state in a variable

    before_count <- nrow(DT[DT$state == state_variable & DT$YEAR < year, ]) # Count the number of observations before the treatment year
    after_count <- nrow(DT[DT$state == state_variable & DT$YEAR >= year, ]) # Count the number of observations after the treatment year
    total_count <- before_count + after_count # Calculate the total number of observations for the current state

    # Add the results to the results data.table
    result <- base::rbind(result, data.table::data.table(State = state, Before_Treatment = before_count, After_Treatment = after_count, Total = total_count))
  }

  # Calculate the total number of observations for all states
  total_before <- sum(result$Before_Treatment)
  total_after <- sum(result$After_Treatment)
  total_overall <- sum(result$Total)

  # Add a row to the data.table with the total counts
  result <- base::rbind(result, data.table::data.table(State = "Total", Before_Treatment = total_before, After_Treatment = total_after, Total = total_overall))

  # Generate a table using stargazer
  stargazer::stargazer(result, title = "Number of Observations by Treated State", type = output_type, summary = FALSE)
}


#' Calculate Unionization Rates per Sector
#'
#' This function calculates the mean union coverage and membership rates by sector.
#' The results are grouped by industry category and displayed in a table using `stargazer`.
#'
#' @param DT A data table containing the data with union and industry information.
#' @param output_type The type of output to be generated: "text", "latex", etc. (default: "text").
#' @return The function prints a table summarizing unionization rates per sector.
#' @export
unionization_rates <- function(DT, output_type = "text") {
  # Calculate descriptive statistics by sector
  stats_table <- DT %>%
    dplyr::filter(!is.na(industry_category)) %>%
    dplyr::mutate(industry_category = as.character(industry_category)) %>%
    dplyr::group_by(industry_category) %>%
    dplyr::summarise(
      mean_union_cover = round(mean(union_cover, na.rm = TRUE), 4), # Calculate mean of union coverage
      mean_union_member = round(mean(union_member, na.rm = TRUE), 4) # Calculate mean of union membership
    )

  # Generate a table using stargazer
  stargazer::stargazer(stats_table,
    title = "Unionization rate per sector",
    type = output_type,
    summary = FALSE
  )
}

# DESCRIPTIVE PLOTS ============================================================

#' Generate USA Maps with RTW States Highlighted
#'
#' This function generates a series of maps highlighting states with Right-to-Work (RTW) laws,
#' separating the maps for the continental USA, Alaska, Hawaii, and Puerto Rico.
#'
#' @param special_states A vector of state names that represent the "new wave of adoption" RTW states.
#' @param already_voted_states A vector of state names that represent the states with RTW laws from the 20th century.
#' @return A list of ggplot objects for the different regions: continental USA, Alaska, Hawaii, and Puerto Rico.
#' @export
generate_usa_maps <- function(special_states, already_voted_states) {
  # Load USA states data
  usa_states <- sf::st_read(
    "https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json",
    quiet = TRUE
  )

  # Define state status
  usa_states <- usa_states %>%
    dplyr::mutate(
      state_status = dplyr::case_when(
        NAME %in% special_states ~ "States with RTW laws from the New Wave of Adoption",
        NAME %in% already_voted_states ~ "States with 20th Century RTW laws",
        TRUE ~ "States without RTW laws"
      )
    )

  # Function to plot states
  plot_states <- function(states, title) {
    ggplot2::ggplot(data = states) +
      ggplot2::geom_sf(ggplot2::aes(fill = state_status)) +
      ggplot2::scale_fill_manual(
        values = c(
          "States with RTW laws from the New Wave of Adoption" = "#E63946",
          "States with 20th Century RTW laws" = "#A8DADC",
          "States without RTW laws" = "#F4F1DE"
        ),
        guide = ggplot2::guide_legend(ncol = 1)
      ) +
      ggplot2::labs(fill = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.title = ggplot2::element_text(size = 22),
        legend.text = ggplot2::element_text(size = 20),
        legend.box.margin = ggplot2::margin(30, 30, 30, 30)
      )
  }

  # Continental USA map
  continental_usa <- dplyr::filter(usa_states, !(NAME %in% c("Alaska", "Hawaii", "Puerto Rico")))
  continental_usa_map <- plot_states(continental_usa, "Continental USA States Map")

  # Alaska map
  alaska <- dplyr::filter(usa_states, NAME == "Alaska")
  alaska_map <- plot_states(alaska, "Alaska States Map")

  # Hawaii map
  hawaii <- dplyr::filter(usa_states, NAME == "Hawaii")
  hawaii_map <- plot_states(hawaii, "Hawaii States Map")

  # Puerto Rico map
  puerto_rico <- dplyr::filter(usa_states, NAME == "Puerto Rico")
  puerto_rico_map <- plot_states(puerto_rico, "Puerto Rico States Map")

  # Return list of plots
  list(
    continental_usa_map = continental_usa_map,
    alaska_map = alaska_map,
    hawaii_map = hawaii_map,
    puerto_rico_map = puerto_rico_map
  )
}

# TESTS ========================================================================

#' Perform T-Tests on Specified Variables Between Three Datasets
#'
#' This function performs t-tests on specified numeric or factor variables between three datasets.
#' For numeric variables, it performs standard t-tests, and for factor variables, it converts each
#' level into a binary variable and performs t-tests for each level.
#'
#' @param dt1 The first data table.
#' @param dt2 The second data table.
#' @param dt3 The third data table.
#' @param variables A character vector of variable names to test.
#' @return Prints the results of the t-tests for each variable and its levels (if applicable).
#' @export
perform_t_tests <- function(dt1, dt2, dt3, variables) {
  for (col in variables) {
    if (col == "employment") next
    cat("Results for variable:", col, "\n")

    if (is.numeric(dt1[[col]])) {
      # Perform t-tests for numeric variables
      t_test_12 <- stats::t.test(dt1[[col]], dt2[[col]]) # t-test between dt1 and dt2
      t_test_13 <- stats::t.test(dt1[[col]], dt3[[col]]) # t-test between dt1 and dt3
      t_test_23 <- stats::t.test(dt2[[col]], dt3[[col]]) # t-test between dt2 and dt3

      cat("\nt-test between dt1 and dt2:\n")
      print(t_test_12)

      cat("\nt-test between dt1 and dt3:\n")
      print(t_test_13)

      cat("\nt-test between dt2 and dt3:\n")
      print(t_test_23)
    } else if (is.factor(dt1[[col]])) {
      # Ensure factors have the same levels
      combined_levels <- union(union(levels(dt1[[col]]), levels(dt2[[col]])), levels(dt3[[col]]))
      dt1[[col]] <- factor(dt1[[col]], levels = combined_levels)
      dt2[[col]] <- factor(dt2[[col]], levels = combined_levels)
      dt3[[col]] <- factor(dt3[[col]], levels = combined_levels)

      # Convert factors to binary for each level and perform t-tests
      levels_to_test <- levels(dt1[[col]])
      for (lvl in levels_to_test) {
        cat("\nResults for level", lvl, "in variable", col, ":\n")

        # Creating dummy variables
        dt1_dummy <- as.numeric(dt1[[col]] == lvl)
        dt2_dummy <- as.numeric(dt2[[col]] == lvl)
        dt3_dummy <- as.numeric(dt3[[col]] == lvl)

        # t-tests between datasets for the current level
        t_test_12 <- stats::t.test(dt1_dummy, dt2_dummy) # t-test between dt1 and dt2 for the current level
        t_test_13 <- stats::t.test(dt1_dummy, dt3_dummy) # t-test between dt1 and dt3 for the current level
        t_test_23 <- stats::t.test(dt2_dummy, dt3_dummy) # t-test between dt2 and dt3 for the current level

        cat("\nt-test between dt1 and dt2 for level", lvl, ":\n")
        print(t_test_12)
        cat("\nt-test between dt1 and dt3 for level", lvl, ":\n")
        print(t_test_13)
        cat("\nt-test between dt2 and dt3 for level", lvl, ":\n")
        print(t_test_23)
      }
    }
    cat("\n--------------------------------------\n")
  }
}
