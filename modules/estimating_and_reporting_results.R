# PARAMETERS ===================================================================

# Package needed
import(dplyr)

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

# FUNCTIONS TO PLOT RESULTS ====================================================

#' Preprocess Data for Plotting Robust TWFE Estimates
#'
#' This function preprocesses data for an event study analysis using the TWFE estimator and optionally
#' includes results from alternative estimators (Borusyak, Jaravel, Spiess (2024) and Callaway and Sant'Anna (2021)).
#' The function combines the results into a single dataset for easier plotting and comparison.
#'
#' @param dt A data table containing the data for the event study.
#' @param yvar The outcome variable used in the analysis.
#' @param twfe_borusyak (Optional) A data frame containing results from the Borusyak et al. estimator.
#' @param twfe_callaway (Optional) A model object from Callaway and Sant'Anna's estimator.
#' @return A combined data table with results from different estimators ready for plotting.
#' @export
preprocessing_robust_twfe <- function(dt, yvar, twfe_borusyak = NULL, twfe_callaway = NULL) {
  # Perform an event study using the TWFE estimator
  twfe <- did2s::event_study(
    data = dt,
    yname = paste0("residuals_", yvar),
    idname = "state",
    gname = "first_treat",
    tname = "YEAR",
    estimator = "TWFE"
  )

  # Store the TWFE results in a list
  results_list <- list(twfe)

  # If twfe_borusyak results are provided, process and add them to the list
  if (!is.null(twfe_borusyak)) {
    twfe_borusyak_processed <- twfe_borusyak[, c("term", "estimate", "std.error")]
    twfe_borusyak_processed <- twfe_borusyak_processed[grep("^(-)?[0-9]+$", twfe_borusyak_processed$term), ]
    twfe_borusyak_processed$term <- as.numeric(twfe_borusyak_processed$term)
    twfe_borusyak_processed$estimator <- "Borusyak, Jaravel, Spiess (2024)"

    results_list <- c(results_list, list(twfe_borusyak_processed))
  }

  # If twfe_callaway results are provided, process and add them to the list
  if (!is.null(twfe_callaway)) {
    twfe_callaway_processed <- broom::tidy(twfe_callaway)
    twfe_callaway_processed$term <- twfe_callaway_processed$event.time
    twfe_callaway_processed <- twfe_callaway_processed[, c("term", "estimate", "std.error")]
    twfe_callaway_processed$estimator <- "Callaway and Sant'Anna (2021)"

    results_list <- c(results_list, list(twfe_callaway_processed))
  }

  # Combine results from all estimators into one data table
  combined_results <- data.table::rbindlist(results_list, use.names = TRUE)

  # Exclude the reference period (term == -1) from the results
  combined_results <- combined_results[term != -1]

  # Return the combined results for plotting
  return(combined_results)
}


#' Plot Heterogeneity-Robust TWFE Estimators
#'
#' This function generates plots for heterogeneity-robust TWFE estimators with the option to include results
#' from Callaway and Sant'Anna (2021) and Borusyak, Jaravel, and Spiess (2024) estimators. It allows for
#' separate or combined plotting of the estimators with customized colors and labels.
#'
#' @param twfe_robust A data frame containing the results from different TWFE estimators.
#' @param color_var A color code to use for the main TWFE estimator.
#' @param var_name The name of the dependent variable to be displayed on the y-axis.
#' @param separate Logical; if TRUE, plots the estimators separately using facets.
#' @return A ggplot object for visualization.
#' @export
plot_robust_twfe <- function(twfe_robust, color_var, var_name, separate = TRUE) {
  # Get list of unique estimators
  estimators <- unique(twfe_robust$estimator)

  # Define the order of estimators with TWFE first
  levels <- c("TWFE", "Callaway and Sant'Anna (2021)", "Borusyak, Jaravel, Spiess (2024)")
  levels <- levels[levels %in% estimators]

  # Convert the estimator column to a factor with the specified levels
  twfe_robust$estimator <- factor(twfe_robust$estimator, levels = levels)

  # Define color scales based on the available estimators
  color_scale <- c("TWFE" = color_var, "Callaway and Sant'Anna (2021)" = "#DB222A", "Borusyak, Jaravel, Spiess (2024)" = "#800080")
  color_scale <- color_scale[names(color_scale) %in% estimators]

  # Calculate confidence intervals
  twfe_robust$ci_lower <- twfe_robust$estimate - 1.96 * twfe_robust$std.error
  twfe_robust$ci_upper <- twfe_robust$estimate + 1.96 * twfe_robust$std.error

  # Add extra points for separate plotting mode
  if (separate) {
    extra_points <- data.frame(term = -1, estimate = 0, ci_lower = 0, ci_upper = 0, std.error = NA, estimator = levels, is_extra = TRUE)
    twfe_robust$is_extra <- FALSE
    twfe_robust <- rbind(extra_points, twfe_robust)

    # Reorder levels to ensure TWFE is first after adding extra points
    twfe_robust$estimator <- factor(twfe_robust$estimator, levels = levels)
  } else {
    twfe_robust$is_extra <- FALSE
  }

  # Determine the position adjustment depending on the plot mode
  position <- if (separate) "identity" else ggplot2::position_dodge(width = 0.5)

  # Define axis limits based on the data
  y_lims <- c(min(twfe_robust$ci_lower), max(twfe_robust$ci_upper)) * 1.05
  x_lims <- if (separate) c(-5.5, 5.5) else c(min(twfe_robust$term) - 1, max(twfe_robust$term) + 1)

  # Exclude TWFE when separate plotting is TRUE
  if (separate) {
    twfe_robust <- dplyr::filter(twfe_robust, estimator != "TWFE")
  }

  # Create the base plot
  p <- ggplot2::ggplot(twfe_robust, ggplot2::aes(x = term, y = estimate, color = estimator, ymin = ci_lower, ymax = ci_upper)) +
    ggplot2::geom_point(position = position, size = 2) +
    ggplot2::geom_errorbar(data = subset(twfe_robust, !is_extra), position = position, size = 1, width = 0.2) +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(y = paste("Change in", var_name), x = "Event Time", color = "Estimator") +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::guides(color = ggplot2::guide_legend(title.position = "top", nrow = 2)) +
    ggplot2::theme(
      legend.position = if (separate) "none" else "bottom",
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
      axis.title = ggplot2::element_text(size = 24, color = "black"),
      axis.text = ggplot2::element_text(size = 20, color = "black"),
      legend.title = ggplot2::element_text(size = 24),
      legend.text = ggplot2::element_text(size = 22),
      strip.text = ggplot2::element_text(size = 24),
      legend.box.margin = ggplot2::margin(30, 30, 30, 30),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 30)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 30))
    )

  # Customize plot based on separate mode
  if (separate) {
    p <- p +
      ggplot2::facet_wrap(~estimator, scales = "free") +
      ggplot2::scale_y_continuous(limits = y_lims) +
      ggplot2::scale_x_continuous(limits = c(-5.1, 5.1)) +
      ggplot2::geom_line(ggplot2::aes(group = estimator), size = 1, color = "black") +
      ggplot2::geom_point(ggplot2::aes(group = estimator), size = 2, color = "black", fill = "black") +
      ggplot2::geom_point(data = subset(twfe_robust, is_extra), ggplot2::aes(x = -1, y = 0), color = "black", size = 2) +
      ggplot2::scale_color_manual(values = color_scale)
  } else {
    p <- p + ggplot2::scale_color_manual(values = color_scale)
  }

  return(p)
}


#' Plot Side-by-Side Comparison of Groups and Population
#'
#' This function generates a side-by-side comparison plot of two groups and the overall population,
#' visualizing changes in a specified variable over time relative to treatment. It includes options
#' for custom legend placement and color schemes.
#'
#' @param model_group1 A fitted model object for Group 1.
#' @param model_group2 A fitted model object for Group 2.
#' @param model_population A fitted model object for the overall population.
#' @param opposition A character vector of two names to label Group 1 and Group 2 (default: `c("Group 1", "Group 2")`).
#' @param variable The name of the variable to be displayed on the y-axis (default: `"variable"`).
#' @param color A color code for the population line (default: `"#808000"`).
#' @param legend_position Position of the legend, either `"bottom left"` or `"top left"` (default: `"bottom left"`).
#' @return A ggplot object displaying the side-by-side comparison.
#' @export
plot_side_by_side <- function(model_group1,
                              model_group2,
                              model_population,
                              opposition = c("Group 1", "Group 2"),
                              variable = "variable",
                              color = "#808000",
                              legend_position = "bottom left") {
  # Define the time periods of interest
  time_periods <- -5:5

  # Create the pattern for the coefficient names, excluding -1
  pattern <- paste0("time_to_treat_5::", time_periods[time_periods != -1], ":rwl_state")

  # Extract coefficients manually for group 1
  coef_group1 <- stats::coef(model_group1)[pattern]

  # Extract coefficients manually for group 2
  coef_group2 <- stats::coef(model_group2)[pattern]

  # Extract coefficients manually for the entire population
  coef_population <- stats::coef(model_population)[pattern]

  # Create data frames for each set of coefficients
  df_group1 <- data.frame(
    time_to_treatment = time_periods[time_periods != -1],
    change_in_variable = coef_group1,
    model = opposition[1]
  )

  df_group2 <- data.frame(
    time_to_treatment = time_periods[time_periods != -1],
    change_in_variable = coef_group2,
    model = opposition[2]
  )

  df_population <- data.frame(
    time_to_treatment = time_periods[time_periods != -1],
    change_in_variable = coef_population,
    model = "Population"
  )

  # Add the -1 period manually with coef = 0
  df_group1 <- rbind(
    df_group1,
    data.frame(
      time_to_treatment = -1,
      change_in_variable = 0,
      model = opposition[1]
    )
  )

  df_group2 <- rbind(
    df_group2,
    data.frame(
      time_to_treatment = -1,
      change_in_variable = 0,
      model = opposition[2]
    )
  )

  df_population <- rbind(
    df_population,
    data.frame(
      time_to_treatment = -1,
      change_in_variable = 0,
      model = "Population"
    )
  )

  # Combine the data frames
  df_combined <- dplyr::bind_rows(df_group1, df_group2, df_population)

  # Define offsets for the legend
  legend_y_gap <- if (legend_position == "top left") 0.1 * max(abs(df_combined$change_in_variable)) else 0.08 * max(abs(df_combined$change_in_variable))

  if (legend_position == "top left") {
    legend_y_start <- max(df_combined$change_in_variable) + 0.1 * max(abs(df_combined$change_in_variable))
  } else {
    legend_y_start <- min(df_combined$change_in_variable) - 0.1 * max(abs(df_combined$change_in_variable))
  }

  # Plot using ggplot2
  p <- ggplot2::ggplot(df_combined, ggplot2::aes(x = time_to_treatment, y = change_in_variable, group = model, color = model)) +
    ggplot2::geom_line(ggplot2::aes(linetype = model), size = 1.3) +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "black", size = 0.75) +
    ggplot2::scale_color_manual(values = stats::setNames(c("black", "black", color), c(opposition[1], opposition[2], "Population"))) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("twodash", "dotted", "solid"), c(opposition[1], opposition[2], "Population"))) +
    ggplot2::labs(
      x = "Event time",
      y = paste("Change in", variable),
      title = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_text(size = 24, color = "black"),
      axis.text = ggplot2::element_text(size = 20, color = "black"),
      plot.margin = ggplot2::unit(c(5, 6, 8, 6), "lines"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 1),
      axis.ticks = ggplot2::element_line(colour = "black"),
      legend.box.margin = ggplot2::margin(10, 10, 10, 10),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 30)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 30))
    ) +
    ggplot2::annotate("rect", xmin = -5.2, xmax = -1.8, ymin = legend_y_start, ymax = legend_y_start + 4 * legend_y_gap, color = "black", fill = "white") +
    ggplot2::annotate("text", x = -5, y = legend_y_start + 3 * legend_y_gap, label = opposition[1], hjust = 0, size = 6) +
    ggplot2::annotate("text", x = -5, y = legend_y_start + 2 * legend_y_gap, label = opposition[2], hjust = 0, size = 6) +
    ggplot2::annotate("text", x = -5, y = legend_y_start + legend_y_gap, label = "Whole sample", hjust = 0, size = 6) +
    ggplot2::annotate("segment", x = -3, xend = -2, y = legend_y_start + 3 * legend_y_gap, yend = legend_y_start + 3 * legend_y_gap, color = "black", linetype = "twodash", size = 1.5) +
    ggplot2::annotate("segment", x = -3, xend = -2, y = legend_y_start + 2 * legend_y_gap, yend = legend_y_start + 2 * legend_y_gap, color = "black", linetype = "dotted", size = 1.5) +
    ggplot2::annotate("segment", x = -3, xend = -2, y = legend_y_start + legend_y_gap, yend = legend_y_start + legend_y_gap, color = color, linetype = "solid", size = 1.5)

  print(p)
}


#' Plot Honest DID Sensitivity Analysis
#'
#' This function plots the results of a sensitivity analysis for the Honest DID method.
#' It combines original and delta-RM results into a single plot, enhancing the presentation.
#'
#' @param originalResults A data frame containing the original Honest DID results.
#' @param delta_rm_results A data frame containing the delta-RM adjusted results.
#' @param color A color code for the original results line.
#' @return A ggplot object for visualization.
#' @export
plot_honestdid <- function(originalResults, delta_rm_results, color) {
  # Combine the datasets
  combined_results <- dplyr::bind_rows(
    originalResults %>% dplyr::mutate(Mbar = 0, method = "Original"),
    delta_rm_results
  )

  # Create the plot
  sensitivity_plot <- ggplot2::ggplot(combined_results, ggplot2::aes(x = Mbar, color = method)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lb[, 1], ymax = ub[, 1]), size = 1, width = 0.02) +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c("Original" = color, "C-LF" = "#632B30")) +
    ggplot2::labs(
      x = expression(bar(M)),
      y = "",
      color = "Method"
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 24, color = "black"),
      axis.text = ggplot2::element_text(size = 20, color = "black"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.ticks = ggplot2::element_line(colour = "black"),
      legend.position = "none",
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 30)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 30))
    )

  # Print the enhanced plot
  print(sensitivity_plot)
}


#' Plot Group Effects for TWFE Callaway and Sant'Anna Method
#'
#' This function visualizes group-level treatment effects with confidence intervals for different states and years.
#' It plots the ATT estimates across different treatment groups using the Callaway and Sant'Anna method.
#'
#' @param aggte_obj An AGGTEobj containing the results from the TWFE Callaway and Sant'Anna method.
#' @param variable_name The name of the variable to be displayed on the x-axis.
#' @param error_bar_color A color code for the error bars.
#' @param state_and_years A named list mapping states to their treatment years (default: `states_and_years_treatment`).
#' @return A ggplot object for visualization.
#' @export
plot_group_effects <- function(aggte_obj, variable_name, error_bar_color, state_and_years = states_and_years_treatment) {
  # Extract the relevant data from the AGGTEobj object
  twfe_callaway_group_df <- data.frame(
    Group = aggte_obj$egt,
    Estimate = aggte_obj$att.egt,
    Std.Error = aggte_obj$se.egt,
    Conf.Low = aggte_obj$att.egt - aggte_obj$crit.val.egt * aggte_obj$se.egt,
    Conf.High = aggte_obj$att.egt + aggte_obj$crit.val.egt * aggte_obj$se.egt
  )

  # Convert the list to a data frame for easy joining
  states_and_years_df <- data.frame(
    Group = unlist(state_and_years),
    State = names(state_and_years)
  )

  # Join the data frames to map the state names
  twfe_callaway_group_df <- dplyr::left_join(twfe_callaway_group_df, states_and_years_df, by = "Group")

  # Add the treatment year to the state name with a space
  twfe_callaway_group_df <- twfe_callaway_group_df %>%
    dplyr::mutate(State = paste(State, " (", Group, ")", sep = ""))

  # Sort the data frame by the treatment year in ascending order
  twfe_callaway_group_df <- twfe_callaway_group_df %>%
    dplyr::arrange(Group)

  # Reorder the factor levels for State in ascending order
  twfe_callaway_group_df$State <- factor(twfe_callaway_group_df$State, levels = twfe_callaway_group_df$State)

  # Create the plot
  ggplot2::ggplot(twfe_callaway_group_df, ggplot2::aes(x = Estimate, y = State)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = Conf.Low, xmax = Conf.High), size = 1, height = 0.2, color = error_bar_color) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "",
      x = paste("ATT :", variable_name),
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 24, color = "black"),
      axis.text = ggplot2::element_text(size = 20, color = "black"),
      legend.title = ggplot2::element_text(size = 24),
      legend.text = ggplot2::element_text(size = 22),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.ticks = ggplot2::element_line(colour = "black"),
      plot.margin = ggplot2::unit(c(1, 1.5, 1, 1.5), "cm"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 30)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 30))
    )
}
