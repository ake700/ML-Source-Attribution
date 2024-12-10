library(dplyr)
library(ggplot2)
library(rlang)
library(ggpubr)
library(dunn.test)

# Descriptions formatted to be compatible with ROxygen2 style 

##############################################
#' Group and Filter Data Based on Conditions
#'
#' @description This function filters a given dataframe for specified serovars and groups it by given columns.
#' It then generates multiple filtered versions of the grouped data based on different conditions for a specified column.
#'
#' @param df A dataframe containing the data.
#' @param df_name A character string representing the name of the dataset (for identification in the results).
#' @param serovar_col The name of the column containing serovar information (character).
#' @param serovar_keep_list A vector of serovar values to keep.
#' @param grouping_cols A character vector of column names by which to group the data.
#' @param filter_col The name of the column on which the conditions will be applied.
#' @param conditions A numeric vector specifying the cutoff values for filtering.
#'
#' @return A list containing the original grouped data ("All"), multiple filtered subsets, and the dataset name.
##############################################
# Function to generalize the grouping and filtering
group_and_filter <- function(df, df_name, serovar_col, serovar_keep_list, grouping_cols, filter_col, conditions) {
  # Group by specified columns and calculate counts
  grouped_df <- df %>%
    filter(!!sym(serovar_col) %in% serovar_keep_list) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(count = n(), .groups = "drop")
  
  # Generate a list of filtered data frames based on conditions
  results <- list("All" = grouped_df)
  for (cond in conditions) {
    results[[paste0("Filter", cond)]] <- grouped_df %>%
      filter(!!sym(filter_col) < cond)
  }
  
  results$dataset <- df_name
  return(results)
}

##############################################
#' Combine Results into a single dataframe
#'
#' @description Takes the output of `group_and_filter` (which includes several filtered data frames)
#' and merges them into a single dataframe, summarizing all unique labels for each serovar.
#'
#' @param results A list object returned by `group_and_filter`.
#' @param serovar_col The name of the column representing serovars.
#' @param grouping_col The name of the column to collapse items from.
#'
#' @return A dataframe containing all labels and filtered sets joined by serovar.
##############################################
# Function to combine results into a single data frame
combine_results <- function(results, serovar_col, grouping_col) {
  collapse_items <- function(x) {
    paste(unique(x), collapse = ", ")
  }
  
  dataset <- results$dataset
  
  # Create a list to store unique states per serovar
  combined <- results$All %>%
    group_by(!!sym(serovar_col)) %>%
    summarise(all_labels = collapse_items(!!sym(grouping_col)), .groups = "drop") %>%
    mutate(dataset = dataset)
  
  # Add each condition column
  for (result_name in names(results)[names(results) != "All" & names(results) != "dataset"]) {
    temp <- results[[result_name]] %>%
      group_by(!!sym(serovar_col)) %>%
      summarise(!!result_name := collapse_items(!!sym(grouping_col)), .groups = "drop")
    combined <- left_join(combined, temp, by = serovar_col)
  }
  
  return(combined)
}

##############################################
#' Create a customized boxplot with Facets
#'
#' @description Generates a boxplot of a given dataset with specified x, y, and fill columns,
#' and applies a facet grid based on two specified facet columns.
#'
#' @param data A dataframe containing the data.
#' @param x_col The name of the column for the x-axis.
#' @param y_col The name of the column for the y-axis.
#' @param fill_col The name of the column to use for fill color grouping.
#' @param facet_cols A character vector of length 2 specifying the columns for facet_grid.
#' @param plot_name Not used within the function, but included for potential naming of the plot.
#'
#' @return A ggplot object (box plot with facets).
##############################################
create_custom_plot <- function(data, x_col, y_col, fill_col, facet_cols, plot_name) {
  # Convert the column name strings into symbols
  cols <- lapply(list(x_col, y_col, fill_col), sym)
  names(cols) <- c("x", "y", "fill")
  
  # Creating a formula for facet_grid
  facet_x_sym <- sym(facet_cols[1])
  facet_y_sym <- sym(facet_cols[2])
  facet_formula <- paste(facet_cols[1], "~", facet_cols[2])
  
  # Create the plot using tidy evaluation
  p <- ggplot(data, aes(x = !!cols$x, y = !!cols$y, fill = !!cols$fill)) + 
    geom_boxplot() + 
    facet_grid(facet_formula, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    expand_limits(y = c(0, 1.0))
  
  # Return the plot
  return(p)
}

##############################################
#' Create customized plots for multiple X columns
#'
#' @description Given a dataframe, a y-column, and a list of x-columns, this function generates boxplots for each x-column.
#' It can return all plots combined (using ggarrange) or a subset of them. Also includes a hardcoded example (num_p plot).
#'
#' @param df The input dataframe.
#' @param y_col The name of the column for the y-axis.
#' @param x_col_list A character vector of column names to be used as x-variables.
#' @param return_type Either "all" to return combined plots or something else to return a list of individual plots.
#' @param return_indices If returning individual plots, specify indices of plots to return.
#' @param nrow Number of rows in the combined plot layout.
#' @param ncol Number of columns in the combined plot layout.
#' @param color The fill color for the boxplots.
#'
#' @return Depending on return_type:
#' - "all": a combined ggplot object (ggarrange).
#' - otherwise: a list of ggplot objects or a subset of them.
##############################################
create_custom_plot2 <- function(df, y_col, x_col_list, return_type = "all", return_indices = NULL,
                                nrow = 1, ncol = 1, color = 'lightblue') {
  y_sym <- sym(y_col)
  
  # List to store individual plots
  plot_list <- list()
  
  # Generate each plot according to x_col_list
  for (x_col in x_col_list) {
    x_sym <- sym(x_col)
    
    df_plot <- if (x_col == "sampling_strat") {
      filter(df, !sampling_strat == "RandomBalancingSampler")  # Filter out specific category only for this plot
    } else if (x_col == "Feat_elim_method") {
      filter(df, !is.na(Feat_elim_method))  # Remove NAs only for this plot
    } else {
      df  
    }
    
    p <- ggplot(df_plot, aes(x = factor(!!x_sym), y = !!y_sym)) +
      geom_boxplot(fill = color) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        x = x_col, 
        y = y_col
      ) +
      expand_limits(y = c(0, 1.0))
    
    plot_list[[x_col]] <- p
  }
  
  num_p <- ggplot(df, aes(x = Serovar, y = num_pred)) +
    geom_col(position = "dodge") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~ filter_threshold, scales = "free_y") + 
    labs(
      x = "Serovar", 
      y = "num_pred"
    ) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 1)) 
  
  # Check return type
  if (return_type == "all") {
    # Use ggarrange to combine plots
    comb_plot_list <- c(plot_list, list("num_p" = num_p))
    
    combined_plot <-
      ggarrange(
        plotlist = comb_plot_list,
        align = "hv",
        labels = "AUTO",
        nrow = nrow,
        ncol = ncol
      )
    return(combined_plot)
  } else {
    # Return list of individual plots
    if (is.null(return_indices)) {
      return(plot_list)
    } else {
      return(plot_list[return_indices])
    }
  }
}

##############################################
#' Run Dunn's Tests on filtered data
#'
#' @description Filters data by specified sero_calling, serovar, and optionally label_filter,
#' then performs Dunn's test on the specified variables against the F1 metric.
#'
#' @param data The input dataframe.
#' @param variables A character vector of variables to test.
#' @param sero_calling_filter The sero_calling value to filter by.
#' @param serovar_filter The serovar to filter by.
#' @param label_filter (Optional) Another label (like Pred_label) to filter by.
#'
#' @return A dataframe of Dunn's test results with adjusted p-values and variable pairs.
##############################################
# Function to perform Dunn's test on different filtered datasets
run_dunn_tests <- function(data, variables, sero_calling_filter, serovar_filter, 
                           label_filter = NULL) {
  # Filter the dataset according to the specified criteria
  filtered_data <- data %>%
    dplyr::filter(Sero_calling == sero_calling_filter) %>%
    dplyr::filter(Serovar == serovar_filter)
  
  if (!is.null(label_filter)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(Pred_label == label_filter)
  }
  
  # Initialize an empty dataframe to store results
  combined_df <- data.frame(Variable = character(), 
                            Pairs = character(), 
                            Bonferroni_adj_P_value = numeric(),
                            stringsAsFactors = FALSE)
  
  for (variable in variables) {
    if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[variable]]))) {
      # Perform Dunn's test comparing F1 against the current variable
      test <- dunn.test(filtered_data$F1, filtered_data[[variable]], method = "bonferroni")
      
      # Create a dataframe for this test's results
      temp_df <- data.frame(
        Variable = rep(variable, length(test$comparisons)),  
        Pairs = sapply(test$comparisons, function(x) {
          groups <- sort(unlist(strsplit(x, " - ")), decreasing = FALSE)
          paste(groups, collapse = " - ")
        }),
        Bonferroni_adj_P_value = round(test$P.adjusted, digits = 4),
        stringsAsFactors = FALSE
      )
      
      # Combine with the main dataframe
      combined_df <- rbind(combined_df, temp_df)
    } else {
      message(paste("Skipping variable", variable, "- not enough data or no valid entries found."))
    }
  }
  
  return(combined_df)
}

##############################################
#' Calculate summary statistics (Median and IQR) for F1-score by groups
#'
#' @description Filters the data by serovar calling method, Serovar, and optionally label type. For each variable,
#' it calculates pairwise median and IQR for F1 metric. Used to complement Dunn's test results.
#'
#' @param data The input dataframe.
#' @param variables A character vector of grouping variables to compare.
#' @param sero_calling_filter The sero_calling value to filter by.
#' @param serovar_filter The serovar to filter by.
#' @param label_filter (Optional) A label to further filter the data.
#'
#' @return A dataframe with median and IQR stats for each pair of groups.
##############################################
calculate_summary_statistics <- function(data, variables, sero_calling_filter, serovar_filter,
                                         label_filter = NULL) {
  # Filter the dataset according to the specified criteria
  filtered_data <- data %>%
    dplyr::filter(Sero_calling == sero_calling_filter) %>%
    dplyr::filter(Serovar == serovar_filter)

  if (!is.null(label_filter)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(Pred_label == label_filter)
  }

  # Initialize results dataframe
  results <- data.frame(
    Variable = character(),
    Pairs = character(),
    median_P1 = numeric(),
    median_P2 = numeric(),
    summary_P1 = character(),
    summary_P2 = character(),
    stringsAsFactors = FALSE
  )

  # Loop through each grouping variable
  for (variable in variables) {
    unique_groups <- unique(filtered_data[[variable]][!is.na(filtered_data[[variable]])])
    pairs <- combn(unique_groups, 2, simplify = FALSE) # Create all pairs of groups

    # Sort pairs ensuring "10" is always in front if it exists
    sorted_pairs <- lapply(pairs, function(x) {
      if ("10" %in% x) {
        sorted_pair <- sort(x, decreasing = TRUE)
      } else {
        sorted_pair <- sort(x, decreasing = FALSE)
      }
      paste(sorted_pair, collapse = " - ")
    })
    unique_sorted_pairs <- unique(sorted_pairs) # Ensure unique pairs
    
    # Iterate over each sorted pair
    for (pair_string in unique_sorted_pairs) {
      # Split the pair string to get group names
      pair <- strsplit(pair_string, " - ")[[1]]
      
      # Filter data for each group in the pair
      group_data_1 <- filtered_data[filtered_data[[variable]] == pair[1],]
      group_data_2 <- filtered_data[filtered_data[[variable]] == pair[2],]
      
      if (nrow(group_data_1) > 0 && nrow(group_data_2) > 0) {
        # Calculate statistics for group 1
        median_P1 <- median(group_data_1$F1, na.rm = TRUE)
        IQR_P1_25 <- quantile(group_data_1$F1, 0.25, na.rm = TRUE)
        IQR_P1_75 <- quantile(group_data_1$F1, 0.75, na.rm = TRUE)
        summary_P1 <- paste(median_P1, " (", IQR_P1_25, "-", IQR_P1_75, ")", sep = "")
        
        # Calculate statistics for group 2
        median_P2 <- median(group_data_2$F1, na.rm = TRUE)
        IQR_P2_25 <- quantile(group_data_2$F1, 0.25, na.rm = TRUE)
        IQR_P2_75 <- quantile(group_data_2$F1, 0.75, na.rm = TRUE)
        summary_P2 <- paste(median_P2, " (", IQR_P2_25, "-", IQR_P2_75, ")", sep = "")
        
        # Append to results
        results <- rbind(results, data.frame(
          Variable = variable,
          Pairs = pair_string,
          median_P1 = median_P1,
          median_P2 = median_P2,
          summary_P1 = summary_P1,
          summary_P2 = summary_P2,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(results)
}

##############################################
#' Generate a name for variables based on prefix, sero_calling, serovar, and pred_label
#'
#' @description Constructs a standardized name by mapping sero_calling and pred_label to short codes and combining with serovar.
#'
#' @param prefix A character string prefix.
#' @param sero_calling The sero_calling string.
#' @param serovar The serovar string.
#' @param pred_label The prediction label string.
#'
#' @return A character string representing a combined name.
##############################################
generate_name <- function(prefix, sero_calling, serovar, pred_label) {
  # Create a mapping for pred_label to names
  label_names <- c("prov", "animal", "animalPlus")
  label_map <- setNames(label_names, pred_label_list)
  
  # Create a mapping for sero_calling to short names
  sero_calling_map <- c("FoodNet_Custom_Label" = "fn", "SISTR_Label" = "sistr")
  
  # Construct the variable name
  paste0(prefix, "_", sero_calling_map[sero_calling], "_", tolower(serovar), "_", label_map[pred_label])
}

##############################################
#' Parameter Analysis Function
#'
#' @description Filters data by sero_calling and serovar (and optional pred_label), generates plots, runs Dunn's tests, and calculates summary statistics.
#' It then stores the results in the provided environment.
#'
#' @param data The input dataframe.
#' @param sero_calling_filter The sero_calling value to filter by.
#' @param serovar_filter The serovar to filter by.
#' @param pred_label Optional label to filter further.
#' @param y_col The column to plot on the y-axis.
#' @param x_col_indices Indices of x columns to plot from x_col_list.
#' @param nrow Number of rows in the combined plot layout.
#' @param ncol Number of columns in the combined plot layout.
#' @param variables_list A list of variables for Dunn's test and summary stats.
#' @param env The environment in which to store the results.
#'
#' @return A list of names of stored results (Plot, Dunn, Summary, Final).
##############################################
param_analysis <- function(data, sero_calling_filter, serovar_filter, pred_label = NULL, 
                             y_col, x_col_indices, nrow, ncol, variables_list, env = globalenv()) {
  # Filter the data according to specified labels
  filtered_data <- data %>%
    filter(Sero_calling == sero_calling_filter) %>%
    filter(Serovar == serovar_filter) 
  
  if (!is.null(pred_label)) {
    filtered_data <- filtered_data %>%
      dplyr::filter(Pred_label == pred_label)
    variables_list <- variables_list[-2]
  }
  else {
    variables_list <- variables_list
  }
  
  # Generate custom plots
  plot_name <- generate_name("plot", sero_calling_filter, serovar_filter, pred_label)
  env[[plot_name]] <- create_custom_plot2(filtered_data, y_col, x_col_list[x_col_indices], 
                                          nrow = nrow, ncol = ncol)
  
  # Perform Dunn's tests excluding "Pred_label"
  dunn_name <- generate_name("dunn", sero_calling_filter, serovar_filter, pred_label)
  env[[dunn_name]] <- run_dunn_tests(data, variables_list, 
                                     sero_calling_filter, serovar_filter, label_filter = pred_label)
  
  # Calculate summary statistics excluding "Pred_label"
  summ_name <- generate_name("summ", sero_calling_filter, serovar_filter, pred_label)
  env[[summ_name]] <- calculate_summary_statistics(data, variables_list, 
                                                   sero_calling_filter, serovar_filter, label_filter = pred_label)
  
  # Join Dunn's test results with summary statistics
  final_name <- generate_name("final", sero_calling_filter, serovar_filter, pred_label)
  env[[final_name]] <- env[[dunn_name]] %>%
    left_join(env[[summ_name]] %>% select(Pairs, median_P1, median_P2, summary_P1, summary_P2), 
              by = "Pairs")
  
  # Return names for user reference
  return(list(Plot = plot_name, Dunn = dunn_name, Summary = summ_name, Final = final_name))
}

##############################################
#' RFE Proportion Analysis
#'
#' @description Filters the data according to specified criteria for data_type, serovar, sero_calling, model, etc.
#' Creates a boxplot for a given y_var vs x_var and performs a Dunn's test.
#'
#' @param data The input dataframe.
#' @param data_type Data types to filter by.
#' @param serovar The serovar of interest.
#' @param sero_calling The sero_calling used.
#' @param all_model Models to include.
#' @param filter_threshold Threshold(s) to filter by.
#' @param sampling_strat Sampling strategy to filter by.
#' @param y_var The y variable name.
#' @param x_var The x variable name.
#' @param pred_label (Optional) Prediction label to filter.
#' @param fe_method (Optional) Feature elimination method to filter.
#' @param test_split (Optional) Train-test splits to filter.
#'
#' @return A list containing the boxplot (ggplot object) and the Dunn test results.
##############################################
RFE_proportion_analysis <- function(data, data_type, serovar, sero_calling, all_model, filter_threshold, 
                                         sampling_strat, y_var, x_var, pred_label = NULL, fe_method = NULL,
                                         test_split = NULL) {
  x_sym <- sym(x_var)
  y_sym <- sym(y_var)
  
  # Filter the data
  filtered_data <- data %>% 
    filter(dataType %in% data_type,
           Serovar == serovar, 
           filter_threshold %in% filter_threshold,
           model_type %in% all_model,
           Feat_elim_method %in% fe_method,
           sampling_strat %in% sampling_strat,
           Sero_calling == sero_calling)
  
  # Apply additional filters if provided
  if (!is.null(pred_label)) {
    filtered_data <- filtered_data %>% 
      filter(Pred_label == pred_label)
  }
  
  if (!is.null(test_split)) {
    filtered_data <- filtered_data %>% 
      filter(train_test_split %in% test_split)
  }
  
  # Create the boxplot
  boxplot <- filtered_data %>% 
    ggplot(aes(x = factor(!!x_sym), y = !!y_sym)) +
    geom_boxplot() +
    xlab(x_var) +
    ylab(y_var) +
    ggtitle(paste("Boxplot of", y_var, "by", x_var))
  
  # Perform Dunn's test
  dunn_test_result <- dunn.test(filtered_data[[y_var]], as.factor(filtered_data[[x_var]]), method = "bonferroni")
  
  # Return the results
  return(list(Boxplot = boxplot, DunnTestResult = dunn_test_result))
}

##############################################
#' Serovar Class Summary
#'
#' @description Filters a dataframe by specified serovars, calculates counts and proportions, and creates a stacked bar plot of relative proportions.
#'
#' @param df Main data frame.
#' @param df2 Secondary data frame for all counts calculation.
#' @param column_name The serovar column name.
#' @param fill_column The column to use for the fill in the plot.
#' @param legend_name The legend title for the fill variable.
#' @param filter_threshold Numeric threshold to include labels in the final plot.
#' @param custom_colors A vector of colors for the fill scale.
#' @param filter_list A vector of serovars to filter by.
#' @param topX_label A label (like "Top_6") to represent a combined category.
#'
#' @return A ggplot object (stacked bar plot).
##############################################
serovar_class_summary <- function(df, df2, column_name, fill_column, legend_name, 
                                  filter_threshold, custom_colors, filter_list, topX_label) {
  # Convert column names to symbols
  column_sym <- sym(column_name)
  fill_sym <- sym(fill_column)
  
  # Filter the DataFrame to include only the serovars in filter_list
  df <- df %>%
    filter(!!column_sym %in% filter_list)
  
  # Count the instances of each Serovar and prediction label column
  serovar_counts <- df %>%
    group_by(!!column_sym, !!fill_sym) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Filter based on the threshold
  filtered_serovar_counts <- serovar_counts %>%
    filter(count >= filter_threshold)
  
  # Calculate total count for each Serovar after filtering
  total_counts <- filtered_serovar_counts %>%
    group_by(!!column_sym) %>%
    summarise(total_count = sum(count), .groups = 'drop')
  
  # Calculate relative proportions
  relative_proportions <- filtered_serovar_counts %>%
    left_join(total_counts, by = column_name) %>%
    mutate(proportion = count / total_count)
  
  # Add total count to Serovar labels
  relative_proportions <- relative_proportions %>%
    mutate(Serovar_label = paste0(!!column_sym, "\n(n=", total_count, ")"))
  
  # Create "TopX" label for the sum of top X serovars
  # topX_counts <- df2 %>%
  #   filter(!!column_sym %in% filter_list) %>%
  #   group_by(!!fill_sym) %>%
  #   summarise(count = n(), .groups = 'drop') %>%
  #   filter(count >= filter_threshold) %>%
  #   mutate(total_count = sum(count)) %>%
  #   mutate(proportion = count / total_count) %>%
  #   mutate(Serovar_label = paste0(topX_label, "\n(n=", total_count, ")"))
  
  # Create "All" label for the sum of all serovars
  all_counts <- df2 %>%
    group_by(!!fill_sym) %>%
    summarise(count = n(), .groups = 'drop') %>%
    filter(count >= filter_threshold) %>%
    mutate(total_count = sum(count)) %>%
    mutate(proportion = count / total_count) %>%
    mutate(Serovar_label = paste0("All\n(n=", total_count, ")"))
  
  # print(all_counts)
  # print(topX_counts)
  
  # Combine the original, top6, and all counts
  combined_counts <- bind_rows(
    relative_proportions %>% select(Serovar_label, !!fill_sym, proportion),
    # topX_counts %>% select(Serovar_label, !!fill_sym, proportion),
    all_counts %>% select(Serovar_label, !!fill_sym, proportion)
  )
  
  combined_counts$Serovar_label <- factor(combined_counts$Serovar_label, 
                                          levels = unique(combined_counts$Serovar_label))
  
  # Generate the stacked barplot with relative proportions
  ggplot(combined_counts, aes(x = Serovar_label, y = proportion, fill = !!fill_sym)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(x = "Serovar", y = "Relative Proportion", fill = legend_name) +
    theme_bw() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_manual(values = custom_colors)
}

##############################################
#' Get Proportions for Each Serovar
#'
#' @description Filters the dataframe for a list of serovars and calculates count and proportion of a specified column.
#'
#' @param df The input dataframe.
#' @param col_name The column name to group and calculate proportions for.
#' @param sero_list_filter A vector of serovars to include.
#' @param serovar_col The column name for serovar.
#'
#' @return A dataframe with counts and proportions of the specified column per serovar.
##############################################
# Function to calculate relative proportions for a column
get_proportions <- function(df, col_name, sero_list_filter, serovar_col) {
  # Initialize an empty list to store data frames for each serovar
  serovar_proportion_list <- list()
  
  # Loop over each serovar in the filter list
  for (serovar in sero_list_filter) {
    # Filter the data for the current serovar
    df_serovar <- df %>%
      filter(!!sym(serovar_col) == serovar) %>%  # Filter by the specific serovar
      group_by(!!sym(col_name)) %>%  # Group by the specified column (e.g., Province)
      summarise(count = n()) %>%  # Calculate the count of each group
      mutate(proportion = count / sum(count)) %>%  # Calculate the proportion
      mutate(Serovar = serovar)  # Add dataset and Serovar columns
    
    # Add the resulting data frame to the list
    serovar_proportion_list[[serovar]] <- df_serovar
  }
  
  # Combine all the data frames in the list into a single data frame
  final_df <- bind_rows(serovar_proportion_list)
  
  return(final_df)
}

##############################################
#' Calculate Mean and CI Matrices
#'
#' @description Given multiple matrices, this function calculates their mean matrix,
#' confidence intervals, and proportion matrices. It allows reordering rows/columns.
#'
#' @param ... Multiple matrices passed as arguments.
#' @param output_mean Variable name for the mean matrix.
#' @param output_ci Variable name for the CI matrix.
#' @param output_prop Variable name for the proportion matrix.
#' @param desired_order (Optional) A vector specifying the desired order of rows/columns.
#'
#' @return Assigns mean, CI, and proportion matrices to global environment.
##############################################
calculate_mean_ci_matrix <- function(..., output_mean = "mean_matrix", output_ci = "mean_ci_mtx",
                                     output_prop = "prop_matrix", desired_order = NULL) {
  # Capture all matrices passed to the function
  matrices <- list(...)
  n <- length(matrices)  # Count the number of matrices passed
  
  # 1. Calculate the mean matrix
  mean_matrix <- Reduce("+", matrices) / n  # Sum all matrices and divide by the number of matrices
  mean_matrix <- round(mean_matrix, 0)  # Round mean to nearest whole number
  
  # 2. Calculate the standard deviation for each element across the n samples
  sd_matrix <- sqrt(Reduce("+", lapply(matrices, function(x) (x - mean_matrix)^2)) / (n - 1))
  
  # 3. Standard error of the mean
  sem_matrix <- sd_matrix / sqrt(n)
  
  # 4. Critical value using the t-distribution, degrees of freedom = n - 1
  t_value <- qt(0.975, df = n - 1)
  
  # 5. Confidence interval matrix
  ci_matrix <- round(t_value * sem_matrix, 0)
  
  # 6. Compute the mean ± CI matrices
  test_ci_mtx <- matrix(paste0(mean_matrix, " ± ", ci_matrix), 
                        nrow = nrow(mean_matrix), 
                        ncol = ncol(mean_matrix))
  rownames(test_ci_mtx) <- rownames(mean_matrix)
  colnames(test_ci_mtx) <- colnames(mean_matrix)
  
  # 7. Calculate the proportion matrix by dividing each element of the mean matrix by the row sum
  row_sums <- rowSums(mean_matrix)  # Sum of each row
  prop_matrix <- (mean_matrix / row_sums)*100  # Divide each element by the respective row sum
  
  # Reorder rows and columns based on desired order, if provided
  if (!is.null(desired_order)) {
    # Find the intersection of existing row/colnames and the desired order
    existing_names <- intersect(desired_order, rownames(mean_matrix))
    
    mean_matrix <- mean_matrix[existing_names, existing_names]
    test_ci_mtx <- test_ci_mtx[existing_names, existing_names]
    prop_matrix <- prop_matrix[existing_names, existing_names]
  }
  
  # Assign the results to user-specified output variable names in the parent environment
  assign(output_mean, mean_matrix, envir = .GlobalEnv)
  assign(output_ci, test_ci_mtx, envir = .GlobalEnv)
  assign(output_prop, round(prop_matrix, 1), envir = .GlobalEnv)
}

##############################################
#' Generate Breaks for a Heatmap
#'
#' @description Given a matrix, this function generates a sequence of breaks for color scaling in a heatmap.
#'
#' @param matrix The input matrix.
#' @param desired_breaks The number of breaks desired.
#'
#' @return A numeric vector of breaks.
##############################################
generate_breaks <- function(matrix, desired_breaks = 5) {
  min_val <- min(matrix)
  max_val <- max(matrix)
  
  # Adjust max_val to the nearest multiple of 5
  max_val <- ceiling(max_val / 5) * 5
  
  # Step size based on range and number of desired breaks
  range_val <- max_val - min_val
  step_size <- ceiling(range_val / (desired_breaks - 1))  # Adjust the step size calculation
  
  # Generate the breaks
  seq(floor(min_val), max_val, by = step_size)
}

##############################################
#' Generate a Heatmap with Custom Cell Labels
#'
#' @description Creates a ComplexHeatmap with given matrix and a label_matrix.
#' Each cell is annotated with text.
#'
#' @param matrix The numeric matrix to plot.
#' @param label_matrix The matrix of labels for each cell.
#' @param cm_breaks The breaks for the color scale.
#' @param light_colors A color function or vector of colors.
#' @param fontsize Font size for text.
#'
#' @return A ComplexHeatmap object.
##############################################
gen_heatmap <- function(matrix, label_matrix, cm_breaks, light_colors, fontsize = 20) {
  
  # Generate the heatmap
  heatmap <- Heatmap(matrix,
                     name = "Proportion (%)",
                     col = light_colors,  # Color palette
                     cluster_rows = FALSE,  # No row clustering
                     cluster_columns = FALSE,  # No column clustering
                     show_heatmap_legend = F,  # Legend will be drawn separately
                     heatmap_legend_param = list(
                       legend_direction = "vertical",
                       at = cm_breaks, 
                       labels = cm_breaks),
                     row_names_side = "left",  # Ensure row labels are on the left
                     column_names_side = "top",  # Ensure column labels are on the top
                     row_title = "True",  # Add row title "True"
                     column_title = "Predicted",  # Add column title "Predicted"
                     row_title_gp = gpar(fontsize = fontsize, fontface = "bold", col = "black"),  # Style row title
                     column_title_gp = gpar(fontsize = fontsize, fontface = "bold", col = "black"),  # Style column title
                     row_names_gp = gpar(fontsize = fontsize),
                     column_names_gp = gpar(fontsize = fontsize),# Row label font size
                     
                     # Custom text inside cells based on label_df
                     cell_fun = function(j, i, x, y, width, height, fill) {
                       grid.text(label_matrix[i, j], x, y, gp = gpar(col = "black", fontsize = fontsize))
                     })
  
  # Draw the heatmap
  return(heatmap)
}

##############################################
#' Generate a Legend for the Heatmap
#'
#' @description Creates a separate legend object for the color scale used in the heatmap.
#'
#' @param cm_breaks A vector of breaks for the legend.
#' @param light_colors The color scale used.
#' @param title Title for the legend.
#' @param direction Direction of the legend ("horizontal" or "vertical").
#'
#' @return A legend object from ComplexHeatmap.
##############################################
gen_legend <- function(cm_breaks, light_colors, title = "Legend", direction = "horizontal") {
  
  # Draw the legend separately
  legend <- Legend(col_fun = light_colors, 
                   title = title, 
                   at = cm_breaks, 
                   labels = cm_breaks, 
                   direction = direction)
  draw(legend)
}

##############################################
#' Calculate Entropy from Counts
#'
#' @description Given a numeric vector of counts, calculates the Shannon entropy.
#'
#' @param counts A numeric vector of counts.
#'
#' @return A numeric value representing entropy.
##############################################
# Define a helper function to calculate entropy based on counts
calculate_entropy_base <- function(counts) {
  # Calculate the probabilities (relative frequencies)
  p_values <- counts / sum(counts)
  
  # Calculate entropy using the entropy formula
  entropy <- -sum(p_values * log2(p_values), na.rm = TRUE)
  
  return(entropy)
}

##############################################
#' Calculate Entropy for Full Data
#'
#' @description Filters data by serovar list if provided. Calculates counts for specified label_col and computes entropy for each serovar.
#'
#' @param df The input dataframe.
#' @param serovar_col The column representing serovar.
#' @param label_col The column used for grouping and counting.
#' @param pred_label A label to store as metadata in the final df.
#' @param dataset A name/string representing the dataset.
#' @param serovar_list (Optional) A vector of serovars to filter by.
#'
#' @return A dataframe containing counts, entropy, and metadata columns.
##############################################
# Define a function to count classes and calculate entropy
calculate_entropy_full <- function(df, serovar_col, label_col, pred_label, dataset, serovar_list = NULL) {
  
  if (!is.null(serovar_list)) {
    df_filtered <- df %>%
      filter(!!sym(serovar_col) %in% serovar_list)
  } else {
    df_filtered <- df
  }
  
  # Step 1: Group by the user-specified column B and the type column
  if (!is.null(serovar_list)) {
    df_counts <- df_filtered %>%
      group_by(!!sym(serovar_col), !!sym(label_col)) %>%  # Group by column B (user-specified) and type
      summarise(n = n(), .groups = "drop")  # Count the occurrences of each type
  } else {
    df_counts <- df_filtered %>%
      group_by(!!sym(label_col)) %>%  # Group only by label
      summarise(n = n(), .groups = 'drop') %>%
      mutate(!!serovar_col := "All")  # Add "All" as the Serovar for the entire dataset
  }
  # Step 2: Calculate entropy for each unique value of column B
  df_entropy <- df_counts %>%
    group_by(!!sym(serovar_col)) %>%  # Group by column B to calculate entropy
    summarise(Entropy = calculate_entropy_base(n), .groups = "drop") # Apply entropy function to the counts
  
  # Step 3: Combine counts and entropy into a single dataframe
  df_final <- df_counts %>%
    left_join(df_entropy, by = serovar_col) %>%  # Join counts with entropy
    rename(Label = label_col, Serovar = !!sym(serovar_col)) %>% 
    mutate(Pred_label = pred_label,
           dataset = dataset)
  
  return(df_final)
}

##############################################
#' Sample Clusters from Data
#'
#' @description For each specified label (in selection_counts), samples a fixed number of unique samples from distinct HC clusters.
#' If not enough unique clusters are found, samples additional rows with replacement.
#'
#' @param data The input dataframe.
#' @param host_label The column name representing the label to filter by (e.g., CombinedSite2).
#' @param selection_counts A dataframe specifying the label, number of samples, and which HC column to use.
#'
#' @return A dataframe of selected samples with HC info added.
##############################################
# Define the function to perform the sampling
sample_clusters <- function(data, host_label, selection_counts) {
  # Initialize an empty data frame to store the final selected samples
  final_selected_samples <- data.frame()
  
  # Loop over each label (CombinedSite2) in the selection_counts
  for (i in 1:nrow(selection_counts)) {
    
    label <- selection_counts[[host_label]][i]
    sample_size <- selection_counts$n_selected[i]
    HC_column <- selection_counts$cluster[i]  # Get the HC column for this label
    
    # Filter the data for the current label
    label_data <- data %>%
      filter(!!sym(host_label) == label)
    
    # Step 1: Get unique HC clusters
    unique_samples <- label_data %>%
      distinct(!!sym(HC_column), .keep_all = TRUE)  # Use !!sym() to dynamically reference the HC column
    
    # Step 2: If fewer unique clusters than required, sample additional
    if (nrow(unique_samples) < sample_size) {
      remaining_count <- sample_size - nrow(unique_samples)
      
      # Sample additional rows from label_data, possibly with replacement
      remaining_samples <- label_data %>%
        sample_n(remaining_count, replace = TRUE)
      
      # Combine unique_samples and remaining_samples
      selected_samples <- bind_rows(unique_samples, remaining_samples)
    } else {
      # If enough unique HC clusters exist, just sample `n_selected` rows
      selected_samples <- unique_samples %>%
        sample_n(sample_size)
    }
    
    # Add columns indicating which HC level was used and the cluster ID
    selected_samples <- selected_samples %>%
      mutate(
        HC_used = HC_column,       # The HC level used (e.g., "HC5")
        HC = .[[HC_column]]        # The cluster ID from the HC level used
      )
    
    # Step 3: Append selected samples for the current label to the final dataframe
    final_selected_samples <- bind_rows(final_selected_samples, selected_samples)
  }
  
  # Return the final selected samples
  return(final_selected_samples %>% 
           select(Serovar, Accession, CombinedSites, !!sym(host_label), HC_used, HC, ID))
}

##############################################
#' Process Selected Samples
#'
#' @description Takes a selected sample dataframe and merges it with global_df and fn_nzv_df to produce a combined dataset.
#'
#' @param selected_sample_df The dataframe of selected samples.
#' @param serovar_name The serovar of interest.
#' @param global_df A global dataframe to join with.
#' @param fn_nzv_df A FoodNet dataframe to join with.
#'
#' @return A combined dataframe containing both selected and FN subsets.
##############################################
process_selected_samples <- function(selected_sample_df, serovar_name, global_df, fn_nzv_df) {
  
  meta_selected <- selected_sample_df %>%
    select(1:4) %>%
    rename(sample_id = "Accession")
  
  fn_meta_selected <- fn_nzv_df %>%
    filter(Serovar == serovar_name) %>%
    select(1:4)
  
  aligned_selected <- meta_selected %>%
    left_join(global_df, by = "sample_id")
  
  aligned_fn <- fn_meta_selected %>%
    left_join(global_df, by = "sample_id")
  
  combined_data <- bind_rows(aligned_selected, aligned_fn)
  
  return(combined_data)
}

# Global list to track seeds for each file pattern
if (!exists("seed_tracker", envir = .GlobalEnv)) {
  seed_tracker <- list()
}

##############################################
#' Generate Variable Name from File Path
#'
#' @description Analyzes a file path to generate a variable name based on prefixes, serovar, data type, and a seed derived from the file name pattern.
#'
#' @param file_path The full file path.
#'
#' @return A character string representing a dynamic variable name or NA if no match found.
##############################################
# Function to create dynamic variable names based on file name patterns
get_variable_name <- function(file_path) {
  
  # Extract the base file name without extension
  base_name <- basename(file_path)
  
  # Define serovar name mappings (Condition 0)
  serovar_mapping <- list(
    Heidelberg = "heid",
    Enteritidis = "entr",
    Typhimurium = "typh",
    Kentucky = "kent",
    Infantis = "infantis",
    i4512 = "i4512"
  )
  
  # Determine the prefix based on the folder
  if (grepl("cm/", file_path)) {
    prefix <- "cm"
  } else if (grepl("features/", file_path)) {
    prefix <- "feat"
  } else if (grepl("test_pred_labels/", file_path)) {
    prefix <- "pred"
  } else {
    prefix <- "unknown"  # Set to "unknown" for files outside expected folders
  }
  
  # Apply serovar prefix
  serovar_prefix <- "unknown"
  for (serovar in names(serovar_mapping)) {
    if (grepl(serovar, base_name)) {
      serovar_prefix <- serovar_mapping[[serovar]]
      break
    }
  }
  
  # Skip files not starting with known serovar
  if (serovar_prefix == "unknown") {
    return(NA)
  }
  
  # Determine the data type based on base name and folder context
  data_type <- "unknown"
  
  # Condition 1: Handle files in cm/ folder
  if (grepl("^cm", base_name)) {
    if (grepl("CombinedSite2", base_name) && grepl("FoodNet", base_name)) {
      data_type <- "fn_animalPlus"
    } else if (grepl("CombinedSites", base_name) && grepl("FoodNet", base_name)) {
      data_type <- "fn_animal"
    } else if (grepl("Enterobase", base_name) && grepl("CombinedSite2", base_name)) {
      data_type <- "global_ebase_animalPlus"
    } else if (grepl("Enterobase", base_name)) {
      data_type <- "global_ebase_animal"
    } else if (grepl("_Combined_", base_name) && grepl("CombinedSite2", base_name)) {
      data_type <- "global_combined_animalPlus"
    } else if (grepl("_Combined_", base_name)) {
      data_type <- "global_combined_animal"
    }
  }
  
  if (grepl("^feat", base_name)) {
    if (grepl("CombinedSite2", base_name) && grepl("FoodNet", base_name)) {
      data_type <- "fn_animalPlus"
    } else if (grepl("CombinedSites", base_name) && grepl("FoodNet", base_name)) {
      data_type <- "fn_animal"
    } else if (grepl("Enterobase", base_name) && grepl("CombinedSite2", base_name)) {
      data_type <- "global_ebase_animalPlus"
    } else if (grepl("Enterobase", base_name)) {
      data_type <- "global_ebase_animal"
    } else if (grepl("_Combined_", base_name) && grepl("CombinedSite2", base_name)) {
      data_type <- "global_combined_animalPlus"
    } else if (grepl("_Combined_", base_name)) {
      data_type <- "global_combined_animal"
    }
  }
  
  if (grepl("^pred", base_name)) {
    if (grepl("CombinedSite2", base_name) && grepl("FoodNet", base_name)) {
      data_type <- "fn_animalPlus"
    } else if (grepl("CombinedSites", base_name) && grepl("FoodNet", base_name)) {
      data_type <- "fn_animal"
    } else if (grepl("Enterobase", base_name) && grepl("CombinedSite2", base_name)) {
      data_type <- "global_ebase_animalPlus"
    } else if (grepl("Enterobase", base_name)) {
      data_type <- "global_ebase_animal"
    } else if (grepl("_Combined_", base_name) && grepl("CombinedSite2", base_name)) {
      data_type <- "global_combined_animalPlus"
    } else if (grepl("_Combined_", base_name)) {
      data_type <- "global_combined_animal"
    }
  }
  
  # # Add the seed (34, 42, 777, etc.)
  # seed <- ifelse(grepl("34", base_name), "1",
  #                ifelse(grepl("42", base_name), "2",
  #                       ifelse(grepl("777", base_name), "3", "unknown")))
  
  # Extract all numeric values from the base_name to identify seeds
  seed_matches <- as.numeric(sub(".*_([0-9]+)\\.csv$", "\\1", base_name))
  
  # Make the pattern key more unique by including prefix, serovar, and data type
  pattern_key <- paste(prefix, serovar_prefix, data_type, sep = "_")
  
  # Filter out only the unique seeds
  if (!is.na(seed_matches) && length(seed_matches) > 0) {
    # If the pattern_key is not in seed_tracker, initialize it
    if (!pattern_key %in% names(seed_tracker)) {
      seed_tracker[[pattern_key]] <<- numeric(0)  # Global assignment
    }
    
    # Add new seeds to the tracker and update globally
    seed_tracker[[pattern_key]] <<- unique(c(seed_tracker[[pattern_key]], seed_matches))
    
    # Sort the seeds for the current pattern
    sorted_seeds <- sort(seed_tracker[[pattern_key]])
    
    # Find the index of the current seed match in the sorted list
    seed <- match(seed_matches, sorted_seeds)
  } else {
    seed <- "unknown"
  }
  
  # Construct the dynamic variable name
  var_name <- paste(prefix, serovar_prefix, data_type, seed, sep = "_")
  
  return(var_name)
}

##############################################
#' Read and Assign CSV Files to Variables
#'
#' @description Reads all CSV files in the specified folder matching a pattern,
#' generates a variable name from `get_variable_name()`, and assigns the data to that variable in the global environment.
#'
#' @param folder The folder to search in.
#' @param pattern The file name pattern to match (default "*.csv").
#'
#' @return Prints status messages and returns no object. Variables are created in the global environment.
##############################################
# Function to read files based on the selected folder and pattern
read_and_assign <- function(folder, pattern = "*.csv") {
  # List the CSV files in the folder
  csv_files <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  
  # Initialize a counter for the number of files loaded
  loaded_count <- 0
  
  # Loop through all files and dynamically assign them to variables
  for (file_path in csv_files) {
    # Generate the variable name based on the file name
    var_name <- get_variable_name(file_path)
    
    # Skip files that do not have a valid variable name
    if (is.na(var_name)) {
      next
    }
    
    # Print the original filename and the variable name it's being turned into
    print(paste("File:", basename(file_path), "-> Variable:", var_name))
    
    # Read the CSV file into the dynamically named variable
    assign(var_name, read.csv(file_path), envir = .GlobalEnv)
    
    # Increment the counter for loaded files
    loaded_count <- loaded_count + 1
    
    # Optionally, print a message after loading the file (for verification)
    print(paste("Loaded file into variable:", var_name))
  }
  
  # Print the total number of files loaded
  print(paste("Total files loaded into variables:", loaded_count))
}

##############################################
#' Generate Variable Name with a Prefix
#'
#' @description Similar to `get_variable_name` but adds a user-specified prefix and handles "set1"/"set2" logic.
#'
#' @param file_path The full file path.
#' @param prefix_name A prefix to add to the generated variable name.
#'
#' @return A character string for the variable name or NA.
##############################################
# Function to create dynamic variable names based on file name patterns
get_variable_name2 <- function(file_path, prefix_name) {
  
  # Extract the base file name without extension
  base_name <- basename(file_path)
  base_name <- sub("\\.csv$", "", base_name)  # Remove .csv extension

  # Determine the data type based on base name and folder context
  data_type <- "unknown"
  
  # Define serovar name mappings (Condition 0)
  serovar_mapping <- list(
    Heidelberg = "heid",
    Enteritidis = "entr",
    Typhimurium = "typh",
    Kentucky = "kent",
    Infantis = "infantis",
    i4512 = "i4512"
  )
  
  # Apply serovar prefix
  serovar_prefix <- "unknown"
  for (serovar in names(serovar_mapping)) {
    if (grepl(serovar, base_name)) {
      serovar_prefix <- serovar_mapping[[serovar]]
      break
    }
  }
  
  # Skip files not starting with known serovar
  if (serovar_prefix == "unknown") {
    return(NA)
  }
  
  # Condition to identify the type from the base name
  if (grepl("CombinedSite2", base_name) && grepl("FoodNet", base_name)) {
    data_type <- "fn_animalPlus"
  } else if (grepl("CombinedSites", base_name) && grepl("FoodNet", base_name)) {
    data_type <- "fn_animal"
  } else if (grepl("Enterobase", base_name) && grepl("CombinedSite2", base_name)) {
    data_type <- "global_ebase_animalPlus"
  } else if (grepl("Enterobase", base_name)) {
    data_type <- "global_ebase_animal"
  } else if (grepl("_Combined_", base_name) && grepl("CombinedSite2", base_name)) {
    data_type <- "global_combined_animalPlus"
  } else if (grepl("_Combined_", base_name)) {
    data_type <- "global_combined_animal"
  }
  
  # Split the data_type into main type and label type
  data_type_split <- unlist(strsplit(data_type, "_"))
  main_type <- paste(data_type_split[1:2], collapse = "_")
  label_type <- data_type_split[3]
  
  # Determine the set based on the presence of "set2" in the file name
  set_value <- ifelse(grepl("set2", base_name, ignore.case = TRUE), "set2", "set1")
  
  # Extract all numeric values from the base_name to identify seeds
  seed_matches <- as.numeric(sub(".*_([0-9]+)$", "\\1", base_name))
  
  # Ensure the seed is found and handle it
  if (!is.na(seed_matches) && length(seed_matches) > 0) {
    # If the data_type is not in seed_tracker, initialize it
    if (!data_type %in% names(seed_tracker)) {
      seed_tracker[[data_type]] <<- numeric(0)  # Global assignment
    }
    
    # Add the new seed to the tracker and update globally
    seed_tracker[[data_type]] <<- unique(c(seed_tracker[[data_type]], seed_matches))
    
    # Sort the seeds for the current data_type
    sorted_seeds <- sort(seed_tracker[[data_type]])
    
    # Find the index of the current seed match in the sorted list
    seed <- match(seed_matches, sorted_seeds)
  } else {
    seed <- "unknown"
  }
  # Construct the dynamic variable name in the desired format
  var_name <- paste(prefix_name, main_type, serovar_prefix, label_type, set_value, seed, sep = "_")
  
  return(var_name)
}

##############################################
#' Read and Assign CSV Files to Variables with a Prefix
#'
#' @description Reads CSV files in the specified folder, uses `get_variable_name2` to generate names, and assigns data to those variables.
#'
#' @param folder The folder path.
#' @param pattern The file name pattern (default "*.csv").
#' @param prefix_name The prefix to add to variable names.
#'
#' @return Prints status messages and loads variables into global environment.
##############################################
# Function to read files based on the selected folder and pattern
read_and_assign2 <- function(folder, pattern = "*.csv", prefix_name) {
  # List the CSV files in the folder
  csv_files <- list.files(path = folder, pattern = pattern, full.names = TRUE)
  
  # Initialize a counter for the number of files loaded
  loaded_count <- 0
  
  # Loop through all files and dynamically assign them to variables
  for (file_path in csv_files) {
    # Generate the variable name based on the file name
    var_name <- get_variable_name2(file_path, prefix_name)
    
    # Skip files that do not have a valid variable name
    if (is.na(var_name)) {
      next
    }
    
    # Print the original filename and the variable name it's being turned into
    print(paste("File:", basename(file_path), "-> Variable:", var_name))
    
    # Read the CSV file into the dynamically named variable
    assign(var_name, read.csv(file_path), envir = .GlobalEnv)
    
    # Increment the counter for loaded files
    loaded_count <- loaded_count + 1
    
    # Optionally, print a message after loading the file (for verification)
    print(paste("Loaded file into variable:", var_name))
  }
  
  # Print the total number of files loaded
  print(paste("Total files loaded into variables:", loaded_count))
}
