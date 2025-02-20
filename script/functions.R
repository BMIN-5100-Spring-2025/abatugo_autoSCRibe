## loading libraries
load_libraries <- function() {
  # Define a vector of required package names
  required_packages <- c("DT", "ggplot2", "dplyr", "knitr", "kableExtra", "tidyverse", 
                         "gtsummary", "gridExtra", "rstatix", "arsenal", "readxl", "rmarkdown", "pagedown")
  
  # Loop over each package name
  for(pkg in required_packages) {
    # Check if the package is installed; if not, install it
    if (!require(pkg, character.only = TRUE)) {
      library(pkg, character.only = TRUE)
    }
  }
}

## getting configurations ##
getting_params <- function(){
  
  # param_data <- readxl::read_excel('./data/input/configs.xlsx')
  #print(getwd())  # Check working directory
  #print(list.files("/data/input", full.names = TRUE))
  #print(file.exists("/data/input/configs.xlsx"))  # Should return TRUE
  input_dir <- Sys.getenv("INPUT_DIR", unset = "./data/input")
  output_dir <- Sys.getenv("OUTPUT_DIR", unset = "./data/output")
  param_path <- file.path(input_dir, "configs.xlsx")
  param_data <- readxl::read_excel(param_path)
  # Convert parameters into a named list
  params_data <<- setNames(as.list(param_data$value), param_data$variable) # setting to global environment
  return(params_data)
}

# ensuring correct datatype for variables based on configurations
validate_data_types <- function(data, config) {
  for (i in 1:nrow(config)) {
    var <- config$variable[i]
    dtype <- config$data_type[i]
    
    # Check if the variable exists in the dataset
    if (!var %in% colnames(data)) {
      stop(glue::glue("Variable {var} not found in the dataset."))
    }
    
    # Enforce data type based on config
    data[[var]] <- switch(
      dtype,
      character = as.character(data[[var]]),
      numeric = as.numeric(data[[var]]),
      stop(glue::glue("Unsupported data type: {dtype} for variable {var}."))
    )
  }
  
  return(data)
}

## total enrollment ## 
total_enrollment_tbl_major <- function(data, major_grouping, minor_grouping, participant_id){
  
  # table
  col_for_list <- list(Var1 = gt::md(paste0('**', major_grouping, '**')), `Freq` = gt::md("**Freq**"), 
                       `Cumulative Freq` = gt::md("**Cumulative Freq**"), Percent =
                         gt::md("**Percent**"), `Cumulative Percent` = gt::md("**Cumulative Percent**"))
  
  enrollment <- data %>% distinct(.data[[participant_id]], .data[[minor_grouping]], .data[[major_grouping]])
  
  tbl <- summary(freqlist(table(enrollment[[as.character(major_grouping)]]))) %>% as.data.frame() %>% gt::gt() %>% gt::cols_label(.list = col_for_list)
  
  tbl
  
}

total_enrollment_graph_major <- function(data, major_grouping, minor_grouping, participant_id){
  
  enrollment <- data %>% distinct(.data[[participant_id]], .data[[minor_grouping]], .data[[major_grouping]])
  
  # graph
  count_by_major_grouping <- enrollment %>% 
    group_by(.data[[major_grouping]]) %>%
    summarise(count = n()) %>% ungroup() %>% mutate(cohort = 'cohort')
  
  bar_plot <- ggplot(count_by_major_grouping, aes(x = cohort, y = count, fill = .data[[major_grouping]])) +
    geom_bar(position="dodge", stat="identity", alpha=0.5) +
    labs(
      y = "Count",
      x = ""
    ) +
    theme_minimal() + coord_flip() 
  
  bar_plot
}

# total enrollment
total_enrollment_tbl_minor <- function(data, major_grouping, minor_grouping, participant_id){
  
  # table
  col_for_list <- list(Var1 = gt::md(paste0('**', minor_grouping, '**')), `Freq` = gt::md("**Freq**"), 
                       `Cumulative Freq` = gt::md("**Cumulative Freq**"), Percent =
                         gt::md("**Percent**"), `Cumulative Percent` = gt::md("**Cumulative Percent**"))
  
  enrollment <- data %>% distinct(.data[[participant_id]], .data[[minor_grouping]], .data[[major_grouping]])
  
  tbl <- summary(freqlist(table(enrollment[[as.character(minor_grouping)]]))) %>% as.data.frame() %>% gt::gt() %>% gt::cols_label(.list = col_for_list)
  
  tbl
}

total_enrollment_graph_minor <- function(data, major_grouping, minor_grouping, participant_id){
  
  enrollment <- data %>% distinct(.data[[participant_id]], .data[[minor_grouping]], .data[[major_grouping]])
  
  # graph
  count_by_minor_grouping <- enrollment %>% 
    group_by(.data[[minor_grouping]]) %>%
    summarise(count = n()) %>% ungroup() %>% mutate(cohort = 'cohort')
  
  bar_plot <- ggplot(count_by_minor_grouping, aes(x = cohort, y = count, fill = .data[[minor_grouping]])) +
    geom_bar(position="dodge", stat="identity", alpha=0.5) +
    labs(
      y = "Count",
      x = ""
    ) +
    theme_minimal() + coord_flip() 
  
  bar_plot
}

## summarizing numeric variables ##
# Define the function to get selected statistics
get_selected_stats <- function(option) {
  if (option == "full") {
    return(c("{N_obs}", "{N_miss}", "{N_nonmiss}", "{mean}", "{sd}", "{median}", "{p25}", "{p75}", "{min}", "{max}"))
  } else if (option == "summary") {
    return(c("{mean}", "{sd}", "{median}"))
  } else {
    return(c("{N_obs}", "{mean}", "{sd}"))
  }
}

#Function to get digits based on the selected statistics
numeric_stats_wrapper <- function(data, variable_name, major_grouping, minor_grouping, participant_id){
  
  # getting variables to summarize
  input_dir <- Sys.getenv("INPUT_DIR", unset = "./data/input")
  output_dir <- Sys.getenv("OUTPUT_DIR", unset = "./data/output")
  param_path <- file.path(input_dir, "configs.xlsx")
  summary_type <- readxl::read_excel(param_path, sheet = 2) %>% 
    filter(variable == variable_name) %>% pull(summary_type)
  
  if (summary_type=='all'){
    # Compute summary statistics for numeric variables
    cat("### Summary Statistics", "\n")
    stats_num_all(data, variable_name, major_grouping = major_grouping)
    stats_num(data, variable_name, major_grouping = major_grouping, minor_grouping = minor_grouping)
    cat('<br>\n\n')
    cat("### Summary Visualizations", "\n")
    graphs_num_all(data, variable_name, major_grouping)
    cat('<br>')
    graphs_num(data, variable_name, major_grouping, minor_grouping)
    # for some reason need to add this code because of issues with adding headers after the plots 
    cat('<br>\n\n') 
    cat("### Data Point of Concern", "\n")
    outliers_all(data, variable_name, major_grouping)
    cat('<br>')
    outliers_minor_group(data, variable_name, major_grouping, minor_grouping)
    cat('<br>')
    filter_na(data, participant_id, major_grouping, minor_grouping, variable_name)
    cat('<br>')
  }
  else if (summary_type=='stats_only'){
    # Compute summary statistics for numeric variables
    cat("### Summary Statistics", "\n")
    stats_num_all(data, variable_name, major_grouping = major_grouping)
    cat('<br>')
    stats_num(data, variable_name, major_grouping = major_grouping, minor_grouping = minor_grouping)
    cat('<br>\n\n') 
    cat("### Data Point of Concern", "\n")
    outliers_all(data, variable_name, major_grouping)
    cat('<br>')
    outliers_minor_group(data, variable_name, major_grouping, minor_grouping)
    cat('<br>')
    filter_na(data, participant_id, major_grouping, minor_grouping, variable_name)
    cat('<br>')
  }
  
  else if (summary_type=='visualizations_only'){
    cat("### Summary Visualizations", "\n")
    graphs_num_all(data, variable_name, major_grouping)
    cat('<br>')
    graphs_num(data, variable_name, major_grouping, minor_grouping)
    # for some reason need to add this code because of issues with adding headers after the plots 
    cat('<br>\n\n') 
    cat("### Data Point of Concern", "\n")
    outliers_all(data, variable_name, major_grouping)
    cat('<br>')
    outliers_minor_group(data, variable_name, major_grouping, minor_grouping)
    cat('<br>')
    filter_na(data, participant_id, major_grouping, minor_grouping, variable_name)
    cat('<br>')
  }
  
}

get_selected_digits <- function(selected_stats) {
  # Create a default list of digits for common statistics
  default_digits <- c(
    "{mean}" = 2, 
    "{sd}" = 2, 
    "{median}" = 2, 
    "{p25}" = 2, 
    "{p75}" = 2, 
    "{min}" = 2, 
    "{max}" = 2,
    "{N_obs}" = 2,
    "{N_miss}" = 2,
    "{N_nonmiss}" = 2
  )
  
  # Filter the digits to only include the selected statistics
  selected_digits <- default_digits[names(default_digits) %in% selected_stats]
  
  # Return the list of digits
  return(selected_digits)
}

stats_num_all <- function(df, selected_variable, major_grouping){
  tbl <- data %>% select(all_of(c(major_grouping, selected_variable)))  %>%
    tbl_summary(
      by = major_grouping,
      type = list(selected_variable ~ "continuous2"),
      digits = list(selected_variable ~ get_selected_digits(get_selected_stats("full"))),  # Use dynamic digits here
      statistic = list(
        selected_variable ~ get_selected_stats("full")  # Use dynamic statistics here
      ),
      missing = 'no'
    ) %>%
    modify_header(
      all_stat_cols() ~ "**{level}**"
    ) %>%
    modify_footnote(
      all_stat_cols() ~ "mean = Mean, sd = Standard Deviation"
    ) %>%
    add_stat_label() %>%
    bold_labels() 
  
  print(tbl)
}

stats_num <- function(data, selected_variable, major_grouping, minor_grouping){
  tbl <- data %>% select(all_of(c(major_grouping, minor_grouping, selected_variable))) %>% tbl_strata(., strata =
                                                                                                        major_grouping, 
                                                                                                      .header = "**{strata}**, N = {n}",
                                                                                                      .tbl_fun =
                                                                                                        ~ .x %>%
                                                                                                        tbl_summary(
                                                                                                          by = minor_grouping,
                                                                                                          type = list(selected_variable ~ "continuous2"),
                                                                                                          digits = list(selected_variable ~ get_selected_digits(get_selected_stats("full"))),  # Use dynamic digits here
                                                                                                          statistic = list(
                                                                                                            selected_variable ~ get_selected_stats("full")  # Use dynamic statistics here
                                                                                                          ),
                                                                                                          missing = 'no') %>%
                                                                                                        modify_header(all_stat_cols() ~ "**{level}**") %>%
                                                                                                        modify_footnote(all_stat_cols() ~ "mean = Mean, sd = Standard Deviation") %>%
                                                                                                        add_stat_label() %>%
                                                                                                        bold_labels())
  
  print(tbl)
}

graphs_num_all <- function(data, var, major_grouping){
  
  # Boxplot using the unquoted symbol
  boxplot <- ggplot(data, aes(x = .data[[major_grouping]], y = .data[[var]],, fill = .data[[major_grouping]])) +
    geom_boxplot(outlier.colour = "black", outlier.shape = 8, outlier.size = 4) +
    ylab(var) +  # Use the string variable name for the label
    theme_classic()
  
  # Histogram using the unquoted symbol
  hist <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(color = .data[[major_grouping]], fill = .data[[major_grouping]]), 
                   position = "identity", bins = 10, alpha = 0.4) +  
    xlab(var) +  # Use the string variable name for the label
    theme_classic()
  
  # Arrange and display the plots
  grid.arrange(boxplot, hist, ncol = 1)
}

graphs_num <- function(data, var, major_grouping, minor_grouping){
  boxplot <- 
    ggplot(data, aes(x=.data[[major_grouping]], y=.data[[var]], fill = .data[[major_grouping]])) + geom_boxplot(outlier.colour="black",
                                                                                                                outlier.shape=8,
                                                                                                                outlier.size=4) +
    facet_grid(.~.data[[minor_grouping]]) + 
    ylab(var) + 
    theme_classic()
  
  hist <- ggplot(data, aes(x=.data[[var]])) +
    geom_histogram(aes(color = .data[[major_grouping]], fill = .data[[major_grouping]]), 
                   position = "identity", bins = 10, alpha = 0.4) + facet_grid(.~.data[[minor_grouping]]) + 
    xlab(var) + 
    theme_classic()
  
  grid.arrange(boxplot, hist, ncol = 1)
}

## summarizing categorical variables ##
cat_stats_wrapper <- function(data, variable_name, major_grouping, minor_grouping, participant_id){
  
  # getting variables to summarize
  input_dir <- Sys.getenv("INPUT_DIR", unset = "./data/input")
  output_dir <- Sys.getenv("OUTPUT_DIR", unset = "./data/output")
  param_path <- file.path(input_dir, "configs.xlsx")
  summary_type <- readxl::read_excel(param_path, sheet = 2) %>% 
    filter(variable == variable_name) %>% pull(summary_type)
  
  if (summary_type=='all'){
    cat("### Summary Statistics", "\n")
    stats_cat_major_grouping(data, major_grouping, variable_name)
    cat('<br>\n\n') 
    stats_cat_minor_grouping(data, major_grouping, minor_grouping, variable_name)
    #cat('<br>')
    cat('<br>\n\n') 
    cat("### Summary Visualizations", "\n")
    graphs_cat(data, variable_name, major_grouping, minor_grouping)
    #cat('<br>')
    cat('<br>\n\n') 
    cat("### Data Point of Concern", "\n")
    filter_na(data, participant_id, major_grouping, minor_grouping, variable_name)
  }
  
  else if (summary_type=='stats_only'){
    cat("### Summary Statistics", "\n")
    stats_cat_major_grouping(data, major_grouping, variable_name)
    cat('<br>\n\n') 
    stats_cat_minor_grouping(data, major_grouping, minor_grouping, variable_name)
    #cat('<br>')
    cat('<br>\n\n') 
    cat("### Data Point of Concern", "\n")
    filter_na(data, participant_id, major_grouping, minor_grouping, variable_name)
  }
  
  else if (summary_type=='visualizations_only'){
    cat("### Summary Visualizations", "\n")
    graphs_cat(data, variable_name, major_grouping, minor_grouping)
    #cat('<br>')
    cat('<br>\n\n') 
    cat("### Data Point of Concern", "\n")
    filter_na(data, participant_id, major_grouping, minor_grouping, variable_name)
  }
  
}

# by major grouping
stats_cat_major_grouping <- function(data, major_grouping, var){
  
  tbl <- data %>% 
    select(.data[[major_grouping]], .data[[var]]) %>% 
    tbl_summary(by=major_grouping)
  
  print(tbl)
}

# by minor grouping 
stats_cat_minor_grouping <- function(data, major_grouping, minor_grouping, var){
  
  tbl <- data %>% 
    select(.data[[major_grouping]], .data[[var]], .data[[minor_grouping]]) %>% 
    tbl_strata(strata = major_grouping, 
               .tbl_fun = 
                 ~ .x %>% tbl_summary(by=minor_grouping))
  
  print(tbl)
}

graphs_cat <- function(data, var, major_grouping, minor_grouping){
  
  # Calculate count per major_grouping
  count_by_major_grouping <- data %>% 
    group_by(.data[[major_grouping]], .data[[minor_grouping]], .data[[var]]) %>%
    summarise(count = n()) %>% ungroup()
  
  # Create the bar plot for counts per group
  bar_plot <- ggplot(count_by_major_grouping, aes(x = .data[[var]], y = count, fill = .data[[minor_grouping]], color = .data[[minor_grouping]])) +
    geom_bar(position="dodge", stat="identity", alpha=0.5) +
    labs(
      y = "Count",
      x = ""
    ) +
    theme_minimal() + facet_grid(.~.data[[major_grouping]]) + theme(axis.text.x = element_text(angle = 90))
  
  print(bar_plot)
  
}

## problematic data points functions ## 
outliers_all <- function(data, var, major_grouping){
  
  tbl <- data %>% select(all_of(c(major_grouping, var))) %>%
    group_by(.data[[major_grouping]]) %>%  # Group by the column name dynamically
    identify_outliers(.data[[var]]) %>%  # Identify outliers dynamically for the variable 
    ungroup() %>%
    gt::gt() %>% 
    gt::tab_header(
      title = gt::md(paste0("Outliers: **", var, "**")),
      subtitle = gt::md(paste0("For all in each ", major_grouping))
    ) %>% gt::tab_options(table.font.size = 14)
  
  print(tbl)
  
}

outliers_minor_group <- function(data, var, major_grouping, minor_grouping){
  
  tbl <- data %>% select(all_of(c(major_grouping, minor_grouping, var))) %>%
    group_by(.data[[major_grouping]], .data[[minor_grouping]]) %>%
    identify_outliers(.data[[var]]) %>% 
    ungroup() %>%
    gt::gt() %>% 
    gt::tab_header(
      title = gt::md(paste0("Outliers: **", var, "**")),
      subtitle = gt::md(paste0("Grouped by: ", major_grouping, ", ", minor_grouping))
    ) %>% gt::tab_options(table.font.size = 14)
  
  print(tbl)
  
}

filter_na <- function(data, participant_id, major_grouping, minor_grouping, var) {
  
  tbl <- data %>%
    # Select relevant columns
    select(all_of(c(participant_id, major_grouping, minor_grouping, var))) %>%
    # Create a new column that checks for NA in the variable column
    mutate(is_na = is.na(data[[var]])) %>%
    # Filter rows where the new column is TRUE
    filter(is_na) %>%
    # Drop the temporary is_na column
    select(-is_na) %>%
    ungroup() %>%
    # Create the table with gt
    gt::gt() %>%
    gt::tab_header(
      title = gt::md(paste0("Missing: **", var, "**"))
    ) %>%
    gt::tab_options(table.font.size = 14)
  
  print(tbl)
}
