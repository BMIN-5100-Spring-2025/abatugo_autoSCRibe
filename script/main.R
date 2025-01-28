# loading in packages
library(DT)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
require(gtsummary)
require(gridExtra)
require(rstatix)
require(arsenal)
require(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
library(rmarkdown)

# sourcing functions
source('./script/functions.R') 

# getting parameters
getting_params() 

# getting report variables
variables <- readxl::read_excel('./data/input/configs.xlsx', sheet = 2) 
report_vars <- variables %>% select(variable) %>% pull()

# getting data
data <- read_csv(paste0('./data/input/', params_data$data))
data <- validate_data_types(data, variables)

selected_variable <- report_vars
major_grouping <- params_data$major_grouping
minor_grouping <- params_data$minor_grouping
participant_id <- params_data$participant_id

# rendering the report
title <- params_data$title
render("./script/scr_rmd.Rmd", params = list(title = title), output_file = '../data/output/scr.html') # outputting as html

# outputting to pdf -- work on pdf output, getting error
# pagedown::chrome_print('./data/output/scr.html', timeout = 5000000000000, output = './data/output/')