# sourcing functions
source('./script/functions.R') 

load_libraries()

Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")


# Set default directories if environment variables are not set
input_dir <- Sys.getenv("INPUT_DIR", unset = "./data/input")
output_dir <- Sys.getenv("OUTPUT_DIR", unset = "./data/output")

# getting parameters
getting_params() 

# getting report variables
variables_file <- file.path(input_dir, "configs.xlsx")
variables <- readxl::read_excel(variables_file, sheet = 2) 
report_vars <- variables %>% select(variable) %>% pull()

# getting data
data <- read_csv(paste0(input_dir, '/', params_data$data))
data <- validate_data_types(data, variables)

selected_variable <- report_vars
major_grouping <- params_data$major_grouping
minor_grouping <- params_data$minor_grouping
participant_id <- params_data$participant_id

# rendering the report
title <- params_data$title
render("./script/scr_rmd.Rmd", params = list(title = title), output_file = file.path(output_dir, "scr.html")) # outputting as html

# outputting to pdf -- work on pdf output, getting error
# pagedown::chrome_print('./data/output/scr.html', timeout = 5000000000000, output = './data/output/')