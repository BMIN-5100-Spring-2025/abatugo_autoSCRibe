# Check if the application is running locally or on Fargate f
running_env <- Sys.getenv("RUNNING_ENV", "local")  

# sourcing functions
source('./script/functions.R') 

load_libraries()

Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

# get session_id and s3 prefix 
session_id <- Sys.getenv("SESSION_ID", unset = "")
s3_prefix <- if (nzchar(session_id)) paste0(session_id, "/") else ""

print(s3_prefix)

# Set default directories if environment variables are not set
input_dir <- Sys.getenv("INPUT_DIR", unset = "/data/input")
output_dir <- Sys.getenv("OUTPUT_DIR", unset = "/data/output")

# s3 variables
bucket_name <- "batugo-autoscribe"

# getting config files
config_file_name <- Sys.getenv("CONFIG_FILE_NAME", "configs.xlsx")  
variables_file <- file.path(input_dir, "configs.xlsx") # local config path
if (running_env == "fargate") {
  # If running on Fargate, download the config file from S3
  save_object(object = paste0(s3_prefix, "input/", config_file_name), bucket = bucket_name, file = variables_file)
  cat("Downloaded config file from S3.\n")
} else {
  # If running locally, the config file should already be available
  cat("Running locally. Files should be present in the local directories.\n")
}

# getting parameters
getting_params(variables_file) 

# getting report variables
variables <- readxl::read_excel(variables_file, sheet = 2) 
report_vars <- variables %>% select(variable) %>% pull()

# getting data
local_data_path <- file.path(input_dir, params_data$data)

if (running_env == "fargate") {
  # Save object back to S3
  save_object(object = paste0(s3_prefix, "input/", params_data$data), bucket = bucket_name, file = local_data_path)
  cat("Downloaded data from S3. \n")
} else {
  # If running locally, you can work with local files
  cat("Running locally. Files should be in local directories.\n")
}

# data <- read_csv(paste0(input_dir, '/', params_data$data)) # commented out because of fargate
data <- read_csv(local_data_path)
data <- validate_data_types(data, variables)

selected_variable <- report_vars
major_grouping <- params_data$major_grouping
minor_grouping <- params_data$minor_grouping
participant_id <- params_data$participant_id

# rendering the report
title <- params_data$title
dir.create(output_dir)
output_file_path <- file.path(output_dir, "scr.html")

# output_file_name <- 'scr.html'
render("./script/scr_rmd.Rmd", params = list(title = title), output_file = output_file_path) # outputting as html

if (running_env == "fargate") {
  s3_output_path <- paste0(s3_prefix, "output/scr.html")
  put_object(file = output_file_path, object = s3_output_path, bucket = bucket_name)
  cat("Processed data uploaded to S3.\n")
} else {
  cat("Running locally. Processed data is saved locally.\n")
}
# outputting to pdf -- work on pdf output, getting error
# pagedown::chrome_print('./data/output/scr.html', timeout = 5000000000000, output = './data/output/')