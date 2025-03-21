# Check if the application is running locally or on Fargate
running_env <- Sys.getenv("RUNNING_ENV", "local")  

# sourcing functions
source('./script/functions.R') 

load_libraries()

Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

# Set default directories if environment variables are not set
input_dir <- Sys.getenv("INPUT_DIR", unset = "/data/input")
output_dir <- Sys.getenv("OUTPUT_DIR", unset = "/data/output")

# see what iam user currently using
curr_role <- get_caller_identity()
print(paste("curr_role: ", curr_role))

# s3 variables
bucket_name <- "batugo-autoscribe"

# getting config files
config_file_name <- Sys.getenv("CONFIG_FILE_NAME", "configs.xlsx")  
variables_file <- file.path(input_dir, "configs.xlsx") # local config path
if (running_env == "fargate") {
  print('hi')
  print(bucketlist())
  # If running on Fargate, download the config file from S3
  s3_config_path <- paste("s3://", bucket_name, "/", config_file_name, sep="")
  print(s3_config_path)
  test <- save_object(object = config_file_name, bucket = bucket_name, file = variables_file) # /data/input/configs.xlsx test
  print(paste('save object:', test))
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
s3_data_path <- paste("s3://", bucket_name, "/", params_data$data, sep="")
local_data_path <- file.path(input_dir, params_data$data)

if (running_env == "fargate") {
  # Download the data file from S3 to ephemeral storage in Fargate
  # get_object(object = s3_data_path, bucket = bucket_name, saveTo = local_data_path)
  save_object(object = s3_data_path, bucket = bucket_name, file = local_data_path)
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
# dir.create(output_file_path)

# output_file_name <- 'scr.html'
render("./script/scr_rmd.Rmd", params = list(title = title), output_file = output_file_path) # outputting as html

if (running_env == "fargate") {
  s3_output_path <- paste("s3://", bucket_name, "/", 'scr.html', sep="")
  put_object(file = output_file_path, object = s3_output_path, bucket = bucket_name)
  cat("Processed data uploaded to S3.\n")
} else {
  cat("Running locally. Processed data is saved locally.\n")
}
# outputting to pdf -- work on pdf output, getting error
# pagedown::chrome_print('./data/output/scr.html', timeout = 5000000000000, output = './data/output/')