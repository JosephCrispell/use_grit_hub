#### Preparation ####

# Load required libraries
library(httr) # Handling http requests
library(jsonlite) # parsing json format

# Set working directory to current script location
current_script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_script_directory)

# Load bespoke functions
source("api_functions.R")

#### Connect to GitHub API ####

# Get credentials from environmental variables
github_app_name <- "joseph_crispell_personal"
github_client_id <- get_environmental_variable("GITHUB_ID")
github_client_secret <- get_environmental_variable("GITHUB_SECRET")

# Connect to the API and prepare for request
github_api_token <- connect_to_github_api(
  app_name = github_app_name,
  id = github_client_id,
  secret = github_client_secret
)

#### Request API data ####

# Get repository names
my_repository_info <- github_api_get_request(
  query_url = "https://api.github.com/users/josephcrispell/repos",
  github_api_token = github_api_token
)
