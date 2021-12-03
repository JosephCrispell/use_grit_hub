#### Preparation ####

# Load required libraries
library(httr) # Handling http requests
library(jsonlite) # parsing json format

# Set working directory to current script location
current_script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_script_directory)

# Load bespoke functions
source("api_functions.R")

# Note my GitHub account base url
my_username <- "josephcrispell"
my_root_url <- paste0("https://api.github.com/users/", my_username)

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

# Get repository urls
repos_info <- github_api_get_request(
  query_url = paste(my_root_url, "repos", sep = "/"),
  github_api_token = github_api_token
)

# Get the commits for all repos
repo_urls <- paste0(unlist(my_repos_info$url), "/commits")
my_commits <- lapply(head(repo_urls),
  FUN = github_api_get_request,
  github_api_token
)
test <- do.call(rbind, my_commits)

# Get commits for single repo
test <- github_api_get_request(
  query_url = paste0(my_root_url, "basicPlotteR/commits"),
  github_api_token = github_api_token,
  page = 200
)
