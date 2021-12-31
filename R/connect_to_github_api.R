#### Preparation ####

# Load required libraries
library(httr) # Handling http requests
library(jsonlite) # parsing json format

# Set working directory to current script location
current_script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_script_directory)

# Load bespoke functions
source("api_functions.R")
source("contributions.R")

# Get today's date
today <- Sys.Date()
year_ago <- today - 365

# Note git username
github_user <- "JosephCrispell"

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

#### Request commit history ####

# Get urls of repos contributed to
repo_urls <- github_get_repos(github_user, github_api_token)

# Get commit information for all repos for me
my_commit_info <- get_repo_commit_info(
  repo_urls,
  github_api_token,
  author_github_user = github_user,
  date_threshold = year_ago
)

#### Get issues, pull requests, and reviews - TO DO ####
#### Plot contributions graph ####

# Count number of commits by day
commits_by_day <- count_contributions_by_day(
  my_commit_info,
  date_column = "commit.author.date",
  count_name = "n_commits"
)

# Create the contributions matrix
contributions_matrix <- create_contributions_matrix(commits_by_day)
