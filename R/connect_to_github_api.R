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

# Get today's date
today <- Sys.Date()
year_ago <- today - 365

# Get repository urls
repos_info <- github_api_request(
  query_url = "https://api.github.com/users/JosephCrispell/repos",
  github_api_token = github_api_token
)

# Get the commit information for all repos
repo_urls <- paste0(unlist(repos_info$url), "/commits")
names(repo_urls) <- repos_info$name
my_commits <- lapply(repo_urls,
  FUN = github_api_request_multi_page,
  github_api_token = github_api_token,
  date_time_threshold = strptime(as.character(year_ago), format = "%F"),
  date_time_column = "commit.author.date",
  per_page = 100
)
names(my_commits) <- names(repo_urls)

# Combine results into single data.frame
my_commits <- do.call(rbind, my_commits)
my_commits$repo <- gsub("\\.[[:digit:]]+$", "", rownames(my_commits))

# Filter for commits by me
my_commits <- my_commits[my_commits$author.login == github_user, ]

#### Get issues, pull requests, and reviews ####
#### Plot contributions graph ####

# Count number of commits by day
my_commits$date <- as.Date(trunc(my_commits$commit.author.date, "day"))
commits_by_day <- aggregate(my_commits$date,
  FUN = length,
  by = list("date" = my_commits$date)
)
colnames(commits_by_day)[2] <- "n_commits"

# Create the contributions matrix
contributions_matrix <- create_contributions_matrix(commits_by_day)
