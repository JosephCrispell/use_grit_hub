#' Send request to GitHub API
#'
#' @param query_url character vector representing URL to use in query
#' @param github_api_token  API token to use alongside query
#'
#' @return dataframe with result from query
github_api_get_request <- function(query_url, github_api_token) {

  # Check query url is string
  check_string(query_url, "API query")

  # Check github api token is correct class
  if (class(github_api_token) != "request") {
    stop(
      "Github API token variable is not of class \"request\".",
      "Variable should be created by connect_to_github_api() function."
    )
  }

  # Send request
  request <- httr::GET(query_url, github_api_token)

  # Check status of request - converts to R warnings/errors
  httr::stop_for_status(request)

  # Extract content from a request
  request_jsonlite <- httr::content(request)
  request_json <- jsonlite::toJSON(request_jsonlite)

  # Convert to a data.frame
  request_dataframe <- jsonlite::fromJSON(request_json)

  return(request_dataframe)
}

#' Connect to github API
#'
#' Uses the httr functionality to connect to GitHub API and prepare for requests
#' @param app_name
#' @param id
#' @param secret
#'
#' @return
#' @export
#'
#' @examples
connect_to_github_api <- function(app_name, id, secret) {

  # Check app_name, id, and secret
  check_string(app_name, "App name")
  check_string(id, "ID")
  check_string(secret, "Secret")

  # Get the github end point (for making requests with credentials)
  github_end_point <- httr::oauth_endpoints("github")

  # Set up the application with credentisl
  github_app <- httr::oauth_app(
    appname = app_name,
    key = id,
    secret = secret
  )

  # Create github token with authorisation end point and application
  github_token <- httr::oauth2.0_token(github_end_point, github_app)

  # Configure the token for use
  github_token <- httr::config(token = github_token)

  return(github_token)
}

#' Check character vector
#'
#' Checks character vector is a character vector and isn't empty
#' @param value Character vector variable to check
#' @param name Name of character vector to use in error.
#'             Defaults to "variable"
check_string <- function(value, name = "variable") {

  # Check if character
  if (is.character(value) == FALSE) {
    stop(name, " provided isn't character vector!")
  }

  # Check non-empty
  if (value == "") {
    stop(name, " provided is empty!")
  }
}

#' Get environmental variable
#'
#' Wrapper for Sys.getenv() with check if empty
#' @param variable_name name of environmental variable
#'
#' @return string repsenting environmental variable
get_environmental_variable <- function(variable_name) {

  # Check if variable name a string
  if (is.character(variable_name) == FALSE) {
    stop(
      "Environmental variable name provided",
      "is not a character vector!"
    )
  }

  # Get value
  value <- Sys.getenv(variable_name)

  # Check not empty
  if (value == "") {
    warning(
      "Environmental variable provided (\"",
      variable_name, "\") is empty!"
    )
  }

  return(value)
}
