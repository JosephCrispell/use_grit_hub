#' Send request to GitHub API and collect results across multiple pages
#'
#' @param query_url character vector representing URL to use in query
#' @param github_api_token API token to use alongside query
#' @param date_time_threshold only results after date time threshold will be
#'     retained. Defaults to NULL - ignored.
#' @param date_time_column column in results with date time. Defaults to NULL.
#' @param date_time_format format of date times. Defaults to "%FT%TZ"
#'     (e.g. "2021-09-17T19:19:10Z")
#' @param per_page Number of results per page. Defaults to 30. Maximum of 100.
#'
#' @return dataframe with result from query
github_api_request_multi_page <- function(query_url, github_api_token,
                                          date_time_threshold = NULL,
                                          date_time_column = NULL,
                                          date_time_format = "%FT%TZ",
                                          per_page = 30) {

  # Check the date time threshold
  check_date_format(date_time_threshold, date_time_format)

  # Get the first page of results
  cat("\r URL:", query_url, "querying page 1\t\t\t\t\t\t\t\t\t")
  query_results <- github_api_request(
    query_url, github_api_token,
    flatten_nested_dataframes = TRUE,
    per_page = per_page
  )

  # Create a dataframe to store all the results
  all_query_results <- query_results

  # Examine all other pages
  page <- 2
  while (length(query_results) != 0) {

    # Check date threshold hasn't been surpassed
    if (is.null(date_time_threshold) == FALSE &&
      is.null(date_time_column) == FALSE &&
      date_time_column %in% colnames(all_query_results)) {

      # Format the date column
      all_query_results[, date_time_column] <- list(strptime(
        all_query_results[, date_time_column],
        format = date_time_format
      ))

      # Check if any dates before threshold are present
      if (sum(all_query_results[, date_time_column] < date_time_threshold)
      > 0) {
        all_query_results <- all_query_results[
          all_query_results[, date_time_column] > date_time_threshold,
        ]
        break
      }
    }

    # Check if more pages available
    if (nrow(query_results) < per_page) {
      break
    }

    # Get the current page of results
    cat("\r URL:", query_url, "querying page", page, "\t\t\t\t\t\t\t\t")
    query_results <- github_api_request(query_url, github_api_token,
      flatten_nested_dataframes = TRUE,
      page = page,
      per_page = per_page
    )

    # Store the results
    all_query_results <- rbind(all_query_results, query_results)

    # Increment the page
    page <- page + 1
  }

  return(all_query_results)
}

#' Check date format
#'
#' Stops (or sends warning) code if date in wrong format
#' @param date date
#' @param format format of date
#' @param warn send warning if incorrect format. Defaults to FALSE
#'   (throws error).
check_date_format <- function(date, format, warn = FALSE) {
  if (is.null(date) == FALSE &&
    inherits(date, format)) {
    stop(
      "The date provided was of class \"",
      class(date), "\" and should be of class \"", format, "\". ",
      "Please provide the date in the correct format."
    )
  }
}

#' Send request to GitHub API
#'
#' @param query_url character vector representing URL to use in query
#' @param github_api_token API token to use alongside query
#' @param flatten_nested_dataframes Whether to flatten nested dataframes into
#'    single. Defaults to TRUE.
#' @param page API results are paginated, set which page. Defaults to 1
#' @param per_page Number of results per page. Defaults to 30. Maximum of 100.
#'
#' @return dataframe with result from query
github_api_request <- function(query_url, github_api_token,
                               flatten_nested_dataframes = TRUE,
                               page = 1, per_page = 30) {

  # Check query url is string
  check_string(query_url, "API query")

  # Check github api token is correct class
  if (class(github_api_token) != "request") {
    stop(
      "Github API token variable is not of class \"request\".",
      "Variable should be created by connect_to_github_api() function."
    )
  }

  # Check per_page parameter
  if (per_page > 100) {
    per_page <- 100
    warning(
      "The per_page parameter exceeds the maximum value (100) and has been",
      " set to 100."
    )
  }

  # Add page and page length to query url
  query_url <- paste0(query_url, "?page=", page, "&per_page=", per_page)

  # Send request
  request <- httr::GET(query_url, github_api_token)

  # Check status of request - converts to R warnings/errors
  httr::stop_for_status(request)

  # Extract content from a request
  request_jsonlite <- httr::content(request)
  request_json <- jsonlite::toJSON(request_jsonlite)

  # Convert to a data.frame
  request_dataframe <- jsonlite::fromJSON(request_json,
    flatten = flatten_nested_dataframes
  )

  # Check data returned
  if (length(request_dataframe) == 0) {
    warning("No data returned for query!")
  }

  return(request_dataframe)
}

#' Connect to github API
#'
#' Uses the httr functionality to connect to GitHub API and prepare for requests
#' @param app_name name used when creating GitHub app
#' @param id application ID
#' @param secret application secret
#'
#' @return GitHub token for use in GitHub API requests
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
