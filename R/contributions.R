#' Count contributions to GitHub by day
#'
#' @param contribution_info data frame of data about contributions to GitHub
#'                          by date
#' @param date_column date column in contribution_info
#' @param count_name name of column in output for counts. Defaults to "Count".
#'
#' @return counts by date in data frame
#' @export
count_contributions_by_day <- function(contribution_info, date_column,
                                       count_name = "Count") {

  # Extract day from dates
  contribution_info$date <- as.Date(
    trunc(contribution_info[, date_column], "day")
  )

  # Count by day
  dates <- contribution_info[, date_column, drop = TRUE]
  contributions_by_day <- aggregate(dates,
    FUN = length,
    by = list("date" = dates)
  )
  colnames(contributions_by_day)[2] <- "count_name"

  return(contributions_by_day)
}

#' Add empty rows for dates without contributions
#'
#' @param contributions_by_day data frame recording number of each type of
#'                             contribution
#' @param today today's date. Defaults to Sys.Date()
#' @param year_ago date for one year ago. Defaults to today - 365
#'
#' @return contributions_by_day data.frame with empty rows insert for
#'         missing dates
insert_days_with_no_counts <- function(contributions_by_day,
                                       today = Sys.Date(),
                                       year_ago = today - 365) {

  # Note contribution columns
  contributions_columns <- colnames(contributions_by_day)[-1]

  # Create date sequence - to add days with no commits
  dates <- seq(from = year_ago, to = today, by = 1)

  # Note days with no contributions
  days_with_no_contributions <- data.frame(
    date = dates[dates %in% contributions_by_day$date == FALSE]
  )
  days_with_no_contributions <- add_empty_columns(
    days_with_no_contributions, contributions_columns
  )

  # Add to contributions data
  contributions_by_day <- rbind(
    days_with_no_contributions,
    contributions_by_day
  )
  contributions_by_day <- contributions_by_day[
    order(contributions_by_day$date),
  ]

  return(contributions_by_day)
}

#' Add new empty columns by name to data.frame
#'
#' @param data data.frame to add columns to
#' @param columns_to_add vector of column names to add
#'
#' @return data with columns added with NA values
add_empty_columns <- function(data, columns_to_add) {
  for (column in columns_to_add) {
    data[, columns_to_add] <- NA
  }

  return(data)
}

#' Pads contributions by day to fit week structure
#'
#' Days of week order defaults to starting on Sunday. This function
#' pads contributions data.frame if doesn't start on a Sunday and
#' end on a Saturday.
#' @param contributions_by_day data frame recording number of each type of
#'                             contribution
#' @param days_of_week character vector of days of week abbreviations used
#'                     to set week order. Defaults to Sun -> Sat.
#'
#' @return contributions_by_day padded to match week order
pad_contributions_to_week <- function(contributions_by_day,
                                      days_of_week = c(
                                        "Sun", "Mon", "Tue",
                                        "Wed", "Thu", "Fri",
                                        "Sat"
                                      )) {

  # Note contribution columns
  contributions_columns <- colnames(contributions_by_day)[-1]

  # Note days of week
  contributions_by_day$day_of_week <- weekdays(
    contributions_by_day$date,
    abbreviate = TRUE
  )

  # Pad front to start on Sunday
  n_rows_to_add <- which(
    days_of_week == contributions_by_day$day_of_week[1]
  ) - 1
  if (n_rows_to_add != 0) {

    # Create empty rows to add
    rows_to_add <- data.frame(
      "date" = seq(
        from = contributions_by_day$date[1] - 1,
        to = contributions_by_day$date[1] - n_rows_to_add,
        by = -1
      ),
      day_of_week = days_of_week[seq_len(n_rows_to_add)]
    )
    rows_to_add <- add_empty_columns(rows_to_add, contributions_columns)

    # Add rows to start
    contributions_by_day <- rbind(rows_to_add, contributions_by_day)
  }

  # Pad bottom to end on Saturday
  n_rows <- nrow(contributions_by_day)
  day_of_week_index <- which(
    days_of_week == contributions_by_day$day_of_week[n_rows]
  )
  n_rows_to_add <- 7 - day_of_week_index
  if (n_rows_to_add != 0) {

    # Create empty rows to add
    rows_to_add <- data.frame(
      "date" = seq(
        from = contributions_by_day$date[n_rows] + 1,
        to = contributions_by_day$date[n_rows] + n_rows_to_add,
        by = 1
      ),
      day_of_week = days_of_week[
        (day_of_week_index + 1):(day_of_week_index + n_rows_to_add)
      ]
    )
    rows_to_add <- add_empty_columns(rows_to_add, contributions_columns)

    # Add rows to end
    contributions_by_day <- rbind(contributions_by_day, rows_to_add)
  }

  return(contributions_by_day)
}

#' Creates contribution matrix from contributions by day counts
#'
#' @param contributions_by_day data frame recording number of each type of
#'                             contribution
#' @param days_of_week character vector of days of week abbreviations used
#'                     to set week order. Defaults to Sun -> Sat.
#'
#' @return contributions matrix counting contributions by day with rows
#' representing days of the week (starting on a Sunday) and columns
#' representing weeks of a year.
#' @export
create_contributions_matrix <- function(contributions_by_day,
                                        days_of_week = c(
                                          "Sun", "Mon", "Tue",
                                          "Wed", "Thu", "Fri",
                                          "Sat"
                                        )) {

  # Note contribution columns
  contributions_columns <- colnames(contributions_by_day)[-1]

  # Add days that had no contributions
  contributions_by_day <- insert_days_with_no_counts(contributions_by_day)

  # Pad top and bottom of data frame to match week order
  # Starts on Sunday, ends on Saturday - by default
  contributions_by_day <- pad_contributions_to_week(contributions_by_day)

  # Add row sum of contributions columns
  contributions_by_day$total_contributions <- rowSums(
    contributions_by_day[, contributions_columns, drop = FALSE]
  )

  # Transform in matrix with rows as week days and columns as weeks of year
  contributions_matrix <- matrix(
    contributions_by_day$total_contributions,
    nrow = 7
  )

  # Add row names
  rownames(contributions_matrix) <- days_of_week

  # Add column names
  months <- format(contributions_by_day$date, "%b")
  colnames(contributions_matrix) <- months[
    seq(from = 1, to = nrow(contributions_by_day), by = 7)
  ]

  return(contributions_matrix)
}
