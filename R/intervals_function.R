#' Calculating Intervals
#'
#' This function calculates intervals for a given break-duration between a given start time (start) and end time (end) for multiple recordings. Returns only the intervals as a vector to append to existing dataframe.
#' @param timetable dataframe containing minimum two columns indicating starting point and end point of each recording (each row is one recording); columns need to be called start and end for the designated timestamps
#' @param breaks Duration between the intervals given as a String. Defaults to 1 min
#' @keywords bats HK intervals
#' @export
cal_intervals <- function(timetable, breaks = "1 min"){
  intervals <- timetable %>%
    rowwise() %>%
    mutate(interval_list = list(seq(start, end, by = "1 min"))) %>%  # Zeitintervalle erstellen
    unnest(interval_list) %>%
    rename(interval_start = interval_list)
  return(intervals)
}



#' Counting contacts for given Species
#'
#' This function counts the number of contacts across set intervals provided filtering beforehand by a single species.
#' Results are returned as a vector suited for appending to original interval dataframe
#' @param intervals dataframe containing a column indicating the start point of each interval
#' @param rufe_HKX dataframe containing a column with time stamps (rz) for each call recorded as well a a column with species identity (Art)
#' @param art String of the species that the calls should be filtered by
#' @param interval_length Length of the interval in seconds. Defaults to 60s
#' @keywords bats HK
#' @export
count_contacts_per_interval_of_sp <- function(intervals, rufe_HKX, art, interval_length = 60) {
  # Filter call dataset based on the given species (Art)
  temp_ruf <- rufe_HKX %>%
    filter(Art == art)

  # Calculate the contact count for each interval
  temp <- intervals %>%
    rowwise() %>%
    mutate(
      contact_count = sum(temp_ruf$time_call >= interval_start & temp_ruf$time_call < interval_start + interval_length)
    ) %>%
    ungroup()

  # return the results as a vector
  return(temp$contact_count)
}

