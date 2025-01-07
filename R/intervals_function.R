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
#' @examples
#' cat_function()
count_contacts_per_interval_of_sp <- function(intervals, rufe_HKX, art, interval_length = 60) {
  # Filter call dataset based on the given species (Art)
  temp_ruf <- rufe_HKX %>%
    filter(Art == art)

  # Calculate the contact count for each interval
  temp <- intervals %>%
    rowwise() %>%
    mutate(
      contact_count = sum(temp_ruf$rz >= interval_start & temp_ruf$rz < interval_start + interval_length)
    ) %>%
    ungroup()

  # return the results as a vector
  return(temp$contact_count)
}
