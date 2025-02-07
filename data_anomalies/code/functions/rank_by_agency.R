
#########################################################################
# Take a dataset and sort it by agency. Then add percentage and cumulative percentage columns.
# Dataset should contain agency as a fields

rank_by_agency <- function(dataset) {
  sorted_dataset <- dataset %>%
    group_by(.data$agency) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(desc(.data$count)) %>%
    mutate(
      percent = round(.data$count / sum(.data$count) * 100, 2),
      cumulative_percent = cumsum(.data$percent)
    )
  return(sorted_dataset)
}

#########################################################################