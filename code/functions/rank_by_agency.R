#########################################################################
# Take a dataset and sort it by agency. Then add percentage and cumulative percentage columns.
# Dataset should contain agency as a fields
rank_by_agency <- function(dataset) {
  sorted_dataset <- dataset %>%
    group_by(agency) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(
      percent = round(count / sum(count) * 100, 2),
      cumulative_percent = cumsum(percent)
    )
  return(sorted_dataset)
}