# Sanity check across all years:
# - na_only:        count of rows with closed_date == NA and created_date < Jan 1(Y)
# - backlog_in:     rows open at the instant Jan 1(Y) (created before Y, not closed before Y)
# - na_only_pct_of_backlog: % of backlog_in that have closed_date == NA

library(data.table)
library(lubridate)

na_only_and_backlog_by_year <- function(DT) {
  stopifnot(data.table::is.data.table(DT))
  if (!("created_date" %in% names(DT))) stop("missing created_date")
  # We don't require closed_date to exist (but it should)
  DTv <- DT[!is.na(created_date)]
  
  years <- sort(unique(year(DTv$created_date)))
  if (!length(years)) return(data.table())
  
  # --- Cumulative created counts by created year ---
  created_by_year <- DTv[, .N, by = .(cy = year(created_date))][order(cy)]
  created_by_year[, created_cum := cumsum(N)]
  setkey(created_by_year, cy)
  
  # --- Cumulative closed counts by closed year (only non-NA closures) ---
  closed_by_year <- DTv[!is.na(closed_date), .N, by = .(cl = year(closed_date))][order(cl)]
  if (nrow(closed_by_year)) {
    closed_by_year[, closed_cum := cumsum(N)]
    setkey(closed_by_year, cl)
  }
  
  # --- Cumulative NA-only (closed_date NA) counts by created year ---
  na_by_created <- DTv[is.na(closed_date), .N, by = .(cy = year(created_date))][order(cy)]
  if (nrow(na_by_created)) {
    na_by_created[, na_cum := cumsum(N)]
    setkey(na_by_created, cy)
  }
  
  # Prepare output frame for each year boundary Jan 1(Y)
  out <- data.table(year = years)
  
  # Values "before year Y" = cumulative up to (Y-1); do rolling joins on Y-1
  out[, created_cum_before := created_by_year[.(year - 1L), created_cum, roll = TRUE]]
  out[is.na(created_cum_before), created_cum_before := 0L]
  
  if (nrow(closed_by_year)) {
    out[, closed_cum_before := closed_by_year[.(year - 1L), closed_cum, roll = TRUE]]
    out[is.na(closed_cum_before), closed_cum_before := 0L]
  } else {
    out[, closed_cum_before := 0L]
  }
  
  if (nrow(na_by_created)) {
    out[, na_only := na_by_created[.(year - 1L), na_cum, roll = TRUE]]
    out[is.na(na_only), na_only := 0L]
  } else {
    out[, na_only := 0L]
  }
  
  # True backlog at the instant Jan 1(Y) = created before Y minus closed before Y
  out[, backlog_in := created_cum_before - closed_cum_before]
  
  # Share of backlog that is NA-only
  out[, na_only_pct_of_backlog := fifelse(backlog_in > 0, 100 * na_only / backlog_in, NA_real_)]
  
  setorder(out, year)
  out[]
}

# ----- Run it on d311 and print nicely -----
res <- na_only_and_backlog_by_year(d311)

cat("\n[Backlog sanity check at Jan 1 of each year]\n")
print(
  res[, .(
    year,
    na_only                 = format(na_only, big.mark = ","),
    backlog_in              = format(backlog_in, big.mark = ","),
    na_only_pct_of_backlog  = ifelse(is.na(na_only_pct_of_backlog), "n/a",
                                     sprintf("%.2f%%", na_only_pct_of_backlog))
  )],
  row.names = FALSE, right = FALSE
)
