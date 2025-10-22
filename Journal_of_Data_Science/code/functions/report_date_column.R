report_date_column <- function(colname, out, original_na, fixed_count) {
  total <- length(out)
  success <- sum(!is.na(out))
  fail <- sum(is.na(out) & !original_na)
  pct_fail <- round(100 * fail / total, 4)
  
  if (fail == 0) {
    cat(sprintf(" - %s converted successfully (%s rows)\n",
                colname, format(success, big.mark = ",")))
  } else {
    cat(sprintf(
      "\n - %s: %s converted, %s failed (%.4f%%), %s DST-fixed\n",
      colname,
      format(success, big.mark = ","),
      format(fail,    big.mark = ","),
      pct_fail,
      format(fixed_count, big.mark = ",")
    ))
  }
}
