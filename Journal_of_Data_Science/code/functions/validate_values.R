#' Validate that a vector's values are in a whitelist of allowed values
#'
#' @param x        Character/factor/atomic vector to validate.
#' @param allowed  Vector of allowed values.
#' @param field    Label used in messages (defaults to deparsed name of x).
#' @param ignore_case Logical; compare case-insensitively (default TRUE).
#' @param trim     Logical; trim leading/trailing whitespace before compare.
#' @param normalize Optional function to transform BOTH x and allowed (e.g., to
#'                  collapse punctuation); applied after trim/ignore_case.
#' @param top_n    How many invalid values to show in the summary table.
#' @param quiet    If FALSE, print a concise summary + top_n table.
#' @param use_fastmatch Logical; if TRUE and fastmatch is available, use it.
#' @return A list with:
#'   - field, valid (TRUE/FALSE),
#'   - counts: total, nonblank, blank, valid, invalid, unique_invalid
#'   - pct_invalid_nonblank
#'   - invalid_values (vector, nonblank only)
#'   - invalid_summary (data.frame: value, count, percentage, cumulative_percentage)
#' @export
validate_values <- function(
    x,
    allowed,
    field = deparse(substitute(x)),
    ignore_case = TRUE,
    trim = TRUE,
    normalize = NULL,
    top_n = 10L,
    quiet = FALSE,
    use_fastmatch = FALSE
) {
  # Coerce to character for display
  x_chr <- as.character(x)
  allowed_chr <- as.character(allowed)
  
  if (trim) {
    x_chr <- trimws(x_chr)
    allowed_chr <- trimws(allowed_chr)
  }
  
  if (ignore_case) {
    x_cmp <- toupper(x_chr)
    allowed_cmp <- toupper(allowed_chr)
  } else {
    x_cmp <- x_chr
    allowed_cmp <- allowed_chr
  }
  
  if (!is.null(normalize)) {
    stopifnot(is.function(normalize))
    x_cmp <- normalize(x_cmp)
    allowed_cmp <- normalize(allowed_cmp)
  }
  
  # Blanks / missing
  is_blank <- is.na(x_cmp) | x_cmp == ""
  n_blank  <- sum(is_blank)
  nonblank <- !is_blank
  x_nb_cmp <- x_cmp[nonblank]
  
  # Deduplicate allowed
  allowed_cmp <- unique(allowed_cmp)
  
  # Membership test
  if (use_fastmatch && requireNamespace("fastmatch", quietly = TRUE)) {
    in_list <- !is.na(fastmatch::fmatch(x_nb_cmp, allowed_cmp))
  } else {
    in_list <- x_nb_cmp %in% allowed_cmp
  }
  
  # Invalids: keep original case for readability
  invalid_values <- x_chr[nonblank][!in_list]
  
  n_nonblank <- sum(nonblank)
  n_invalid  <- length(invalid_values)
  n_valid    <- n_nonblank - n_invalid
  
  pct_invalid_nonblank <- if (n_nonblank > 0L) {
    round(100 * n_invalid / n_nonblank, 2)
  } else {
    NA_real_
  }
  
  # Frequency table for invalids
  if (n_invalid > 0L) {
    tab <- sort(table(invalid_values), decreasing = TRUE)
    invalid_summary <- data.frame(
      value = names(tab),
      count = as.integer(tab),
      stringsAsFactors = FALSE
    )
    invalid_summary$percentage <- round(100 * (invalid_summary$count / sum(invalid_summary$count)), 2)
    invalid_summary$cumulative_percentage <- cumsum(invalid_summary$percentage)
  } else {
    invalid_summary <- data.frame(
      value = character(), count = integer(),
      percentage = numeric(), cumulative_percentage = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  res <- list(
    field = field,
    valid = (n_invalid == 0L),
    counts = list(
      total          = length(x_chr),
      nonblank       = n_nonblank,
      blank          = n_blank,
      valid          = n_valid,
      invalid        = n_invalid,
      unique_invalid = length(unique(invalid_values))
    ),
    pct_invalid_nonblank = pct_invalid_nonblank,
    invalid_values  = invalid_values,
    invalid_summary = invalid_summary
  )
  class(res) <- c("validation_result", class(res))
  
  if (!quiet) {
    if (n_invalid > 0L) {
      cat(
        sprintf(
          "\nThere are %s invalid %s entries (%.2f%% of non-blank entries), \ncomprised of %s unique values.\n",
          format(n_invalid, big.mark = ","), field, pct_invalid_nonblank,
          format(length(unique(invalid_values)), big.mark = ",")
        )
      )
      n_show <- min(top_n, nrow(invalid_summary))
      if (n_show > 0L) {
        cat(sprintf("\nTop %d invalid '%s':\n", n_show, field))
        print(utils::head(invalid_summary, n_show), row.names = FALSE, right = FALSE)
      }
    } else {
      cat(sprintf("\nAll non-blank values of '%s' are valid.\n", field))
      if (n_blank > 0L) {
        cat(sprintf("(Note: %s NA values were excluded.)\n", format(n_blank, big.mark=",")))
      }
    }
  }
  
  res
}
