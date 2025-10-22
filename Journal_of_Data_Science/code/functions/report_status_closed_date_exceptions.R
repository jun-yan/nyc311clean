# ---- Exceptions-only console report + adaptive Pareto charts ----------------
report_status_closed_date_exceptions <- function(
    DT,
    status_col     = "status",
    closed_col     = "closed_date",
    closed_values  = c("Closed"),
    id_col         = NULL,
    n_show         = 10L,
    make_charts    = FALSE,
    chart_dir      = NULL,
    x_col_name     = "agency",   # grouping variable to plot by
    top_n          = 30L,
    flip           = FALSE,
    min_count      = 1L
) {
  stopifnot(data.table::is.data.table(DT))
  stopifnot(status_col %in% names(DT), closed_col %in% names(DT))
  
  if (make_charts) {
    if (is.null(chart_dir) || !nzchar(chart_dir)) {
      stop("make_charts=TRUE requires a non-empty chart_dir.")
    }
    if (!exists("plot_pareto_combo", mode = "function")) {
      warning("plot_pareto_combo() not found; charts will be skipped.")
      make_charts <- FALSE
    } else {
      dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  total_n <- nrow(DT)
  status_vec <- DT[[status_col]]
  
  # Classify rows
  is_closed_status <- !is.na(status_vec) & (toupper(status_vec) %chin% toupper(closed_values))
  has_closed_date  <- !is.na(DT[[closed_col]])
  
  # Denominators
  den_closed_status <- sum(is_closed_status)
  den_has_cdate     <- sum(has_closed_date)
  
  # Exceptions
  idx_A <- which(is_closed_status & !has_closed_date)   # CLOSED but missing date
  idx_B <- which(!is_closed_status & has_closed_date)   # NOT CLOSED but has date
  n_A   <- length(idx_A)
  n_B   <- length(idx_B)
  
  pct_A <- if (den_closed_status == 0) NA_real_ else 100 * n_A / den_closed_status
  pct_B <- if (den_has_cdate == 0)     NA_real_ else 100 * n_B / den_has_cdate
  
  # Pretty helpers
  fmtI <- function(x) format(x, big.mark = ",", scientific = FALSE)
  fmtP <- function(x) if (is.na(x)) "n/a" else sprintf("%.3f%%", x)
  
  cat("\nStatus vs closed_date — Exceptions Only\n")
  cat("--------------------------------------------------------------\n")
  cat(sprintf("Denominator A (CLOSED status): %s\n", fmtI(den_closed_status)))
  cat(sprintf("A) CLOSED status but missing %s: %s (%s of CLOSED)\n",
              closed_col, fmtI(n_A), fmtP(pct_A)))
  cat(sprintf("\nDenominator B (has %s): %s\n", closed_col, fmtI(den_has_cdate)))
  cat(sprintf("B) NOT CLOSED status but has %s: %s (%s of has-%s)\n\n",
              closed_col, fmtI(n_B), fmtP(pct_B), closed_col))
  
  # Small previews
  cols_preview <- unique(na.omit(c(id_col, status_col, closed_col, "created_date", "agency", "complaint_type")))
  if (length(cols_preview)) {
    if (n_A > 0) {
      cat(sprintf("Preview A — first %d rows (CLOSED status, missing %s):\n",
                  min(n_show, n_A), closed_col))
      print(DT[idx_A, ..cols_preview][1:min(n_show, .N)]); cat("\n")
    }
    if (n_B > 0) {
      cat(sprintf("Preview B — first %d rows (NOT CLOSED, has %s):\n",
                  min(n_show, n_B), closed_col))
      print(DT[idx_B, ..cols_preview][1:min(n_show, .N)]); cat("\n")
    }
  }
  
  # ---------- Charts (adaptive) ----------
  if (make_charts && (n_A > 0 || n_B > 0)) {
    if (!(x_col_name %in% names(DT))) {
      warning(sprintf("Charts skipped: x_col_name '%s' not found in DT.", x_col_name))
    } else {
      dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
      
      .pareto_bridge <- function(DT_sub, filename, title) {
        fmls <- names(formals(plot_pareto_combo))
        # recognize your plotter's grouping arg
        x_arg <- if ("x_col"  %in% fmls) "x_col"  else
          if ("x_name" %in% fmls) "x_name" else
            if ("x_expr" %in% fmls) "x_expr" else
              if ("by_col" %in% fmls) "by_col" else
                if ("x"      %in% fmls) "x"      else NA_character_
        
        # base args limited to what the plotter actually accepts
        base <- list(DT = DT_sub, chart_dir = chart_dir, filename = filename, title = title)
        optional <- list(top_n = top_n, min_count = min_count, flip = flip,
                         include_na = TRUE, show_threshold_80 = TRUE)
        args <- c(base, optional[intersect(names(optional), fmls)])
        
        if (is.na(x_arg)) {
          # no grouping arg in signature; try call as-is (your plotter will error if it needs x_expr)
          res0 <- try(do.call(plot_pareto_combo, args), silent = TRUE)
          if (inherits(res0, "try-error")) {
            stop("plot_pareto_combo() has no x/by parameter and calling without it failed: ",
                 conditionMessage(attr(res0, "condition")))
          }
          return(invisible(TRUE))
        }
        
        # If the plotter wants x_expr (NSE), prefer BARE symbol first
        if (x_arg == "x_expr") {
          args_bare <- args; args_bare[[x_arg]] <- as.name(x_col_name)  # bare agency
          res_bare <- try(do.call(plot_pareto_combo, args_bare), silent = TRUE)
          if (!inherits(res_bare, "try-error")) {
#            cat("  -> Plotted using NSE bare interface (", x_arg, " = ", x_col_name, ")\n", sep = "")
            return(invisible(TRUE))
          }
          # fallback: string (in case your plotter also supports it)
          args_str <- args; args_str[[x_arg]] <- x_col_name
          res_str <- try(do.call(plot_pareto_combo, args_str), silent = TRUE)
          if (!inherits(res_str, "try-error")) {
#            cat("  -> Plotted using NSE string fallback (", x_arg, " = \"", x_col_name, "\")\n", sep = "")
            return(invisible(TRUE))
          }
          stop("Both bare and string calls to plot_pareto_combo(x_expr=...) failed.\n",
               "  Bare error: ",   conditionMessage(attr(res_bare, "condition")), "\n",
               "  String error: ", conditionMessage(attr(res_str,  "condition")))
        }
        
        # Otherwise (x_col/by_col/x): try string then bare
        args1 <- args; args1[[x_arg]] <- x_col_name
        res1 <- try(do.call(plot_pareto_combo, args1), silent = TRUE)
        if (!inherits(res1, "try-error")) {
#          cat("  -> Plotted using string interface (", x_arg, " = \"", x_col_name, "\")\n", sep = "")
          return(invisible(TRUE))
        }
        args2 <- args; args2[[x_arg]] <- as.name(x_col_name)
        res2 <- try(do.call(plot_pareto_combo, args2), silent = TRUE)
        if (!inherits(res2, "try-error")) {
          cat("  -> Plotted using bare interface (", x_arg, " = ", x_col_name, ")\n", sep = "")
          return(invisible(TRUE))
        }
        stop("Both string and bare calls to plot_pareto_combo() failed.\n",
             "  String error: ", conditionMessage(attr(res1, "condition")), "\n",
             "  Bare   error: ", conditionMessage(attr(res2, "condition")))
      }
      
      if (n_A > 0) {
        fn_A  <- sprintf("pareto_closed_status_missing_%s_by_%s.pdf", closed_col, x_col_name)
        ttl_A <- sprintf("CLOSED status but missing %s (%.3f%% of CLOSED)", closed_col, pct_A)
        cat(sprintf("Plot A: n=%s | file=%s\n", fmtI(n_A), fn_A))
        .pareto_bridge(DT[idx_A], fn_A, ttl_A)  # REMOVE x_col = x_col_name
#        cat(sprintf("  -> Saved: %s/%s\n", chart_dir, fn_A))
      } else cat("Plot A: skipped (no exceptions in A)\n")
      
      if (n_B > 0) {
        fn_B  <- sprintf("pareto_nonclosed_status_with_%s_by_%s.pdf", closed_col, x_col_name)
        ttl_B <- sprintf("NOT CLOSED but has %s (%.3f%% of has-%s)", closed_col, pct_B, closed_col)
 #       cat(sprintf("Plot B: n=%s | file=%s\n", fmtI(n_B), fn_B))
        .pareto_bridge(DT[idx_B], fn_B, ttl_B)  # REMOVE x_col = x_col_name
#        cat(sprintf("  -> Saved: %s/%s\n", chart_dir, fn_B))
      } else cat("Plot B: skipped (no exceptions in B)\n")
    }
  } else if (make_charts) {
    cat("Charts: skipped (no exceptions to plot)\n")
  } else {
    cat("Charts: skipped (make_charts=FALSE)\n")
  }
  
  
  invisible(list(
    closed_status_missing_closed_date = DT[idx_A],
    nonclosed_status_with_closed_date = DT[idx_B],
    denominators = list(
      total = total_n,
      closed_status = den_closed_status,
      has_closed_date = den_has_cdate
    ),
    percents = list(
      pct_A_of_closed_status = pct_A,
      pct_B_of_has_closed_date = pct_B
    )
  ))
}
