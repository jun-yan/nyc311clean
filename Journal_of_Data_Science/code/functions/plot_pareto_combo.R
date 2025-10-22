plot_pareto_combo <- function(
    DT,
    x_name = NULL,         # existing style (string)
    x_col  = "NULL",         # NEW: legacy style, bare or string
    x_expr = NULL,         # NEW: very legacy NSE style, bare/string/var
    chart_dir,
    filename,
    title,
    subtitle          = NULL,
    top_n             = 30L,
    include_na        = FALSE,
    na_label          = "(NA)",
    width_in          = 10,
    height_in         = 6,
    annotation_size   = 3.5,
    x_axis_label_angle= 0,
    x_axis_label_size = 9,
    plot_subtitle_size= 9,
    min_count         = 1L,
    flip              = FALSE,
    show_threshold_80 = TRUE,
    show_labels       = TRUE
) {
  
# cat("\n Entering plot_pareto_combo\n\n")
   
  .resolve_expr_to_name <- function(expr, env = parent.frame()) {
    if (is.null(expr)) return(NULL)
    if (is.symbol(expr)) {
      # Could be bare column (agency) OR variable name; eval to allow x_col = x_col_name
      val <- tryCatch(eval(expr, env), error = function(e) NULL)
      if (is.character(val) && length(val) == 1 && nzchar(val)) return(val)
      return(deparse(expr))  # bare symbol -> "agency"
    } else if (is.character(expr) && length(expr) == 1 && nzchar(expr)) {
      # Literal string in the call: x_col = "agency"
      return(expr)
    } else {
      # Anything else: try to evaluate (e.g., a name that holds "agency")
      val <- tryCatch(eval(expr, env), error = function(e) NULL)
      if (is.character(val) && length(val) == 1 && nzchar(val)) return(val)
      return(NULL)
    }
  }
  
  x_col_name <- .resolve_expr_to_name(substitute(x_col))
  # if (is.null(x_col_name)) {
  #   stop("Could not resolve x_col: ", deparse(substitute(x_col)))
  # } else {
  #   message("Resolved x_col: ", x_col_name)
  # }

  # Capture call-time code WITHOUT evaluating values
  x_col_expr  <- substitute(x_col)
  x_name_expr <- substitute(x_name)
  
  # Prefer x_col (legacy), then x_name (current)
  col_from_x_col  <- .resolve_expr_to_name(x_col_expr)
  col_from_x_name <- {
    # prioritize direct string in x_name if provided
    if (is.character(x_name) && length(x_name) == 1 && nzchar(x_name)) x_name
    else .resolve_expr_to_name(x_name_expr)
  }
  
  resolved_col <- if (!is.null(col_from_x_col)) col_from_x_col else 
    col_from_x_name
  if (is.null(resolved_col)) {
    stop("plot_pareto_combo(): unable to resolve grouping column. ",
         "Use x_col = agency (bare) or x_col = \"agency\", or x_name = \"agency\".", 
         call. = FALSE)
  }
  if (!(resolved_col %in% names(DT))) {
    stop(sprintf("plot_pareto_combo(): column '%s' not found in DT.", 
                 resolved_col), call. = FALSE)
  }
  
  # From here on, use x_name (a single character string) everywhere
  x_name <- resolved_col
  
  x_str <- resolved_col
  
   if (!(x_name %in% names(DT))) {
    stop(sprintf("Column '%s' not found in DT.", x_name), call. = FALSE)
  }

  # --- hard requirements
  if (!data.table::is.data.table(DT)) 
    stop("DT must be a data.table. Use data.table::setDT() first.")
  
  if (is.name(x_name)) {
    # bare column name (e.g., agency)
    x_str <- deparse(x_name)
  } else if (is.character(x_name) && length(x_name) == 1) {
    # string provided (e.g., "agency")
    x_str <- x_name
    x_name <- as.name(x_name)
  } else {
    stop("x_name must be either an unquoted column name (e.g., agency) or a string (\"agency\").")
  }
  
  if (!x_str %in% names(DT)) 
    stop(sprintf("Column '%s' not found in DT.", x_str))
  
  for (pkg in c("ggplot2","scales")) if (!requireNamespace(pkg, quietly = TRUE)) 
    stop(sprintf("Package '%s' is required.", pkg))
  
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, 
                                         showWarnings = FALSE)
  
  # cat("\n Aggregatiing in plot_pareto_combo\n\n")
  # 
  # #    Right before the aggregate section:
  # cat("\n=== DEBUG plot_pareto_combo aggregation ===\n")
  # cat("  x_str:", x_str, "\n")
  # cat("  class(x_str):", class(x_str), "\n")
  # cat("  length(x_str):", length(x_str), "\n")
  # cat("  include_na:", include_na, "\n")
  # cat("  nrow(DT):", nrow(DT), "\n")
  
  # --- aggregate (DT-only)
  if (include_na) {
    agg <- DT[, .(N = .N), by = .(group = get(x_str))][order(-N)]
  } else {
    agg <- DT[!is.na(get(x_str)), .(N = .N), by = .(group = get(x_str))][order(-N)]
  }
  if (!nrow(agg)) {
    message(sprintf("plot_pareto_combo: no rows to plot for '%s'.", x_str))
    return(invisible(NULL))
  }
  if (include_na) agg[is.na(group), group := na_label]
  
  totalN <- sum(agg$N)
  agg[, `:=`(
    pct     = N / totalN,
    cum_pct = cumsum(N) / totalN
  )]
  
  # Filter out groups with too few observations
  if (min_count > 1L) {
    before_count <- nrow(agg)
    agg <- agg[N >= min_count]
    after_count <- nrow(agg)
    if (before_count > after_count) {
      cat(sprintf("\nFiltered out %d groups with < %d observations\n", 
                  before_count - after_count, min_count))
    }
    # Recalculate percentages after filtering
    totalN <- sum(agg$N)
    agg[, `:=`(
      pct     = N / totalN,
      cum_pct = cumsum(N) / totalN
    )]
  }
  
  # --- summary table (top N) - MOVED OUTSIDE min_count block
  total_groups <- nrow(agg)
  df <- agg[1:min(top_n, total_groups)]
  df[, `:=`(
    pct = round(pct, 2),
    cum_pct = round(cum_pct, 2)
  )]
  df[, group := factor(group, levels = df$group)]  # lock order
  
  cat("\nPareto summary by ", x_str,
      if (total_groups > nrow(df)) sprintf(" (first %d of %d)", nrow(df), 
                                        total_groups) else "", ":\n", sep = "")
  
  old_opt <- options(datatable.print.class = FALSE); on.exit(options(old_opt), 
                                                             add = TRUE)
  
  temp_dt <- df[, .(group, N, pct = round(pct, 4), 
                    cum_pct = round(cum_pct, 4))]
  temp_df <- data.frame(
    group = format(temp_dt$group, justify = "left"),
    N = format(temp_dt$N, justify = "right"),
    pct = format(temp_dt$pct, justify = "right"),
    cum_pct = format(temp_dt$cum_pct, justify = "right")
  )
  print(temp_df, row.names = FALSE)
  
  # --- plot (vertical by default)
  maxN <- max(df$N)
  count_labels <- df$N
  cum_labels   <- round(df$cum_pct, 2)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = group, y = N)) +
    ggplot2::geom_col(width = 0.55, fill = "#009E73")
  
  if (isTRUE(show_labels)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = count_labels),
      colour = "black", hjust = 0.5, vjust = -0.5,
      size = annotation_size
    )
  }
  
  # Always include cumulative line and points
  p <- p +
    ggplot2::geom_line(ggplot2::aes(y = cum_pct * maxN, group = 1)) +
    ggplot2::geom_point(ggplot2::aes(y = cum_pct * maxN))
  
  # Conditionally include 80% line + label
  if (isTRUE(show_threshold_80)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0.8 * maxN,
        linetype   = "dotted",
        linewidth  = 1.25,
        color      = "#D55E00",
        alpha      = 0.7
      ) +
      ggplot2::annotate(
        "text",
        x = Inf, y = 0.8 * maxN,
        label = "80%",
        hjust = 1.1, vjust = -0.3,
        size  = 3,
        color = "#D55E00"
      )
  }
  
  # Y-axis secondary scale and labels
  p <- p +
    ggplot2::scale_y_continuous(
      sec.axis = ggplot2::sec_axis(
        ~ . / maxN,
        labels = scales::percent_format(accuracy = 1),
        name   = NULL
      )
    ) +
    ggplot2::labs(
      title    = if (is.null(title)) sprintf("Pareto by %s (counts & cumulative %% )", 
                                             x_str) else title,
      subtitle = if (is.null(subtitle)) sprintf("n = %d", totalN) else subtitle,
      x = NULL, y = NULL
    ) +
    david_theme(text_size = 12, x_axis_text_size = 9, x_axis_angle = 30)
  
  
  if (isTRUE(flip)) p <- p + ggplot2::coord_flip()
  
  print(p)
  Sys.sleep(3)
  
  outfile <- file.path(chart_dir, filename)
  ggplot2::ggsave(outfile, plot = p, width = width_in, height = height_in, 
                  dpi = 300)
  
  invisible(list(plot = p, table = df, file = outfile))
}