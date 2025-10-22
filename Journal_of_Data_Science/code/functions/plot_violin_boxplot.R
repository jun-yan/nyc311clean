plot_violin_boxplot <- function(
    DT,                         # MUST be a data.table
    value_col,                  # UNQUOTED numeric column (e.g., duration_days)
    chart_dir,
    filename     = "violin_boxplot.pdf",
    title        = NULL,
    by_col,                     # OPTIONAL UNQUOTED grouping column (e.g., agency)
    include_na_group = FALSE,
    na_group_label  = "(NA)",
    top_n        = 30L,
    order_by        = c("median","count","median_desc"),
    flip         = TRUE,        # horizontal by default
    # plot type options
    plot_type    = c("hybrid", "boxplot", "violin"),
    violin_scale = c("width", "area", "count"),
    violin_alpha = 0.5,
    violin_trim  = FALSE,
    # appearance / jitter & outlier control
    box_width    = 0.5,
    jitter       = NULL,        # NULL means auto-decide based on plot_type
    jitter_width = 0.15,
    jitter_size  = 2,
    jitter_alpha = 0.6,
    jitter_shape_inlier = 17,           # triangle
    jitter_color_inlier = "#0072B2",    # steelblue
    outlier_size = 1.8,
    outlier_shape = 16,
    outlier_color = "gray40",
    outlier_alpha = 0.8,
    jitter_sample_inliers = 5000L,
    jitter_sample_outliers = NULL,
    x_axis_tick_size   = 15,
    x_axis_label_angle = 0,
    y_axis_tick_size   = 13,
    y_axis_label_size  = NULL,
    y_axis_side        = c("auto","left","right"),
    plot_title_size    = 15,
    plot_subtitle_size = 12,
    width_in     = 13,
    height_in    = 8.5,
    coef_iqr     = 1.5,
    zero_line    = TRUE,
    # scale options for extreme outliers
    x_scale_type = c("linear", "pseudo_log", "sqrt_signed"),
    x_limits     = NULL,
    outlier_threshold = NULL,
    min_count    = 1L
) {
  
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  v_expr <- substitute(value_col); if (!is.name(v_expr)) stop("value_col must be unquoted.")
  v_str  <- deparse(v_expr);      if (!v_str %in% names(DT)) stop(sprintf("Column '%s' not found.", v_str))
  for (pkg in c("ggplot2","scales","grid")) if (!requireNamespace(pkg, quietly = TRUE)) stop(sprintf("Package '%s' is required.", pkg))
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Normalize arguments
  plot_type <- match.arg(plot_type)
  violin_scale <- match.arg(violin_scale)
  x_scale_type <- match.arg(x_scale_type)
  
  # Auto-decide jitter: disable for pure violin, enable for others
  if (is.null(jitter)) {
    jitter <- if (plot_type == "violin") FALSE else TRUE
  }
  
  # copy & normalize
  df <- data.table::copy(DT)[, val := as.numeric(get(v_str))][!is.na(val)]
  by_given <- !missing(by_col)
  
  # --- START X_LIMITS CALCULATION HERE ---
  if (is.null(x_limits)) {
    min_val <- min(df$val, na.rm = TRUE)
    max_val <- max(df$val, na.rm = TRUE)
    range_val <- max_val - min_val
    
    # 1. Base Limits (e.g., your existing margin logic)
    margin_factor <- ifelse(max_val >= 2000, 0.045, 0.075) 
    lower_limit_base <- ifelse(min_val >= 0, min_val * (1 - margin_factor), min_val * (1 + margin_factor))
    upper_limit_base <- ifelse(max_val >= 0, max_val * (1 + margin_factor), max_val * (1 - margin_factor))
    
    # 2. Violin Expanded Limits (e.g., +10% of range)
    violin_expansion <- 0.10 * range_val
    upper_limit_violin <- upper_limit_base + violin_expansion
    lower_limit_violin <- lower_limit_base - violin_expansion
    
    # 3. Label Expanded Limits (from the previous fix, based on scale type)
    # ... (Add the label limit logic here) ...
    label_offset_factor <- 0.15 # Use 15% as a general safety margin for labels
    
    # ... (full label calculation logic goes here) ...
    
    # 4. Final Limits Selection
    x_limits[1] <- min(lower_limit_base, lower_limit_violin, label_lower_limit)
    x_limits[2] <- max(upper_limit_base, upper_limit_violin, label_upper_limit)
  }
  # --- END X_LIMITS CALCULATION --
  
  # ---- GROUPING & ORDERING ---- (No changes in this section)
  keep_levels <- "(all)"
  if (by_given) {
    g_expr <- substitute(by_col); if (!is.name(g_expr)) stop("by_col must be unquoted.")
    g_str  <- deparse(g_expr);    if (!g_str %in% names(df)) stop(sprintf("Group column '%s' not found.", g_str))
    if (include_na_group) { df[, grp := get(g_str)]; df[is.na(grp), grp := na_group_label] }
    else { df <- df[!is.na(get(g_str))]; df[, grp := get(g_str)] }
    
    choices <- c("median", "count", "median_desc")
    default <- "median"
    ob <- if (missing(order_by) || is.null(order_by) || !length(order_by)) default else order_by
    ob <- tolower(trimws(as.character(ob)[1]))
    if (!nzchar(ob) || !(ob %in% choices)) {
      warning(sprintf("order_by='%s' not recognized; defaulting to '%s'", ob, default))
      ob <- default
    }
    order_by <- ob
    
    stats <- df[, .(N = .N, med = stats::median(val)), by = grp]
    if (!is.null(outlier_threshold)) {
      stats <- stats[med >= outlier_threshold]
      cat(sprintf("Filtered to groups with median >= %.3f\n", outlier_threshold))
    }
    if (min_count > 1L) {
      before_count <- nrow(stats)
      stats <- stats[N >= min_count]
      after_count <- nrow(stats)
      if (before_count > after_count) {
        cat(sprintf("Filtered out %d groups with < %d observations\n", before_count - after_count, min_count))
      }
    }
    if (order_by == "median") { data.table::setorder(stats, med, grp) }
    else if (order_by == "median_desc") { data.table::setorder(stats, -med, grp) }
    else if (order_by == "count") { data.table::setorder(stats, -N, grp) }
    
    keep_levels <- stats$grp[seq_len(min(top_n, nrow(stats)))]
    df <- df[grp %in% keep_levels]
    df[, grp := factor(as.character(grp), levels = keep_levels)]
  } else {
    df[, grp := factor("(all)")]
  }
  
  if (!nrow(df)) { message("plot_violin_boxplot: no data to plot after filtering."); return(invisible(NULL)) }
  
  # ---- Outlier tagging (No changes) ----
  bounds <- df[, {
    q1  <- as.numeric(stats::quantile(val, 0.25, na.rm = TRUE))
    q3  <- as.numeric(stats::quantile(val, 0.75, na.rm = TRUE))
    iqr <- q3 - q1
    .(lo = ifelse(is.finite(iqr), q1 - coef_iqr*iqr, -Inf),
      hi = ifelse(is.finite(iqr), q3 + coef_iqr*iqr,  Inf))
  }, by = grp]
  df <- bounds[df, on = "grp"]
  df[, is_outlier := (val < lo) | (val > hi)]
  inliers  <- df[is_outlier == FALSE]
  outliers <- df[is_outlier == TRUE]
  
  if (isTRUE(jitter) && nrow(inliers) > jitter_sample_inliers) { set.seed(42L); inliers <- inliers[sample(.N, jitter_sample_inliers)] }
  if (isTRUE(jitter) && !is.null(jitter_sample_outliers) && nrow(outliers) > jitter_sample_outliers) { set.seed(43L); outliers <- outliers[sample(.N, jitter_sample_outliers)] }
  
  # ---- Y-axis side & label size ---- (No changes)
  y_axis_side <- match.arg(y_axis_side)
  if (y_axis_side == "auto") y_axis_side <- if (isTRUE(flip)) "right" else "left"
  label_size <- if (is.null(y_axis_label_size)) y_axis_tick_size else y_axis_label_size
  
  # -------- Build plot (No changes in geoms/scales) --------
  if (isTRUE(flip)) {
    # ... (plot setup code omitted for brevity) ...
    if (plot_type %in% c("violin", "hybrid")) {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = val, y = grp)) +
        ggplot2::geom_violin(orientation = "y", scale = violin_scale, fill = "#56B4E9", color = "gray40", alpha = violin_alpha, linewidth = 0.5, trim = violin_trim)
    } else {
      p <- ggplot2::ggplot(df, ggplot2::aes(x = val, y = grp))
    }
    if (plot_type %in% c("boxplot", "hybrid")) {
      box_width_final <- if (plot_type == "hybrid") box_width * 0.3 else box_width
      p <- p + ggplot2::geom_boxplot(orientation = "y", width = box_width_final, outlier.shape = NA, fill = "#E69F00", color = "gray40", alpha = if (plot_type == "hybrid") 0.9 else 0.8, linewidth = 0.7)
    }
    if (isTRUE(jitter)) {
      if (nrow(inliers)) { p <- p + ggplot2::geom_jitter(data = inliers, ggplot2::aes(x = val, y = grp), width = 0, height = jitter_width, size = jitter_size, alpha = jitter_alpha, shape = jitter_shape_inlier, color = jitter_color_inlier) }
      if (nrow(outliers)) { p <- p + ggplot2::geom_jitter(data = outliers, ggplot2::aes(x = val, y = grp), width = 0, height = jitter_width, size = outlier_size, alpha = outlier_alpha, shape = outlier_shape, color = outlier_color) }
    }
    if (isTRUE(zero_line)) {
      if (!is.null(x_limits)) { if (0 >= x_limits[1] && 0 <= x_limits[2]) { p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, linetype = "solid", color = "gray40") } }
      else { p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, linetype = "solid", color = "gray40") }
    }
    p <- p + ggplot2::scale_y_discrete(position = y_axis_side, limits = rev(keep_levels))
    
    # Continuous X scale
    x_scale <- switch(
      x_scale_type,
      "pseudo_log" = ggplot2::scale_x_continuous(
        trans  = "pseudo_log",
        # Increase the expansion multiplier to ensure violin tails are visible
        expand = ggplot2::expansion(mult = c(0.065, 0.065)), 
        oob    = scales::oob_squish,
        limits = x_limits
      ),
      "sqrt_signed" = ggplot2::scale_x_continuous(
        trans = scales::trans_new(
          name      = "sqrt_signed",
          transform = function(x) sign(x) * sqrt(abs(x)),
          inverse   = function(x) sign(x) * x^2
        ),
        # Increase the expansion multiplier
        expand = ggplot2::expansion(mult = c(0.065, 0.065)),
        oob    = scales::oob_squish,
        limits = x_limits
      ),
      ggplot2::scale_x_continuous(
        # Increase the expansion multiplier (e.g., from 0.02 to 0.05)
        expand = ggplot2::expansion(mult = c(0.065, 0.065)),
        oob    = scales::oob_squish,
        limits = x_limits
      )
    )
    p <- p + x_scale
    # ... (theme application code omitted for brevity) ...
    txt_left  <- if (y_axis_side == "left")  ggplot2::element_text(size = label_size) else ggplot2::element_blank()
    txt_right <- if (y_axis_side == "right") ggplot2::element_text(size = label_size) else ggplot2::element_blank()
    tck_left  <- if (y_axis_side == "left")  ggplot2::element_line() else ggplot2::element_blank()
    tck_right <- if (y_axis_side == "right") ggplot2::element_line() else ggplot2::element_blank()
    p <- p + david_theme(text_size = 11, x_axis_angle = 45, remove_x_title = TRUE, remove_y_title = TRUE, plot_margin = c(0.5, 0.2, 0.5, 0.5)) +
      ggplot2::theme(axis.ticks.x = ggplot2::element_line(), axis.text.y.left = txt_left, axis.ticks.y.left = tck_left, axis.text.y.right = txt_right, axis.title.y.right = ggplot2::element_blank(), axis.ticks.y.right = tck_right, axis.text.x.top = ggplot2::element_blank(), axis.title.x.top = ggplot2::element_blank(), axis.ticks.x.top = ggplot2::element_blank())
    
  } else {
    # === VERTICAL: x = grp, y = val === (no label changes needed here)
    # ... (vertical plot code omitted for brevity) ...
    if (plot_type %in% c("violin", "hybrid")) { p <- ggplot2::ggplot(df, ggplot2::aes(x = grp, y = val)) + ggplot2::geom_violin(scale = violin_scale, fill = "#56B4E9", color = "gray40", alpha = violin_alpha, linewidth = 0.5, trim = violin_trim) } else { p <- ggplot2::ggplot(df, ggplot2::aes(x = grp, y = val)) }
    if (plot_type %in% c("boxplot", "hybrid")) { box_width_final <- if (plot_type == "hybrid") box_width * 0.3 else box_width; p <- p + ggplot2::geom_boxplot(width = box_width_final, outlier.shape = NA, fill = "#E69F00", color = "gray40", alpha = if (plot_type == "hybrid") 0.9 else 0.85, linewidth = 0.7) }
    if (isTRUE(jitter)) {
      if (nrow(inliers)) { p <- p + ggplot2::geom_jitter(data = inliers, ggplot2::aes(x = grp, y = val), width = jitter_width, height = 0, size = jitter_size, alpha = jitter_alpha, shape = jitter_shape_inlier, color = jitter_color_inlier) }
      if (nrow(outliers)) { p <- p + ggplot2::geom_jitter(data = outliers, ggplot2::aes(x = grp, y = val), width = jitter_width, height = 0, size = outlier_size, alpha = outlier_alpha, shape = outlier_shape, color = outlier_color) }
    }
    if (isTRUE(zero_line)) { p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, linetype = "solid", color = "gray40") }
    p <- p + ggplot2::scale_x_discrete(limits = keep_levels)
    p <- p + ggplot2::scale_y_continuous(position = y_axis_side, oob = scales::oob_squish, limits = x_limits)
    p <- p + david_theme(text_size = label_size, plot_title_size = plot_title_size, x_axis_angle = x_axis_label_angle, remove_x_title = TRUE, remove_y_title = TRUE)
  }
  
  # Add title and subtitle (No changes)
  if (!is.null(title)) {
    total_n <- nrow(df)
    subtitle_text <- sprintf("n = %s | type: %s | x-scale: %s", format(total_n, big.mark = ","), plot_type, x_scale_type)
    p <- p + ggplot2::ggtitle(title) + ggplot2::labs(subtitle = subtitle_text) + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = plot_subtitle_size))
  }
  
  # Add count labels if grouping 
  if (by_given) {
    counts <- df[, .N, by = grp]
    max_value <- max(df$val, na.rm = TRUE)
    min_value <- min(df$val, na.rm = TRUE)
    range_val <- max_value - min_value
    
    if (flip) {
      # === START OF KEY FIXES FOR HORIZONTAL PLOTS ===
      
      # 1. Define offset factors for the label anchor point
      offset_factor <- switch(
        x_scale_type,
        "pseudo_log" = 0.15,
        "sqrt_signed" = 0.10,
        "linear" = 0.07 # Increase linear offset slightly for safety
      )
      
      # 2. Conditional calculation of label_x and hjust
      if (max_value <= 0) {
        # NEGATIVE/ZERO PLOT: Label goes on the LEFT.
        label_hjust <- 1 # RIGHT-justify text (so it extends INWARD from the anchor)
        
        if (x_scale_type == "pseudo_log") {
          # Multiply negative min_value to push it more negative
          label_x <- min_value * (1 + offset_factor)
        } else {
          # Subtract offset from min_value
          label_x <- min_value - range_val * offset_factor
        }
      } else {
        # POSITIVE/MIXED PLOT: Label goes on the RIGHT.
        label_hjust <- 0 # LEFT-justify text (so it extends INWARD from the anchor)
        
        if (x_scale_type == "pseudo_log") {
          # Multiply positive max_value to push it more positive
          label_x <- max_value * (1 + offset_factor)
        } else {
          # Add offset to max_value
          label_x <- max_value + range_val * offset_factor
        }
      }
      
      p <- p + 
        ggplot2::geom_text(
          data = counts,
          ggplot2::aes(x = label_x, y = grp, label = paste0("n=", N)),
          color = "gray20",
          size = 3,
          hjust = label_hjust, # <-- NOW DYNAMICALLY SET
          vjust = 0.5,
          inherit.aes = FALSE
        )
      # === END OF KEY FIXES ===
    } else {
      # Vertical plot (No changes needed for label placement)
      label_y <- max_value + (max_value - min_value) * 0.05
      
      p <- p + 
        ggplot2::geom_text(
          data = counts,
          ggplot2::aes(x = grp, y = label_y, label = paste0("n=", N)),
          size = 3,
          hjust = 0.5,
          vjust = 0,
          inherit.aes = FALSE
        )
    }
  }
  
  print(p)
  Sys.sleep(3)
  
  outfile <- file.path(chart_dir, filename)
  ggplot2::ggsave(outfile, plot = p, width = width_in, height = height_in, dpi = 300)
  
  invisible(list(plot = p, file = outfile, data = df[, .(grp, val, is_outlier)]))
}