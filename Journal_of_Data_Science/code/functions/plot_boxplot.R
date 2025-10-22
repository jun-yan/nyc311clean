  plot_boxplot <- function(
      DT,                         # MUST be a data.table
      value_col,                  # UNQUOTED numeric column (e.g., duration_days)
      chart_dir,
      filename     = "boxplot.pdf",
      title        = NULL,
      by_col,                     # OPTIONAL UNQUOTED grouping column (e.g., agency)
      include_na_group = FALSE,
      na_group_label  = "(NA)",
      top_n        = 30L,
      order_by        = c("median","count","median_desc"),
      flip         = TRUE,        # horizontal by default
      # appearance / jitter & outlier control
      box_width    = 0.5,
      show_count_labels = TRUE,  # <-- ADD THIS
      count_label_hjust = 0.5,  # 0 = left-aligned, 1 = right-aligned, 0.5 = centered
      jitter       = TRUE,
      jitter_width = 0.15,
      jitter_size  = 2,
      jitter_alpha = 0.5,
      jitter_shape_inlier = 17,           # triangle
      jitter_color_inlier = "#0072B2",    # steelblue
      use_raster = TRUE,      # Rasterize the jitter points only
      raster_dpi = 300,       # DPI for raster layers
      outlier_size = 1.8,
      outlier_shape = 16,
      outlier_color = "gray40",
      outlier_alpha = 0.8,      # NEW: transparency for outlier dots
      jitter_sample_inliers = 5000L,      # sample INLIERS only; plot ALL outliers
      jitter_sample_outliers = NULL,      # optionally sample outliers too (NULL = plot all)
      x_axis_tick_size   = 15,
      x_axis_label_angle = 0,
      y_axis_tick_size   = 13,
      y_axis_label_size  = NULL,          # optional override for Y label size
      y_axis_side        = c("auto","left","right"),
      plot_title_size    = 15,
      plot_subtitle_size = 12,            # size for the "n = xxxx" subtitle
      width_in     = 13,
      height_in    = 8.5,
      coef_iqr     = 1.5,
      zero_line    = TRUE,                # draw reference line at 0 (if in view)
      # scale options for extreme outliers
      x_scale_type = c("linear", "pseudo_log", "sqrt_signed"),
      x_limits     = NULL,                # c(min, max) to manually set x-axis limits
      outlier_threshold = NULL,           # include only groups with median >= threshold (if set)
      min_count    = 1L                   # minimum number of observations per group to include
  ) {
  
  if (!data.table::is.data.table(DT)) stop("DT must be a data.table.")
  v_expr <- substitute(value_col); 
  
  if (!is.name(v_expr)) stop("value_col must be unquoted.")
  v_str  <- deparse(v_expr);      
  
  if (!v_str %in% names(DT)) stop(sprintf("Column '%s' not found.", v_str))
  for (pkg in c("ggplot2","scales","grid")) 
    if (!requireNamespace(pkg, quietly = TRUE)) 
      stop(sprintf("Package '%s' is required.", pkg))
  
  if (!dir.exists(chart_dir)) dir.create(chart_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (use_raster && !requireNamespace("ggrastr", quietly = TRUE)) {
    stop("Package 'ggrastr' is required when use_raster=TRUE.")
  }
  
  # copy & normalize
  df <- data.table::copy(DT)[, val := as.numeric(get(v_str))][!is.na(val)]
  by_given <- !missing(by_col)
  
  # ---- GROUPING & ORDERING ----
  keep_levels <- "(all)"  # will be replaced if grouping provided
  if (by_given) {
    g_expr <- substitute(by_col); if (!is.name(g_expr)) stop("by_col must be unquoted.")
    g_str  <- deparse(g_expr);    if (!g_str %in% names(df)) stop(sprintf("Group column '%s' not found.", g_str))
    if (include_na_group) { df[, grp := get(g_str)]; df[is.na(grp), grp := na_group_label] }
    else { df <- df[!is.na(get(g_str))]; df[, grp := get(g_str)] }
    
    # --- normalize order_by robustly ---
    choices <- c("median", "count", "median_desc")
    default <- "median"
    ob <- if (missing(order_by) || is.null(order_by) || !length(order_by)) default else order_by
    ob <- tolower(trimws(as.character(ob)[1]))
    if (!nzchar(ob) || !(ob %in% choices)) {
      warning(sprintf("order_by='%s' not recognized; defaulting to '%s'", ob, default))
      ob <- default
    }
    order_by <- ob
    # -----------------------------------
    
    # Compute per-group stats
    stats <- df[, .(N = .N, med = stats::median(val)), by = grp]
    
    # Optional median threshold
    if (!is.null(outlier_threshold)) {
      stats <- stats[med >= outlier_threshold]
      cat(sprintf("Filtered to groups with median >= %.3f\n", outlier_threshold))
    }
    
    # Minimum count per group
    if (min_count > 1L) {
      before_count <- nrow(stats)
      stats <- stats[N >= min_count]
      after_count <- nrow(stats)
      if (before_count > after_count) {
        cat(sprintf("Filtered out %d groups with < %d observations\n",
                    before_count - after_count, min_count))
      }
    }
    
    # Determine order of groups (table order, not visual)
    if (order_by == "median") {
              data.table::setorder(stats, med, grp)     # ascending median
    } else if (order_by == "median_desc") {
              data.table::setorder(stats, -med, grp)    # descending median
    } else if (order_by == "count") {
              data.table::setorder(stats, -N, grp)  #DESC by count (largest 1st)
    }
    
    # Select Top-N in that order
    keep_levels <- stats$grp[seq_len(min(top_n, nrow(stats)))]
    
    # Restrict data and LOCK the order in the factor levels
    df <- df[grp %in% keep_levels]
    df[, grp := factor(as.character(grp), levels = keep_levels)]
    
  } else {
    df[, grp := factor("(all)")]
  }
  
  if (!nrow(df)) { message("plot_boxplot: no data to plot after filtering."); return(invisible(NULL)) }
  
  # ---- Outlier tagging (1.5 * IQR) ----
  bounds <- df[, {
    q1  <- as.numeric(stats::quantile(val, 0.25, na.rm = TRUE))
    q3  <- as.numeric(stats::quantile(val, 0.75, na.rm = TRUE))
    iqr <- q3 - q1
    .(lo = ifelse(is.finite(iqr), q1 - coef_iqr*iqr, -Inf),
      hi = ifelse(is.finite(iqr), q3 + coef_iqr*iqr,  Inf))
  }, by = grp]
  df <- bounds[df, on = "grp"]
  df[, is_outlier := (val < lo) | (val > hi)]
  
  # split
  inliers  <- df[is_outlier == FALSE]
  outliers <- df[is_outlier == TRUE]
  
  # Sample inliers/outliers if too many
  if (isTRUE(jitter) && nrow(inliers) > jitter_sample_inliers) {
    set.seed(42L); inliers <- inliers[sample(.N, jitter_sample_inliers)]
  }
  if (isTRUE(jitter) && !is.null(jitter_sample_outliers) && nrow(outliers) > 
      jitter_sample_outliers) {
    set.seed(43L); outliers <- outliers[sample(.N, jitter_sample_outliers)]
  }
  
  # ---- Y-axis side & label size ----
  y_axis_side <- match.arg(y_axis_side)
  if (y_axis_side == "auto") y_axis_side <- if (isTRUE(flip)) "right" else "left"
  label_size <- if (is.null(y_axis_label_size)) y_axis_tick_size else y_axis_label_size
  
  # -------- Build plot --------
  if (isTRUE(flip)) {
    # Horizontal: x = val, y = grp
    p <- ggplot2::ggplot(df, ggplot2::aes(x = val, y = grp)) +
      ggplot2::geom_boxplot(
        orientation   = "y",
        width         = box_width,
        outlier.shape = NA,
        fill          = "#E69F00",
        color         = "gray30",
        alpha         = 0.8,
        linewidth     = 0.7
      )
    
    if (isTRUE(jitter)) {
      # Inliers
      if (nrow(inliers)) {
        if (use_raster) {
          p <- p + ggrastr::geom_point_rast(
            data = inliers,
            ggplot2::aes(x = val, y = grp),
            size  = jitter_size,
            alpha = jitter_alpha,
            shape = jitter_shape_inlier,
            color = jitter_color_inlier,
            raster.dpi = raster_dpi
          )
        } else {
          p <- p + ggplot2::geom_jitter(
            data = inliers,
            ggplot2::aes(x = val, y = grp),
            width = 0, height = jitter_width,
            size  = jitter_size, alpha = jitter_alpha,
            shape = jitter_shape_inlier, color = jitter_color_inlier
          )
        }
      }
      
      # Outliers
      if (nrow(outliers)) {
        if (use_raster) {
          p <- p + ggrastr::geom_point_rast(
            data = outliers,
            ggplot2::aes(x = val, y = grp),
            size  = outlier_size,
            alpha = outlier_alpha,
            shape = outlier_shape,
            color = outlier_color,
            raster.dpi = raster_dpi
          )
        } else {
          p <- p + ggplot2::geom_jitter(
            data  = outliers,
            ggplot2::aes(x = val, y = grp),
            width = 0, height = jitter_width,
            size  = outlier_size,
            alpha = outlier_alpha,
            shape = outlier_shape,
            color = outlier_color
          )
        }
      }
    }
    
    
    # optional zero-line only if 0 inside limits (only checks when x_limits provided)
    # Remove the x_limits check so zero line always shows when requested
    if (isTRUE(zero_line)) {
      p <- p + ggplot2::geom_vline(xintercept = 0, linewidth = 0.5, 
                                   linetype = "solid", color = "gray30")
    }
    
    # Discrete Y on chosen side — ENFORCE ORDER with limits = keep_levels
    p <- p + ggplot2::scale_y_discrete(
      position = y_axis_side,
      limits   = rev(keep_levels)   # ← reverse for TOP=largest
    )
    
    # ---- Add exactly ONE x-scale ----
    x_scale_type <- match.arg(x_scale_type)
    x_scale <- switch(
      x_scale_type,
      "pseudo_log" = ggplot2::scale_x_continuous(
        trans  = "pseudo_log",
        expand = ggplot2::expansion(mult = c(0.05, 0.05)),
        oob    = scales::oob_squish,
        limits = x_limits
      ),
      "sqrt_signed" = ggplot2::scale_x_continuous(
        trans = scales::trans_new(
          name      = "sqrt_signed",
          transform = function(x) sign(x) * sqrt(abs(x)),
          inverse   = function(x) sign(x) * x^2
        ),
        expand = ggplot2::expansion(mult = c(0.05, 0.05)),
        oob    = scales::oob_squish,
        limits = x_limits
      ),
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.05)),
        oob    = scales::oob_squish,
        limits = x_limits
      )
    )
    p <- p + x_scale
    
    
    # Theme
    txt_left  <- if (y_axis_side == "left")  ggplot2::element_text(size = label_size, 
                                     face = "bold") else ggplot2::element_blank()
    txt_right <- if (y_axis_side == "right") ggplot2::element_text(size = label_size, 
                                     face = "bold") else ggplot2::element_blank()
    tck_left  <- if (y_axis_side == "left")  ggplot2::element_line() else 
      ggplot2::element_blank()
    tck_right <- if (y_axis_side == "right") ggplot2::element_line() else 
      ggplot2::element_blank()
    
    p <- p +
      david_theme(
        text_size = 11,
        x_axis_angle = 45,
        remove_x_title = TRUE,
        remove_y_title = TRUE,
        plot_margin = c(0.5, 0.2, 0.5, 0.5)
      ) +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_line(),
        axis.text.y.right = txt_right,
        axis.title.y.right = ggplot2::element_blank(),
        axis.ticks.y.right = tck_right,
        axis.text.x.top = ggplot2::element_blank(),
        axis.title.x.top = ggplot2::element_blank(),
        axis.ticks.x.top = ggplot2::element_blank()
      )
    
    
  } else {
    # Vertical: x = grp, y = val
    p <- ggplot2::ggplot(df, ggplot2::aes(x = grp, y = val)) +
      ggplot2::geom_boxplot(
        width = box_width,
        outlier.shape = NA,
        fill = "#E69F00", color = "gray40",
        alpha = 0.85, linewidth = 0.7
      )
    
    if (isTRUE(jitter)) {
      if (nrow(inliers)) {
        p <- p + ggplot2::geom_jitter(
          data = inliers,
          ggplot2::aes(x = grp, y = val),
          width = jitter_width, height = 0,
          size  = jitter_size, alpha = jitter_alpha,
          shape = jitter_shape_inlier, color = jitter_color_inlier
        )
      }
      if (nrow(outliers)) {
        p <- p + ggplot2::geom_jitter(
          data  = outliers,
          ggplot2::aes(x = val, y = grp),   # or aes(x = grp, y = val) in vertical branch
          width = 0, height = jitter_width, # (or width = jitter_width, height = 0)
          size  = outlier_size,
          alpha = outlier_alpha,            # use the new parameter
          shape = outlier_shape,
          color = outlier_color
        )
      }
      
    }
    
    if (isTRUE(zero_line)) {
      p <- p + ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, 
                                   linetype = "solid", color = "gray40")
    }
    
    # Continuous Y
    p <- p + ggplot2::scale_y_continuous(
      position = y_axis_side,
      oob      = scales::oob_squish,
      limits   = x_limits
    )
    
    p <- p + david_theme(
      text_size = label_size,
      plot_title_size = plot_title_size,
      x_axis_angle = x_axis_label_angle,
      remove_x_title = TRUE,
      remove_y_title = TRUE
    )
    
  }
  
  if (!is.null(title)) {
    total_n <- nrow(df)
    subtitle_text <- sprintf("n = %s | x-scale: %s", 
                             format(total_n, big.mark = ","),
                             x_scale_type)
    
    p <- p +
      ggplot2::ggtitle(title) +
      ggplot2::labs(subtitle = subtitle_text) +
      ggplot2::theme(
        plot.subtitle = ggplot2::element_text(
          hjust = 0,
          size = plot_subtitle_size
        )
      )
  }
  
  # Add count labels only if we have grouping AND show_count_labels is TRUE
  if (by_given && show_count_labels) {  # <-- ADD show_count_labels here
    # Calculate counts per group
    counts <- df[, .N, by = grp]  # <-- This is fine HERE because grp exists now
    
    # Calculate max value for positioning
    max_value <- max(df$val, na.rm = TRUE)
    min_value <- min(df$val, na.rm = TRUE)
    
    # Add count labels to the plot
    # Add count labels to the plot
    if (flip) {
      
      # Determine if data is predominantly negative
      if (max_value <= 0) {
        # All negative: labels go on the left (more negative side)
        # Add 10% margin in the negative direction
        margin <- abs(min_value) * 0.10
        label_x <- min_value - margin
        count_label_hjust <- 1  # right-align text
      } else {
        # Positive or mixed: labels go on the right
        # Add 10% margin in the positive direction
        margin <- abs(max_value) * 0.10
        label_x <- max_value + margin
        # count_label_hjust already set by parameter
      }
      
      p <- p + 
        ggplot2::geom_text(
          data = counts,
          ggplot2::aes(x = label_x, y = grp, label = paste0("n=", N)),
          size = 3,
          hjust = count_label_hjust,
          vjust = 0.5,
          color = "gray20",
          inherit.aes = FALSE
        )
    } else {
      # For vertical boxplots, place labels above with 10% margin
      margin <- abs(max_value) * 0.10
      label_y <- max_value + margin
      
      p <- p + 
        ggplot2::geom_text(
          data = counts,
          ggplot2::aes(x = grp, y = label_y, label = paste0("n=", N)),
          size = 3,
          hjust = count_label_hjust,
          vjust = 0,
          color = "gray20",
          inherit.aes = FALSE
        )
    }
  }
  
  print(p)
  Sys.sleep(3)
  
  outfile <- file.path(chart_dir, filename)
  ggplot2::ggsave(
    filename = outfile,
    plot = p,
    width = width_in,
    height = height_in,
    dpi = 300,
    device = cairo_pdf   # Ensures proper raster embedding
  )
  
#  cat(sprintf("\nSaved chart (with rasterized points) to: %s\n", outfile))
  
  invisible(list(plot = p, file = outfile, data = df[, .(grp, val, is_outlier)]))
}