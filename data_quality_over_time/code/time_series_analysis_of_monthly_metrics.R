################################################################################
# Comprehensive Time Series Analysis for Data in Long Format
# Analyzes multiple time series datasets from a CSV file in long format

# Load required libraries
library(tidyverse)
library(tseries)
library(forecast)
library(strucchange)
library(Kendall)
library(stats)
library(dplyr)
library(nortest)
library(lmtest)

################################################################################
# Function to analyze a single time series
analyze_time_series <- function(ts_data, metric_name) {
  
  cat("\n\n=======================================\n")
  cat("ANALYSIS FOR:", metric_name, "\n")
  cat("=======================================\n\n")
  
  ####################
  # 1. Basic statistics
  cat("Basic Statistics:\n")
  cat("Mean:", mean(ts_data), "\n")
  cat("Median:", median(ts_data), "\n")
  cat("Standard Deviation:", sd(ts_data), "\n")
  cat("Min:", min(ts_data), "\n")
  cat("Max:", max(ts_data), "\n\n")
  
  # Initialize result variables with NA
  runs_test_pvalue <- NA
  lb_test_pvalue <- NA
  adf_test_pvalue <- NA
  seasonal_strength <- NA
  normalized_entropy <- NA
  r_squared <- NA
  mk_test_pvalue <- NA
  arima_vs_noise <- NA
  ad_test_pvalue <- NA
  dw_test_pvalue <- NA
  
  # Initialize distribution test results
  dist_results <- list(
    normal_pvalue = NA,
    uniform_pvalue = NA,
    exponential_pvalue = NA
  )
  
  ####################
  # 2. Runs Test for randomness - only if we have enough unique values
  cat("\n2\n")
  if(var(ts_data) > 0 && length(unique(ts_data)) > 2) {
    above_below_median <- factor(ts_data > median(ts_data))
    runs_test <- try(tseries::runs.test(above_below_median), silent = TRUE)
    if(!inherits(runs_test, "try-error")) {
      cat("Runs Test for Randomness:\n")
      print(runs_test)
      cat("\n")
      runs_test_pvalue <- runs_test$p.value
    } else {
      cat("Runs Test skipped: insufficient variation in data\n\n")
    }
  } else {
    cat("Runs Test skipped: insufficient variation in data\n\n")
  }
  
  ####################
  # 3. Ljung-Box test for autocorrelation - only if we have enough data points
  cat("\n3\n")
  if(length(ts_data) >= 12) {
    lb_test <- try(Box.test(ts_data, lag = min(12, length(ts_data) - 1), 
                            type = "Ljung-Box"), silent = TRUE)
    if(!inherits(lb_test, "try-error")) {
      cat("Ljung-Box Test for Autocorrelation:\n")
      print(lb_test)
      cat("\n")
      lb_test_pvalue <- lb_test$p.value
    } else {
      cat("Ljung-Box Test skipped: computation failed\n\n")
    }
  } else {
    cat("Ljung-Box Test skipped: insufficient data points\n\n")
  }
  
  ####################
  # 4. ADF test for stationarity - only if we have enough data points
  cat("\n4\n")
  if(length(ts_data) > 10 && var(ts_data) > 0) {
    adf_test <- try(adf.test(ts_data), silent = TRUE)
    if(!inherits(adf_test, "try-error")) {
      cat("Augmented Dickey-Fuller Test for Stationarity:\n")
      print(adf_test)
      cat("\n")
      adf_test_pvalue <- adf_test$p.value
    } else {
      cat("ADF Test skipped: computation failed\n\n")
    }
  } else {
    cat("ADF Test skipped: insufficient data points or variation\n\n")
  }
  
  ####################
  # 5. Seasonality analysis - only if we have enough data for a full cycle
  cat("\n5\n")
  if(frequency(ts_data) > 1 && length(ts_data) >= frequency(ts_data) * 2) {
    decomp <- try(stl(ts_data, s.window = "periodic"), silent = TRUE)
    if(!inherits(decomp, "try-error")) {
      seasonal_strength <- var(decomp$time.series[, "seasonal"]) / 
        (var(decomp$time.series[, "seasonal"]) + 
           var(decomp$time.series[, "remainder"]))
      cat("Seasonal Strength (0 = no seasonality, 1 = pure seasonality):", 
          seasonal_strength, "\n\n")
    } else {
      cat("Seasonality analysis skipped: computation failed\n\n")
    }
  } else {
    cat("Seasonality analysis skipped: insufficient data for a complete cycle\n\n")
  }
  
  ####################
  # 6. Shannon Entropy - works with any non-constant data
  cat("\n6\n")
  if(var(ts_data) > 0) {
    calculate_entropy <- function(x) {
      bins <- min(20, length(unique(x)))
      counts <- hist(x, breaks = bins, plot = FALSE)$counts
      probs <- counts / sum(counts)
      probs <- probs[probs > 0]
      -sum(probs * log2(probs))
    }
    
    # Calculate actual entropy
    entropy_actual <- calculate_entropy(ts_data)
    
    # Calculate entropy of random data with same distribution properties
    set.seed(123)
    entropy_random <- calculate_entropy(rnorm(length(ts_data), mean(ts_data), sd(ts_data)))
    
    # Calculate maximum possible entropy
    bins_used <- min(20, length(unique(ts_data)))
    max_entropy <- log2(bins_used)
    
    # Ensure normalized entropy doesn't exceed 1 (100%)
    normalized_entropy <- min(entropy_actual / max_entropy, 1)
    
    cat("Shannon Entropy Analysis:\n")
    cat("Entropy of actual data:", entropy_actual, "\n")
    cat("Entropy of random data:", entropy_random, "\n")
    cat("Maximum possible entropy:", max_entropy, "\n")
    cat("Normalized entropy:", normalized_entropy, "\n")
    cat("Is the data random based on entropy?", 
        ifelse(entropy_actual > 0.9 * entropy_random, "Likely random", "Likely not random"), "\n\n")
  } else {
    cat("Shannon Entropy analysis skipped: constant data\n\n")
    normalized_entropy <- NA
    entropy_random <- NA  # Define it even when skipped
    entropy_actual <- NA  # Define it even when skipped
  }
  
  ####################
  # 7. Trend Analysis - works with any data
  cat("\n7\n")
  # Create a dataframe for trend analysis
  df <- data.frame(
    Period = 1:length(ts_data),
    Value = as.numeric(ts_data)
  )
  
  # Linear trend
  trend_model <- try(lm(Value ~ Period, data = df), silent = TRUE)
  if(!inherits(trend_model, "try-error")) {
    trend_summary <- summary(trend_model)
    cat("Linear Trend Analysis:\n")
    cat("Slope:", coef(trend_model)[2], "\n")
    cat("R-squared:", trend_summary$r.squared, "\n")
    if(length(coef(summary(trend_model))[,4]) > 1) {
      cat("p-value:", coef(summary(trend_model))[2,4], "\n\n")
    } else {
      cat("p-value: NA (insufficient data)\n\n")
    }
    r_squared <- trend_summary$r.squared
  } else {
    cat("Linear trend analysis skipped: computation failed\n\n")
  }
  
  # Mann-Kendall test - only if we have enough unique values
  if(length(unique(ts_data)) > 2) {
    mk_test <- try(MannKendall(ts_data), silent = TRUE)
    if(!inherits(mk_test, "try-error")) {
      cat("Mann-Kendall Test for Trend:\n")
      print(mk_test)
      cat("\n")
      mk_test_pvalue <- mk_test$sl[1]
    } else {
      cat("Mann-Kendall test skipped: computation failed\n\n")
    }
  } else {
    cat("Mann-Kendall test skipped: insufficient unique values\n\n")
  }
  
  ####################
  # 8. Model Fitting - only if we have sufficient data points
  cat("\n8\n")
  if(length(ts_data) > 3 && var(ts_data) > 0) {
    arima_model <- try(auto.arima(ts_data), silent = TRUE)
    
    if(!inherits(arima_model, "try-error")) {
      white_noise_model <- try(arima(ts_data, order = c(0,0,0)), silent = TRUE)
      
      if(!inherits(white_noise_model, "try-error")) {
        cat("ARIMA Model Comparison:\n")
        cat("Best ARIMA model:", capture.output(arima_model)[1], "\n")
        cat("AIC of ARIMA model:", arima_model$aic, "\n")
        cat("AIC of white noise model:", white_noise_model$aic, "\n")
        arima_vs_noise <- arima_model$aic < white_noise_model$aic
        cat("Is there a detectable pattern?", 
            ifelse(arima_vs_noise, "Yes, ARIMA model detects a pattern", 
                   "No, data appears random"), "\n\n")
      } else {
        cat("ARIMA comparison skipped: white noise model computation failed\n\n")
      }
    } else {
      cat("ARIMA comparison skipped: computation failed\n\n")
    }
  } else {
    cat("ARIMA analysis skipped: insufficient data or variation\n\n")
  }
  
  ####################
  # 9. Distribution tests
  cat("\n9\n")
  cat("\nLength(ts_data):", length(ts_data))
  cat("\nVar(ts_data):", var(ts_data), "\n")
  
  if(length(ts_data) > 5 && var(ts_data) > 0) {
    cat("Distribution Testing Results:\n")
    
    # Shapiro-Wilk test for normality
    if (length(ts_data) <= 5000) {  # Shapiro-Wilk has sample size limits
      sw_test <- try(shapiro.test(ts_data), silent = TRUE)
      if (!inherits(sw_test, "try-error")) {
        cat("Shapiro-Wilk test for normality: W =", sw_test$statistic, 
            ", p-value =", sw_test$p.value, "\n")
        if(sw_test$p.value < 0.05) {
          cat("  Conclusion: Data does NOT follow a normal distribution\n")
        } else {
          cat("  Conclusion: Cannot reject normality\n")
        }
        normal_pvalue <- sw_test$p.value
      } else {
        cat("Shapiro-Wilk test failed.\n")
        normal_pvalue <- NA
      }
    } else {
      cat("Data too large for Shapiro-Wilk test.\n")
      normal_pvalue <- NA
    }
    
    # For uniform and exponential, use K-S with jittering to break ties
    # Add tiny random noise to break ties (doesn't meaningfully affect results)
    set.seed(123)  # For reproducibility
    jittered_data <- ts_data + runif(length(ts_data), -1e-10, 1e-10)
    
    # Test against uniform distribution
    min_val <- min(jittered_data)
    max_val <- max(jittered_data)
    ks_unif <- try(ks.test(jittered_data, "punif", min = min_val, max = max_val), silent = TRUE)
    
    # Test against exponential distribution (if all values are positive)
    if(all(ts_data > 0)) {
      ks_exp <- try(ks.test(jittered_data, "pexp", rate = 1/mean(jittered_data)), silent = TRUE)
    } else {
      ks_exp <- NULL
    }
    
    if(!inherits(ks_unif, "try-error")) {
      cat("K-S test for uniform distribution: D =", ks_unif$statistic, 
          ", p-value =", ks_unif$p.value, "\n")
      if(ks_unif$p.value < 0.05) {
        cat("  Conclusion: Data does NOT follow a uniform distribution\n")
      } else {
        cat("  Conclusion: Cannot reject uniform distribution\n")
      }
      uniform_pvalue <- ks_unif$p.value
    } else {
      uniform_pvalue <- NA
    }
    
    if(!is.null(ks_exp) && !inherits(ks_exp, "try-error")) {
      cat("K-S test for exponential distribution: D =", ks_exp$statistic, 
          ", p-value =", ks_exp$p.value, "\n")
      if(ks_exp$p.value < 0.05) {
        cat("  Conclusion: Data does NOT follow an exponential distribution\n")
      } else {
        cat("  Conclusion: Cannot reject exponential distribution\n")
      }
      exponential_pvalue <- ks_exp$p.value
    } else {
      exponential_pvalue <- NA
    }
    
    cat("\n")
    
    # Store the results
    dist_results <- list(
      normal_pvalue = normal_pvalue,
      uniform_pvalue = uniform_pvalue,
      exponential_pvalue = exponential_pvalue
    )
  } else {
    cat("Distribution tests skipped: insufficient data or variation\n\n")
    dist_results <- list(
      normal_pvalue = NA,
      uniform_pvalue = NA,
      exponential_pvalue = NA
    )
  }
  
  ####################
  # 10. Anderson-Darling test for normality - only if we have enough unique values
  cat("\n10\n")
  cat("\n--- Anderson-Darling Test Diagnostics ---\n")
  cat("Data variance:", var(ts_data), "\n")
  cat("Number of unique values:", length(unique(ts_data)), "\n")
  
  if(var(ts_data) > 0 && length(unique(ts_data)) > 5) {
    cat("Initial conditions met for Anderson-Darling test\n")
    
    # Check if nortest package is available
    if (!requireNamespace("nortest", quietly = TRUE)) {
      cat("Error: nortest package is not available. Please install it with install.packages('nortest')\n\n")
      ad_test_pvalue <- NA
    } else {
      # Try running the test with detailed error handling
      tryCatch({
        # Print first few data points to diagnose issues
        cat("First 10 data points:", head(ts_data, 10), "\n")
        
        ad_test <- nortest::ad.test(ts_data)
        cat("Anderson-Darling Test for Normality:\n")
        print(ad_test)
        cat("\n")
        ad_test_pvalue <- ad_test$p.value
        cat("P-value extracted:", ad_test_pvalue, "\n")
      }, 
      error = function(e) {
        cat("Anderson-Darling Test error:", conditionMessage(e), "\n\n")
        ad_test_pvalue <<- NA
      },
      warning = function(w) {
        cat("Anderson-Darling Test warning:", conditionMessage(w), "\n")
      })
    }
  } else {
    cat("Anderson-Darling Test skipped: insufficient variation in data or unique values\n\n")
    ad_test_pvalue <- NA
  }
  
  ####################
  # 11. Durbin-Watson test for serial correlation - only if we have enough data points
  cat("\11\n")
  if(length(ts_data) >= 4) {
    # First create a simple time index and fit a linear model
    time_index <- 1:length(ts_data)
    linear_model <- try(lm(ts_data ~ time_index), silent = TRUE)
    
    if(!inherits(linear_model, "try-error")) {
      dw_test <- try(lmtest::dwtest(linear_model), silent = TRUE)
      if(!inherits(dw_test, "try-error")) {
        cat("Durbin-Watson Test for Serial Correlation:\n")
        print(dw_test)
        cat("\n")
        dw_test_pvalue <- dw_test$p.value
      } else {
        cat("Durbin-Watson Test skipped: computation failed\n\n")
      }
    } else {
      cat("Durbin-Watson Test skipped: linear model fitting failed\n\n")
    }
  } else {
    cat("Durbin-Watson Test skipped: insufficient data points\n\n")
  }
  
  # Return a summary of results
  return(list(
    metric = metric_name,
    runs_test_pvalue = runs_test_pvalue,
    lb_test_pvalue = lb_test_pvalue,
    adf_test_pvalue = adf_test_pvalue,
    seasonal_strength = seasonal_strength,
    normalized_entropy = normalized_entropy,
    r_squared = r_squared,
    mk_test_pvalue = mk_test_pvalue,
    arima_vs_noise = arima_vs_noise,
    dist_normal_pvalue = dist_results$normal_pvalue,
    dist_uniform_pvalue = dist_results$uniform_pvalue,
    dist_exponential_pvalue = dist_results$exponential_pvalue,
    ad_test_pvalue = ad_test_pvalue,
    dw_test_pvalue = dw_test_pvalue
  ))
}  # End of function
  
################################################################################
# Main function to analyze all datasets in a long-format CSV file
analyze_long_format <- function(file_path, output_dir = "analysis_results") {
  
  # Read the data file
#  cat("Reading data file...\n")
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Convert date format if needed
  if(grepl("/", data$year_month[1])) {
    data$year_month <- as.Date(data$year_month, format = "%m/%d/%Y")
  } else {
    data$year_month <- as.Date(data$year_month)
  }
  
  # Ensure numeric types
  data$count <- as.numeric(data$count)
  data$N <- as.numeric(data$N)
  data$fraction <- as.numeric(data$fraction)
  
  # Get unique datasets
  datasets <- unique(data$dataset)
#  cat("Found", length(datasets), "unique datasets in the file.\n")
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create a subdirectory for plots
  plots_dir <- file.path(output_dir, "plots")
  if (!dir.exists(plots_dir)) {
    dir.create(plots_dir, recursive = TRUE)
  }
  
  # # Debug info for each dataset
  # for (dataset_name in datasets) {
  #   df <- data[data$dataset == dataset_name, ]
  #   #cat("\nDataset:", dataset_name, "has", nrow(df), "rows\n")
  #   # cat("Date range:", format(min(df$year_month), "%Y-%m-%d"), "to", 
  #   #     format(max(df$year_month), "%Y-%m-%d"), "\n")
  #   #cat("Fraction range:", min(df$fraction, na.rm = TRUE), "to", 
  #   #    max(df$fraction, na.rm = TRUE), "\n")
  #   #cat("Non-zero values:", sum(df$fraction > 0, na.rm = TRUE), 
  #    #   "out of", nrow(df), "\n")
  # }
  # 
  
  # Initialize results list
  all_results <- list()
  
  # Process each dataset
  for (dataset_name in datasets) {
#    cat("\nProcessing dataset:", dataset_name, "\n")
    
    # Get the dataset
    df <- data[data$dataset == dataset_name, ]
    df <- df[order(df$year_month), ]  # Ensure chronological order
    
    # Skip empty datasets
    if (nrow(df) == 0) {
      cat("Skipping empty dataset:", dataset_name, "\n")
      next
    }
    
    # Convert to time series
    ts_data <- ts(df$fraction, frequency = 12, 
                  start = c(as.numeric(format(min(df$year_month), "%Y")), 
                            as.numeric(format(min(df$year_month), "%m"))))
    
    # Create plots
    ####################
    # For the time series plot
    pdf(file.path(plots_dir, paste0(make.names(dataset_name), "_time_series.pdf")), 
        width = 13, height = 8.5 )
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
    
    # Plot with scaled y-axis
    plot(ts_data * display_multiplier, main = paste("Time Series Plot -", dataset_name),
         xlab = "Year", 
         ylab = paste0("Fraction (per ", display_multiplier, ")"), 
         type = "o", col = "steelblue",
         bg = "white", fg = "black", col.axis = "black", col.lab = "black", col.main = "black")
    
    # Add trend line (need to adjust for scaling)
    trend <- try(lm(df$fraction ~ as.numeric(df$year_month)), silent = TRUE)
    if (!inherits(trend, "try-error")) {
      abline(coef(trend)[1] * display_multiplier, coef(trend)[2] * display_multiplier, 
             col = "red", lwd = 2)
      r2 <- round(summary(trend)$r.squared, 3)
      legend("topright", legend = paste("R² =", r2), bty = "n", text.col = "black")
    }
    dev.off()
    
    # For the ACF and PACF plots
    ####################
    pdf(file.path(plots_dir, paste0(make.names(dataset_name), "_acf_pacf.pdf")), 
        width = 13, height = 8.5 )
    par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1,
        col.axis = "black", col.lab = "black", col.main = "black", 
        col.sub = "black")
    acf(ts_data, main = paste("ACF -", dataset_name), lag.max = 36, 
        col = "steelblue")
    pacf(ts_data, main = paste("PACF -", dataset_name), lag.max = 36, 
         col = "steelblue")
    dev.off()
    
    # For the decomposition plot
    if (frequency(ts_data) > 1 && length(ts_data) >= 2*frequency(ts_data)) {
      tryCatch({
        decomp <- stl(ts_data, s.window = "periodic")
        pdf(file.path(plots_dir, paste0(make.names(dataset_name), 
                                        "_decomposition.pdf")), 
            width = 13, height = 8.5, )
        par(bg = "white", col.axis = "black", col.lab = "black", 
            col.main = "black")
        plot(decomp, main = paste("Decomposition -", dataset_name), 
             col = "steelblue")
        dev.off()
      }, error = function(e) {
        cat("Error in decomposition for", dataset_name, ":", e$message, "\n")
      })
    }
    
    ####################
    # For the changepoint detection plot
    try({
      pdf(file.path(plots_dir, paste0(make.names(dataset_name), 
                                      "_changepoints.pdf")), 
          width = 13, height = 8.5 )
      par(mfrow = c(1, 1), col.axis = "black", col.lab = "black", 
          col.main = "black")
      bp <- breakpoints(fraction ~ 1, data = df)
      plot(ts_data, main = paste("Structural Breaks -", dataset_name),
           xlab = "Year", ylab = "Fraction", col = "steelblue")
      lines(bp, col = "red", lwd = 2)
      dev.off()
    }, silent = TRUE)
    
    ####################    
    # QQ plot for normal distribution
    pdf(file.path(plots_dir, paste0(make.names(dataset_name), "_qq_plot.pdf")), 
        width = 13, height = 8.5 )
    par(mfrow = c(1, 1), col.axis = "black", col.lab = "black", col.main = "black")
    qqnorm(ts_data, main = paste("Q-Q Plot (Normal) -", dataset_name), col = "steelblue")
    qqline(ts_data, col = "red", lwd = 2)
    dev.off()
    
    # Histogram with density curves
    pdf(file.path(plots_dir, paste0(make.names(dataset_name), "_distribution.pdf")), 
        width = 13, height = 8.5 )
    par(mfrow = c(1, 1), col.axis = "black", col.lab = "black", col.main = "black")
    hist(ts_data, freq = FALSE, main = paste("Distribution -", dataset_name), 
         col = "lightblue", border = "white", breaks = 20,
         xlab = "Value")
    # Add normal density curve
    curve(dnorm(x, mean = mean(ts_data), sd = sd(ts_data)), 
          add = TRUE, col = "red", lwd = 2)
    # Add legend
    legend("topright", legend = c("Data", "Normal Distribution"), 
           fill = c("lightblue", NA), border = c("white", NA),
           col = c(NA, "red"), lwd = c(NA, 2), bty = "n")
    dev.off()
    
    ####################
    # Create a text file for detailed analysis output
    output_file <- file.path(output_dir, paste0(make.names(dataset_name), 
                                                "_analysis.txt"))
#    sink(output_file)
    results <- analyze_time_series(ts_data, dataset_name)
#    sink()
    
    # Store results for summary
    all_results[[dataset_name]] <- results
    
    cat("\nAnalysis for", dataset_name, "complete.\n")
  }
  
  ####################
  results_df <- do.call(rbind, lapply(all_results, function(x) {
    data.frame(
      Metric = x$metric,
      RunsTest_pvalue = x$runs_test_pvalue,
      LjungBox_pvalue = x$lb_test_pvalue,
      ADF_pvalue = x$adf_test_pvalue,
      SeasonalStrength = x$seasonal_strength,
      NormalizedEntropy = x$normalized_entropy,
      R_squared = x$r_squared,
      MannKendall_pvalue = x$mk_test_pvalue,
      Pattern_Detected = x$arima_vs_noise,
      Normal_pvalue = x$dist_normal_pvalue,
      Uniform_pvalue = x$dist_uniform_pvalue,
      Exponential_pvalue = x$dist_exponential_pvalue,
      AndersonDarling_pvalue = x$ad_test_pvalue,  # New addition
      DurbinWatson_pvalue = x$dw_test_pvalue,     # New addition
      stringsAsFactors = FALSE
    )
  }))
  
  # Write summary to CSV
  write.csv(results_df, file.path(output_dir, "analysis_summary.csv"), 
            row.names = FALSE)
  
  ####################
  # Create comparative visualizations using ggplot2 with white background
  try({
    
    # Prepare data for plotting
    plot_data <- results_df
    plot_data$Metric_num <- 1:nrow(plot_data)
    
    # Define a clean white theme with readable elements
    clean_theme <- theme_light() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.background = element_rect(fill = "white", color = NA)
      )
    
    ####################
    # R-squared plot
    r2_data <- plot_data[!is.na(plot_data$R_squared),]
    if(nrow(r2_data) > 0) {
      p1 <- ggplot(r2_data, aes(x = reorder(Metric, R_squared), 
                                y = R_squared)) +
        geom_bar(stat = "identity", fill = "#4682B4") +  # Steel blue
        geom_text(aes(label = round(R_squared, 3)), vjust = -0.5, size = 3.5, 
                  color = "black") +
        labs(title = "R² Comparison Across Metrics", 
             x = "Metric", y = "R-squared") +
        clean_theme
      
      ggsave(file.path(output_dir, "r_squared_comparison.pdf"), p1, width = 13, 
             height = 8.5)
    }
    
    ####################
    # Normalized Entropy plot
    entropy_data <- plot_data[!is.na(plot_data$NormalizedEntropy),]
    if(nrow(entropy_data) > 0) {
      p2 <- ggplot(entropy_data, aes(x = reorder(Metric, NormalizedEntropy), y = NormalizedEntropy)) +
        geom_bar(stat = "identity", fill = "green4") +  # Forest green
        # Convert to percentage and display with % symbol
        geom_text(aes(label = paste0(round(NormalizedEntropy * 100, 1), "%")), 
                  vjust = -0.5, size = 3.5, color = "black") +
        # Scale y-axis as percentage
        scale_y_continuous(labels = scales::percent_format(scale = 100)) +
        labs(title = "Normalized Entropy Comparison", 
             x = "Metric", y = "Normalized Entropy (%)",
             subtitle = "Higher values indicate more randomness (100% = purely random)") +
        clean_theme
      
      ggsave(file.path(output_dir, "entropy_comparison.pdf"), p2, width = 13, height = 8.5)
    }
    
    ####################
    # Significance of Runs Test (-log10 p-value)
    runs_data <- plot_data[!is.na(plot_data$RunsTest_pvalue),]
    if(nrow(runs_data) > 0) {
      runs_data$log_p <- -log10(runs_data$RunsTest_pvalue)
      # Cap infinity values for visualization
      max_finite <- max(runs_data$log_p[is.finite(runs_data$log_p)])
      runs_data$log_p[is.infinite(runs_data$log_p)] <- max_finite * 1.1
      
      p3 <- ggplot(runs_data, aes(x = reorder(Metric, log_p), y = log_p)) +
        geom_bar(stat = "identity", fill = "indianred4") +  # Firebrick
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", 
                   color = "black") +
        geom_text(aes(label = format(RunsTest_pvalue, scientific = TRUE, 
                                     digits = 4)), 
                  vjust = -0.5, size = 3.5, color = "black") +
        labs(title = "Runs Test Significance", 
             x = "Metric", y = "-log10(p-value)",
             subtitle = "Values above dashed line are significant (p < 0.05)") +
        clean_theme
      
      ggsave(file.path(output_dir, "runs_test_significance.pdf"), p3, 
             width = 13, height = 8.5)
    }
    
    ####################
    # Mann-Kendall Test Significance
    mk_data <- plot_data[!is.na(plot_data$MannKendall_pvalue),]
    if(nrow(mk_data) > 0) {
      mk_data$log_p <- -log10(mk_data$MannKendall_pvalue)
      max_finite <- max(mk_data$log_p[is.finite(mk_data$log_p)])
      mk_data$log_p[is.infinite(mk_data$log_p)] <- max_finite * 1.1
      
      # Improved p-value formatting
      mk_data$p_value_label <- sapply(mk_data$MannKendall_pvalue, function(p) {
        if (p < 1e-20) {
          return("p < 1e-20")  # For extremely small values
        } else if (p < 1e-10) {
          return("p < 1e-10")  # For very small values
        } else if (p < 0.001) {
          return(format(p, scientific = TRUE, digits = 2))
        } else {
          return(format(round(p, 4), nsmall = 4))
        }
      })
      
      p4 <- ggplot(mk_data, aes(x = reorder(Metric, log_p), y = log_p)) +
        geom_bar(stat = "identity", fill = "mediumpurple3") +  # Blue Violet
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", 
                   color = "black") +
        geom_text(aes(label = p_value_label), 
                  vjust = -0.5, size = 3.5, color = "black") +
        # Add a text annotation to explain significance
        annotate("text", x = nrow(mk_data)/2, y = -log10(0.05) + 0.5, 
                 label = "p < 0.05 (significant)", 
                 color = "black", size = 3, fontface = "italic") +
        labs(title = "Mann-Kendall Test Significance", 
             x = "Metric", y = "-log10(p-value)",
             subtitle = "Higher bars indicate stronger evidence of trend") +
        clean_theme
      
      ggsave(file.path(output_dir, "mann_kendall_significance.pdf"), p4,
             width = 13, height = 8.5)
    }
    
    ####################
    # Combined summary plot with gradient coloring based on p-values
    pattern_df <- data.frame(
      Metric = results_df$Metric,
      RunsTest = results_df$RunsTest_pvalue,
      LjungBox = results_df$LjungBox_pvalue,
      MannKendall = results_df$MannKendall_pvalue,
      ARIMA = as.numeric(results_df$Pattern_Detected),
      R_squared = results_df$R_squared,
      Normal = results_df$Normal_pvalue,
      Uniform = results_df$Uniform_pvalue,
      Exponential = results_df$Exponential_pvalue,
      AndersonDarling = results_df$AndersonDarling_pvalue,  # New addition
      DurbinWatson = results_df$DurbinWatson_pvalue         # New addition
    )
    
    # Create a type identifier for each test (pattern vs distribution)
    test_types <- data.frame(
      Test = c("RunsTest", "LjungBox", "MannKendall", "ARIMA", "R_squared", 
               "Normal", "Uniform", "Exponential", 
               "AndersonDarling", "DurbinWatson"),  # New additions
      Type = c(rep("Pattern", 5), rep("Distribution", 3), 
               "Distribution", "Pattern")  # AndersonDarling is Distribution, DurbinWatson is Pattern
    )
    
    # Convert to long format for plotting
    pattern_long <- tidyr::pivot_longer(
      pattern_df, 
      cols = c("RunsTest", "LjungBox", "MannKendall", "ARIMA", "R_squared", 
               "Normal", "Uniform", "Exponential",
               "AndersonDarling", "DurbinWatson"),  # New additions
      names_to = "Test", 
      values_to = "Value"
    )
    
    # Add test type information
    pattern_long <- merge(pattern_long, test_types, by = "Test")
    
    # Replace NA values with a default
    pattern_long$Value[is.na(pattern_long$Value)] <- 1
    
    # Create effect size metrics and add reliability and display value information
    pattern_long <- pattern_long %>%
      mutate(
        # Directly map statistical significance to effect size
        EffectSize = case_when(
          # For p-value tests, convert p-values to effect size
          Test %in% c("RunsTest", "LjungBox", "MannKendall", "Normal", "Uniform", 
                      "Exponential", "AndersonDarling", "DurbinWatson") &  # Added new tests
            !is.na(Value) ~ 
            ifelse(Value == 0, 1,  # Strongest possible evidence
                   ifelse(Value < 1e-10, 0.9,  # Very strong evidence
                          ifelse(Value < 0.001, 0.7,  # Strong evidence
                                 ifelse(Value < 0.01, 0.5,  # Moderate evidence
                                        ifelse(Value < 0.05, 0.3,  # Weak evidence
                                               0.1))))),  # Negligible evidence
          
          # ARIMA is binary
          Test == "ARIMA" & Value > 0 ~ 1,  # Pattern detected
          Test == "ARIMA" & Value == 0 ~ 0.1,  # No pattern
          
          # R-squared scales directly
          Test == "R_squared" & !is.na(Value) ~ 
            pmin(1, Value / 0.5),
          
          # Default for any other cases
          TRUE ~ 0.1
        ),
        
        # Simplified reliability indicator
        Reliable = case_when(
          Value == 0 | Value < 1e-100 ~ "Questionable",
          Value < 1e-10 ~ "Cautious",
          TRUE ~ "Reliable"
        ),
        
        # Create custom display label
        DisplayValue = case_when(
          # For p-value tests
          Test %in% c("RunsTest", "LjungBox", "MannKendall", "Normal", "Uniform", 
                      "Exponential", "AndersonDarling", "DurbinWatson") &  # Added new tests
            !is.na(Value) & Value < 1e-10 ~ "< 1e-10",
          Test %in% c("RunsTest", "LjungBox", "MannKendall", "Normal", "Uniform", 
                      "Exponential", "AndersonDarling", "DurbinWatson") &  # Added new tests
            !is.na(Value) ~ sprintf("%.2e", Value),
          # For R_squared
          Test == "R_squared" & !is.na(Value) ~ sprintf("%.4f", Value),
          # For ARIMA
          Test == "ARIMA" ~ ifelse(Value > 0, "Yes", "No"),
          TRUE ~ "NA"
        ),
        
        # Add text color directly as a column
        TextColor = ifelse(EffectSize > 0.5, "white", "black")
      )
    
    # Create a factor to group and order the tests
    pattern_long$Test <- factor(pattern_long$Test, 
                                levels = c("R_squared", "RunsTest", "LjungBox", "MannKendall", 
                                           "ARIMA", "DurbinWatson",  # Added DurbinWatson to Pattern group
                                           "Normal", "Uniform", "Exponential", "AndersonDarling"))  # Added AndersonDarling to Distribution group
    
    # Create heatmap using effect size 
    p5 <- ggplot(pattern_long, aes(x = Test, y = Metric, fill = EffectSize)) +
      geom_tile(color = "white", linewidth = 0.8) +
      # Use a continuous green gradient
      scale_fill_gradientn(
        colors = c("gray95", "palegreen1", "palegreen2", "palegreen3", "palegreen4", "darkgreen"),
        limits = c(0, 1),
        name = "Strength of Evidence"
      ) +
      # Remove the fill legend
      guides(fill = "none") +
      # Simplified title explaining the visualization
      labs(
        x = NULL, 
        y = NULL,
        title = "Randomness and Distribution Analysis",
        subtitle = "Green intensity indicates strength of evidence against randomness"
      ) +
      # Add vertical separator between pattern tests and distribution tests
      geom_vline(xintercept = 6.5, linetype = "dashed", color = "gray50") +  # Updated position to 6.5
      # Improve x-axis labeling
      scale_x_discrete(
        labels = c("RunsTest" = "Runs\nTest", 
                   "LjungBox" = "Ljung-\nBox",
                   "MannKendall" = "Mann-\nKendall", 
                   "ARIMA" = "ARIMA\nPattern",
                   "R_squared" = "R²",
                   "Normal" = "Normal\nDist.",
                   "Uniform" = "Uniform\nDist.", 
                   "Exponential" = "Exp.\nDist.",
                   "AndersonDarling" = "A-D\nTest",  # New label
                   "DurbinWatson" = "Durbin-\nWatson")) +  # New label
      clean_theme +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        # Adjust plot margins to make room for potential annotations
        plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "cm")
      )
    
    # Add text for the display values (with color as a column rather than a function)
    p5 <- p5 + 
      geom_text(
        data = pattern_long,
        aes(label = DisplayValue, color = TextColor),
        size = 2.8
      ) +
      scale_color_identity()  # Use the color values directly
    
    print(p5)
    
    # Save the plot in both PNG and PDF formats
    ggsave(file.path(output_dir, "pattern_distribution_summary.pdf"), p5, 
           width = 13, height = 8.5)
  }, silent = TRUE)
  
  cat("\nAnalysis complete.\n Results saved to directory:", output_dir, "\n")
  return(results_df)
}

################################################################################
########## Start program timing ###########
programStart <- as.POSIXct(Sys.time())
formattedStartTime <- format(programStart, "%Y-%m-%d %H:%M:%S")

display_multiplier <- 100

cat("\nExecution begins at:", formattedStartTime, "\n")

working_dir <- getwd()
data_dir <- file.path(working_dir, "datacleaningproject", "nyc311clean", 
                      "data_quality_over_time", "data")
main_data_file <- "condition_plot_results.csv"

main_data_file_path <-file.path(data_dir, main_data_file)

########## 

results <- analyze_long_format(main_data_file_path, output_dir = data_dir)

########## 

# Store the program end time and calculate the duration
programStop <- as.POSIXct(Sys.time())
formatted_end_time <- format(programStop, "%Y-%m-%d %H:%M:%S")

# Calculate the duration of the program (in seconds)
duration_seconds <- as.numeric(difftime(programStop, programStart, units = "secs"))

# Convert the duration to a formatted string (hours, minutes, and seconds with 4 decimal places)
hours <- floor(duration_seconds / 3600)
minutes <- floor((duration_seconds %% 3600) / 60)
seconds <- round(duration_seconds %% 60, 4)  # Round to 4 decimal places

# Create the formatted duration string
duration_string <- paste0(
  if (hours > 0) paste0(hours, " hours, ") else "",
  if (minutes > 0) paste0(minutes, " minutes, ") else "",
  seconds, " seconds"
)

# Print the final program information to the console
cat("\n\n*****END OF PROGRAM*****\n")
cat("\n📅 Execution ends at:", formatted_end_time, "\n")
cat("\n⏱️ Program run-time:", duration_string, "\n")

################################################################################