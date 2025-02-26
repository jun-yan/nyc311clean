##################################################################################
# fuzzy_matching_app.R
library(shiny)
library(data.table)
library(stringdist)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(phonics)
library(stringdist)
library(future)
library(promises)
library(later)

# Specify the port (choose a unique port number)
options(shiny.port = 4007)

##################################################################################
# Load data
data_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/data_anomalies/data/3-month_311SR_10-01-2024_thru_12-31-2024_AS_OF_02-02-2025.rds"
cleaned_data <- readRDS(data_path)
setDT(cleaned_data)

# Initialize clean_already flag
clean_already <- FALSE

# Set normalize_count for tracking normalizatio
normalize_count <- 0  

##################################################################################
# Create a vector of street suffix mappings
street_suffix_map <- c(
  "AVENUE" = "AVE",
  "STREET" = "ST",
  "ROAD" = "RD",
  "BOULEVARD" = "BLVD",
  "DRIVE" = "DR",
  "BEND" = "BND",
  "PARKWAY" = "PKWY",
  "COURT" = "CT",
  "LANE" = "LN",
  "EXPRESSWAY" = "EXPY",
  "TERRACE" = "TER",
  "PATH" = "PATH",
  "HIGHWAY" = "HWY",
  "WAY" = "WAY",
  "ALLEY" = "ALY",
  "WALK" = "WALK",
  "LOOP" = "LOOP",
  "CIRCLE" = "CIR",
  "PLAZA" = "PLZ",
  "CRESCENT" = "CRES",
  "BRIDGE" = "BRG",
  "VIADUCT" = "VIA",
  "TURNPIKE" = "TPKE",
  "OVERPASS" = "OPAS",
  "SQUARE" = "SQ",
  "PLACE" = "PL"
)

##################################################################################
street_prefix_map <- c(
  "AVENUE" = "AVE",
  "EAST" = "E",
  "WEST" = "W",
  "NORTH" = "N",
  "SOUTH" = "S",
  "SAINT" = "ST"
)

##################################################################################
# Create pattern lists for faster matching
street_patterns <- list(
  suffix = mapply(
    function(suffix, replacement) {
      list(
        pattern = paste0("\\b", suffix, "$"),
        replacement = replacement
      )
    },
    names(street_suffix_map),
    street_suffix_map,
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  ),
  
  prefix = mapply(
    function(prefix, replacement) {
      list(
        pattern = paste0("^", prefix, "\\s"),
        replacement = paste0(replacement, " ")
      )
    },
    names(street_prefix_map),
    street_prefix_map,
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
)

##################################################################################
# Create a filter function
filter_valid_pairs <- function(data, field1, field2) {
  # Remove rows where either or both fields are NA or blank
  valid_rows <- !is.na(data[[field1]]) & 
    !is.na(data[[field2]]) & 
    data[[field1]] != "" & 
    data[[field2]] != ""
  
  return(data[valid_rows,])
}

##################################################################################
# Optimized clean_text function
clean_text <- function(x) {
  if (clean_already) return(x)
  if (!is.character(x)) return(x)
  
  # Combine operations to reduce iterations
  x <- gsub("[^[:alpha:][:space:]]", "", 
            toupper(trimws(x)))
  x <- gsub("\\s+", " ", x)
  
  # Apply street name normalization
  x <- normalize_street_name(x)
  
  return(x)
}
##################################################################################
normalize_street_name <- function(street, debug = FALSE) {
#  cat("\n Entering normalization function at:", format(Sys.time(), "%H:%M:%S"))

  # Ensure street is a character vector and handle NAs
  street <- ifelse(is.na(as.character(street)), "", as.character(street))
  
  # Remove punctuation and normalize spaces in one step
  street <- trimws(gsub("[[:punct:]\\s]+", " ", street))
  
  # Debug check if needed
  if (debug) {
    problematic_entries <- street[
      !grepl("^[A-Z]+ [A-Z]+$|^[A-Z]+ [A-Z]{1,3}$", street)
    ]
  }
  
  # Vectorized prefix replacement
  for (p in street_patterns$prefix) {
    street <- gsub(p$pattern, p$replacement, street, perl = TRUE)
  }
  
  # Vectorized suffix replacement
  for (s in street_patterns$suffix) {
    street <- gsub(s$pattern, s$replacement, street, perl = TRUE)
  }
  
  # Remove extra whitespace
  street <- trimws(gsub("\\s+", " ", street))
  
#  cat("\n Leaving normalization function at:", format(Sys.time(), "%H:%M:%S"), "\n\n")
  return(street)
}

##################################################################################
compute_levenshtein_matches <- function(
    data, 
    field1, 
    field2, 
    max_distance = 2,
    debug = FALSE
) {
  
  # Strict filtering to remove NA and empty strings
  valid_cases <- filter_valid_pairs(data, field1, field2)
  
  # Early return if no valid cases
  if (nrow(valid_cases) == 0) {
    if (debug) cat("No valid cases found for matching.\n")
    return(list(
      potential_matches = data.table(),
      exact_match_count = 0,
      non_matching_count = 0,
      potential_match_count = 0
    ))
  }

  # Apply text cleaning only if it hasn't been done before
  if (!clean_already) {
    valid_cases[, clean_field1 := clean_text(get(field1))]
    valid_cases[, clean_field2 := clean_text(get(field2))]
    clean_already <<- TRUE  # Ensure future calls know cleaning is done
  } else {
    # Ensure the columns exist, otherwise redefine them
    if (!("clean_field1" %in% names(valid_cases))) valid_cases[, clean_field1 := get(field1)]
    if (!("clean_field2" %in% names(valid_cases))) valid_cases[, clean_field2 := get(field2)]
  }
  
  # Identify exact matches
  exact_matches <- valid_cases[clean_field1 == clean_field2]
  
  # Identify non-matching cases
  non_matching_cases <- valid_cases[
    clean_field1 != clean_field2 & 
      !is.na(clean_field1) & !is.na(clean_field2) & 
      clean_field1 != "" & clean_field2 != ""
  ]
  
  # Compute Levenshtein distance
  non_matching_cases[, levenshtein_dist := stringdist(
    clean_field1, 
    clean_field2, 
    method = "lv"
  )]
  
  # Identify potential matches (within max Levenshtein distance)
  potential_matches <- non_matching_cases[levenshtein_dist <= max_distance]
  
  return(list(
    potential_matches = potential_matches,
    exact_match_count = nrow(exact_matches),
    non_matching_count = nrow(non_matching_cases),
    potential_match_count = nrow(potential_matches)
  ))
}

##################################################################################
compute_metaphone_matches <- function(
    data, 
    field1, 
    field2, 
    dm_threshold = 0.85, 
    debug = FALSE) {
  
  # First apply the filtering
  valid_cases <- filter_valid_pairs(data, field1, field2)
  
  # Apply text cleaning only if it hasn't been done before
  if (!clean_already) {
    valid_cases[, clean_field1 := clean_text(get(field1))]
    valid_cases[, clean_field2 := clean_text(get(field2))]
    clean_already <<- TRUE  # Ensure future calls know cleaning is done
  } else {
    # Ensure the columns exist, otherwise redefine them
    if (!("clean_field1" %in% names(valid_cases))) valid_cases[, clean_field1 := get(field1)]
    if (!("clean_field2" %in% names(valid_cases))) valid_cases[, clean_field2 := get(field2)]
  }
  
  suppressWarnings({
    valid_cases[, dm_field1 := phonics::metaphone(clean_field1)]
    valid_cases[, dm_field2 := phonics::metaphone(clean_field2)]
  })
  
  exact_matches <- valid_cases[clean_field1 == clean_field2]
  non_matching <- valid_cases[clean_field1 != clean_field2]
  
  if (nrow(non_matching) > 0) {
    non_matching[, `:=`(
      str_sim = stringdist::stringsim(clean_field1, clean_field2, method = "jw"),
      metaphone_match = (dm_field1 == dm_field2)
    )]
    
    potential_matches <- non_matching[str_sim >= dm_threshold | metaphone_match == TRUE]
  } else {
    potential_matches <- data.table()
  }
  
  list(
    exact_matches = exact_matches,
    potential_matches = potential_matches,
    exact_match_count = nrow(exact_matches),
    potential_match_count = nrow(potential_matches),
    non_matching_count = nrow(non_matching)
  )
}

##################################################################################
compute_JW_combined_matches <- function(
    data, 
    field1, 
    field2, 
    jw_threshold = 0.85, 
    require_both = FALSE, 
    debug = FALSE) {
  
  # First apply the filtering
  valid_cases <- filter_valid_pairs(data, field1, field2)
  
  # Apply text cleaning only if it hasn't been done before
  if (!clean_already) {
    valid_cases[, clean_field1 := clean_text(get(field1))]
    valid_cases[, clean_field2 := clean_text(get(field2))]
    clean_already <<- TRUE  # Ensure future calls know cleaning is done
  } else {
    # Ensure the columns exist, otherwise redefine them
    if (!("clean_field1" %in% names(valid_cases))) valid_cases[, clean_field1 := get(field1)]
    if (!("clean_field2" %in% names(valid_cases))) valid_cases[, clean_field2 := get(field2)]
  }
  
  suppressWarnings({
    valid_cases[, ph_field1 := phonics::soundex(clean_field1)]
    valid_cases[, ph_field2 := phonics::soundex(clean_field2)]
  })
  
  exact_matches <- valid_cases[clean_field1 == clean_field2]
  non_matching <- valid_cases[clean_field1 != clean_field2]
  
  if (nrow(non_matching) > 0) {
    non_matching[, `:=`(
      jw_dist = stringdist::stringsim(clean_field1, clean_field2, method = "jw"),
      phonetic_match = (ph_field1 == ph_field2)
    )]
    
    potential_matches <- if(require_both) {
      non_matching[jw_dist >= jw_threshold & phonetic_match == TRUE]
    } else {
      non_matching[jw_dist >= jw_threshold | phonetic_match == TRUE]
    }
  } else {
    potential_matches <- data.table()
  }
  
  list(
    exact_matches = exact_matches,
    potential_matches = potential_matches,
    exact_match_count = nrow(exact_matches),
    potential_match_count = nrow(potential_matches),
    non_matching_count = nrow(non_matching)
  )
}

##################################################################################
compute_metaphone_matches <- function(
    data, 
    field1, 
    field2, 
    dm_threshold = 0.85, 
    debug = FALSE) {
  
  # First apply the filtering
  valid_cases <- filter_valid_pairs(data, field1, field2)
  
  # Apply text cleaning only if it hasn't been done before
  if (!clean_already) {
    valid_cases[, clean_field1 := clean_text(get(field1))]
    valid_cases[, clean_field2 := clean_text(get(field2))]
    clean_already <<- TRUE  # Ensure future calls know cleaning is done
  } else {
    # Ensure the columns exist, otherwise redefine them
    if (!("clean_field1" %in% names(valid_cases))) valid_cases[, clean_field1 := get(field1)]
    if (!("clean_field2" %in% names(valid_cases))) valid_cases[, clean_field2 := get(field2)]
  }
  
  suppressWarnings({
    valid_cases[, dm_field1 := phonics::metaphone(clean_field1)]
    valid_cases[, dm_field2 := phonics::metaphone(clean_field2)]
  })
  
  exact_matches <- valid_cases[clean_field1 == clean_field2]
  non_matching <- valid_cases[clean_field1 != clean_field2]
  
  if (nrow(non_matching) > 0) {
    non_matching[, `:=`(
      str_sim = stringdist::stringsim(clean_field1, clean_field2, method = "jw"),
      metaphone_match = (dm_field1 == dm_field2)
    )]
    
    potential_matches <- non_matching[str_sim >= dm_threshold | metaphone_match == TRUE]
  } else {
    potential_matches <- data.table()
  }
  
  list(
    exact_matches = exact_matches,
    potential_matches = potential_matches,
    exact_match_count = nrow(exact_matches),
    potential_match_count = nrow(potential_matches),
    non_matching_count = nrow(non_matching)
  )
}

##################################################################################
compute_cosine_matches <- function(
    data, 
    field1, 
    field2, 
    cosine_threshold = 0.7, 
    ngram_size = 2, 
    debug = FALSE) {
  
  # First apply the filtering
  valid_cases <- filter_valid_pairs(data, field1, field2)
  
  # Apply text cleaning only if it hasn't been done before
  if (!clean_already) {
    valid_cases[, clean_field1 := clean_text(get(field1))]
    valid_cases[, clean_field2 := clean_text(get(field2))]
    clean_already <<- TRUE  # Ensure future calls know cleaning is done
  } else {
    # Ensure the columns exist, otherwise redefine them
    if (!("clean_field1" %in% names(valid_cases))) valid_cases[, clean_field1 := get(field1)]
    if (!("clean_field2" %in% names(valid_cases))) valid_cases[, clean_field2 := get(field2)]
  }
  
  exact_matches <- valid_cases[clean_field1 == clean_field2]
  non_matching <- valid_cases[clean_field1 != clean_field2]
  
  if (nrow(non_matching) > 0) {
    non_matching[, cosine_sim := stringdist::stringsim(clean_field1, clean_field2, 
                                                       method = "cosine", 
                                                       q = ngram_size)]
    
    potential_matches <- non_matching[cosine_sim >= cosine_threshold]
  } else {
    potential_matches <- data.table()
  }
  
  list(
    exact_matches = exact_matches,
    potential_matches = potential_matches,
    exact_match_count = nrow(exact_matches),
    potential_match_count = nrow(potential_matches),
    non_matching_count = nrow(non_matching)
  )
}

##################################################################################
# Function to run all matching methods
compute_all_method_matches <- function(
    data, 
    field1, 
    field2 ) {
  
  # First apply the filtering
  valid_cases <- filter_valid_pairs(data, field1, field2)
  
  # Apply text cleaning only if it hasn't been done before
  if (!clean_already) {
    valid_cases[, clean_field1 := clean_text(get(field1))]
    valid_cases[, clean_field2 := clean_text(get(field2))]
    clean_already <<- TRUE  # Ensure future calls know cleaning is done
  } else {
    # Ensure the columns exist, otherwise redefine them
    if (!("clean_field1" %in% names(data))) valid_cases[, clean_field1 := get(field1)]
    if (!("clean_field2" %in% names(data))) valid_cases[, clean_field2 := get(field2)]
  }
  
  # Run each matching method with `clean_already = TRUE`
  lev_results <- compute_levenshtein_matches(
    valid_cases, 
    field1, 
    field2, 
    max_distance = 2, 
    debug = FALSE
  )
  
  combined_results <- compute_JW_combined_matches(
    valid_cases, field1, field2, 
    jw_threshold = 0.85, 
    require_both = FALSE, 
    debug = FALSE
  )
  
  metaphone_results <- compute_metaphone_matches(
    valid_cases, field1, field2, 
    dm_threshold = 0.85, 
    debug = FALSE 
  )
  
  cosine_results <- compute_cosine_matches(
    valid_cases, field1, field2, 
    cosine_threshold = 0.7, 
    ngram_size = 2, 
    debug = FALSE
  )
  
  # Compile summary
  summary_table <- data.table(
    Method = c("Levenshtein", "Combined (JW/Phonetic)", "Metaphone", "Cosine N-gram"),
    Exact_Matches = c(
      lev_results$exact_match_count,
      combined_results$exact_match_count,
      metaphone_results$exact_match_count,
      cosine_results$exact_match_count
    ),
    Non_Matching = c(
      lev_results$non_matching_count,
      combined_results$non_matching_count,
      metaphone_results$non_matching_count,
      cosine_results$non_matching_count
    ),
    Potential_Matches = c(
      lev_results$potential_match_count,
      combined_results$potential_match_count,
      metaphone_results$potential_match_count,
      cosine_results$potential_match_count
    )
    
  )
  
  # Calculate match percentages
  summary_table[, Match_Percentage := round(Potential_Matches / Non_Matching * 100, 2)]
  
  return(summary_table)
}

##################################################################################
# UI
ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  titlePanel("Fuzzy Street Name Matching"),
  
  sidebarLayout(
    sidebarPanel(
      
      # Matching Method Selection
      selectInput("match_method", "Matching Method",
                  choices = c("Levenshtein" = "lev",
                              "Combined Jaro-Winkler/Phonetic" = "JW_combined",
                              "Double Metaphone" = "metaphone",
                              "Cosine N-gram" = "cosine"),
                  selected = "JW_combined"),
      
      # Field Selection
      selectInput("field1", "First Field", 
                  choices = c("cross_street_1",
                              "cross_street_2",
                              "street_name"),
                  selected = "cross_street_1"),
      
      selectInput("field2", "Second Field", 
                  choices = c("intersection_street_1",
                              "intersection_street_2",  
                              "landmark"), 
                  selected = "intersection_street_1"),
      
      # Method-Specific Parameters
      # Levenshtein Distance
      conditionalPanel(
        condition = "input.match_method == 'lev'",
        sliderInput("max_distance", "Max Levenshtein Distance", 
                    min = 1, max = 5, value = 2, step = 1)
      ),
      
      # Combined Jaro-Winkler
      conditionalPanel(
        condition = "input.match_method == 'JW_combined'",
        sliderInput("jw_threshold", "Jaro-Winkler Distance Threshold", 
                    min = 0.7, max = 1.0, value = 0.85, step = 0.05),
        checkboxInput("require_both", "Require both JW and Phonetic match", value = FALSE)
      ),
      
      # Metaphone
      conditionalPanel(
        condition = "input.match_method == 'metaphone'",
        sliderInput("dm_threshold", "String Similarity Threshold", 
                    min = 0.7, max = 1.0, value = 0.85, step = 0.05)
      ),
      
      # Cosine N-gram
      conditionalPanel(
        condition = "input.match_method == 'cosine'",
        sliderInput("cosine_threshold", "Cosine Similarity Threshold", 
                    min = 0.5, max = 1.0, value = 0.7, step = 0.05),
        sliderInput("ngram_size", "N-gram Size", 
                    min = 1, max = 4, value = 2, step = 1)
      ),
      
      # Action Buttons
      div(
        style = "display: flex; justify-content: space-between; margin-top: 20px;",
        actionButton("run_matching", "Run Fuzzy Matching", 
                     icon = icon("search"),
                     class = "btn-primary"),
        actionButton(
          "run_comparative_analysis", 
          "Compare Methods",
          icon = icon("chart-bar"),
          class = "btn-secondary"
        )
      ),
      
      # Help Text
      helpText(
        "Select matching method and parameters. ",
        "Use 'Run Fuzzy Matching' for current method, ",
        "'Compare Methods' for comprehensive analysis."
      )
    ),
    
    mainPanel(
      # Tabset for different result views
      
      
      tabsetPanel(
        id = "results_tabs",
        
        # Matches Table Tab
        tabPanel(
          "Matches Table", 
          shinycssloaders::withSpinner(
            DTOutput("matches_table"),
            type = 4,
            color = "#0275D8"
        )
      ),
        # Match Summary Tab
        tabPanel(
          "Match Summary",
          shinycssloaders::withSpinner(
            uiOutput("match_summary"),
            type = 4,
            color = "#0275D8"
          )
        ),
        
        # Method Comparison Tab
        tabPanel(
          "Method Comparison",
          shinycssloaders::withSpinner(
            DTOutput("method_comparison"),
            type = 4,
            color = "#0275D8"
          )
        )
      )
    )
  )
)

##################################################################################
server <- function(input, output, session) {
  # Create reactive data
  data <- reactive({
    cleaned_data
  })
  
  # Reactive values for tracking matching results and analysis state
  matching_results <- reactiveVal(NULL)
  comparative_results <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  
  ##########################################
  # Observe changes in matching method to show/hide relevant inputs
  observe({

    # Force the tab switch immediately via JavaScript
    shinyjs::runjs("$('#results_tabs a[data-value=\"Matches Table\"]').tab('show');")
    
    # Show/hide relevant inputs based on matching method
    if (input$match_method == "lev") {
      shinyjs::hideElement("jw_threshold")
      shinyjs::hideElement("require_both")
      shinyjs::hideElement("dm_threshold")
      shinyjs::hideElement("cosine_threshold")
      shinyjs::hideElement("ngram_size")
      shinyjs::showElement("max_distance")
    } else if (input$match_method == "JW_combined") {
      shinyjs::hideElement("max_distance")
      shinyjs::hideElement("dm_threshold")
      shinyjs::hideElement("cosine_threshold")
      shinyjs::hideElement("ngram_size")
      shinyjs::showElement("jw_threshold")
      shinyjs::showElement("require_both")
    } else if (input$match_method == "metaphone") {
      shinyjs::hideElement("max_distance")
      shinyjs::hideElement("jw_threshold")
      shinyjs::hideElement("require_both")
      shinyjs::hideElement("cosine_threshold")
      shinyjs::hideElement("ngram_size")
      shinyjs::showElement("dm_threshold")
    } else {  # cosine
      shinyjs::hideElement("max_distance")
      shinyjs::hideElement("jw_threshold")
      shinyjs::hideElement("require_both")
      shinyjs::hideElement("dm_threshold")
      shinyjs::showElement("cosine_threshold")
      shinyjs::showElement("ngram_size")
    }
  })
  
  ##########################################
  # Run Fuzzy Matching
  observeEvent(input$run_matching, {
   
    # Switch to Matches Table tab immediately
    shinyjs::runjs("$('#results_tabs a[data-value=\"Matches Table\"]').tab('show');")
    
    # Show loading notification
    notif_id <- showNotification(
      "Running fuzzy matching...", 
      type = "message", 
      duration = NULL
    )
    
    is_loading(TRUE)  # Set loading state
    
    # Run selected fuzzy matching method
    results <- if (input$match_method == "lev") {
      compute_levenshtein_matches(
        data(), 
        input$field1, 
        input$field2, 
        input$max_distance,
        debug = TRUE
      )
    } else if (input$match_method == "JW_combined") {  
      compute_JW_combined_matches(
        data(),
        input$field1,
        input$field2,
        input$jw_threshold,
        input$require_both,
        debug = TRUE
      )
    } else if (input$match_method == "metaphone") {
      compute_metaphone_matches(
        data(),
        input$field1,
        input$field2,
        input$dm_threshold,
        debug = TRUE
      )
    } else {  # cosine
      compute_cosine_matches(
        data(),
        input$field1,
        input$field2,
        input$cosine_threshold,
        input$ngram_size,
        debug = TRUE
      )
    }
    
    # Store results
    matching_results(results)
    is_loading(FALSE)  # Clear loading state
    
    # Remove specific notification
    removeNotification(id = notif_id)
  })
  
  ##########################################
  # Comparative analysis
  observeEvent(input$run_comparative_analysis, {

    # Force the tab switch immediately
    shinyjs::runjs("$('#results_tabs a[data-value=\"Method Comparison\"]').tab('show');")
    
    # Show loading notification
    notif_id <- showNotification(
      "Running comparative analysis...", 
      type = "message", 
      duration = NULL
    )
    

    # Run comparative analysis
    comp_results <- compute_all_method_matches(
      data(), 
      input$field1, 
      input$field2
    )
    

    # Store results
    comparative_results(comp_results)
    
    # Remove notification after a slight delay to ensure visibility
    shinyjs::delay(300, {
      removeNotification(id = notif_id)
    })
    
  })
  
  ##########################################
  # Match Summary Output
  output$match_summary <- renderUI({
    if(is_loading()) return(NULL)
    req(matching_results())
    
    if (is.null(matching_results())) {
      return(HTML("<strong>No matching cases found.</strong>"))
    }
    
    results <- matching_results()
    
    # Calculate total rows from sum of exact matches and non-matching records
    total_rows <- results$exact_match_count + results$non_matching_count
    
    HTML(paste0(
      "<strong>Total number of rows:</strong> ",
      formatC(total_rows, format="f", big.mark=",", digits=0), "<br><br>",
      
      "<strong># of Exact matches:</strong> ", 
      formatC(results$exact_match_count %||% 0, format="f", big.mark=",", digits=0), "<br>",
      
      "<strong># of Non-matching records:</strong> ", 
      formatC(results$non_matching_count %||% 0, format="f", big.mark=",", digits=0), "<br>",
      
      "<strong># of potential Fuzzy matches:</strong> ", 
      formatC(results$potential_match_count %||% 0, format="f", big.mark=",", digits=0), "<br>",
      
      "<strong>Fuzzy match percentage:</strong> ", 
      if (!is.null(results$non_matching_count) && results$non_matching_count > 0) {
        sprintf("%.2f%%", (results$potential_match_count / results$non_matching_count) * 100)
      } else {
        "N/A"
      }
    ))
  })
  
  ##########################################
  # Matches Table Output
  output$matches_table <- renderDT({

    if (is_loading()) {
      return(NULL)
    }
    if (is.null(matching_results())) {
      return(datatable(data.frame()))
    }
    
    matches_df <- matching_results()$potential_matches

    dt <- if (input$match_method == "lev") {
      data.table(
        original_field1 = matches_df[[input$field1]],
        original_field2 = matches_df[[input$field2]],
        distance = matches_df$levenshtein_dist
      )
    } else if (input$match_method == "JW_combined") {
      if (!all(c("jw_dist", "phonetic_match") %in% names(matches_df))) {
        return(datatable(data.frame()))
      }
      data.table(
        original_field1 = matches_df[[input$field1]],
        original_field2 = matches_df[[input$field2]],
        jw_distance = round(matches_df$jw_dist, 4),
        phonetic_match = matches_df$phonetic_match
      )
    } else if (input$match_method == "metaphone") {
      if (!all(c("str_sim", "metaphone_match") %in% names(matches_df))) {
        return(datatable(data.frame()))
      }
      data.table(
        original_field1 = matches_df[[input$field1]],
        original_field2 = matches_df[[input$field2]],
        similarity = round(matches_df$str_sim, 4),
        metaphone_match = matches_df$metaphone_match
      )
    } else {  # cosine
      if (!all(c("cosine_sim") %in% names(matches_df))) {
        return(datatable(data.frame()))
      }
      data.table(
        original_field1 = matches_df[[input$field1]],
        original_field2 = matches_df[[input$field2]],
        cosine_similarity = round(matches_df$cosine_sim, 4)
      )
    }
    
    datatable(dt, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  ##########################################
  # Method Comparison Output
  output$method_comparison <- renderDT({
    req(comparative_results())
    
    # Get results data
    comp_data <- comparative_results()
    
    # Calculate total from first row
    total_records <- comp_data$Exact_Matches[1] + comp_data$Non_Matching_Records[1]
    
    # Create header text with total
    header_text <- paste("Total Records:", formatC(total_records, format="f", big.mark=",", digits=0))
    
    datatable(
      comp_data,
      options = list(
        pageLength = 5,
        searching = FALSE,
        ordering = FALSE,
        dom = 't'
      ),
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: left;", header_text),
      rownames = FALSE,
      colnames = c(
        "Fuzzy Algorithm",
        "Exact Matches",
        "Non-Matching Records", 
        "Fuzzy Matches",
        "Fuzzy Match %"
      )
    )
  })
  
} # End of server function

##################################################################################
# Run the application 
shinyApp(ui, server)

##################################################################################