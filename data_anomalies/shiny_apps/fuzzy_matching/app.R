##################################################################################

########## FUZZY LOGIC MATCHING APP ##########

##################################################################################
# fuzzy_matching_app.R
library(shiny)
library(data.table)
library(stringdist)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(phonics)
library(stringr)
library(stringdist)
library(future)
library(promises)
library(later)

# Specify the port (choose a unique port number)
options(shiny.port = 4007)

##################################################################################
# standarizing street names

standardize_street_name <- function(street_name) {
  start_time <- Sys.time()  # Start timing
  
  message("Standardizing street names...")  # Debug message
  
  # Define street type patterns (in order of priority)
  street_patterns <- c(
    "\\bSTREET\\b" = "ST",
    "\\bAVENUE\\b" = "AVE",
    "\\bBOULEVARD\\b" = "BLVD",
    "\\bROAD\\b" = "RD",
    "\\bPLACE\\b" = "PL",
    "\\bPARKWAY\\b" = "PKWY",
    "\\bDRIVE\\b" = "DR",
    "\\bLANE\\b" = "LN",
    "\\bCOURT\\b" = "CT",
    "\\bTERRACE\\b" = "TER",
    "\\bSQUARE\\b" = "SQ",
    "\\bPKWY\\b" = "PKWY",  # Already abbreviated, keep as-is
    "\\bWAY\\b" = "WAY"     # Already abbreviated, keep as-is
  )
  
  # Define direction patterns
  direction_patterns <- c(
    "\\bNORTH\\b" = "N",
    "\\bSOUTH\\b" = "S",
    "\\bEAST\\b" = "E",
    "\\bWEST\\b" = "W"
  )
  
  # Apply replacements in one step for each category (street & directions)
  street_name <- str_replace_all(street_name, street_patterns)
  street_name <- str_replace_all(street_name, direction_patterns)
  
  # Remove extra spaces
  street_name <- str_squish(street_name)  # `str_squish()` trims & collapses spaces
  
  end_time <- Sys.time()  # Stop timing
  elapsed_time <- end_time - start_time  # Compute elapsed time
  
  message(sprintf("Standardization completed in %.4f seconds", elapsed_time))
  
  return(street_name)
}

##################################################################################
# Load data
data_file <- file.path("data", "dataset.RDS")

if (!file.exists(data_file)) {
  stop("\nData file not found in the 'data' directory.")
}

cleaned_data <- tryCatch({
  readRDS(data_file)
}, error = function(e) {
  stop("\nError loading dataset: ", e$message)
})

# Convert to data.table if not already
if (!inherits(cleaned_data, "data.table")) {
  cleaned_data <- as.data.table(cleaned_data)
}

# Ensure required columns exist before checking matches
required_cols <- c("street_name", "landmark", "cross_street_1", "intersection_street_1", "cross_street_2", "intersection_street_2")
missing_cols <- setdiff(required_cols, names(cleaned_data))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Capture raw exact matches before standardization
raw_matches <- list(
  "street_name == landmark" = sum(cleaned_data$street_name == cleaned_data$landmark, na.rm = TRUE),
  "cross_street_1 == intersection_street_1" = sum(cleaned_data$cross_street_1 == cleaned_data$intersection_street_1, na.rm = TRUE),
  "cross_street_2 == intersection_street_2" = sum(cleaned_data$cross_street_2 == cleaned_data$intersection_street_2, na.rm = TRUE)
)

# Attach the raw match counts as attributes to the dataset
attr(cleaned_data, "original_exact_matches") <- raw_matches

# Standardize street names immediately upon loading
cleaned_data[, street_name := standardize_street_name(street_name)]
cleaned_data[, landmark := standardize_street_name(landmark)]
cleaned_data[, cross_street_1 := standardize_street_name(cross_street_1)]
cleaned_data[, intersection_street_1 := standardize_street_name(intersection_street_1)]
cleaned_data[, cross_street_2 := standardize_street_name(cross_street_2)]
cleaned_data[, intersection_street_2 := standardize_street_name(intersection_street_2)]

# Remove extra blank spaces from all character columns
char_cols <- names(cleaned_data)[sapply(cleaned_data, is.character)]
cleaned_data[, (char_cols) := lapply(.SD, function(x) gsub("\\s+", " ", trimws(x))),
             .SDcols = char_cols]

##################################################################################
########## Levenshtein algorithm ##########

compute_levenshtein_matches <- function(
    data, 
    field1, 
    field2, 
    max_distance = 2
) {
  # Identify exact matches directly using the provided fields
  exact_matches <- data[data[[field1]] == data[[field2]]]
  
  # Identify non-matching cases as all rows in data that are not exact matches
  non_matching <- data[data[[field1]] != data[[field2]]]
  
  # Compute Levenshtein distance for non-matching cases using the subset
  if (nrow(non_matching) > 0) {
    non_matching[, levenshtein_dist := stringdist(
      non_matching[[field1]], 
      non_matching[[field2]], 
      method = "lv"
    )]
  }
  
  # Identify potential matches within the specified Levenshtein distance
  potential_matches <- non_matching[levenshtein_dist <= max_distance]
  
  return(list(
    potential_matches = potential_matches,
    exact_match_count = nrow(exact_matches),
    non_matching_count = nrow(non_matching),
    potential_match_count = nrow(potential_matches)
  ))
}

##################################################################################
########## Jaro-Winkler/Phonetic algorithm ##########

compute_JW_combined_matches <- function(
    data, 
    field1, 
    field2, 
    jw_threshold = 0.1, 
    require_both = FALSE 
) {
  suppressWarnings({
    data[, ph_field1 := phonics::soundex(get(field1))]
    data[, ph_field2 := phonics::soundex(get(field2))]
  })
  
  # Get raw JW scores
  jw_scores <- stringdist::stringsim(data[[field1]], data[[field2]], method = "jw")
  
  # First handle exact matches
  exact_matches <- data[get(field1) == get(field2)]
  
  # Filter out empty strings BEFORE doing string distance calculations
  non_matching <- data[get(field1) != get(field2)]
  
  if (nrow(non_matching) > 0) {
    non_matching[, `:=`(
      jw_dist = stringdist::stringsim(get(field1), get(field2), method = "jw"),
      phonetic_match = !is.na(ph_field1) & !is.na(ph_field2) & (ph_field1 == ph_field2)
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
########## Metaphone algorithm ##########

compute_metaphone_matches <- function(
    data, 
    field1, 
    field2, 
    dm_threshold = 0.85 
) {
  suppressWarnings({
    data[, dm_field1 := phonics::metaphone(get(field1))]
    data[, dm_field2 := phonics::metaphone(get(field2))]
  })
  
  exact_matches <- data[data[[field1]] == data[[field2]]]
  non_matching <- data[data[[field1]] != data[[field2]]]
  
  if (nrow(non_matching) > 0) {
    non_matching[, `:=`(
      str_sim = stringdist::stringsim(non_matching[[field1]], non_matching[[field2]], method = "jw"),
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
########## Cosine algorithm ##########

compute_cosine_matches <- function(
    data, 
    field1, 
    field2, 
    cosine_threshold = 0.7, 
    ngram_size = 2 
) {
  # Identify non-matching cases directly using the provided fields
  non_matching <- data[data[[field1]] != data[[field2]]]
  
  # Compute Cosine similarity for non-matching cases
  if (nrow(non_matching) > 0) {
    non_matching[, cosine_sim := stringdist::stringsim(
      non_matching[[field1]], non_matching[[field2]], method = "cosine", q = ngram_size
    )]
  } else {
    non_matching[, cosine_sim := numeric(0)]  # Ensure the column exists
  }
  
  # Identify potential matches based on the cosine similarity threshold
  potential_matches <- non_matching[cosine_sim >= cosine_threshold]
  
  return(list(
    exact_matches = data[data[[field1]] == data[[field2]]],
    potential_matches = potential_matches,
    exact_match_count = sum(data[[field1]] == data[[field2]], na.rm = TRUE),
    potential_match_count = nrow(potential_matches),
    non_matching_count = nrow(non_matching)
  ))
}

##################################################################################
# Function to run all matching methods
compute_all_method_matches <- function(
    data, 
    field1, 
    field2 
) {
  # Run each matching method using the original fields directly
  lev_results <- compute_levenshtein_matches(
    data, 
    field1, 
    field2, 
    max_distance = 2
  )
  
  combined_results <- compute_JW_combined_matches(
    data, field1, field2, 
    jw_threshold = 0.85, 
    require_both = FALSE 
  )
  
  metaphone_results <- compute_metaphone_matches(
    data, field1, field2, 
    dm_threshold = 0.85
  )
  
  cosine_results <- compute_cosine_matches(
    data, field1, field2, 
    cosine_threshold = 0.7, 
    ngram_size = 2
  )
  
  # Compile summary table of results
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
      conditionalPanel(
        condition = "input.match_method == 'lev'",
        sliderInput("max_distance", "Max Levenshtein Distance", 
                    min = 1, max = 5, value = 2, step = 1)
      ),
      
      conditionalPanel(
        condition = "input.match_method == 'JW_combined'",
        sliderInput("jw_threshold", "Jaro-Winkler Distance Threshold", 
                    min = 0.1, max = 1.0, value = 0.85, step = 0.05),
        checkboxInput("require_both", "Require both JW and Phonetic match", value = FALSE)
      ),
      
      conditionalPanel(
        condition = "input.match_method == 'metaphone'",
        sliderInput("dm_threshold", "String Similarity Threshold", 
                    min = 0.7, max = 1.0, value = 0.85, step = 0.05)
      ),
      
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
        actionButton("run_comparative_analysis", 
                     "Compare Methods",
                     icon = icon("chart-bar"),
                     class = "btn-secondary")
      ),
      
      # Help Text
      helpText(
        "Select matching method and parameters. ",
        "Use 'Run Fuzzy Matching' for current method, ",
        "'Compare Methods' for comprehensive analysis."
      )
    ),
    
    mainPanel(
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
  
  ##########################################
  # Ensure cleaned_data is available globally
  data <- reactiveVal(cleaned_data)
  
  # Extract original match counts
  original_exact_matches <- reactive({
    attr(data(), "original_exact_matches")
  })
  
  # Reactive values for tracking matching results and analysis state
  matching_results <- reactiveVal(NULL)
  comparative_results <- reactiveVal(NULL)
  is_loading <- reactiveVal(FALSE)
  
  ##########################################
  # Ensure correct field2 choices based on selected field1
  observe({
    req(input$field1)
    
    new_field2 <- switch(input$field1,
                         "cross_street_1" = "intersection_street_1",
                         "cross_street_2" = "intersection_street_2",
                         "street_name" = "landmark")
    
    updateSelectInput(session, "field2",
                      choices = c("intersection_street_1", "intersection_street_2", "landmark"),
                      selected = new_field2)
  })
  
  ##########################################
  # Observe changes in matching method to show/hide relevant inputs
  observe({
    shinyjs::runjs("$('#results_tabs a[data-value=\"Matches Table\"]').tab('show');")
    
    method <- input$match_method
    
    shinyjs::hideElement(c("jw_threshold", "require_both", "dm_threshold", "cosine_threshold", "ngram_size", "max_distance"))
    
    if (method == "lev") {
      shinyjs::showElement("max_distance")
    } else if (method == "JW_combined") {
      shinyjs::showElement(c("jw_threshold", "require_both"))
    } else if (method == "metaphone") {
      shinyjs::showElement("dm_threshold")
    } else {  # cosine
      shinyjs::showElement(c("cosine_threshold", "ngram_size"))
    }
  })
  
  ##########################################
  # Run Fuzzy Matching
  observeEvent(input$run_matching, {
    shinyjs::runjs("$('#results_tabs a[data-value=\"Matches Table\"]').tab('show');")
    
    notif_id <- showNotification("Running fuzzy matching...", type = "message", duration = NULL)
    is_loading(TRUE)  
    
    # Run selected fuzzy matching method
    results <- switch(input$match_method,
                      "lev" = compute_levenshtein_matches(data(), input$field1, input$field2, input$max_distance),
                      "JW_combined" = compute_JW_combined_matches(data(), input$field1, input$field2, input$jw_threshold, input$require_both),
                      "metaphone" = compute_metaphone_matches(data(), input$field1, input$field2, input$dm_threshold),
                      compute_cosine_matches(data(), input$field1, input$field2, input$cosine_threshold, input$ngram_size) # Default to cosine
    )
    
    matching_results(results)
    is_loading(FALSE)  
    removeNotification(id = notif_id)
  })
  
  ##########################################
  # Comparative analysis
  observeEvent(input$run_comparative_analysis, {
    shinyjs::runjs("$('#results_tabs a[data-value=\"Method Comparison\"]').tab('show');")
    
    notif_id <- showNotification("Running comparative analysis...", type = "message", duration = NULL)
    
    comp_results <- compute_all_method_matches(data(), input$field1, input$field2)
    
    comparative_results(comp_results)
    
    shinyjs::delay(300, {
      removeNotification(id = notif_id)
    })
  })
  
  ##########################################
  # Outputs
  ##########################################
  # Display match summary
  output$match_summary <- renderUI({
    req(data())
    
    # Get precomputed exact matches before normalization
    exact_matches <- original_exact_matches()
    
    # Ensure input selections exist
    field1 <- input$field1
    field2 <- input$field2
    key <- paste(field1, "==", field2)
    
    original_exact_matches_count <- ifelse(!is.null(exact_matches[[key]]), exact_matches[[key]], 0)
    
    # Compute exact matches after normalization
    after_normalization_count <- matching_results()$exact_match_count %||% 0
    
    # Compute normalization increase
    normalization_increase <- after_normalization_count - original_exact_matches_count
    
    # Compute total rows after loading
    total_rows <- nrow(data())
    
    HTML(paste0(
      "<strong>Total number of rows:</strong> ",
      formatC(total_rows, format = "f", big.mark = ",", digits = 0), "<br><br>",
      
      "<strong># of Exact matches before normalization:</strong> ", 
      formatC(original_exact_matches_count, format = "f", big.mark = ",", digits = 0), "<br>",
      
      "<strong># of Exact matches after normalization:</strong> ", 
      formatC(after_normalization_count, format = "f", big.mark = ",", digits = 0), "<br>",
      
      "<strong>Normalization increase:</strong> ", 
      formatC(normalization_increase, format = "f", big.mark = ",", digits = 0), "<br>",
      
      "<strong># of Non-matching:</strong> ", 
      formatC(matching_results()$non_matching_count %||% 0, format = "f", big.mark = ",", digits = 0), "<br>",
      
      "<strong># of potential Fuzzy matches:</strong> ", 
      formatC(matching_results()$potential_match_count %||% 0, format = "f", big.mark = ",", digits = 0), "<br>",
      
      "<strong>Fuzzy match percentage:</strong> ", 
      if (!is.null(matching_results()$non_matching_count) && matching_results()$non_matching_count > 0) {
        sprintf("%.2f%%", (matching_results()$potential_match_count / matching_results()$non_matching_count) * 100)
      } else {
        "N/A"
      }
    ))
  })
  
  ##########################################
  # Matches Table Output
  output$matches_table <- renderDT({
    if (is_loading()) return(NULL)
    if (is.null(matching_results())) return(datatable(data.frame()))
    
    matches_df <- matching_results()$potential_matches
    
    dt <- switch(input$match_method,
                 "lev" = data.table(
                   original_field1 = matches_df[[input$field1]], 
                   original_field2 = matches_df[[input$field2]], 
                   distance = matches_df$levenshtein_dist
                 ),
                 "JW_combined" = data.table(
                   original_field1 = matches_df[[input$field1]], 
                   original_field2 = matches_df[[input$field2]], 
                   jw_distance = if ("jw_dist" %in% names(matches_df)) round(as.numeric(matches_df$jw_dist), 4) else NA_real_, 
                   phonetic_match = matches_df$phonetic_match
                 ),
                 "metaphone" = data.table(
                   original_field1 = matches_df[[input$field1]], 
                   original_field2 = matches_df[[input$field2]], 
                   similarity = if ("str_sim" %in% names(matches_df)) round(as.numeric(matches_df$str_sim), 4) else NA_real_, 
                   metaphone_match = matches_df$metaphone_match
                 ),
                 data.table(
                   original_field1 = matches_df[[input$field1]], 
                   original_field2 = matches_df[[input$field2]], 
                   cosine_similarity = if ("cosine_sim" %in% names(matches_df)) round(as.numeric(matches_df$cosine_sim), 4) else NA_real_
                 ) # Default to cosine
    )
    
    datatable(dt, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  ##########################################
  # Method Comparison Output
  output$method_comparison <- renderDT({
    req(comparative_results())
    
    comp_data <- as.data.table(comparative_results())  # Ensure it's a data.table
    
    if ("Match_Percentage" %in% names(comp_data)) {
      comp_data[["Match_Percentage"]] <- paste0(formatC(as.numeric(comp_data[["Match_Percentage"]]), format = "f", digits = 2), "%")
    } else {
      warning("Column 'Match_Percentage' not found in data.")
    }
    
    total_records <- comp_data$Exact_Matches[1] + comp_data$Non_Matching[1]
    
    header_text <- paste("Total Records:", formatC(total_records, format="f", big.mark=",", digits=0))
    
    datatable(
      comp_data,
      options = list(
        pageLength = 5,
        searching = FALSE,
        ordering = FALSE,
        dom = 't',
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ),
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: left;", header_text),
      rownames = FALSE,
      colnames = c(
        "Fuzzy Algorithm",
        "Normalized Matches",  # 🔹 Renamed from "Exact Matches"
        "Non-Matching", 
        "Potential Fuzzy Matches",  # 🔹 Previously changed from "Fuzzy Matches"
        "Fuzzy Match Recovery %"
      )
    )
  })
  
} # End of server function

##################################################################################
# Run the application 
shinyApp(ui, server)

##################################################################################