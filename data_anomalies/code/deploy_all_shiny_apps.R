################################################################################
################################################################################

########## Deploy multiple Shiny apps to shinyapps.io ##########
########## Deploy 5 apps to davidtussey@gmail.com account ######

################################################################################

library(rsconnect)

# Define  Shiny apps and their directories
# Using relative paths from your current working directory

# Set working directory and base path
working_dir <- getwd()
base_shiny_path <- file.path(working_dir, "datacleaningproject", "nyc311clean", "data_anomalies", "shiny_apps")

# Define apps and their associated accounts
apps <- list(
  "nycopendataweek2025" = list(
    "zip_validator" = file.path(base_shiny_path, "zip_validator"),
    "duplicate_fields" = file.path(base_shiny_path, "duplicate_fields"),
    "illogical_dates" = file.path(base_shiny_path, "illogical_dates"),
    "missing_values" = file.path(base_shiny_path, "missing_values"),
    "schema_validator" = file.path(base_shiny_path, "schema_validator")
  ),
  "nycodw2025" = list(
    "fuzzy_matching" = file.path(base_shiny_path, "fuzzy_matching"),
    "daylight_saving_time_begins" = file.path(base_shiny_path, "daylight_saving_time_begins"),
    "daylight_saving_time_ends" = file.path(base_shiny_path, "daylight_saving_time_ends")
  )
)

# Define rsconnect credentials
rsconnect_credentials <- list(
  "nycopendataweek2025" = list(
    "token"  = "DACD710234595CD43FB98EE3DF3BB310",
    "secret" = "cy1tFjJW760AJ7Rxmfr4+qAik1JRAjRI6XioTWqf"
  ),
  "nycodw2025" = list(
    "token"  = "76F679A5FCAC0AAB8B03187BB850495F",
    "secret" = "YEkvtg58zfQNWLNhzywzsdkG5Wvt/dUpa7gRyt+J"
  )
)

# Function to authenticate account
authenticate_account <- function(account) {
  creds <- rsconnect_credentials[[account]]
  rsconnect::setAccountInfo(name = account, token = creds$token, secret = creds$secret)
  message("🔑 Authenticated account: ", account, "\n")
}

# Function to deploy an app
deploy_app <- function(app_name, app_path) {
  message("\n🚀 Deploying: ", app_name, " ...")
  tryCatch({
    rsconnect::deployApp(app_path, appName = app_name, forceUpdate = TRUE)
    message("✅ Deployment complete: ", app_name, "\n")
  }, error = function(e) {
    message("❌ Error deploying ", app_name, ": ", e$message, "\n")
  })
}

# Deploy apps, switching accounts as needed
for (account in names(apps)) {
  authenticate_account(account)  # Set the correct account
  for (app in names(apps[[account]])) {
    deploy_app(app, apps[[account]][[app]])
  }
}

##############################################################################
################################################################################