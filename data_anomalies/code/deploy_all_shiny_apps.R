################################################################################
################################################################################

########## Deploy multiple Shiny apps to shinyapps.io ###########
########## Deploy 5 apps to davidtussey@gmail.com account #######

################################################################################
required_packages <- c("future", "phonics", "shinyjs", "rsconnect")

# Install any missing packages
installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load the packages
lapply(required_packages, library, character.only = TRUE)

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
  )
)

# Define rsconnect credentials
rsconnect_credentials <- list(
  "nycopendataweek2025" = list(
    "token"  = "DACD710234595CD43FB98EE3DF3BB310",
    "secret" = "cy1tFjJW760AJ7Rxmfr4+qAik1JRAjRI6XioTWqf"
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