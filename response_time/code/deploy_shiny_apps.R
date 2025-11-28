# MINIMAL DEPLOYMENT SCRIPT - only load what's absolutely needed

# Only load rsconnect for deployment
required_packages <- c("rsconnect")

# Install any missing packages
installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load only rsconnect
library(rsconnect)

# Set working directory and base path
base_shiny_path <- "C:/Users/David/OneDrive/Documents/datacleaningproject/nyc311clean/response_time/shiny_apps"

# Define apps
apps <- list(
  "nycopendataweek2025" = list(
    "response_times" = base_shiny_path
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
deploy_app <- function(app_name, app_path, account) {
  message("\n🚀 Deploying: ", app_name, " from path: ", app_path, " ...")
  
  if (!file.exists(file.path(app_path, "app.R"))) {
    message("❌ Error: app.R not found in ", app_path)
    return()
  }
  
  tryCatch({
    rsconnect::deployApp(
      appDir = app_path, 
      appName = app_name, 
      account = account, 
      forceUpdate = TRUE,
      launch.browser = FALSE
    )
    message("✅ Deployment complete: ", app_name, "\n")
    message("🌐 App URL: https://", account, ".shinyapps.io/", app_name, "\n")
  }, error = function(e) {
    message("❌ Error deploying ", app_name, ": ", e$message, "\n")
  })
}

# Deploy apps
for (account in names(apps)) {
  authenticate_account(account)
  for (app in names(apps[[account]])) {
    deploy_app(app, apps[[account]][[app]], account)
  }
}