################################################################################

# deploy_all_apps.R - Deploy multiple Shiny apps to shinyapps.io
library(rsconnect)

# Define your Shiny apps and their directories
# Using relative paths from your current working directory
base_path <- "datacleaningproject/nyc311clean/data_anomalies"

apps <- list(
    "zip_validator" = file.path(base_path, "shiny_apps/zip_validator"),
    "duplicate_fields" = file.path(base_path, "shiny_apps/duplicate_fields"),
    "illogical_dates" = file.path(base_path, "shiny_apps/illogical_dates"),
    "missing_values" = file.path(base_path, "shiny_apps/missing_values"),
    "schema_validator" = file.path(base_path, "shiny_apps/schema_validator")
   )


# Authenticate rsconnect (Only needs to be done once per session)
#Uncomment and set your credentials if needed
rsconnect::setAccountInfo(name = "nycopendataweek2025",
                          token = "DACD710234595CD43FB98EE3DF3BB310",
                          secret= "cy1tFjJW760AJ7Rxmfr4+qAik1JRAjRI6XioTWqf" )


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

# Loop through all apps and deploy them
for (app in names(apps)) {
  deploy_app(app, apps[[app]])
}

message("\n🎉 All Shiny apps deployed successfully!")

################################################################################