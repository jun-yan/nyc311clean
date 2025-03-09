##########################################################################

# deploy_threee_apps.R - Deploy multiple Shiny apps to shinyapps.io

##########################################################################
library(rsconnect)

# Define your Shiny apps and their directories
# Using relative paths from your current working directory
base_path <- "GitHub/nyc311clean/data_anomalies"

apps <- list(
  "fuzzy_matching" = file.path(base_path, "shiny_apps/fuzzy_matching"),
  "daylight_saving_time_begins" = file.path(base_path, "shiny_apps/daylight_saving_time_begins"),
  "daylight_saving_time_ends" = file.path(base_path, "shiny_apps/daylight_saving_time_ends")
)

# Authenticate rsconnect (Only needs to be done once per session)
rsconnect::setAccountInfo(name='nycodw2025', 
                          token='76F679A5FCAC0AAB8B03187BB850495F',
                          secret='YEkvtg58zfQNWLNhzywzsdkG5Wvt/dUpa7gRyt+J')

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

##########################################################################    
# 
# 
# 
# # Authenticate rsconnect (Only needs to be done once per session)
# rsconnect::setAccountInfo(name='nycodw2025', 
#                           token='76F679A5FCAC0AAB8B03187BB850495F',
#                           secret='YEkvtg58zfQNWLNhzywzsdkG5Wvt/dUpa7gRyt+J')
# 
