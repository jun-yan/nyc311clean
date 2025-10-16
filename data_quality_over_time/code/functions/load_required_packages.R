################################################################################
load_required_packages <- function(pkg_list) {
  missing <- pkg_list[!(pkg_list %in% installed.packages()[, "Package"])]
  if (length(missing)) {
    install.packages(missing)
    cat("📦 Installed missing packages:", paste(missing, collapse = ", "), "\n")
  }
  sapply(pkg_list, function(pkg) {
    suppressPackageStartupMessages(
      tryCatch({
        library(pkg, character.only = TRUE)
        TRUE
      }, error = function(e) {
        warning(paste("❌ Failed to load package:", pkg))
        FALSE
      })
    )
  })
}

################################################################################