#########################################################################
# Consolidate agencies where names have changed.

consolidate_agencies <- function(DT, drop_agencies = character(), verbose = TRUE) {
  stopifnot(is.data.frame(DT))
  if (!is.data.table(DT)) setDT(DT)
  if (!"agency" %in% names(DT)) stop("'agency' column not found.")
  if (!is.character(DT$agency)) DT[, agency := as.character(agency)]
  
  # Map legacy/alias values -> current codes (exact matches only)
  lookup <- c(
    "DCA"  = "DCWP",
    "DEPARTMENT OF CONSUMER AND WORKER PROTECTION" = "DCWP",
    "NYC311-PRD" = "OTI",
    "DOITT" = "OTI",
    "311"   = "OTI",
    "3-1-1" = "OTI"
  )
  
  # Consolidate agency values
  mapped     <- lookup[DT$agency]                 # NA where no match
  changed_ix <- which(!is.na(mapped) & mapped != DT$agency)
  if (length(changed_ix)) DT[changed_ix, agency := mapped[changed_ix]]
  n_changed <- length(changed_ix)
  
  # Optionally drop selected agencies (expect post-consolidation codes)
  drop_agencies <- c("FDNY")
  
  n_before <- nrow(DT)
  if (length(drop_agencies)) {
    DT <- DT[!(agency %in% drop_agencies)]
  }
  n_dropped <- n_before - nrow(DT)
  
  if (verbose) {
    cat(
      "\n\nAgencies consolidated: ", format(n_changed, big.mark = ","),
      "; Rows dropped: ", format(n_dropped, big.mark = ","),
      "; Rows remaining: ", format(nrow(DT), big.mark = ",")
    )
  }
  DT
}


#########################################################################