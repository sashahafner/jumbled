# Set working directory to script file location for RStudio users

rsswd <- function(quiet = TRUE) {
  if (Sys.getenv("RSTUDIO") == 1) {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  } else {
    if (!quiet) message('Not using RStudio (or RSTUDIO != 1) so working directory not changed.')
  }
  if (!quiet) message('Working directory now: ', getwd())
}
