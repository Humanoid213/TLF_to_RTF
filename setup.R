library(readxl)
library(openxlsx)
library(janitor)
library(tidyverse)
library(haven)
library(stringr)
library(stringi)
library(labelled)
library(kableExtra)
library(purrr)
library(hms)
library(lubridate)
library(reporter)
library(flextable)
library(officer)
library(sassy)
library(r2rtf)
library(emmeans)
library(ggplot2)
library(rlang)
library(cowplot)
library(gghighlight)
library(gt)
library(patchwork)
library(survival)
library(ggsurvfit)
library(survminer)
library(psych)
library(writexl)

options(scipen = 999, digits = 10)

study_path <- "C:/Users/User/Desktop/sas/R_final"

scan_r <- function(text, n, delim = " ") {
  str_split(text, delim, simplify = TRUE)[, n]
}

# Function to add a blank (NA) row to a data frame
add_blank_row <- function(data) {
  # Create a new row with NA values for all columns
  blank_row <- data.frame(matrix(NA, nrow = 1, ncol = ncol(data)))
  colnames(blank_row) <- colnames(data)
  
  # Bind the blank row to the end of the group's data
  bind_rows(data, blank_row)
}

# ===============================
# Internal loader (not global)
# ===============================
create_env <- function(env_name, path) {
  
  # Create or reset environment
  if (!exists(env_name, envir = .GlobalEnv, inherits = FALSE) ||
      !is.environment(get(env_name, envir = .GlobalEnv))) {
    
    assign(env_name, new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  
  env <- get(env_name, envir = .GlobalEnv)
  
  # Clear env on re-source
  rm(list = ls(envir = env), envir = env)
  
  # -------------------------------
  # Detect files
  # -------------------------------
  xpt_files <- list.files(path, pattern = "\\.xpt$", ignore.case = TRUE, full.names = TRUE)
  sas_files <- list.files(path, pattern = "\\.sas7bdat$", ignore.case = TRUE, full.names = TRUE)
  
  if (length(xpt_files) == 0 && length(sas_files) == 0) {
    warning("No TLF files found in: ", path)
    return(invisible(NULL))
  }
  
  # -------------------------------
  # Load XPT files
  # -------------------------------
  if (length(xpt_files) > 0) {
    for (f in xpt_files) {
      nm <- tolower(tools::file_path_sans_ext(basename(f)))
      assign(nm, read_xpt(f), envir = env)
      obj <- get(nm, envir = env)
      obj[do.call(cbind, lapply(obj, '%in%', c("")))] <- NA
      assign(nm, obj, envir = env)
    }
  }
  
  # -------------------------------
  # Load SAS files
  # -------------------------------
  if (length(sas_files) > 0) {
    for (f in sas_files) {
      nm <- tolower(tools::file_path_sans_ext(basename(f)))
      assign(nm, read_sas(f), envir = env)
      obj <- get(nm, envir = env)
      obj[do.call(cbind, lapply(obj, '%in%', c("")))] <- NA
      assign(nm, obj, envir = env)
    }
  }
  
  invisible(NULL)
}