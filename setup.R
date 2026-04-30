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