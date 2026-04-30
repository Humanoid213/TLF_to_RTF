rm(list = ls())
source("C:/Users/User/Desktop/sas/R_final/setup/setup.R")

##################################################################################

adae <- filter(adam$adae, SAFFL == "Y" & TRTEMFL == "Y")

ls1 <- adae %>% 
  mutate(
    ord = TRTAN, col1 = TRTA, col2 = SUBJID,
    col3 = paste(sep = "/\n ", AEBODSYS, AEDECOD, AETERM),
    col4 = paste(sep = "/\n ", toupper(trimws(format(ASTDT, "%d%b%Y"))), coalesce(toupper(trimws(format(AENDT, "%d%b%Y"))), AENRF)),
    col5 = as.character(ASTDY - AENDY + 1), col6 = AESEV, col7 = AEREL, col8 = AEOUT, col9 = AESER, col10 = AEACN
  )

groups <- list(
  list(
    label = paste(sep = "|",
                  "Treatment Group",
                  "Subject ID",
                  "System Organ Class/\nPreferred Term/\nVerbatim",
                  "Start Date/\nStop Date or Ongoing?",
                  "Duration",
                  "Severity",
                  "Causality",
                  "Outcome",
                  "SAE?",
                  "Action Taken"),
    cols = c(0.7, 0.7, 2.4, 1, 0.7, 0.9, 0.9, 1.1, 0.7, 1.1), 
    just = c("c", "c", "l", rep("c", 7))
  )
)

TLF_output(data = ls1,
           tlf_name = "l16_3_1",
           target = "dev",
           type = "listing",
           n_col = 10,
           groups = groups,
           by_vars = c("ord", "col1", "col2"),
           bl_vars = c("ord", "col1", "col2"),
           max_rows = 2,
           db_vars = c("col1", "col2"),
           cont = "N",
           cont_vars = ,
           cont_ord = ,
           sort_vars = c("ord", "col1", "col2"))
