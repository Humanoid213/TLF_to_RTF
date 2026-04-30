rm(list = ls())
source("C:/Users/User/Desktop/sas/R_final/setup/setup.R")

##################################################################################

bign <- filter(adam$adsl, SAFFL == "Y") %>% 
  distinct(USUBJID, TRT01AN) %>% count(TRT01AN) %>% pull(n)

adae <- filter(adam$adae, SAFFL == "Y" & TRTEMFL == "Y")

line1 <- distinct(adae, TRTAN, USUBJID) %>% 
  group_by(TRTAN, USUBJID) %>% 
  arrange(TRTAN, USUBJID) %>% 
  slice_tail() %>% ungroup() %>% count(TRTAN) %>% 
  pivot_wider(names_from = TRTAN, names_prefix = "dr", values_from = n) %>% 
  mutate(ASOC = "Number of Subjects With Any TEAE", ord1 = 0, ord2 = 0)

line2 <- distinct(adae, TRTAN, USUBJID, ASEVN) %>% 
  group_by(TRTAN, USUBJID, ASEVN) %>% 
  arrange(TRTAN, USUBJID, ASEVN) %>% 
  slice_tail() %>% ungroup() %>% count(TRTAN, ASEVN) %>% 
  pivot_wider(names_from = ASEVN, names_prefix = "ASEV", values_from = n) %>% 
  mutate(ASOC = "Number of Subjects With Any TEAE", ord1 = 0, ord2 = 1)

line3 <- distinct(adae, TRTAN, USUBJID, ASOC) %>% 
  group_by(TRTAN, USUBJID, ASOC) %>% 
  arrange(TRTAN, USUBJID, ASOC) %>% 
  slice_tail() %>% ungroup() %>% count(TRTAN, ASOC) %>% 
  pivot_wider(names_from = TRTAN, names_prefix = "dr", values_from = n) %>% 
  mutate(ord1 = dense_rank(ASOC), ord2 = 0)

line4 <- distinct(adae, TRTAN, USUBJID, ASEVN, ASOC) %>% 
  group_by(TRTAN, USUBJID, ASOC, ASEVN) %>% 
  arrange(TRTAN, USUBJID, ASOC, ASEVN) %>% 
  slice_tail() %>% ungroup() %>% count(TRTAN, ASOC, ASEVN) %>% 
  pivot_wider(names_from = ASEVN, names_prefix = "ASEV", values_from = n) %>% 
  mutate(ord2 = 0) %>% left_join(., distinct(line3, ASOC, ord1), by = "ASOC")

line5 <- distinct(adae, TRTAN, USUBJID, ASOC, ADECOD) %>% 
  group_by(TRTAN, USUBJID, ASOC, ADECOD) %>% 
  arrange(TRTAN, USUBJID, ASOC, ADECOD) %>% 
  slice_tail() %>% ungroup() %>% count(TRTAN, ASOC, ADECOD) %>% 
  pivot_wider(names_from = TRTAN, names_prefix = "dr", values_from = n) %>% 
  left_join(., distinct(line3, ASOC, ord1), by = "ASOC") %>% group_by(ord1) %>% 
  mutate(ord2 = dense_rank(ADECOD)) %>% ungroup()

line6 <- distinct(adae, TRTAN, USUBJID, ASEVN, ASOC, ADECOD) %>% 
  group_by(TRTAN, USUBJID, ASOC, ADECOD, ASEVN) %>% 
  arrange(TRTAN, USUBJID, ASOC, ADECOD, ASEVN) %>% 
  slice_tail() %>% ungroup() %>% count(TRTAN, ASOC, ADECOD, ASEVN) %>% 
  pivot_wider(names_from = ASEVN, names_prefix = "ASEV", values_from = n) %>% 
  left_join(., distinct(line3, ASOC, ord1), by = "ASOC") %>% 
  left_join(., distinct(line5, ASOC, ADECOD, ord2), by = c("ASOC", "ADECOD"))

tab0 <- bind_rows(line2, line4, line6) %>% 
  arrange(ord1, ord2)

vars <- c("ASEV1", "ASEV2", "ASEV3", "ASEV4")

for (v in vars) {
  if (!v %in% names(tab0)) {
    tab0[[v]] <- NA
  }
}

tab1 <- pivot_longer(tab0, cols = c("ASEV1", "ASEV2", "ASEV3", "ASEV4"), names_to = "ASEV", values_to = "n") %>% 
  pivot_wider(names_from = TRTAN, names_prefix = "dr", values_from = n) %>%
  bind_rows(., line1, line3, line5) %>% arrange(ord1, ord2) %>% 
  mutate(ord3 = coalesce(as.numeric(gsub("ASEV", "", ASEV)), 0),
         ASEV = recode(toupper(ASEV), "ASEV1" = "Missing", "ASEV2" = "Mild", "ASEV3" = "Moderate", "ASEV4" = "Severe", .default = NA_character_),
         col1 = coalesce(ASEV, ADECOD, ASOC),
         col2 = ifelse(is.na(dr1), "0", str_c(format(dr1, digits = 2), " (", format(dr1/bign[1] * 100, digits = 2, nsmall = 1), "%)")),
         col3 = ifelse(is.na(dr2), "0", str_c(format(dr2, digits = 2), " (", format(dr2/bign[2] * 100, digits = 2, nsmall = 1), "%)")),
         col4 = ifelse(is.na(dr3), "0", str_c(format(dr3, digits = 2), " (", format(dr3/bign[3] * 100, digits = 2, nsmall = 1), "%)")),
         col1 = ifelse(ord3 != 0 , str_c("    ", col1), ifelse(ord2 != 0 , str_c("  ", col1), col1))
         ) %>% arrange(ord1, ord2, ord3)

groups <- list(
  list(
    label = paste(sep = "|",
                  " ",
                  " ",
                  "PTI-125"),
    cols  = c(4, 1.5, 3), 
    just = "c"
  ),
  list(
    label = paste(sep = "|",
                  "System Organ Class/\n Preferred Term/ Severity",
                  paste0("Placebo\n(N = ", bign[1], ")"), 
                  paste0("50 mg\n(N = ", bign[2], ")"),
                  paste0("100 mg\n(N = ", bign[3], ")")),
    cols  = c(4, 1.5, 1.5, 1.5), 
    just = c("l", "c", "c", "c")
  )
)

TLF_output(data = tab1,
           tlf_name = "t14_3_2_2",
           target = "dev",
           type = "table",
           n_col = 4,
           groups = groups,
           by_vars = c("ord1", "ord2"),
           bl_vars = c("ord1", "ord2"), 
           max_rows = 1,
           cont = "Y",
           cont_vars = asoc,
           cont_ord = ord1,
           sort_vars = c("ord1", "ord2", "ord3"))
