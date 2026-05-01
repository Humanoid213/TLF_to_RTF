rm(list = ls())
source("C:/Users/User/Desktop/sas/R_final/setup/setup.R")

##################################################################################

bign <- filter(adam$adsl, FASFL == "Y") %>% 
  distinct(USUBJID, TRT01AN) %>% count(TRT01AN) %>% pull(n)

adlb <- filter(adam$adlb, FASFL == "Y" & PARCAT1 == "CHEMISTRY" & ANL01FL == "Y" & !is.na(AVAL)) %>% 
  group_by(PARAMCD) %>%
  mutate(dg = max(ifelse(grepl("\\.", as.character(AVAL)), nchar(scan_r(as.character(AVAL), 2, "\\.")), 0)),
         dg = ifelse(dg >= 3, 3, dg), AVISITN = 10 * AVISITN) %>% ungroup() %>%
  group_by(TRTAN, PARAMN, PARAMCD, PARAM, dg, AVISITN, AVISIT)


avalmn <- adlb %>% 
  summarise(n_ = n(), mean = mean(AVAL), sd = sd(AVAL), median = median(AVAL), min = min(AVAL), max = max(AVAL)) %>% ungroup() %>% 
  mutate(n = as.character(n_),
         mnsd = ifelse(!is.na(sd),
                       str_c(format(round(mean, digits = dg + 1), nsmall = dg + 1), " (", format(round(sd, digits = dg + 2), nsmall = dg + 2), ")"),
                       str_c(format(round(mean, digits = dg + 1), nsmall = dg + 1), " (NE)")),
         med = format(round(median, digits = dg + 1), nsmall = dg + 1),
         mm = str_c(format(round(min, digits = dg), nsmall = dg), ", ", format(round(max, digits = dg), nsmall = dg)),
         pnull = NA_character_, vnull1 = NA_character_) %>% 
  pivot_longer(c(pnull, vnull1, n, mnsd, med, mm), names_to = "stat", values_to = "value") %>% 
  pivot_wider(id_cols = c(PARAMN, PARAMCD, PARAM, AVISITN, AVISIT, stat), names_from = TRTAN, names_prefix = "dr", values_from = value) %>% 
  filter(!(AVISITN != 0 & stat == "pnull"))

chgmn <- adlb %>% filter(!is.na(CHG)) %>% 
  summarise(n_ = n(), mean = mean(CHG), sd = sd(CHG), median = median(CHG), min = min(CHG), max = max(CHG)) %>%
  mutate(n = as.character(n_),
         mnsd = ifelse(!is.na(sd),
                       str_c(format(round(mean, digits = dg + 1), nsmall = dg + 1), " (", format(round(sd, digits = dg + 2), nsmall = dg + 2), ")"),
                       str_c(format(round(mean, digits = dg + 1), nsmall = dg + 1), " (NE)")),
         med = format(round(median, digits = dg + 1), nsmall = dg + 1),
         mm = str_c(format(round(min, digits = dg), nsmall = dg), ", ", format(round(max, digits = dg), nsmall = dg)),
         vnull2 = NA_character_, AVISITN = 2 + AVISITN, AVISIT = str_c(sep = " ", AVISIT, "Change from Baseline")) %>%
  pivot_longer(c(vnull2, n, mnsd, med, mm), names_to = "stat", values_to = "value") %>%
  pivot_wider(id_cols = c(PARAMN, PARAMCD, PARAM, AVISITN, AVISIT, stat), names_from = TRTAN, names_prefix = "dr", values_from = value)

adlb1 <- adlb %>% ungroup() %>% filter(!is.na(CHG))
parn <- distinct(adlb1, PARAMN, PARAMCD, PARAM, AVISITN, AVISIT)

chglms <- data.frame()
chgdiff <- data.frame()

for (par in unique(adlb1$PARAMCD)) {
  subdata <- adlb1 %>% filter(PARAMCD == par)

  for (vis in unique(subdata$AVISIT)) {
    
  fit <- lm(CHG ~ TRTA, data = subdata)
  fit_lsm <- emmeans(fit, "TRTA")
  fit_dif <- pairs(fit_lsm, reverse = TRUE)

  t1 <- as.data.frame(fit_lsm) %>% 
    mutate(PARAMCD = par, AVISIT = vis)
  t2 <- as.data.frame(fit_dif) %>% 
    mutate(PARAMCD = par, AVISIT = vis)
  t3 <- confint(fit_dif) %>% 
    select(contrast, lower.CL, upper.CL)
  
  chglms <- bind_rows(chglms, t1)
  chgdiff <- bind_rows(chgdiff, left_join(t2, t3, by = c("contrast")))
  }
}

chglms <- left_join(chglms, parn, by = c("PARAMCD", "AVISIT")) %>% 
  mutate(AVISITN = 4 + AVISITN, TRTAN = recode(toupper(TRTA), "PLACEBO" = 1, "PTI-125 50 MG" = 2, "PTI-125 100 MG" = 3),
    lsmn = ifelse(!is.na(SE),
                  str_c(trimws(format(round(emmean, digits = 3), nsmall = 3)), " (", trimws(format(round(SE, digits = 4), nsmall = 4)), ")"),
                  str_c(trimws(format(round(emmean, digits = 3), nsmall = 3)), " (NE)")),
    ci95 = str_c(trimws(format(round(lower.CL, digits = 3), nsmall = 3)), ", ", trimws(format(round(upper.CL, digits = 3), nsmall = 3))),
  ) %>% 
  pivot_longer(c(lsmn, ci95), names_to = "stat", values_to = "value") %>%
  pivot_wider(id_cols = c(PARAMN, PARAMCD, PARAM, AVISITN, AVISIT, stat), names_from = TRTAN, names_prefix = "dr", values_from = value)

chgdiff <- left_join(chgdiff, parn, by = c("PARAMCD", "AVISIT")) %>% filter(grepl("PLACEBO", toupper(contrast))) %>% 
  mutate(AVISITN = 6 + AVISITN, TRTAN = recode(toupper(contrast), "(PTI-125 50 MG) - PLACEBO" = 2, "(PTI-125 100 MG) - PLACEBO" = 3),
         dflsmn = ifelse(!is.na(SE),
                       str_c(trimws(format(round(estimate, digits = 3), nsmall = 3)), " (", trimws(format(round(SE, digits = 4), nsmall = 4)), ")"),
                       str_c(trimws(format(round(estimate, digits = 3), nsmall = 3)), " (NE)")),
         dfci95 = str_c(trimws(format(round(lower.CL, digits = 3), nsmall = 3)), ", ", trimws(format(round(upper.CL, digits = 3), nsmall = 3))),
         pval = ifelse(p.value < 10^-3, "<0.001", trimws(format(round(p.value, digits = 3), nsmall = 3)))
  ) %>% 
  pivot_longer(c(dflsmn, dfci95, pval), names_to = "stat", values_to = "value") %>%
  pivot_wider(id_cols = c(PARAMN, PARAMCD, PARAM, AVISITN, AVISIT, stat), names_from = TRTAN, names_prefix = "dr", values_from = value)

dummy <- filter(avalmn, AVISITN != 0) %>% distinct(PARAMN, .keep_all = TRUE) %>% 
  mutate(stat = "Post-Baseline")

tab0 <- bind_rows(avalmn, chgmn, chglms, chgdiff, dummy) %>% ungroup() %>% 
  mutate(ord1 = PARAMN, ord2 = ifelse(AVISITN %% 10 != 0, AVISITN %/% 10 * 10 + 5, AVISITN), 
         ord3 = ifelse(toupper(stat) == "POST-BASELINE", AVISITN - 1, AVISITN), 
         ord4 = recode(toupper(stat), "PNULL" = 0, "POST-BASELINE" = 1, "VNULL1" = 2, "VNULL2" = 3,
                       "N" = 4, "MNSD" = 5, "MED" = 6, "MM" = 7, "LSMN" = 8, "CI95" = 9,
                       "DFLSMN" = 10, "DFCI95" = 11, "PVAL" = 12, .default = 0),
         stat = recode(toupper(stat), "N" = "    n", "MNSD" = "    Mean (SD)", "MED" = "    Median", "MM" = "    Min, Max", 
                       "LSMN" = "    LS Mean (SE)", "CI95" = "    95% CI", "DFLSMN" = "    Difference of LS Means (SE)", 
                       "DFCI95" = "    95% CI of Difference", "PVAL" = "    p-value", "POST-BASELINE" = "  Post-Baseline"),
         col1 = case_when(ord4 == 0 ~ trimws(PARAM),
                          ord4 == 2 | ord4 == 3 ~ str_c("  ", trimws(AVISIT)),
                          TRUE ~ stat),
         col2 = trimws(dr1), col3 = trimws(dr2), col4 = trimws(dr3)
  ) %>% arrange(ord1, ord2, ord3, ord4)

groups <- list(
  list(
    label = paste(sep = "|",
                  "Parameter",
                  " ",
                  "Treatment"),
    cols  = c(4, 1.5, 3),
    just = "c"
  ),
  list(
    label = paste(sep = "|",
                  "  Visit\n Statistics",
                  paste0("Placebo\n(N = ", bign[1], ")"), 
                  paste0("50 mg\n(N = ", bign[2], ")"),
                  paste0("100 mg\n(N = ", bign[3], ")")),
    cols  = c(4, 1.5, 1.5, 1.5), 
    just = c("l", "c", "c", "c")
  )
)

TLF_output(data = tab0,
           tlf_name = "t14_2_1",
           target = "dev",
           type = "table",
           n_col = 4,
           groups = groups,
           by_vars = c("ord1", "ord2"),
           bl_vars = c("ord1", "ord2", "ord3"), 
           max_rows = 19,
           cont = "Y",
           cont_vars = param,
           cont_ord = ord1,
           sort_vars = c("ord1", "ord2", "ord3", "ord4"))
