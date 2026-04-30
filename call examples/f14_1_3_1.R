rm(list = ls())
source("C:/Users/User/Desktop/sas/R_final/setup/setup.R")
######################################################################

adlb <- filter(adam$adlb, PARAMCD == "CGLUC" & SAFFL == "Y" & ANL01FL == "Y" & !is.na(CHG)) %>% 
  mutate(AGEGRP = ifelse(AGE <= 60, "<= 60",
                         ifelse(AGE <= 70, "<= 70",
                                ifelse(AGE > 70, "> 70", NA_character_))),
         RACEGRP = str_to_title(trimws(scan_r(ARACE, 1, " ")))
         )

adsl <- filter(adam$adsl, SAFFL == "Y") %>% 
  mutate(AGEGRP = ifelse(AGE <= 60, "<= 60",
                         ifelse(AGE <= 70, "<= 70",
                                ifelse(AGE > 70, "> 70", NA_character_))),
         RACEGRP = str_to_title(trimws(scan_r(ARACE, 1, " ")))
         )

glm_model <- function(dt, group) {
  est <- data.frame()
  prc <- data.frame()
  
  blank <- data.frame(stat = group,
                      statn = 1:length(group))
  
  for (grp in group) {
    
    formula_obj <- reformulate(grp, response = "CHG")
    model <- glm(formula_obj, data = dt)
    
    t1 <- as.data.frame(emmeans(model, grp)) %>% 
      mutate(stat = grp) %>% rename("val" = grp)
    est <- bind_rows(est, t1)
    
    t2 <- tabyl(adsl, !!sym(grp)) %>% 
      mutate(stat = grp) %>% rename("val" = grp)
    prc <- bind_rows(prc, t2)
  }
  
  mcr <- left_join(est, prc, by = c("stat", "val")) %>% 
    bind_rows(., select(blank, stat)) %>% left_join(., blank, by = "stat")
  return(mcr)
}

blank <- data.frame(col1 = "Subgroup", statn = 0,
                    col2 = "N (%)", col3 = "Estimate (95% CI)")

tab <- glm_model(adlb, c("ASEX", "AGEGRP")) %>% 
  mutate(valn = recode(toupper(val), "<= 60" = 1, "<= 70" = 2, "> 70" = 3,
                       "FEMALE" = 1, "MALE" = 2,
                       "ASIAN" = 1, "BLACK" = 2, "WHITE" = 3,
                       .missing = 0),
         stat = recode(stat,
                       "ASEX" = "Sex",
                       "RACEGRP" = "Race",
                       "AGEGRP" = "Age"),
         col1 = ifelse(valn != 0, str_c("  ", val), stat),
         col2 = ifelse(valn != 0, str_c(n, " (", round(percent*100, digits = 1), "%)"), NA_character_),
         col3 = ifelse(valn != 0, str_c(round(emmean, digits = 2), " (", 
                                        round(lower.CL, digits = 2), ", ",
                                        round(upper.CL, digits = 2), ")"), NA_character_)
         ) %>% bind_rows(., blank) %>% arrange(statn, valn)

lvl <- distinct(tab, statn, valn, col1) %>% pull(col1)
tab$col1 <- factor(tab$col1, levels = lvl)

p <- ggplot(tab, aes(y = fct_rev(col1))) + theme_classic() +
  geom_point(aes(x=emmean), shape=15, size=3, na.rm = T) +
  geom_linerange(aes(xmin=lower.CL, xmax=upper.CL), na.rm = T) +
  geom_vline(xintercept = 0, linetype="dashed") +
  labs(x="Estimate (95% CI)", title="Glucose")+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

p_left <- ggplot(tab, aes(y = fct_rev(col1)))+
  geom_text(aes(x=0, label=col1), hjust = 0, na.rm = T) +
  geom_text(aes(x=1, label=col2), hjust = 0, na.rm = T) +
  theme_void() + 
  coord_cartesian(xlim=c(0,2))

p_rigth <- ggplot(tab, aes(y = fct_rev(col1)))+
  geom_text(aes(x=0, label=col3), hjust = 0, na.rm = T) +
  theme_void() + 
  scale_x_continuous(expand = expansion(mult = c(0, 1)))

fig <- p_left + p + p_rigth
fig

temp_path <- tempfile(fileext = ".png")

ggsave(
  filename = temp_path,
  plot     = fig,
  width    = 8,
  height   = 5,
  dpi      = 400
)

F_output(data = tab,
         fig_vector = temp_path,
         tlf_name = "f14_1_3_1",
         target = "dev",
         n_col = 3,
         sort_vars = c("statn", "valn")
)
