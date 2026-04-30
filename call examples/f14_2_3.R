rm(list = ls())
source("C:/Users/User/Desktop/sas/R_final/setup/setup.R")
######################################################################

adlb <- filter(adam$adlb, FASFL == "Y" & PARCAT1 == "CHEMISTRY" & ANL01FL == "Y" & !is.na(AVAL) &
                 PARAMCD %in% c("AST", "ALT")) %>% 
  pivot_wider(id_cols = c(USUBJID, TRTAN, TRTA, AVISITN, AVISIT),
              names_from = PARAMCD,
              values_from = AVAL) %>% 
  mutate(col1=TRTAN, col2=TRTA, col3=USUBJID, col4=ALT, col5=AST)

adlb_split <- split(adlb, adlb$AVISITN)

make_adlb_plot <- function(df, suffix = "NA") {
  
  ggplot(df, aes(x=AST, y=ALT, group = TRTA, colour = TRTA)) +
    geom_point() +
    geom_smooth(method = lm, se = TRUE) +
    labs(title=paste0("Analysis Visit: ", suffix),
         x="Aspartate Aminotransferase",
         y="Alanine Aminotransferase") +
    theme_light()
}

temp_path <- vector("list", length(adlb_split))
names(temp_path) <- names(adlb_split)

for (i in seq_along(adlb_split)) {
  dti <- adlb_split[[i]]
  
  frtf <- make_adlb_plot(dti, unique(dti$AVISIT))
  print(frtf)
  
  temp_path[[i]] <- tempfile(fileext = ".png")
  
  ggsave(
    filename = temp_path[[i]],
    plot     = frtf,
    width    = 8,
    height   = 5,
    dpi      = 400)
}

F_output(data = adlb,
         fig_vector = temp_path,
         tlf_name = "f14_2_3",
         target = "dev",
         n_col = 5,
         sort_vars = c("col1", "col3")
)