rm(list = ls())
source("C:/Users/User/Desktop/sas/R_final/setup/setup.R")
######################################################################

adlb <- filter(adam$adlb, FASFL == "Y" & PARCAT1 == "CHEMISTRY" & PARAMCD == "CREAT" &
                 ANL01FL == "Y" & !is.na(PCHG) & PCHG != 0) %>% 
  mutate(PCHG = round(PCHG, digits = 1), col1=TRTAN, col2=TRTA, col3=USUBJID, col4=PCHG)

trt_lvl <- distinct(adlb, TRTAN, TRTA) %>% arrange(TRTAN) %>% pull(TRTA)
adlb$TRTA_ <- factor(adlb$TRTA, levels = trt_lvl)

dt <- split(adlb, adlb$AVISITN)

watherfall <- function(df, suffix = "NA") {
  
  ggplot(df, aes(x=reorder(SUBJID, -PCHG), y=PCHG, fill=TRTA_)) + 
    geom_bar(stat = "identity") + 
    labs(x="Subject Identifier", y="Percent Change from Baseline (%)", 
         title=paste0("Analysis Visit: ", suffix), fill = "Treatment:") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom")
}

temp_path <- vector("list", length(dt))
names(temp_path) <- names(dt)

for (i in seq_along(dt)) {
  dti <- dt[[i]]
  
  frtf <- watherfall(dti, suffix = unique(dti$AVISIT))
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
         tlf_name = "f14_3_3_1_1",
         target = "dev",
         n_col = 4,
         sort_vars = c("col1", "col4")
)