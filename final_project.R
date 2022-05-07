library(tidyverse)
library(gtsummary)
library(haven)

#Load data
bfi <- read_sas("~/biostat_512/bfi_data.sas7bdat")

#Create table 1
tbl1 <- bfi%>%
  filter(week == 1)%>%
  select(group, BFI, age, White, Hispanic, BMI, BPI_Sev,
         BPI_Int, vas, comorbidities, YrsFromDx, menopause, stage)%>%
  tbl_summary(by = group,
              type = list(comorbidities ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "(Missing)")%>%
  add_overall()%>%
  add_p() %>% 
  modify_caption("**Table 1. Baseline Participant Characteristics Across Treatment Groups**") %>%
  modify_header( label = '**Variable**',
                 stat_1 = '**Relaxation Acupressure**, N = 80',
                 stat_2 = '**Stimulating Acupressure**, N = 79',
                 stat_3 = '**Usual Care**, N = 77')%>%
  bold_labels()%>%
  as_gt() %>%            
  gt::gtsave(           
    filename = "bfi_table1.png", 
  )