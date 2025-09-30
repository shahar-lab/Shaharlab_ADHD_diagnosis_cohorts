# THIS FILE ADD SUMSCORES TO THE DIVA AFTER DRUG AND DIAGNOSIS MISMATCH EXCLUSION
library(dplyr)

load('data/all_cohorts_raw_data/diva_after_exclusions_with_sumscores.Rdata')
load('data/all_cohorts_raw_data/patas.Rdata')


df = df |>
  left_join(patas, by = "subjectid") 



df = df |> filter(diva_group == "ADHD" & is.na(patas)==F)

write.csv(df |> filter(diva_group == "ADHD"), file =  'data/all_cohorts_raw_data/patas_filtered_data.csv')

write.csv(df|>filter(cohort == "תשפה", diva_group == "ADHD") , file =  'data/all_cohorts_raw_data/patas_filtered_data_תשפה.csv')
write.csv(df|>filter(cohort == "תשפד", diva_group == "ADHD") , file =  'data/all_cohorts_raw_data/patas_filtered_data_תשפד.csv')
