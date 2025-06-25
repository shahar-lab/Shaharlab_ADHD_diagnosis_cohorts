# THIS FILE ADD SUMSCORES TO THE DIVA AFTER DRUG AND DIAGNOSIS MISMATCH EXCLUSION
library(dplyr)

#### ADD SUMSCORE ----

load('data/all_cohorts_raw_data/icar.Rdata')
load('data/all_cohorts_raw_data/asrs.Rdata')

load('data/all_cohorts_raw_data/diva_after_exclusion.Rdata')

df = diva |>
  left_join(icar |> select(subjectid, icar), by = "subjectid") |>
  left_join(asrs |> select(subjectid, asrs,asrs_ia,asrs_hi), by = "subjectid") 



write.csv(df , file =  'data/all_cohorts_raw_data/diva_after_exclusions_with_sumscores.csv')

