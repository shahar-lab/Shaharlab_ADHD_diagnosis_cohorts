# THIS FILE ADD SUMSCORES TO THE DIVA AFTER DRUG AND DIAGNOSIS MISMATCH EXCLUSION
library(dplyr)

#### ADD SUMSCORE ----
load('data/all_cohorts_raw_data/diva_after_exclusion.Rdata')

load('data/all_cohorts_raw_data/asrs.Rdata')
load('data/all_cohorts_raw_data/wurs.Rdata')
load('data/all_cohorts_raw_data/icar.Rdata')
load('data/all_cohorts_raw_data/bdi.Rdata')
load('data/all_cohorts_raw_data/stai.Rdata')
load('data/all_cohorts_raw_data/ocir.Rdata')

load('data/all_cohorts_raw_data/aq.Rdata')
load('data/all_cohorts_raw_data/patas.Rdata')


#duplicates
#diva |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()

df = diva |>
  left_join(asrs |> select(subjectid, asrs,asrs_ia,asrs_hi, asrs_ia_count, asrs_hi_count), by = "subjectid") |>
  left_join(wurs |> select(subjectid, wurs), by = "subjectid") |>
  
  left_join(icar |> select(subjectid, icar), by = "subjectid") |>
  left_join(bdi  |> select(subjectid, bdi), by = "subjectid") |>
  left_join(stai  |> select(subjectid,stai, stai_state, stai_trait ), by = "subjectid") |>
  left_join(ocir  |> select(subjectid,ocir ), by = "subjectid") |>
  
  left_join(aq  |> select(subjectid, aq), by = "subjectid") 


write.csv(df , file =  'data/all_cohorts_raw_data/diva_after_exclusions_with_sumscores.csv')
save(df , file =  'data/all_cohorts_raw_data/diva_after_exclusions_with_sumscores.Rdata')

