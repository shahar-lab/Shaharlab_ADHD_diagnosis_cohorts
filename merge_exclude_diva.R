# THIS FILE EXLUCDE BASED ON DRUGS AND DIVA MISMATCH.
# THEN COMBINES THE SUMSCORES

library(dplyr)


#### MERGE DIVA and HP1  ----

load('data/all_cohorts_raw_data/hp1.Rdata')
load('data/all_cohorts_raw_data/diva.Rdata')


diva = hp1 |> select(-cohort) |>
  left_join(diva, by = "subjectid") 


save(diva, file = 'data/all_cohorts_raw_data/diva_before_exclusion.Rdata')
write.csv(diva , file =  'data/all_cohorts_raw_data/diva_before_exclusions.csv')

#### EXCLUDE FOR DRUGS ----

#count how many above drug cutoff
diva |> 
  summarise(
    above_alcohol = sum(alcohol_use_cutoff  == "above_audit_cutoff", na.rm = TRUE),
    above_cannabis = sum(cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE),
    above_both     = sum(alcohol_use_cutoff  == "above_audit_cutoff" & 
                           cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE)
  )
#> A tibble: 1 × 3
#> above_alcohol above_cannabis above_both
#>      10             22          1

#exclude these
diva = diva |> 
  filter(alcohol_use_cutoff  != "above_audit_cutoff",
         cannabis_use_cutoff != "above_cudit_cutofff")


#### EXCLUDE FOR DIAGNOSIS MISMATCH ----
library(yardstick)
conf_mat(diva, truth = declared_group, estimate = diva_group)

#>           Truth
#> Prediction  TD ADHD
#> TD   157   28
#> ADHD   7  129


#exclude mismatch
diva <- diva |> 
  filter(declared_group == diva_group)

save(diva, file = 'data/all_cohorts_raw_data/diva_after_exclusion.Rdata')

