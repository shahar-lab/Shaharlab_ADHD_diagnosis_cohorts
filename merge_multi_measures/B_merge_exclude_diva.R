# THIS FILE EXLUCDE BASED ON DRUGS AND DIVA MISMATCH.
# THEN COMBINES THE SUMSCORES

library(dplyr)


#### MERGE DIVA and HP1  ----

load('data/all_cohorts_raw_data/hp1.Rdata')
load('data/all_cohorts_raw_data/diva.Rdata')


diva = hp1  |>
  full_join(diva |> select(-cohort), by = "subjectid") 


save(diva, file = 'data/all_cohorts_raw_data/diva_before_exclusion.Rdata')
write.csv(diva , file =  'data/all_cohorts_raw_data/diva_before_exclusions.csv')

#### EXCLUDE FOR DRUGS ----

#count how many above drug cutoff
diva |> 
  #group_by(cohort) |>
  summarise(
    above_alcohol  = sum(alcohol_use_cutoff  == "above_audit_cutoff", na.rm = TRUE),
    above_cannabis = sum(cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE),
    above_both     = sum(alcohol_use_cutoff  == "above_audit_cutoff" & 
                           cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE)
  )

# A tibble: 1 × 3
#> above_alcohol above_cannabis above_both
#> <int>          <int>      <int>
#> 1            12             29          2

#exclude these
diva = diva |> 
  filter(alcohol_use_cutoff  != "above_audit_cutoff",
         cannabis_use_cutoff != "above_cudit_cutoff")


#### EXCLUDE FOR DIAGNOSIS MISMATCH ----
#diva |> 
#  filter(is.na(declared_group)) |> 
#  View()

diva |> group_by(declared_group, diva_group) |> summarise(n = n())

#>  declared_group diva_group     n
#> <fct>          <fct>      <int>
#> 1 TD             TD           206
#>2 TD             ADHD          11
#>3 ADHD           TD            31
#>4 ADHD           ADHD         166
#>5 NA             NA            45
#>
#>5.07%  error for Ss that declared TD and turned out ADHD
#>15.73% error for Ss that declared TD and turned out ADHD
#>
#>the NA are those who droped out, mostly due to no need in course credit points.


#exclude mismatch
diva <- diva |> 
  filter(declared_group == diva_group)

save(diva, file = 'data/all_cohorts_raw_data/diva_after_exclusion.Rdata')

