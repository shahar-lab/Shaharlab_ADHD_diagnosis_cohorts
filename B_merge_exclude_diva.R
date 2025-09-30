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
diva |> group_by(declared_group) |>
  summarise(
    above_alcohol  = sum(alcohol_use_cutoff  == "above_audit_cutoff", na.rm = TRUE),
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
         cannabis_use_cutoff != "above_cudit_cutoff")


#### EXCLUDE FOR DIAGNOSIS MISMATCH ----
#diva |> 
#  filter(is.na(declared_group)) |> 
#  View()

library(yardstick)

diva |> group_by(declared_group, diva_group) |> summarise(n = n())

#>  declared_group diva_group     n
#> <fct>          <fct>      <int>
#> 1 TD             TD           154
#>2 TD             ADHD          10
#>3 ADHD           TD            22
#>4 ADHD           ADHD         136
#>5 NA             NA            45
#>
#>6.09%  error for Ss that declared TD and turned out ADHD
#>13.92% error for Ss that declared TD and turned out ADHD
#>
#>the NA are thoese how droped out, mostly due to no need in course credit points.


#exclude mismatch
diva <- diva |> 
  filter(declared_group == diva_group)

save(diva, file = 'data/all_cohorts_raw_data/diva_after_exclusion.Rdata')

