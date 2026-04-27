# THIS FILE ADD SUMSCORES TO THE DIVA AFTER DRUG AND DIAGNOSIS MISMATCH EXCLUSION
library(dplyr)

#### ADDING PATAS ----

# loading DIVA for תשפג תשפד תשפה
load('data/raw_data/diva_after_exclusions_with_sumscores.Rdata')
df = df |> filter(diva_group == "ADHD")



# first adding תשפד תשפה
load('data/raw_data/patas.Rdata')

df = df |>
  left_join(patas, by = "subjectid") 

rm(patas)


df = df |> filter(!(is.na(patas)))

write.csv(df , file =  'data/raw_data/patas_analysis_data.csv')

#### TAL תשפג DATA ---
# now adding tal's data that is mostly for תשפג

tal = read.csv("df_collected_tal_erdinast.csv")
tal <- tal |> 
        filter( Group == "ADHD") |>
        rename(subjectid = shaharID) |>
        select(subjectid, PATAS_baseline, PATAS_followup)


df <- df |> 
  left_join(tal, by = "subjectid")


df = df |> filter(!(is.na(patas) & is.na(PATAS_followup)))
df$new_patas = coalesce(as.numeric(df$patas), 0) + coalesce(as.numeric(df$PATAS_followup), 0)



