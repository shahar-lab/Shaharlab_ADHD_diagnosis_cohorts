# B: Load df_agg, apply exclusions, save filtered aggregate to data/filtered_data/
rm(list=ls())
library(dplyr)
library(writexl)

if (basename(getwd()) == "merge_multi_measures") setwd("..")

#### Load data ----
load("data/raw_data/df_agg.Rdata")
message("Loaded df_agg: N = ", nrow(df_agg))

#### Remove participants with missing gender ----
n_before <- nrow(df_agg)
df_agg <- df_agg |> filter(!is.na(gender))
message("\nRemoved ", n_before - nrow(df_agg), " participant(s) with missing gender. N = ", nrow(df_agg))

#### Remove participants with undeclared group ----
n_before <- nrow(df_agg)
df_agg <- df_agg |> filter(group_declared %in% c("ADHD", "TD"))
message(
  "\nRemoved ", n_before - nrow(df_agg),
  " participant(s) with group_declared not in {ADHD, TD}. N = ", nrow(df_agg)
)

message("\nParticipants per declared group:")
print(table(df_agg$group_declared, useNA = "always"))

#### Save filtered aggregate ----

save(df_agg, file = "data/export_sets/df_agg_without_gender_and_undeclared_group.Rdata")
write_xlsx(df_agg, "data/export_sets/df_agg_without_gender_and_undeclared_group.xlsx")

message("\nSaved filtered aggregate to data/export_sets/df_agg_without_gender_and_undeclared_group (.Rdata + .xlsx). N = ", nrow(df_agg))
