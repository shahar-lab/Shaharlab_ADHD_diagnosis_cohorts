# B: Load df_agg, drop subjectid duplicates, create sum-scores version

library(dplyr)
library(tidyr)

if (basename(getwd()) == "merge_multi_measures") setwd("..")

load("data/all_cohorts_raw_data/df_agg.Rdata")

#### ADHD vs TD by cohort ----
if ("diva_group" %in% names(df_agg)) {
  # Ensure cohort includes תשפו (RecordedDate after 1-Sep-2025 from תשפה)
  df_agg <- df_agg |> mutate(cohort = factor(cohort, levels = c("תשפג", "תשפד", "תשפה", "תשפו")))
  tbl <- df_agg |> count(cohort, diva_group, .drop = FALSE) |>
    pivot_wider(names_from = diva_group, values_from = n, values_fill = 0)
  print(tbl)
}

#### Sum scores only ----
sum_cols <- c(
  "subjectid", "cohort", "date_recorded",
  "age", "gender", "EHI_hand_dominance", "EHI", "AUDIT", "CUDIT",
  "alcohol_use_cutoff", "cannabis_use_cutoff", "education",
  "place_of_residence_until12yo", "vision_correction", "primary_lang",
  "diva_group", "diva_diagnosis", "diva_diagnosis_type",
  "diva_IA_symptoms", "diva_HI_symptoms", "diva_childhood_symptoms",
  "diva_function_adulthood", "diva_function_childhood",
  "DSM_criteria_A1", "DSM_criteria_A2", "DSM_criteria_B", "DSM_criteria_C_D",
  "declared_group", "exclusion_criteria",
  "asrs", "asrs_ia", "asrs_hi", "asrs_ia_count", "asrs_hi_count",
  "wurs", "bdi", "stai_state", "stai_trait", "stai", "aq", "icar",
  "ocir", "ocir_hoarding", "ocir_checking", "ocir_ordering",
  "ocir_neutralizing", "ocir_washing", "ocir_obsessing",
  "patas", "pqb", "pqb_distress"
)

df_agg_sum <- df_agg |> select(any_of(sum_cols))

save(df_agg, file = "data/all_cohorts_raw_data/df_agg.Rdata")
save(df_agg_sum, file = "data/all_cohorts_raw_data/df_agg_sum_scores.Rdata")
