library(dplyr)
library(tidyr)
library(readr)

if (basename(getwd()) == "merge_multi_measures") setwd("..")

load("data/all_cohorts_raw_data/df_agg.Rdata")

#### 1. Drop due to high CUDIT, high AUDIT, or both ----
n_above_audit  <- sum(df_agg$alcohol_use_cutoff == "above_audit_cutoff", na.rm = TRUE)
n_above_cudit  <- sum(df_agg$cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE)
n_above_both   <- sum(df_agg$alcohol_use_cutoff == "above_audit_cutoff" &
                      df_agg$cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE)

message("Should drop due to drugs:")
message("  high AUDIT:  ", n_above_audit)
message("  high CUDIT:  ", n_above_cudit)
message("  both:       ", n_above_both)

df_agg <- df_agg |>
  filter(alcohol_use_cutoff != "above_audit_cutoff", cannabis_use_cutoff != "above_cudit_cutoff")

n_na_diva <- sum(is.na(df_agg$diva_group))
if (n_na_diva > 0) {
  na_diva_rows <- df_agg |> filter(is.na(diva_group))
  View(na_diva_rows, title = "NA diva_group after CUDIT/AUDIT exclusion")
  stop("After drug exclusion, diva_group should have no NA. Found ", n_na_diva, " rows. See View() above.")
}

#### 3. How many failed DIVA (TD above cutoff / ADHD below cutoff) ----
n_fail_td   <- sum(df_agg$declared_group == "TD" & df_agg$diva_group == "ADHD", na.rm = TRUE)
n_fail_adhd <- sum(df_agg$declared_group == "ADHD" & df_agg$diva_group == "TD", na.rm = TRUE)
n_fail_diva <- n_fail_td + n_fail_adhd
message("Failed DIVA: ", n_fail_diva, " (TD→ADHD: ", n_fail_td, ", ADHD→TD: ", n_fail_adhd, ")")

#### 4. ADHD vs TD table (all have diva_group; no NA expected) ----
df_c <- df_agg |> mutate(cohort = factor(cohort, levels = c("תשפג", "תשפד", "תשפה", "תשפו")))

tbl <- df_c |> count(cohort, diva_group, .drop = FALSE) |>
  pivot_wider(names_from = diva_group, values_from = n, values_fill = 0)

message("ADHD vs TD (after drug exclusion):")
print(tbl)

#### 5. Save df: full items and sum scores only ----
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

df_c_sum <- df_c |> select(any_of(sum_cols))
library(writexl)
write_xlsx(df_c, path = "data/all_cohorts_raw_data/df_c_all_items.xlsx")
write_xlsx(df_c_sum, path = "data/all_cohorts_raw_data/df_c_sum_scores.xlsx")

