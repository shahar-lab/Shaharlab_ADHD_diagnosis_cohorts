# Merge pipeline for Tal/Erdi creativity analyses.
# Run with working directory set to the repository root.

library(dplyr)

#### HP1 ----
load("data/raw_data/hp1.Rdata")

extra_hp1_subjectids <- c(
  "QPKs1S", "lgAYMdeU", "onRQQXIH", "V25QLT4f", "EVhG6aHF",
  "pv54XYs0", "uT2p5Kxu", "od8GVpsB", "YyLeol", "skLjvwiT",
  "xPZUwWmQ", "eC06dywT", "sFcoQxyJ", "fzgsNA", "95xyap",
  "kPjb3e", "RRMb53"
) # These are subjects that were excluded from the hp1 data because they were tahpad but completed the creativity task

hp1 <- hp1 |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  filter(
    cohort %in% c("תשפו", "תשפה") | subjectid %in% extra_hp1_subjectids
  ) |>
  select(subjectid, cohort, age, gender,education, place_of_residence_until12yo,  
        EHI_hand_dominance, EHI_score, 
        alcohol_use_cutoff, cannabis_use_cutoff,  AUDIT, CUDIT_USE, CUDIT)

dim(hp1)
# 246 subjects

#### ADD NOTION ----
load("data/raw_data/notion.Rdata")

notion <- notion |>
  filter(!is.na(declared_group)) |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  distinct(subjectid, .keep_all = TRUE)

dim(notion)

df <- hp1 |>
  left_join(notion, by = "subjectid", relationship = "many-to-one")

dim(df)
#246 subjects

#### EXCLUDE FOR DRUGS ----
df <- df |>
  filter(alcohol_use_cutoff != "above_audit_cutoff",
         cannabis_use_cutoff != "above_cudit_cutoff")
dim(df)
# drug exclusion reduced to 232 subjects

#### EXCLUDE FOR DIAGNOSIS MISMATCH ----
load("data/raw_data/diva.Rdata")
diva <- diva |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  distinct(subjectid, .keep_all = TRUE) |>
  mutate(
    diva_group = case_when(
      as.character(diva_diagnosis) == "meet_diva_criteria"  ~ "ADHD",
      as.character(diva_diagnosis) == "below_diva_criteria" ~ "TD",
      TRUE ~ NA_character_
    )
  ) |>
  select(subjectid, diva_group, diva_IA_symptoms, diva_HI_symptoms, 
        psychiatric_diagnosis, psychiatric_diagnosis_freetext, psychiatric_diagnosis_meds,
        community_diagnosis_age, community_diagnosis_meds, community_diagnosis_meds_type,
        community_diagnosis_meds_dosage, community_diagnosis_meds_freq)
dim(diva)

df <- df |>
  left_join(diva, by = "subjectid", relationship = "many-to-one")
dim(df)

cat("\nConfusion matrix (rows = declared_group, cols = diva_group):\n")
print(
  table(
    declared = as.character(df$declared_group),
    diva = as.character(df$diva_group),
    useNA = "ifany"
  )
)
#         diva
# declared ADHD TD <NA>
#     ADHD   93 15   10
#     TD      4 94   15
#     <NA>    0  0    1
df <- df |>
  filter(
    !is.na(declared_group),
    !is.na(diva_group),
    as.character(declared_group) == as.character(diva_group)
  )
dim(df)
# 187 subjects remaining

#### ADD PSYCHPATHOLOGY SCORES ----
load("data/raw_data/asrs.Rdata")
asrs <- asrs |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, asrs, asrs_ia, asrs_hi, asrs_ia_count, asrs_hi_count) |>
  distinct(subjectid, .keep_all = TRUE)

load("data/raw_data/wurs.Rdata")
wurs <- wurs |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, wurs) |>
  distinct(subjectid, .keep_all = TRUE)

load("data/raw_data/bdi.Rdata")
bdi <- bdi |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, bdi) |>
  distinct(subjectid, .keep_all = TRUE)

load("data/raw_data/stai.Rdata")
stai <- stai |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, stai, stai_state, stai_trait) |>
  distinct(subjectid, .keep_all = TRUE)
    
load("data/raw_data/aq.Rdata")
aq <- aq |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, aq_sum,     social_comm,atten_switching,
    atten_to_details,communication,imagination) |>
  distinct(subjectid, .keep_all = TRUE)

load("data/raw_data/icar.Rdata")
icar <- icar |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, icar) |>
  distinct(subjectid, .keep_all = TRUE)

load("data/raw_data/ocir.Rdata")
ocir <- ocir |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, ocir) |>
  distinct(subjectid, .keep_all = TRUE)

load("data/raw_data/pqb.Rdata")
pqb <- pqb |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, pqb) |>
  distinct(subjectid, .keep_all = TRUE)

load("data/raw_data/patas.Rdata")
patas <- patas |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  select(subjectid, patas_sum) |>
  distinct(subjectid, .keep_all = TRUE)

df <- df |>
  left_join(asrs, by = "subjectid", relationship = "many-to-one") |>
  left_join(wurs, by = "subjectid", relationship = "many-to-one") |>
  left_join(bdi, by = "subjectid", relationship = "many-to-one") |>
  left_join(stai, by = "subjectid", relationship = "many-to-one") |>
  left_join(aq, by = "subjectid", relationship = "many-to-one") |>
  left_join(icar, by = "subjectid", relationship = "many-to-one") |>
  left_join(ocir, by = "subjectid", relationship = "many-to-one") |>
  left_join(pqb, by = "subjectid", relationship = "many-to-one") |>
  left_join(patas, by = "subjectid", relationship = "many-to-one")

arabic_diva_ids <- c(
  "V3EJt8", "wQP4Rw", "EY5GTz", "9JS4rf", "957H63",
  "yDKkeH", "kPjb3e", "DnqxaP", "KdS4cj"
)
df <- df |>
  mutate(
    diva_language = if_else(subjectid %in% arabic_diva_ids, "arabic", "hebrew")
  )


# #### TAKE OUT SUBJECTS WITH NA QUESTIONNAIRES IN ANY OF THE SCORES ----
# df <- df |>
#   filter(
#     !is.na(asrs)
#   )
# dim(df)
# # 175 subjects remaining
# View(df)

#### RESTRICT TO SHAHAR ID LIST (TAL / ERDI) ----
# Keep every subjectid in shahar_id_for_tal_erdinast_creativity.csv; left_join df so
# IDs in the list but not in df remain (NA for df columns). Drop subjects only in df.
ids_tal <- read.csv("data/shahar_id_for_tal_erdinast_creativity.csv", stringsAsFactors = FALSE) |>
  mutate(subjectid = trimws(as.character(subjectid))) |>
  distinct(subjectid, .keep_all = TRUE)

df <- ids_tal |>
  left_join(df, by = "subjectid") |>
  mutate(
    diva_language = if_else(subjectid %in% arabic_diva_ids, "arabic", "hebrew")
  )
dim(df)
View(df)
#### SAVE ----

save(df, file = "data/export_sets/df_for_creativity.Rdata")
write.csv(df, file = "data/export_sets/df_for_creativity.csv")
