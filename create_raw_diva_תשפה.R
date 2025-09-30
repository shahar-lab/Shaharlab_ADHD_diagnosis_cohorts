library(readr)
library(dplyr)

#Note: Compensation method, and exclusion etc, are in the notion part

#### CREATE DIVA RAW ####

# load raw DIVA data
diva <- read_tsv("data/תשפה/collected_data/שאלון+מקדים+++DIVA_תשפה_May+31,+2025_11.55.tsv",
                 locale = locale(encoding = "UTF-16")) 
#View(as.data.frame(names(diva)))

diva <- diva |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  rename(subjectid = shahar_id) |>
  filter(
    (nchar(subjectid) == 6 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)) 
  ) |>
  filter(subjectid !="000000")|>
  
  # age
  rename(age = `DIVA-personalinfo_1`) |>
  
  # Cohort
  mutate (cohort = "תשפה") |>
  rename(date_recorded      = RecordedDate) |> 
  
  # community and diagnostic info
  rename(
    community_diagnosis_age = `DIVA-diagnosis2_5`,
    community_diagnosis_meds = `DIVA-meds1`,
    community_diagnosis_meds_type = `DIVA-meds2`,
    community_diagnosis_meds_dosage = `DIVA-meds3`,
    community_diagnosis_meds_freq   = `DIVA-meds4`,
    
    
    psychiatric_diagnosis = `DIVA-insurance1`,
    psychiatric_diagnosis_freetext = `DIVA-insurance1_1_TEXT`,
    psychiatric_diagnosis_meds = `DIVA-insurance2`,
    psychiatric_diagnosis_meds_type = `DIVA-insurance2_1_TEXT`
    
  ) |>
  mutate(
    psychiatric_diagnosis_meds = recode(psychiatric_diagnosis_meds, `כן` = "yes", `לא` = "no"),
  ) |>
  
   
  # declared_group
  rename(declared_group = group) |>
  mutate(declared_group = recode(declared_group, `קשב` = "ADHD", `ביקורת` = "TD")) |>
  
  # IA symptoms
  rename(
    IA1_adulthood = `A1_decision`, IA1_childhood = `A1_decision_child`,
    IA2_adulthood = `A2_decision`, IA2_childhood = `A2_decision_child`,
    IA3_adulthood = `A3_decision`, IA3_childhood = `A3_decision_child`,
    IA4_adulthood = `A4_decision`, IA4_childhood = `A4_decision_child`,
    IA5_adulthood = `A5_decision`, IA5_childhood = `A5_decision_child`,
    IA6_adulthood = `A6_decision`, IA6_childhood = `A6_decision_child`,
    IA7_adulthood = `A7_decision`, IA7_childhood = `A7_decision_child`,
    IA8_adulthood = `A8_decision`, IA8_childhood = `A8_decision_child`,
    IA9_adulthood = `A9_decision`, IA9_childhood = `A9_decision_child`
  ) |>
  
  # HI symptoms
  rename(
    HI1_adulthood = `H/I 1_decision`, HI1_childhood = `H/I 1_decision_child`,
    HI2_adulthood = `H/I 2_decision`, HI2_childhood = `H/I 2_decision_child`,
    HI3_adulthood = `H/I 3_decision`, HI3_childhood = `H/I 3_decision_child`,
    HI4_adulthood = `H/I 4_decision`, HI4_childhood = `H/I 4_decision_child`,
    HI5_adulthood = `H/I 5_decision`, HI5_childhood = `H/I 5_decision_child`,
    HI6_adulthood = `H/I 6_decision`, HI6_childhood = `H/I 6_decision_child`,
    HI7_adulthood = `H/I 7_decision`, HI7_childhood = `H/I 7_decision_child`,
    HI8_adulthood = `H/I 8_decision`, HI8_childhood = `H/I 8_decision_child`,
    HI9_adulthood = `H/I 9_decision`, HI9_childhood = `H/I 9_decision_child`
  )|>
  
  # Functional impairment
  rename(
    function_occupational_adulthood = C1_decision, function_occupational_childhood = C1_decision_child,
    function_family_adulthood       = C2_decision, function_family_childhood = C2_decision_child,
    function_social_adulthood       = C3_decision, function_social_childhood = C3_decision_child,
    function_leisure_adulthood      = C4_decision, function_leisure_childhood = C4_decision_child,
    function_selfimage_adulthood    = C5_decision, function_selfimage_childhood = C5_decision_child
  ) |>
  mutate(
    across(starts_with("function_"), ~ recode(.x, `1` = "yes", `0` = "no"))
  ) |>
  
  # DIVA summary scores and DSM criteria flags
  mutate(
    diva_IA_symptoms = as.numeric(SC0),
    diva_HI_symptoms = as.numeric(SC1),
    diva_childhood_symptoms = as.numeric(SC2),
    diva_function_adulthood = as.numeric(SC3),
    diva_function_childhood = as.numeric(SC4),
    DSM_criteria_A1 = if_else(diva_IA_symptoms >= 5, "present", "absent"),
    DSM_criteria_A2 = if_else(diva_HI_symptoms >= 5, "present", "absent"),
    DSM_criteria_B = if_else(diva_childhood_symptoms >= 3, "present", "absent"),
    DSM_criteria_C_D = if_else(diva_function_adulthood >= 2 & diva_function_childhood >= 1, "present", "absent"),
    diva_diagnosis = if_else(
      (DSM_criteria_A1 == "present" | DSM_criteria_A2 == "present") &
        DSM_criteria_B == "present" &
        DSM_criteria_C_D == "present", "meet_diva_criteria", "below_diva_criteria"),
    diva_diagnosis_type = case_when(
      diva_diagnosis == "below_diva_criteria" ~ "below_diva_criteria",  
      diva_diagnosis == "meet_diva_criteria" & DSM_criteria_A1 == "present" & DSM_criteria_A2 == "absent" ~ "primary_inattentive",
      diva_diagnosis == "meet_diva_criteria" & DSM_criteria_A1 == "absent" & DSM_criteria_A2 == "present" ~ "primary_hyperactive/impulsive",
      diva_diagnosis == "meet_diva_criteria" & DSM_criteria_A1 == "present" & DSM_criteria_A2 == "present" ~ "combined",
      TRUE ~ NA_character_
    )
  ) |>
  
  # original RA-coded diagnosis and subtype
  rename(diva_diagnosis_coded_by_RA = ADHD_decision) |>
  mutate(diva_diagnosis_coded_by_RA = recode(diva_diagnosis_coded_by_RA, `1` = "ADHD", `0` = "TD")) |>
  rename(diva_diagnosis_subtype_coded_by_RA = ADHD_subtype) |>
  
  select(
    subjectid,
    cohort,date_recorded,
    age,
    declared_group,
    
    diva_IA_symptoms,
    diva_HI_symptoms,
    diva_childhood_symptoms,
    diva_function_adulthood,
    diva_function_childhood,
    DSM_criteria_A1,
    DSM_criteria_A2,
    DSM_criteria_B,
    DSM_criteria_C_D,
    diva_diagnosis,
    diva_diagnosis_type,
    
    community_diagnosis_age,
    community_diagnosis_meds,
    psychiatric_diagnosis,
    psychiatric_diagnosis_freetext,
    psychiatric_diagnosis_meds,

    diva_diagnosis_coded_by_RA,
    diva_diagnosis_subtype_coded_by_RA,
    starts_with("IA"),
    starts_with("HI"),
    
    function_occupational_adulthood,
    function_family_adulthood,
    function_social_adulthood,
    function_leisure_adulthood,
    function_selfimage_adulthood
    
  )



#### TYPE OF COLUMNS ----
diva <- diva |>
  
  # ensure numeric for IA/HI items
  mutate(
    across(starts_with("IA"), as.numeric),
    across(starts_with("HI"), as.numeric)
  ) |>
  # ensure correct factor levels
  mutate(
    declared_group = factor(declared_group, levels = c("TD", "ADHD")),
    diva_diagnosis = factor(diva_diagnosis, levels = c("below_diva_criteria", "meet_diva_criteria")),
    diva_diagnosis_type = factor(diva_diagnosis_type, levels = c("below_diva_criteria", "primary_inattentive", "primary_hyperactive/impulsive", "combined"))
  ) 



#### VALIDATIONS ----

# 1. Examine subjectid duplicates

  dup_count <- diva |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())
  print(dup_count)
  
  #diva |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()

  # filter out a technical duplicate
  diva = diva |> filter(!(subjectid == "cL5U0l" & duplicated(subjectid)))



# 2) Validate symptom sum scores consistency
# Compute sum of individual IA/HI adulthood items
diva_validation_counts <- diva |>
  mutate(
    IA_adulthood_sum = rowSums(across(starts_with("IA") & ends_with("adulthood")), na.rm = TRUE),
    HI_adulthood_sum = rowSums(across(starts_with("HI") & ends_with("adulthood")), na.rm = TRUE),
    IA_sum_check = diva_IA_symptoms == IA_adulthood_sum,
    HI_sum_check = diva_HI_symptoms == HI_adulthood_sum
  )

diva_validation_counts <- diva_validation_counts |>
  summarise(
    IA_mismatch_count = sum(!IA_sum_check, na.rm = TRUE),
    HI_mismatch_count = sum(!HI_sum_check, na.rm = TRUE)
  )
print(diva_validation_counts)


# 3) Diagnosis labeling validation: flag mismatches between diva_diagnosis_coded_by_RA and computed diva_diagnosis
diva_validation_failures <- diva |>
  filter(
      (diva_diagnosis_coded_by_RA == "ADHD" & diva_diagnosis == "below_diva_criteria") |
        (diva_diagnosis_coded_by_RA == "TD" & diva_diagnosis == "meet_diva_criteria")
    
  ) |>
  arrange(diva_diagnosis_coded_by_RA) |>
  select(
    subjectid,
    age,
    declared_group,
    diva_diagnosis_coded_by_RA,
    diva_diagnosis,
    diva_diagnosis_type,
    diva_IA_symptoms,
    diva_HI_symptoms,
    diva_childhood_symptoms,
    diva_function_adulthood,
    diva_function_childhood,
    DSM_criteria_A1,
    DSM_criteria_A2,
    DSM_criteria_B,
    DSM_criteria_C_D
  ) |> View(title = "Symptom or Diagnosis Validation Failures")


#### QUICK DESCRIPTIVE CHECK ----

# 1) Confusion matrix: declared_group vs computed diva_diagnosis for inclusion passed
confusion_matrix <- diva |>
  count(declared_group, diva_diagnosis) |>
  tidyr::pivot_wider(names_from = diva_diagnosis, values_from = n, values_fill = 0)

print(confusion_matrix)

# 2) Summary table: average IA and HI adulthood sums by diva_diagnosis_type
diagnosis_type_summary <- diva |>
  group_by(diva_diagnosis_type) |>
  summarise(
    avg_IA_adulthood_sum = mean(diva_IA_symptoms, na.rm = TRUE),
    avg_HI_adulthood_sum = mean(diva_HI_symptoms, na.rm = TRUE),
    n(),
    .groups = 'drop'
  )

print(diagnosis_type_summary)

#### SAVE ----
save(diva, file = 'data/תשפה/raw_data/diva.Rdata')
