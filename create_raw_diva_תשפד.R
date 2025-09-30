library(readr)
library(dplyr)


#### CREATE DIVA RAW ####

# load raw DIVA data
diva <- read_tsv("data/תשפד/collected_data/שאלון+מקדים+++DIVA_May+30,+2025_08.59.tsv",
                 locale = locale(encoding = "UTF-16")) 
diva <- diva |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  filter(
    nchar(subjectid) == 8 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)
  ) |>
  
  # age
  rename(age = `DIVA-personalinfo_1`) |>
  
  # Cohort
  mutate (cohort = "תשפד") |>
  rename(date_recorded      = RecordedDate) |> 
  
  # compensation_method
  rename(compensation_method = `DIVA-personalinfo_5`) |>
  mutate(compensation_method = recode(compensation_method, `קרדיט` = "course_credit", `תשלום` = "monetray")) |>
  
  # community and diagnostic info
  rename(
    community_diagnosis_age = `DIVA-diagnosis2_5`,
    community_diagnosis_meds = `DIVA-meds1`,
    psychiatric_diagnosis = `DIVA-insurance1`,
    psychiatric_diagnosis_freetext = `DIVA-insurance1_1_TEXT`,
    psychiatric_diagnosis_meds = `DIVA-insurance2`,
    psychotic_diagnosis = `DIVA-insurance4`,
    neurological_diagnosis = `DIVA-insurance5`
  ) |>
  mutate(
    psychiatric_diagnosis = recode(psychiatric_diagnosis, `כן` = "yes", `לא` = "no"),
    psychiatric_diagnosis_meds = recode(psychiatric_diagnosis_meds, `כן` = "yes", `לא` = "no"),
    psychotic_diagnosis = recode(psychotic_diagnosis, `כן` = "yes", `לא` = "no"),
    neurological_diagnosis = recode(neurological_diagnosis, `כן` = "yes", `לא` = "no")
  ) |>
  
  # exclusion criteria
  rename(exclusion_criteria = `DIVA-insurance8`) |>
  mutate(exclusion_criteria = recode(exclusion_criteria, `כן` = "pass", `לא` = "fail")) |>
  
  # declared_group
  rename(declared_group = group) |>
  mutate(declared_group = recode(declared_group, `קשב` = "ADHD", `ביקורת` = "TD")) |>
  
  # IA symptoms
  rename(
    IA1_adulthood = `A1.3`, IA1_childhood = `A1.5`,
    IA2_adulthood = `A2.3`, IA2_childhood = `A2.5`,
    IA3_adulthood = `A3.3`, IA3_childhood = `A3.5`,
    IA4_adulthood = `A4.3`, IA4_childhood = `A4.5`,
    IA5_adulthood = `A5.3`, IA5_childhood = `A5.5`,
    IA6_adulthood = `A6.3`, IA6_childhood = `A6.5`,
    IA7_adulthood = `A7.3`, IA7_childhood = `A7.5`,
    IA8_adulthood = `A8.3`, IA8_childhood = `A8.5`,
    IA9_adulthood = `A9.3`, IA9_childhood = `A9.5`
  ) |>
  
  # HI symptoms
  rename(
    HI1_adulthood = `H/I 1.3`, HI1_childhood = `H/I 1.5`,
    HI2_adulthood = `H/I 2.3`, HI2_childhood = `H/I 2.5`,
    HI3_adulthood = `H/I 3.3`, HI3_childhood = `H/I 3.5`,
    HI4_adulthood = `H/I 4.3`, HI4_childhood = `H/I 4.5`,
    HI5_adulthood = `H/I 5.3`, HI5_childhood = `H/I 5.5`,
    HI6_adulthood = `H/I 6.3`, HI6_childhood = `H/I 6.5`,
    HI7_adulthood = `H/I 7.3`, HI7_childhood = `H/I 7.5`,
    HI8_adulthood = `H/I 8.3`, HI8_childhood = `H/I 8.5`,
    HI9_adulthood = `H/I 9.3`, HI9_childhood = `H/I 9.5`
  ) |>
  
  # Functional impairment
  rename(
    function_occupational_adulthood = `C1.2`, function_occupational_childhood = `C1.3`,
    function_family_adulthood = `C2.2`, function_family_childhood = `C2.3`,
    function_social_adulthood = `C3.2`, function_social_childhood = `C3.3`,
    function_leisure_adulthood = `C4.2`, function_leisure_childhood = `C4.3`,
    function_selfimage_adulthood = `C5.2...182`, function_selfimage_childhood = `C5.2...183`
  ) |>
  mutate(
    across(starts_with("function_"), ~ recode(.x, `כן` = "yes", `לא` = "no"))
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
  mutate(diva_diagnosis_coded_by_RA = recode(diva_diagnosis_coded_by_RA, `כן` = "ADHD", `לא` = "TD")) |>
  rename(diva_diagnosis_subtype_coded_by_RA = ADHD_subtype) |>
  
  select(
    subjectid,
    cohort,date_recorded,
    age,
    exclusion_criteria,
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
    
    compensation_method,
    community_diagnosis_age,
    community_diagnosis_meds,
    psychiatric_diagnosis,
    psychiatric_diagnosis_freetext,
    psychiatric_diagnosis_meds,
    psychotic_diagnosis,
    neurological_diagnosis,
    
    diva_diagnosis_coded_by_RA,
    diva_diagnosis_subtype_coded_by_RA,
    starts_with("IA"),
    starts_with("HI"),
    starts_with("function_")
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

# 1) Examine subjectid duplicates
dup_count <- diva |>
  count(subjectid) |>
  filter(n > 1) |>
  summarise(subjectid_duplicate_count = n())
print(dup_count)

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
    exclusion_criteria != "fail" & (
      (diva_diagnosis_coded_by_RA == "ADHD" & diva_diagnosis == "below_diva_criteria") |
      (diva_diagnosis_coded_by_RA == "TD" & diva_diagnosis == "meet_diva_criteria")
    )
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
  filter(exclusion_criteria == "pass") |>
  count(declared_group, diva_diagnosis) |>
  tidyr::pivot_wider(names_from = diva_diagnosis, values_from = n, values_fill = 0)

print(confusion_matrix)

# 2) Summary table: average IA and HI adulthood sums by diva_diagnosis_type
diagnosis_type_summary <- diva |>
  filter(exclusion_criteria == "pass") |>
  group_by(diva_diagnosis_type) |>
  summarise(
    avg_IA_adulthood_sum = mean(diva_IA_symptoms, na.rm = TRUE),
    avg_HI_adulthood_sum = mean(diva_HI_symptoms, na.rm = TRUE),
    n(),
    .groups = 'drop'
  )

print(diagnosis_type_summary)

#### SAVE ----
save(diva, file = 'data/תשפד/raw_data/diva.Rdata')
