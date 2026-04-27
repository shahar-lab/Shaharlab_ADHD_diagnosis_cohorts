library(readr)
library(dplyr)
library(lubridate)
library(writexl)

#### CREATE DIVA RAW ####

diva_config <- list(
  תשפג = list(input = "data/collected_data/תשפג/אבחונים+-+Diva+-+V3+-+Copy_October+1,+2025_09.55_labels.tsv", subjectid_nchar = 8, subjectid_rename = NULL),
  תשפד = list(input = "data/collected_data/תשפד/שאלון+מקדים+++DIVA_May+30,+2025_08.59.tsv", subjectid_nchar = 8, subjectid_rename = NULL),
  תשפה = list(input = "data/collected_data/תשפה_תשפו/שאלון_מקדים_DIVA_תשפה-values.tsv", subjectid_nchar = 6, subjectid_rename = "shahar_id")
)

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded
# Read TSV for each cohort; rename subjectid column for תשפה
diva_list <- list()
for (cohort_name in names(diva_config)) {
  cfg <- diva_config[[cohort_name]]
  if (!file.exists(cfg$input)) {
    warning("Skipping cohort ", cohort_name, ": input file not found: ", cfg$input)
    next
  }
  diva <- read_tsv(cfg$input, locale = locale(encoding = "UTF-16"))
  if (!is.null(cfg$subjectid_rename)) {
    diva <- diva |> rename(subjectid = all_of(cfg$subjectid_rename))
  }
  diva_list[[cohort_name]] <- diva
}

#### STEP 2: HANDLE SPECIFIC ITEM RATINGS ----
# Remove Qualtrics header rows (rows 1-2); cohort-specific renames and transforms; select needed columns
for (cohort_name in names(diva_list)) {
  diva <- diva_list[[cohort_name]]
  diva <- diva |> slice(-1, -2) |> rename(date_recorded = RecordedDate)

  if (cohort_name == "תשפג") {
    diva <- diva |>
      rename(group_declared = Q1, age = Q2_1) |>
      
      rename(compensation_method = Q2_5) |>
      mutate(compensation_method = recode(compensation_method, `קרדיט` = "course_credit", `תשלום` = "monetray")) |>
      mutate(
        group = recode(group_declared, `ביקורת` = "TD", `קשב` = "ADHD"),
        community_diagnosis_meds = if_else(Q6 == "No", "no", "yes"),
        community_diagnosis_meds_type = Q6,
        community_diagnosis_meds_dosage = Q7,
        community_diagnosis_meds_freq = Q8,
        community_diagnosis_age = Q4_5,
        community_diagnosis_meds = Q6,
        psychiatric_diagnosis = Q13,
        psychiatric_diagnosis_freetext = Q13_1_TEXT,
        psychiatric_diagnosis_meds = Q14,
        psychotic_diagnosis = Q15,
        neurological_diagnosis = Q16
      ) |>
      mutate(
        psychiatric_diagnosis = recode(psychiatric_diagnosis, `כן` = "yes", `לא` = "no"),
        psychiatric_diagnosis_meds = recode(psychiatric_diagnosis_meds, `כן` = "yes", `לא` = "no"),
        psychotic_diagnosis = recode(psychotic_diagnosis, `כן` = "yes", `לא` = "no"),
        neurological_diagnosis = recode(neurological_diagnosis, `כן` = "yes", `לא` = "no")
      ) |>
      rename(exclusion_criteria = Q19) |>
      mutate(exclusion_criteria = recode(exclusion_criteria, `כן` = "pass", `לא` = "fail")) |>
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
      mutate(
        across(
          matches("^(IA|HI)[0-9]+_(adulthood|childhood)$"),
          ~ case_when(
            .x == "כן" ~ 1L,
            .x == "לא" ~ 0L,
            TRUE       ~ NA_integer_
          )
        )
      ) |>
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
      mutate(
        diva_IA_symptoms =
          coalesce(as.numeric(IA1_adulthood), 0) +
          coalesce(as.numeric(IA2_adulthood), 0) +
          coalesce(as.numeric(IA3_adulthood), 0) +
          coalesce(as.numeric(IA4_adulthood), 0) +
          coalesce(as.numeric(IA5_adulthood), 0) +
          coalesce(as.numeric(IA6_adulthood), 0) +
          coalesce(as.numeric(IA7_adulthood), 0) +
          coalesce(as.numeric(IA8_adulthood), 0) +
          coalesce(as.numeric(IA9_adulthood), 0),
        diva_HI_symptoms =
          coalesce(as.numeric(HI1_adulthood), 0) +
          coalesce(as.numeric(HI2_adulthood), 0) +
          coalesce(as.numeric(HI3_adulthood), 0) +
          coalesce(as.numeric(HI4_adulthood), 0) +
          coalesce(as.numeric(HI5_adulthood), 0) +
          coalesce(as.numeric(HI6_adulthood), 0) +
          coalesce(as.numeric(HI7_adulthood), 0) +
          coalesce(as.numeric(HI8_adulthood), 0) +
          coalesce(as.numeric(HI9_adulthood), 0),
        diva_childhood_symptoms =
          coalesce(as.numeric(IA1_childhood), 0) +
          coalesce(as.numeric(IA2_childhood), 0) +
          coalesce(as.numeric(IA3_childhood), 0) +
          coalesce(as.numeric(IA4_childhood), 0) +
          coalesce(as.numeric(IA5_childhood), 0) +
          coalesce(as.numeric(IA6_childhood), 0) +
          coalesce(as.numeric(IA7_childhood), 0) +
          coalesce(as.numeric(IA8_childhood), 0) +
          coalesce(as.numeric(IA9_childhood), 0) +
          coalesce(as.numeric(HI1_childhood), 0) +
          coalesce(as.numeric(HI2_childhood), 0) +
          coalesce(as.numeric(HI3_childhood), 0) +
          coalesce(as.numeric(HI4_childhood), 0) +
          coalesce(as.numeric(HI5_childhood), 0) +
          coalesce(as.numeric(HI6_childhood), 0) +
          coalesce(as.numeric(HI7_childhood), 0) +
          coalesce(as.numeric(HI8_childhood), 0) +
          coalesce(as.numeric(HI9_childhood), 0),
        diva_function_adulthood =
          coalesce(as.numeric(Discussion8_1), 0) +
          coalesce(as.numeric(Discussion8_2), 0) +
          coalesce(as.numeric(Discussion8_3), 0) +
          coalesce(as.numeric(Discussion8_4), 0) +
          coalesce(as.numeric(Discussion8_5), 0),
        diva_function_childhood =
          coalesce(as.numeric(Discussion10_1), 0) +
          coalesce(as.numeric(Discussion10_2), 0) +
          coalesce(as.numeric(Discussion10_3), 0) +
          coalesce(as.numeric(Discussion10_4), 0) +
          coalesce(as.numeric(Discussion10_5), 0),
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
      rename(diva_diagnosis_coded_by_RA = Discussion12) |>
      mutate(diva_diagnosis_coded_by_RA = recode(diva_diagnosis_coded_by_RA, `כן` = "ADHD", `לא` = "TD")) |>
      rename(diva_diagnosis_subtype_coded_by_RA = Discussion13) |>
      select(
        subjectid, date_recorded, age, exclusion_criteria,
        diva_IA_symptoms, diva_HI_symptoms, diva_childhood_symptoms, diva_function_adulthood, diva_function_childhood,
        DSM_criteria_A1, DSM_criteria_A2, DSM_criteria_B, DSM_criteria_C_D, diva_diagnosis, diva_diagnosis_type,
        compensation_method, community_diagnosis_age, community_diagnosis_meds, community_diagnosis_meds_type,
        community_diagnosis_meds_dosage, community_diagnosis_meds_freq,
        psychiatric_diagnosis, psychiatric_diagnosis_freetext, psychiatric_diagnosis_meds,
        psychotic_diagnosis, neurological_diagnosis,
        diva_diagnosis_coded_by_RA, diva_diagnosis_subtype_coded_by_RA,
        starts_with("IA"), starts_with("HI"), starts_with("function_")
      ) |>
      mutate(
        across(starts_with("IA"), as.numeric),
        across(starts_with("HI"), as.numeric),
        diva_diagnosis = factor(diva_diagnosis, levels = c("below_diva_criteria", "meet_diva_criteria")),
        diva_diagnosis_type = factor(diva_diagnosis_type, levels = c("below_diva_criteria", "primary_inattentive", "primary_hyperactive/impulsive", "combined"))
      )

  } else if (cohort_name == "תשפד") {
    diva <- diva |>
      rename(group_declared = group, age = `DIVA-personalinfo_1`) |>
      
      rename(compensation_method = `DIVA-personalinfo_5`) |>
      mutate(compensation_method = recode(compensation_method, `קרדיט` = "course_credit", `תשלום` = "monetray")) |>
      mutate(
        group_declared = recode(group_declared, `ביקורת` = "TD", `קשב` = "ADHD"),
        community_diagnosis_age = `DIVA-diagnosis2_5`,
        community_diagnosis_meds = if_else(`DIVA-meds1` == "No", "no", "yes"),
        community_diagnosis_meds_type = `DIVA-meds1`,
        community_diagnosis_meds_dosage = `DIVA-meds2`,
        community_diagnosis_meds_freq = `DIVA-meds3`,
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
      rename(exclusion_criteria = `DIVA-insurance8`) |>
      mutate(exclusion_criteria = recode(exclusion_criteria, `כן` = "pass", `לא` = "fail")) |>
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
      rename(diva_diagnosis_coded_by_RA = ADHD_decision) |>
      mutate(diva_diagnosis_coded_by_RA = recode(diva_diagnosis_coded_by_RA, `כן` = "ADHD", `לא` = "TD")) |>
      rename(diva_diagnosis_subtype_coded_by_RA = ADHD_subtype) |>
      select(
        subjectid, date_recorded, age, exclusion_criteria,
        diva_IA_symptoms, diva_HI_symptoms, diva_childhood_symptoms, diva_function_adulthood, diva_function_childhood,
        DSM_criteria_A1, DSM_criteria_A2, DSM_criteria_B, DSM_criteria_C_D, diva_diagnosis, diva_diagnosis_type,
        compensation_method, community_diagnosis_age, community_diagnosis_meds, community_diagnosis_meds_type,
        community_diagnosis_meds_dosage, community_diagnosis_meds_freq,
        psychiatric_diagnosis, psychiatric_diagnosis_freetext, psychiatric_diagnosis_meds,
        psychotic_diagnosis, neurological_diagnosis,
        diva_diagnosis_coded_by_RA, diva_diagnosis_subtype_coded_by_RA,
        starts_with("IA"), starts_with("HI"), starts_with("function_")
      )

    diva <- diva |>
      mutate(
        across(starts_with("IA"), as.numeric),
        across(starts_with("HI"), as.numeric)
      ) |>
      mutate(
        diva_diagnosis = factor(diva_diagnosis, levels = c("below_diva_criteria", "meet_diva_criteria")),
        diva_diagnosis_type = factor(diva_diagnosis_type, levels = c("below_diva_criteria", "primary_inattentive", "primary_hyperactive/impulsive", "combined"))
      )

  } else if (cohort_name == "תשפה") {
    diva <- diva |>
      rename(group_declared = group, age = `DIVA-personalinfo_1`) |>
      rename( 
        community_diagnosis_age = `DIVA-diagnosis2_5`,
        community_diagnosis_meds = `DIVA-meds1`,
        community_diagnosis_meds_type = `DIVA-meds2`,
        community_diagnosis_meds_dosage = `DIVA-meds3`,
        community_diagnosis_meds_freq = `DIVA-meds4`,
        psychiatric_diagnosis = `DIVA-insurance1`,
        psychiatric_diagnosis_freetext = `DIVA-insurance1_1_TEXT`,
        psychiatric_diagnosis_meds = `DIVA-insurance2`,
        psychiatric_diagnosis_meds_type = `DIVA-insurance2_1_TEXT`
      ) |>
      mutate(
        group_declared = recode(group_declared, `2` = "TD", `1` = "ADHD"),
        psychiatric_diagnosis_meds = recode(psychiatric_diagnosis_meds, `כן` = "yes", `לא` = "no"),
      ) |>
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
      ) |>
      rename(
        function_occupational_adulthood = C1_decision, function_occupational_childhood = C1_decision_child,
        function_family_adulthood = C2_decision, function_family_childhood = C2_decision_child,
        function_social_adulthood = C3_decision, function_social_childhood = C3_decision_child,
        function_leisure_adulthood = C4_decision, function_leisure_childhood = C4_decision_child,
        function_selfimage_adulthood = C5_decision, function_selfimage_childhood = C5_decision_child
      ) |>
      mutate(
        across(starts_with("function_"), ~ recode(.x, `1` = "yes", `0` = "no"))
      ) |>
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
      rename(diva_diagnosis_coded_by_RA = ADHD_decision) |>
      mutate(diva_diagnosis_coded_by_RA = recode(diva_diagnosis_coded_by_RA, `1` = "ADHD", `0` = "TD")) |>
      rename(diva_diagnosis_subtype_coded_by_RA = ADHD_subtype) |>
      select(
        subjectid, date_recorded, age,
        diva_IA_symptoms, diva_HI_symptoms, diva_childhood_symptoms, diva_function_adulthood, diva_function_childhood,
        DSM_criteria_A1, DSM_criteria_A2, DSM_criteria_B, DSM_criteria_C_D, diva_diagnosis, diva_diagnosis_type,
        community_diagnosis_age, community_diagnosis_meds, community_diagnosis_meds_type,
        community_diagnosis_meds_dosage, community_diagnosis_meds_freq,
        psychiatric_diagnosis, psychiatric_diagnosis_freetext, psychiatric_diagnosis_meds,
        diva_diagnosis_coded_by_RA, diva_diagnosis_subtype_coded_by_RA,
        starts_with("IA"), starts_with("HI"),
        function_occupational_adulthood, function_family_adulthood, function_social_adulthood,
        function_leisure_adulthood, function_selfimage_adulthood
      )

    diva <- diva |>
      mutate(
        across(starts_with("IA"), as.numeric),
        across(starts_with("HI"), as.numeric)
      ) |>
      mutate(
        diva_diagnosis = factor(diva_diagnosis, levels = c("below_diva_criteria", "meet_diva_criteria")),
        diva_diagnosis_type = factor(diva_diagnosis_type, levels = c("below_diva_criteria", "primary_inattentive", "primary_hyperactive/impulsive", "combined"))
      )

  }

  diva_list[[cohort_name]] <- diva
}

#### STEP 3: HANDLE SPECIAL COLUMNS ----
# Keep only valid subject IDs: correct length, alphanumeric, exclude "example"
for (cohort_name in names(diva_list)) {
  cfg <- diva_config[[cohort_name]]
  diva <- diva_list[[cohort_name]]
  diva <- diva |>
    filter(
      nchar(subjectid) == cfg$subjectid_nchar &
        grepl("^[A-Za-z0-9]+$", subjectid) &
        !grepl("example", subjectid, ignore.case = TRUE)
    )
  if (cohort_name == "תשפה") {
    diva <- diva |> filter(subjectid != "000000")
  }
  diva_list[[cohort_name]] <- diva
}

diva <- if (length(diva_list) > 0) bind_rows(diva_list) else tibble()
diva <- diva |>
  mutate(diva_language = if_else(subjectid %in% c("V3EJt8", "wQP4Rw", "EY5GTz", "9JS4rf", "957H63", "yDKkeH", "kPjb3e", "DnqxaP", "KdS4cj"), "arabic", "hebrew", missing = "hebrew"))
#### STEP 4: HANDLE SHAHARID DUPLICATES ----
# Keep first entry that has data

keep_first_entry_with_data <- function(df) {
  if (nrow(df) == 0) return(df)

  data_cols <- setdiff(names(df), c("subjectid", "date_recorded"))
  if (length(data_cols) == 0) {
    return(df |> distinct(subjectid, .keep_all = TRUE))
  }

  df |>
    mutate(
      .has_data = rowSums(!is.na(pick(all_of(data_cols)))) > 0,
      .date_order = parse_date_time(
        as.character(date_recorded),
        orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM", "mdy HMS", "mdy HM", "ymd", "dmy"),
        quiet = TRUE
      ) |> as.POSIXct(tz = "UTC")
    ) |>
    arrange(subjectid, desc(.has_data), .date_order) |>
    group_by(subjectid) |>
    slice(1) |>
    ungroup() |>
    select(-.has_data, -.date_order)
}

diva <- keep_first_entry_with_data(diva) |>
  select(
    subjectid,
    date_recorded,
    exclusion_criteria,

    community_diagnosis_age,
    community_diagnosis_meds,
    community_diagnosis_meds_type,
    community_diagnosis_meds_dosage,
    community_diagnosis_meds_freq,
    psychiatric_diagnosis,
    psychiatric_diagnosis_freetext,
    psychiatric_diagnosis_meds,
    psychotic_diagnosis,
    neurological_diagnosis,
    diva_language,
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
    DSM_criteria_C_D,
    diva_diagnosis_coded_by_RA,
    diva_diagnosis_subtype_coded_by_RA,
    IA1_adulthood,
    IA1_childhood,
    IA2_adulthood,
    IA2_childhood,
    IA3_adulthood,
    IA3_childhood,
    IA4_adulthood,
    IA4_childhood,
    IA5_adulthood,
    IA5_childhood,
    IA6_adulthood,
    IA6_childhood,
    IA7_adulthood,
    IA7_childhood,
    IA8_adulthood,
    IA8_childhood,
    IA9_adulthood,
    IA9_childhood,
    HI1_adulthood,
    HI1_childhood,
    HI2_adulthood,
    HI2_childhood,
    HI3_adulthood,
    HI3_childhood,
    HI4_adulthood,
    HI4_childhood,
    HI5_adulthood,
    HI5_childhood,
    HI6_adulthood,
    HI6_childhood,
    HI7_adulthood,
    HI7_childhood,
    HI8_adulthood,
    HI8_childhood,
    HI9_adulthood,
    HI9_childhood,
    function_occupational_adulthood,
    function_occupational_childhood,
    function_family_adulthood,
    function_family_childhood,
    function_social_adulthood,
    function_social_childhood,
    function_leisure_adulthood,
    function_leisure_childhood,
    function_selfimage_adulthood,
    function_selfimage_childhood
  )

dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
save(diva, file = "data/raw_data/diva.Rdata")
write_xlsx(diva, path = "data/raw_data/diva.xlsx")





