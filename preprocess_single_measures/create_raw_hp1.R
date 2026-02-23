
rm(list = ls())
library(readr)
library(dplyr)
library(lubridate)
library(writexl)
source("preprocess_single_measures/parse_cohort_date.R")
source("preprocess_single_measures/add_ses_to_df.R")

#### CREATE HP1 RAW ####

keep_first_entry_with_data <- function(df) {
  if (nrow(df) == 0) return(df)

  data_cols <- setdiff(names(df), c("subjectid", "cohort", "date_recorded"))
  if (length(data_cols) == 0) {
    return(df |> distinct(subjectid, .keep_all = TRUE))
  }

  df |>
    mutate(
      .has_data = rowSums(!is.na(pick(all_of(data_cols)))) > 0,
      .date_order = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p"))
    ) |>
    arrange(subjectid, desc(.has_data), .date_order) |>
    group_by(subjectid) |>
    slice(1) |>
    ungroup() |>
    select(-.has_data, -.date_order)
}

#### STEP 1: LOAD ----
# Validate shaharID
# Validate and generate cohort and date_recorded variables

hp1_tshpg <- if (file.exists("data/תשפג/collected_data/דמוגרפי,+EHI,+AUDIT,+CUDIT-R_October+1,+2025_09.52__value.tsv")) {
  read_tsv("data/תשפג/collected_data/דמוגרפי,+EHI,+AUDIT,+CUDIT-R_October+1,+2025_09.52__value.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE)) |>
    mutate(
      date_recorded = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")),
      cohort = "תשפג"
    )
} else { tibble() }

hp1_tshpd <- if (file.exists("data/תשפד/collected_data/דמוגרפי,+EHI,+AUDIT,+CUDIT-R_May+31,+2025_13.39.tsv")) {
  read_tsv("data/תשפד/collected_data/דמוגרפי,+EHI,+AUDIT,+CUDIT-R_May+31,+2025_13.39.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE)) |>
    mutate(
      date_recorded = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")),
      cohort = "תשפד"
    )
} else { tibble() }

hp1_tshpe <- if (file.exists("data/תשפה/collected_data/home-pack-01_תשפה - labels.tsv")) {
  read_tsv("data/תשפה/collected_data/home-pack-01_תשפה - labels.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(subjectid = `shahar_id...18`, date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 6, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE)) |>
    mutate(
      date_recorded = parse_date_time(as.character(date_recorded), orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM"), quiet = TRUE) |> as.POSIXct(tz = "UTC"),
      cohort = cohort_from_date(date_recorded)
    )
} else { tibble() }

#### STEP 2: HANDLE SPECIFIC ITEM RATINGS ----
# Harmonize item ratings and compute main scores (AUDIT/CUDIT/EHI)

hp1_tshpg <- hp1_tshpg |>
  select(-matches("^shahar_id")) |>
  rename(age = Q1, gender = Q3, education = Q5, place_of_residence_until12yo = Q8, vision_correction = Q10) |>
  mutate(age = as.numeric(age), gender = if_else(gender == "3", "female", "male")) |>
  rename(EHI = SC1, EHI4 = EHDI4, AUDIT = SC2, CUDIT = SC3) |>
  mutate(EHI = if_else(is.na(EHI1), NA, EHI)) |>
  rename(CUDIT_USE = `CUDIT-RYES/NO`, CUDIT1 = `CUDIT-R1`, CUDIT2 = `CUDIT-R2`, CUDIT3 = `CUDIT-R3`, CUDIT4 = `CUDIT-R4`, CUDIT5 = `CUDIT-R5`, CUDIT6 = `CUDIT-R6`, CUDIT7 = `CUDIT-R7`, CUDIT8 = `CUDIT-R8`) |>
  mutate(CUDIT_USE = if_else(CUDIT_USE == "1", "yes", "no"))

hp1_tshpd <- hp1_tshpd |>
  select(-matches("^shahar_id")) |>
  rename(age = demographic1_age, gender = demographic3_gender, education = demographic4_educati, place_of_residence_until12yo = demographic7_residen, vision_correction = demographic8_vision) |>
  mutate(age = as.numeric(age)) |>
  rename(EHI = SC0, EHI4 = EHDI4, AUDIT = SC1, CUDIT = SC2) |>
  rename(CUDIT_USE = `CUDIT-RYES/NO`, CUDIT1 = `CUDIT-R1`, CUDIT2 = `CUDIT-R2`, CUDIT3 = `CUDIT-R3`, CUDIT4 = `CUDIT-R4`, CUDIT5 = `CUDIT-R5`, CUDIT6 = `CUDIT-R6`, CUDIT7 = `CUDIT-R7`, CUDIT8 = `CUDIT-R8`)

hp1_tshpe <- hp1_tshpe |>
  select(-matches("^shahar_id")) |>
  rename(DOB_month = demographic2_DOB_month, DOB_year = demographic2_DOB_year, gender = demographic3_gender, education = demographic4_educati, place_of_residence_until12yo = demographic7_residan, vision_correction = demographic8_vision) |>
  mutate(
    DOB_month = recode(DOB_month, "ינואר" = "January", "פברואר" = "February", "מרץ" = "March", "אפריל" = "April", "מאי" = "May", "יוני" = "June", "יולי" = "July", "אוגוסט" = "August", "ספטמבר" = "September", "אוקטובר" = "October", "נובמבר" = "November", "דצמבר" = "December"),
    DOB = parse_date_time(paste(15, DOB_month, DOB_year, sep = "-"), orders = "d-B-Y"),
    age = as.numeric(interval(DOB, date_recorded) / years(1))
  ) |>
  select(-DOB_month, -DOB_year, -DOB) |>
  rename(EHI = SC0, EHI4 = EHDI4, AUDIT = SC1, CUDIT = SC2) |>
  rename(CUDIT_USE = `CUDIT-RYES/NO`, CUDIT1 = `CUDIT-R1`, CUDIT2 = `CUDIT-R2`, CUDIT3 = `CUDIT-R3`, CUDIT4 = `CUDIT-R4`, CUDIT5 = `CUDIT-R5`, CUDIT6 = `CUDIT-R6`, CUDIT7 = `CUDIT-R7`, CUDIT8 = `CUDIT-R8`)

#### STEP 3: HANDLE SPECIAL COLUMNS ----
# Handle HP1-specific columns: hand dominance, language, and education harmonization

# Common invalid numeric sentinels to treat as NA
invalid_numeric <- function(x) {
  x <- as.numeric(x)
  x[!is.na(x) & x %in% c(999, -999, 9999, 999.99, -999.99)] <- NA_real_
  x
}

hp1 <- bind_rows(hp1_tshpg, hp1_tshpd, hp1_tshpe) |>
  mutate(
    primary_lang = if_else(subjectid %in% c("V3EJt8", "wQP4Rw", "EY5GTz", "9JS4rf", "957H63", "yDKkeH", "kPjb3e", "DnqxaP", "KdS4cj"), "arabic", "hebrew", missing = "hebrew"),
    EHI_score = EHI
  ) |>
  mutate(across(c(age, starts_with("EH"), CUDIT, CUDIT1:CUDIT8, AUDIT, AUDIT1:AUDIT10), as.numeric)) |>
  mutate(across(c(age, EHI_score, EHI1, EHI2, EHI3, EHI4, CUDIT, CUDIT1:CUDIT8, AUDIT, AUDIT1:AUDIT10), invalid_numeric)) |>
  mutate(
    EHI_hand_dominance = case_when(
      EHI_score <= -61 ~ "Left handers",
      EHI_score >= 61 ~ "Right handers",
      EHI_score > -61 & EHI_score < 61 ~ "Mixed handers",
      TRUE ~ NA_character_
    )
  ) |>
  mutate(
    alcohol_use_cutoff = if_else(AUDIT < 8, "below_audit_cutoff", "above_audit_cutoff"),
    cannabis_use_cutoff = if_else(CUDIT < 8, "below_cudit_cutoff", "above_cudit_cutoff")
  ) |>
  mutate(
    education = trimws(tolower(as.character(education))),
    education = case_when(
      is.na(education) | education == "" | education == "na" ~ NA_character_,
      education %in% c("1", "high_school", "high school") ~ "high_school",
      grepl("סיום בית ספר תיכון", education, fixed = TRUE) ~ "high_school",
      grepl("תעודת בגרות מלאה", education, fixed = TRUE) ~ "high_school",
      grepl("עד 12 שנות לימוד", education, fixed = TRUE) ~ "high_school",
      grepl("ללא תעודת בגרות", education, fixed = TRUE) ~ "high_school",
      education %in% c("3", "ma", "master", "graduate", "ms", "phd") ~ "graduate",
      grepl("תואר שני", education, fixed = TRUE) ~ "graduate",
      grepl("תואר שלישי", education, fixed = TRUE) ~ "graduate",
      education %in% c("2", "ba", "bachelor", "undergraduate") ~ "undergraduate",
      grepl("תואר ראשון", education, fixed = TRUE) ~ "undergraduate",
      TRUE ~ NA_character_
    ),
    education = factor(education, levels = c("high_school", "undergraduate", "graduate")),
    # Vision correction: only "normal" and "corrected"; 1/normal/no -> normal, 2/yes/corrected -> corrected
    vision_correction = trimws(tolower(as.character(vision_correction))),
    vision_correction = case_when(
      is.na(vision_correction) | vision_correction == "" ~ NA_character_,
      vision_correction %in% c("1", "normal", "no", "ללא", "לא") ~ "normal",
      vision_correction %in% c("2", "corrected", "yes", "משקפיים", "עדשות", "כן") ~ "corrected",
      TRUE ~ NA_character_
    )
  ) |>
  add_ses_to_df_by_hebrew(city_hebrew_col = "place_of_residence_until12yo", ses_path = "data/ses_cbs_2006.xls") |>
  select(subjectid, cohort, date_recorded, primary_lang, age, gender, EHI_hand_dominance, EHI_score, EHI1, EHI2, EHI3, EHI4, AUDIT, CUDIT, education, place_of_residence_until12yo, city_hebrew, city_english, SES_rank, SES_cluster, vision_correction, AUDIT1:AUDIT10, CUDIT_USE, CUDIT1:CUDIT8, alcohol_use_cutoff, cannabis_use_cutoff)

#### STEP 4: HANDLE SHAHARID DUPLICATES ----
# Keep first entry that has data

hp1 <- keep_first_entry_with_data(hp1)

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(hp1, file = "data/all_cohorts_raw_data/hp1.Rdata")
write_xlsx(hp1, path = "data/all_cohorts_raw_data/hp1.xlsx")
