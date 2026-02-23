library(readr)
library(dplyr)
library(writexl)

#### CREATE ASRS RAW ####

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

asrs_tshpg <- if (file.exists("data/תשפג/collected_data/ASRS+בעברית_October+1,+2025_09.57_values.tsv")) {
  read_tsv("data/תשפג/collected_data/ASRS+בעברית_October+1,+2025_09.57_values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(
      nchar(subjectid) == 8,
      grepl("^[A-Za-z0-9]+$", subjectid),
      !grepl("example", subjectid, ignore.case = TRUE)
    )
} else {
  tibble()
}

asrs_tshpd <- if (file.exists("data/תשפד/collected_data/ASRS_July+1,+2025_10.50.tsv")) {
  read_tsv("data/תשפד/collected_data/ASRS_July+1,+2025_10.50.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(
      nchar(subjectid) == 8,
      grepl("^[A-Za-z0-9]+$", subjectid),
      !grepl("example", subjectid, ignore.case = TRUE)
    )
} else {
  tibble()
}

asrs_tshpe <- if (file.exists("data/תשפה/collected_data/ASRS_תשפה-values.tsv")) {
  read_tsv("data/תשפה/collected_data/ASRS_תשפה-values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(subjectid = shahar_id, date_recorded = RecordedDate) |>
    filter(
      nchar(subjectid) == 6,
      grepl("^[A-Za-z0-9]+$", subjectid),
      !grepl("example", subjectid, ignore.case = TRUE)
    )
} else {
  tibble()
}

asrs <- bind_rows(asrs_tshpg, asrs_tshpd, asrs_tshpe)

#### STEP 2: DATE ----
asrs <- asrs |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 3: HANDLE ITEM RATINGS AND SCORES ----
# SC0=asrs_ia, SC1=asrs_hi, SC2=asrs (Qualtrics totals). No reversed items. DSM 0/1 and manual sums.

asrs <- asrs |>
  filter(Finished == 1) |>
  rename(asrs_ia = SC0, asrs_hi = SC1, asrs = SC2) |>
  rename_with(.cols = matches("^ASRS\\d+$"), .fn = ~ paste0("asrs", seq_along(.))) |>
  mutate(across(c(asrs, asrs_ia, asrs_hi, asrs1:asrs18), as.numeric)) |>
  mutate(
    asrs_dsm_ia1 = if_else(asrs1 >= 2, 1, 0),
    asrs_dsm_ia2 = if_else(asrs2 >= 2, 1, 0),
    asrs_dsm_ia3 = if_else(asrs3 >= 2, 1, 0),
    asrs_dsm_ia4 = if_else(asrs4 >= 3, 1, 0),
    asrs_dsm_ia7 = if_else(asrs7 >= 3, 1, 0),
    asrs_dsm_ia8 = if_else(asrs8 >= 3, 1, 0),
    asrs_dsm_ia9 = if_else(asrs9 >= 2, 1, 0),
    asrs_dsm_ia10 = if_else(asrs10 >= 3, 1, 0),
    asrs_dsm_ia11 = if_else(asrs11 >= 3, 1, 0),
    asrs_dsm_hi5 = if_else(asrs5 >= 3, 1, 0),
    asrs_dsm_hi6 = if_else(asrs6 >= 3, 1, 0),
    asrs_dsm_hi12 = if_else(asrs12 >= 2, 1, 0),
    asrs_dsm_hi13 = if_else(asrs13 >= 3, 1, 0),
    asrs_dsm_hi14 = if_else(asrs14 >= 3, 1, 0),
    asrs_dsm_hi15 = if_else(asrs15 >= 3, 1, 0),
    asrs_dsm_hi16 = if_else(asrs16 >= 2, 1, 0),
    asrs_dsm_hi17 = if_else(asrs17 >= 3, 1, 0),
    asrs_dsm_hi18 = if_else(asrs18 >= 2, 1, 0),
    asrs_ia_count = asrs_dsm_ia1 + asrs_dsm_ia2 + asrs_dsm_ia3 + asrs_dsm_ia4 + asrs_dsm_ia7 + asrs_dsm_ia8 + asrs_dsm_ia9 + asrs_dsm_ia10 + asrs_dsm_ia11,
    asrs_hi_count = asrs_dsm_hi5 + asrs_dsm_hi6 + asrs_dsm_hi12 + asrs_dsm_hi13 + asrs_dsm_hi14 + asrs_dsm_hi15 + asrs_dsm_hi16 + asrs_dsm_hi17 + asrs_dsm_hi18,
    asrs_count = asrs_ia_count + asrs_hi_count,
    asrs_ia_sum = asrs1 + asrs2 + asrs3 + asrs4 + asrs7 + asrs8 + asrs9 + asrs10 + asrs11,
    asrs_hi_sum = asrs5 + asrs6 + asrs12 + asrs13 + asrs14 + asrs15 + asrs16 + asrs17 + asrs18,
    asrs_sum = asrs_ia_sum + asrs_hi_sum
  )

# Flag inconsistencies: raw vs computed
asrs_mismatch <- asrs |> filter(
  (!is.na(asrs) & !is.na(asrs_sum) & asrs != asrs_sum) |
  (!is.na(asrs_ia) & !is.na(asrs_ia_sum) & asrs_ia != asrs_ia_sum) |
  (!is.na(asrs_hi) & !is.na(asrs_hi_sum) & asrs_hi != asrs_hi_sum)
)
if (nrow(asrs_mismatch) > 0) {
  message("ASRS: raw total(s) differ from item sum(s) for ", nrow(asrs_mismatch), " row(s). subjectid: ", paste(asrs_mismatch$subjectid, collapse = ", "))
}

#### STEP 4: HANDLE DUPLICATE SUBJECTID ----
item_cols <- paste0("asrs", 1:18)
asrs <- asrs |>
  mutate(
    .has_data = rowSums(!is.na(pick(all_of(item_cols)))) > 0,
    .date_order = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p"))
  ) |>
  arrange(subjectid, desc(.has_data), .date_order) |>
  group_by(subjectid) |>
  slice(1) |>
  ungroup() |>
  select(-.has_data, -.date_order)

#### STEP 5: COLUMN ORDER AND EXPORT ----
asrs <- asrs |>
  select(
    subjectid,
    date_recorded,
    asrs, asrs_ia, asrs_hi,
    asrs_ia_count, asrs_hi_count, asrs_count,
    asrs1:asrs18,
    starts_with("asrs_dsm_")
  )

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(asrs, file = "data/all_cohorts_raw_data/asrs.Rdata")
tryCatch(
  write_xlsx(asrs, path = "data/all_cohorts_raw_data/asrs.xlsx"),
  error = function(e) warning("Could not write asrs.xlsx (close file if open): ", conditionMessage(e))
)
