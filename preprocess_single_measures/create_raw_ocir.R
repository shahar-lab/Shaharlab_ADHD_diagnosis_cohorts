library(readr)
library(dplyr)
library(writexl)

#### CREATE OCIR RAW ####

# OCI-R subscale item mapping (standard: washing 5,11,17; checking 2,8,14; ordering 3,9,15; hoarding 1,7,13; obsessing 6,12,18; neutralizing 4,10,16)
ocir_washing_items     <- c(5, 11, 17)
ocir_checking_items   <- c(2, 8, 14)
ocir_ordering_items   <- c(3, 9, 15)
ocir_hoarding_items   <- c(1, 7, 13)
ocir_obsessing_items  <- c(6, 12, 18)
ocir_neutralizing_items <- c(4, 10, 16)

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

ocir_tshpg <- if (file.exists("data/תשפג/collected_data/OCI-R_October+1,+2025_10.05_values.tsv")) {
  read_tsv("data/תשפג/collected_data/OCI-R_October+1,+2025_10.05_values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

ocir_tshpd <- if (file.exists("data/תשפד/collected_data/OCI-R_July+1,+2025_11.19.tsv")) {
  read_tsv("data/תשפד/collected_data/OCI-R_July+1,+2025_11.19.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

ocir_tshpe <- if (file.exists("data/תשפה/collected_data/OCI-R_תשפה - values.tsv")) {
  read_tsv("data/תשפה/collected_data/OCI-R_תשפה - values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(subjectid = shahar_id, date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 6, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

# Cohort-specific SC mapping: תשפג uses SC0=total, SC1–6=subscales; תשפד/תשפה use SC0–5=subscales, SC6=total
if ((nrow(ocir_tshpg) > 0 || ncol(ocir_tshpg) > 0) && all(c("SC0", "SC6") %in% names(ocir_tshpg))) {
  ocir_tshpg <- ocir_tshpg |>
    filter(Finished == 1) |>
    rename(
      ocir_hoarding = SC6,
      ocir_checking = SC4,
      ocir_ordering = SC3,
      ocir_neutralizing = SC5,
      ocir_washing = SC1,
      ocir_obsessing = SC2,
      ocir = SC0
    ) |>
    rename_with(.cols = matches("^OCI-R\\d+$"), .fn = ~ paste0("ocir", seq_along(.)))
} else {
  ocir_tshpg <- tibble()
}

if (nrow(ocir_tshpd) > 0 || ncol(ocir_tshpd) > 0) {
  ocir_tshpd <- ocir_tshpd |>
    filter(Finished == 1) |>
    rename(
      ocir_hoarding = SC0,
      ocir_checking = SC1,
      ocir_ordering = SC2,
      ocir_neutralizing = SC3,
      ocir_washing = SC4,
      ocir_obsessing = SC5,
      ocir = SC6
    ) |>
    rename_with(.cols = matches("^OCI-R\\d+$"), .fn = ~ paste0("ocir", seq_along(.)))
}

if (nrow(ocir_tshpe) > 0 || ncol(ocir_tshpe) > 0) {
  ocir_tshpe <- ocir_tshpe |>
    filter(Finished == 1) |>
    rename(
      ocir_hoarding = SC0,
      ocir_checking = SC1,
      ocir_ordering = SC2,
      ocir_neutralizing = SC3,
      ocir_washing = SC4,
      ocir_obsessing = SC5,
      ocir = SC6
    ) |>
    rename_with(.cols = matches("^OCI-R\\d+$"), .fn = ~ paste0("ocir", seq_along(.)))
}

ocir <- bind_rows(ocir_tshpg, ocir_tshpd, ocir_tshpe) |>
  mutate(across(c(ocir, ocir_hoarding, ocir_checking, ocir_ordering, ocir_neutralizing, ocir_washing, ocir_obsessing, ocir1:ocir18), as.numeric))

#### STEP 2: DATE ----
ocir <- ocir |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 3: COMPUTE SUM AND SIX SUBSCALE SUMS FROM ITEMS ----
ocir <- ocir |>
  mutate(
    ocir_sum = rowSums(pick(ocir1:ocir18), na.rm = FALSE),
    ocir_washing_sum     = rowSums(pick(ocir5, ocir11, ocir17), na.rm = FALSE),
    ocir_checking_sum    = rowSums(pick(ocir2, ocir8, ocir14), na.rm = FALSE),
    ocir_ordering_sum    = rowSums(pick(ocir3, ocir9, ocir15), na.rm = FALSE),
    ocir_hoarding_sum    = rowSums(pick(ocir1, ocir7, ocir13), na.rm = FALSE),
    ocir_obsessing_sum   = rowSums(pick(ocir6, ocir12, ocir18), na.rm = FALSE),
    ocir_neutralizing_sum = rowSums(pick(ocir4, ocir10, ocir16), na.rm = FALSE)
  )

# Flag inconsistencies: raw total and raw subscales vs computed from items
ocir_mismatch_total <- ocir |> filter(!is.na(ocir) & !is.na(ocir_sum) & ocir != ocir_sum)
if (nrow(ocir_mismatch_total) > 0) {
  message("OCIR: raw total (ocir) differs from item sum (ocir_sum) for ", nrow(ocir_mismatch_total), " row(s). subjectid: ", paste(ocir_mismatch_total$subjectid, collapse = ", "))
}
ocir_mismatch_wash <- ocir |> filter(!is.na(ocir_washing) & !is.na(ocir_washing_sum) & ocir_washing != ocir_washing_sum)
if (nrow(ocir_mismatch_wash) > 0) {
  message("OCIR: raw ocir_washing differs from item sum for ", nrow(ocir_mismatch_wash), " row(s). subjectid: ", paste(ocir_mismatch_wash$subjectid, collapse = ", "))
}
ocir_mismatch_check <- ocir |> filter(!is.na(ocir_checking) & !is.na(ocir_checking_sum) & ocir_checking != ocir_checking_sum)
if (nrow(ocir_mismatch_check) > 0) {
  message("OCIR: raw ocir_checking differs from item sum for ", nrow(ocir_mismatch_check), " row(s). subjectid: ", paste(ocir_mismatch_check$subjectid, collapse = ", "))
}
ocir_mismatch_ord <- ocir |> filter(!is.na(ocir_ordering) & !is.na(ocir_ordering_sum) & ocir_ordering != ocir_ordering_sum)
if (nrow(ocir_mismatch_ord) > 0) {
  message("OCIR: raw ocir_ordering differs from item sum for ", nrow(ocir_mismatch_ord), " row(s). subjectid: ", paste(ocir_mismatch_ord$subjectid, collapse = ", "))
}
ocir_mismatch_hoard <- ocir |> filter(!is.na(ocir_hoarding) & !is.na(ocir_hoarding_sum) & ocir_hoarding != ocir_hoarding_sum)
if (nrow(ocir_mismatch_hoard) > 0) {
  message("OCIR: raw ocir_hoarding differs from item sum for ", nrow(ocir_mismatch_hoard), " row(s). subjectid: ", paste(ocir_mismatch_hoard$subjectid, collapse = ", "))
}
ocir_mismatch_obs <- ocir |> filter(!is.na(ocir_obsessing) & !is.na(ocir_obsessing_sum) & ocir_obsessing != ocir_obsessing_sum)
if (nrow(ocir_mismatch_obs) > 0) {
  message("OCIR: raw ocir_obsessing differs from item sum for ", nrow(ocir_mismatch_obs), " row(s). subjectid: ", paste(ocir_mismatch_obs$subjectid, collapse = ", "))
}
ocir_mismatch_neut <- ocir |> filter(!is.na(ocir_neutralizing) & !is.na(ocir_neutralizing_sum) & ocir_neutralizing != ocir_neutralizing_sum)
if (nrow(ocir_mismatch_neut) > 0) {
  message("OCIR: raw ocir_neutralizing differs from item sum for ", nrow(ocir_mismatch_neut), " row(s). subjectid: ", paste(ocir_mismatch_neut$subjectid, collapse = ", "))
}

#### STEP 4: HANDLE DUPLICATE SUBJECTID ----
item_cols <- paste0("ocir", 1:18)
ocir <- ocir |>
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
# subjectid, date_recorded, total and subscales (raw + computed sums), then items. Same to .Rdata and Excel.

ocir <- ocir |>
  select(
    subjectid,
    date_recorded,
    ocir, ocir_sum,
    ocir_washing, 
    ocir_checking, 
    ocir_ordering, 
    ocir_hoarding, 
    ocir_obsessing, 
    ocir_neutralizing, 
    ocir1:ocir18
  )

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(ocir, file = "data/all_cohorts_raw_data/ocir.Rdata")
tryCatch(
  write_xlsx(ocir, path = "data/all_cohorts_raw_data/ocir.xlsx"),
  error = function(e) warning("Could not write ocir.xlsx (close file if open): ", conditionMessage(e))
)
