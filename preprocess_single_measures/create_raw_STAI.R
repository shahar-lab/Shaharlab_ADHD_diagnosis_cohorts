library(readr)
library(dplyr)
library(writexl)

#### CREATE STAI RAW ####

# Reversed items (already flipped in raw data; we only add * to colnames at export)
sai_reversed <- c(1, 2, 5, 8, 10, 11, 15, 16, 19, 20)
tai_reversed <- c(1, 3, 6, 7, 10, 13, 14, 16, 19)

# Date parsing (used once)
stai_date_fmt <- c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")

#### STEP 1: LOAD ----
# Cohort-specific subjectid checks. Only process and rename when file was loaded (nrow > 0).

stai_tshpg <- if (file.exists("data/collected_data/תשפג/STAI+template+-+עברית+-+Copy_October+1,+2025_10.00_values.tsv")) {
  read_tsv("data/collected_data/תשפג/STAI+template+-+עברית+-+Copy_October+1,+2025_10.00_values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

stai_tshpd <- if (file.exists("data/collected_data/תשפד/STAI_July+1,+2025_11.22.tsv")) {
  read_tsv("data/collected_data/תשפד/STAI_July+1,+2025_11.22.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

stai_tshpe <- if (file.exists("data/collected_data/תשפה_תשפו/STAI_תשפה - values.tsv")) {
  read_tsv("data/collected_data/תשפה_תשפו/STAI_תשפה - values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(subjectid = shahar_id, date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 6, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

# Only תשפג has STAI1:STAI40; rename to SAI1:SAI20, TAI1:TAI20. תשפד/תשפה: just totals.
if (nrow(stai_tshpg) > 0) {
  stai_tshpg <- stai_tshpg |>
    filter(Finished == 1) |>
    rename(stai_state = SC0, stai_trait = SC1, stai = SC2) |>
    rename(
      SAI1 = STAI1, SAI2 = STAI2, SAI3 = STAI3, SAI4 = STAI4, SAI5 = STAI5,
      SAI6 = STAI6, SAI7 = STAI7, SAI8 = STAI8, SAI9 = STAI9, SAI10 = STAI10,
      SAI11 = STAI11, SAI12 = STAI12, SAI13 = STAI13, SAI14 = STAI14, SAI15 = STAI15,
      SAI16 = STAI16, SAI17 = STAI17, SAI18 = STAI18, SAI19 = STAI19, SAI20 = STAI20,
      TAI1 = STAI21, TAI2 = STAI22, TAI3 = STAI23, TAI4 = STAI24, TAI5 = STAI25,
      TAI6 = STAI26, TAI7 = STAI27, TAI8 = STAI28, TAI9 = STAI29, TAI10 = STAI30,
      TAI11 = STAI31, TAI12 = STAI32, TAI13 = STAI33, TAI14 = STAI34, TAI15 = STAI35,
      TAI16 = STAI36, TAI17 = STAI37, TAI18 = STAI38, TAI19 = STAI39, TAI20 = STAI40
    )
}
if (nrow(stai_tshpd) > 0) {
  stai_tshpd <- stai_tshpd |> filter(Finished == 1) |> rename(stai_state = SC0, stai_trait = SC1, stai = SC2)
}
if (nrow(stai_tshpe) > 0) {
  stai_tshpe <- stai_tshpe |> filter(Finished == 1) |> rename(stai_state = SC0, stai_trait = SC1, stai = SC2)
}

stai <- bind_rows(stai_tshpg, stai_tshpd, stai_tshpe)

#### STEP 2: COERCE AND DATE ----
if (ncol(stai) > 0) {
  stai <- stai |>
    mutate(across(any_of(c("stai_state", "stai_trait", "stai", paste0("SAI", 1:20), paste0("TAI", 1:20))), as.numeric)) |>
    mutate(date_recorded = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = stai_date_fmt))
}

#### STEP 3: SUM SCORES AND CHECK ----
# Reversed items already flipped in data; sum raw items and flag mismatches vs Qualtrics totals.
if (ncol(stai) > 0 && "SAI1" %in% names(stai)) {
  stai <- stai |>
    mutate(
      stai_state_sum = rowSums(pick(any_of(paste0("SAI", 1:20))), na.rm = FALSE),
      stai_trait_sum = rowSums(pick(any_of(paste0("TAI", 1:20))), na.rm = FALSE),
      stai_sum = stai_state_sum + stai_trait_sum
    )
  mism <- stai |> filter(
    (!is.na(stai) & !is.na(stai_sum) & stai != stai_sum) |
    (!is.na(stai_state) & !is.na(stai_state_sum) & stai_state != stai_state_sum) |
    (!is.na(stai_trait) & !is.na(stai_trait_sum) & stai_trait != stai_trait_sum)
  )
  if (nrow(mism) > 0) {
    message("STAI: raw total(s) differ from item sum(s) for ", nrow(mism), " row(s). subjectid: ", paste(mism$subjectid, collapse = ", "))
  }
}

#### STEP 4: ONE ROW PER SUBJECTID ----
# Keep first row that has item data; order by date.
stai_item_cols <- c(paste0("SAI", 1:20), paste0("TAI", 1:20))
if (ncol(stai) > 0) {
  stai <- stai |>
    mutate(.has_data = rowSums(!is.na(pick(any_of(stai_item_cols)))) > 0) |>
    arrange(subjectid, desc(.has_data), date_recorded) |>
    group_by(subjectid) |>
    slice(1) |>
    ungroup() |>
    select(-.has_data)
}

#### STEP 5: EXPORT ----
# Column order: ids, date, totals and sums, items. Add * to reversed-item colnames at the end.
stai <- stai |>
  select(
    subjectid, date_recorded,
    stai_state, stai_trait, stai,
    any_of(paste0("SAI", 1:20)), any_of(paste0("TAI", 1:20))
  ) |>
  rename_with(~ paste0(., "*"), any_of(paste0("SAI", sai_reversed))) |>
  rename_with(~ paste0(., "*"), any_of(paste0("TAI", tai_reversed)))

dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
save(stai, file = "data/raw_data/stai.Rdata")
tryCatch(
  write_xlsx(stai, path = "data/raw_data/stai.xlsx"),
  error = function(e) warning("Could not write stai.xlsx (close file if open): ", conditionMessage(e))
)
