library(readr)
library(dplyr)
library(writexl)

#### CREATE WURS RAW ####

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

wurs_tshpg <- if (file.exists("data/תשפג/collected_data/WURS+-+עברית+-+template+-+Copy_October+1,+2025_10.06_values.tsv")) {
  read_tsv("data/תשפג/collected_data/WURS+-+עברית+-+template+-+Copy_October+1,+2025_10.06_values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

wurs_tshpd <- if (file.exists("data/תשפד/collected_data/WURS_July+1,+2025_11.23.tsv")) {
  read_tsv("data/תשפד/collected_data/WURS_July+1,+2025_11.23.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

wurs_tshpe <- if (file.exists("data/תשפה/collected_data/WURS_תשפה - values.tsv")) {
  read_tsv("data/תשפה/collected_data/WURS_תשפה - values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(subjectid = shahar_id, date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 6, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

#### STEP 2: HANDLE ITEM NAMES (cohort-specific) ----
# תשפג: template uses different WURS item numbers; map to wurs1:wurs25, then drop only those source columns.
# תשפד/תשפה: columns are WURS1..WURS25; rename to wurs1..wurs25 (one-to-one so values are preserved).

# Template source columns we copy from (drop only these after copying, so we don't remove any other columns)
wurs_tshpg_src <- c("WURS3", "WURS4", "WURS5", "WURS6", "WURS7", "WURS9", "WURS10", "WURS11", "WURS12",
  "WURS15", "WURS16", "WURS17", "WURS20", "WURS21", "WURS24", "WURS25", "WURS26", "WURS27", "WURS28", "WURS29",
  "WURS40", "WURS41", "WURS51", "WURS56", "WURS59")

if (nrow(wurs_tshpg) > 0) {
  wurs_tshpg <- wurs_tshpg |>
    filter(Finished == 1) |>
    rename(wurs = SC0) |>
    mutate(
      wurs1 = WURS3, wurs2 = WURS4, wurs3 = WURS5, wurs4 = WURS6, wurs5 = WURS7,
      wurs6 = WURS9, wurs7 = WURS10, wurs8 = WURS11, wurs9 = WURS12, wurs10 = WURS15,
      wurs11 = WURS16, wurs12 = WURS17, wurs13 = WURS20, wurs14 = WURS21, wurs15 = WURS24,
      wurs16 = WURS25, wurs17 = WURS26, wurs18 = WURS27, wurs19 = WURS28, wurs20 = WURS29,
      wurs21 = WURS40, wurs22 = WURS41, wurs23 = WURS51, wurs24 = WURS56, wurs25 = WURS59
    ) |>
    select(-any_of(wurs_tshpg_src))
}

if (nrow(wurs_tshpd) > 0) {
  wurs_tshpd <- wurs_tshpd |>
    filter(Finished == 1) |>
    rename(wurs = SC0) |>
    rename(
      wurs1 = WURS1, wurs2 = WURS2, wurs3 = WURS3, wurs4 = WURS4, wurs5 = WURS5,
      wurs6 = WURS6, wurs7 = WURS7, wurs8 = WURS8, wurs9 = WURS9, wurs10 = WURS10,
      wurs11 = WURS11, wurs12 = WURS12, wurs13 = WURS13, wurs14 = WURS14, wurs15 = WURS15,
      wurs16 = WURS16, wurs17 = WURS17, wurs18 = WURS18, wurs19 = WURS19, wurs20 = WURS20,
      wurs21 = WURS21, wurs22 = WURS22, wurs23 = WURS23, wurs24 = WURS24, wurs25 = WURS25
    )
}

if (nrow(wurs_tshpe) > 0) {
  wurs_tshpe <- wurs_tshpe |>
    filter(Finished == 1) |>
    rename(wurs = SC0) |>
    rename(
      wurs1 = WURS1, wurs2 = WURS2, wurs3 = WURS3, wurs4 = WURS4, wurs5 = WURS5,
      wurs6 = WURS6, wurs7 = WURS7, wurs8 = WURS8, wurs9 = WURS9, wurs10 = WURS10,
      wurs11 = WURS11, wurs12 = WURS12, wurs13 = WURS13, wurs14 = WURS14, wurs15 = WURS15,
      wurs16 = WURS16, wurs17 = WURS17, wurs18 = WURS18, wurs19 = WURS19, wurs20 = WURS20,
      wurs21 = WURS21, wurs22 = WURS22, wurs23 = WURS23, wurs24 = WURS24, wurs25 = WURS25
    )
}

wurs <- bind_rows(wurs_tshpg, wurs_tshpd, wurs_tshpe)
if (ncol(wurs) > 0) {
  wurs <- wurs |> mutate(across(any_of(c("wurs", paste0("wurs", 1:25))), as.numeric))
}

#### STEP 3: DATE ----
wurs <- wurs |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 4: COMPUTE SUM FROM ITEMS AND FLAG INCONSISTENCY ----
# Items: keep as-is (missing stays NA). Sum: only defined when all 25 items are present; if any item is NA, wurs_sum = NA.
wurs <- wurs |>
  mutate(wurs_sum = rowSums(pick(wurs1:wurs25), na.rm = FALSE))

wurs_mismatch <- wurs |> filter(!is.na(wurs) & !is.na(wurs_sum) & wurs != wurs_sum)
if (nrow(wurs_mismatch) > 0) {
  message("WURS: raw total (wurs) differs from item sum (wurs_sum) for ", nrow(wurs_mismatch), " row(s). subjectid: ", paste(wurs_mismatch$subjectid, collapse = ", "))
}

#### STEP 5: HANDLE DUPLICATE SUBJECTID ----
wurs_item_cols <- paste0("wurs", 1:25)
wurs <- wurs |>
  mutate(
    .has_data = rowSums(!is.na(pick(all_of(wurs_item_cols)))) > 0,
    .date_order = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p"))
  ) |>
  arrange(subjectid, desc(.has_data), .date_order) |>
  group_by(subjectid) |>
  slice(1) |>
  ungroup() |>
  select(-.has_data, -.date_order)

#### STEP 6: COLUMN ORDER AND EXPORT ----
wurs <- wurs |>
  select(subjectid, date_recorded, wurs, wurs_sum, wurs1:wurs25)

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(wurs, file = "data/all_cohorts_raw_data/wurs.Rdata")
tryCatch(
  write_xlsx(wurs, path = "data/all_cohorts_raw_data/wurs.xlsx"),
  error = function(e) warning("Could not write wurs.xlsx (close file if open): ", conditionMessage(e))
)
