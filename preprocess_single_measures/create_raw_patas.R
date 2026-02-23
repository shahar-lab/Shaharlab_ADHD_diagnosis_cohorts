library(readr)
library(dplyr)
library(writexl)

#### CREATE PATAS RAW ####

# Reversed items: 10 and 12 (already reverse-coded in raw data; mark with * in name only)
patas_reversed_items <- c(10, 12)

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

patas_tshpd <- if (file.exists("data/תשפד/collected_data/PATAS_July+1,+2025_11.20.tsv")) {
  read_tsv("data/תשפד/collected_data/PATAS_July+1,+2025_11.20.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

patas_tshpe <- if (file.exists("data/תשפה/collected_data/PATAS_תשפה-values.tsv")) {
  read_tsv("data/תשפה/collected_data/PATAS_תשפה-values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(subjectid = shahar_id, date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 6, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

patas <- bind_rows(patas_tshpd, patas_tshpe)

#### STEP 2: DATE ----
patas <- patas |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 3: HANDLE ITEM RATINGS AND SCORES ----
# Keep finished responses; harmonize item names. Items 10 and 12 already reversed in data; mark with *.

patas <- patas |>
  filter(Finished == 1) |>
  rename_with(.cols = matches("^PATAS\\d+$"), .fn = ~ paste0("patas", seq_along(.))) |>
  rename(patas = SC0) |>
  mutate(across(c(patas, patas1:patas12), as.numeric)) |>
  mutate(patas_sum = rowSums(pick(patas1:patas12), na.rm = FALSE)) |>
  rename_with(.fn = ~ paste0(., "*"), .cols = all_of(paste0("patas", patas_reversed_items)))

patas_item_names <- ifelse(1:12 %in% patas_reversed_items, paste0("patas", 1:12, "*"), paste0("patas", 1:12))

# Flag inconsistency between raw total (patas) and item sum
patas_mismatch <- patas |> filter(!is.na(patas) & !is.na(patas_sum) & patas != patas_sum)
if (nrow(patas_mismatch) > 0) {
  message("PATAS: raw total (patas) differs from item sum (patas_sum) for ", nrow(patas_mismatch), " row(s). subjectid: ", paste(patas_mismatch$subjectid, collapse = ", "))
}

#### STEP 4: HANDLE DUPLICATE SUBJECTID ----
patas <- patas |>
  mutate(
    .has_data = rowSums(!is.na(pick(all_of(patas_item_names)))) > 0,
    .date_order = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p"))
  ) |>
  arrange(subjectid, desc(.has_data), .date_order) |>
  group_by(subjectid) |>
  slice(1) |>
  ungroup() |>
  select(-.has_data, -.date_order)

#### STEP 5: COLUMN ORDER AND EXPORT ----
# subjectid, date_recorded, total and sum, then items. Same to .Rdata and Excel.

patas <- patas |>
  select(subjectid, date_recorded, patas_sum, all_of(patas_item_names))

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(patas, file = "data/all_cohorts_raw_data/patas.Rdata")
tryCatch(
  write_xlsx(patas, path = "data/all_cohorts_raw_data/patas.xlsx"),
  error = function(e) warning("Could not write patas.xlsx (close file if open): ", conditionMessage(e))
)
