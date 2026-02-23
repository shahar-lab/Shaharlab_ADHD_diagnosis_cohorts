library(readr)
library(dplyr)
library(writexl)

#### CREATE BDI RAW ####

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

bdi_tshpg <- if (file.exists("data/תשפג/collected_data/BDI+-+עברית+template+-+Copy_October+1,+2025_09.59_values.tsv")) {
  read_tsv("data/תשפג/collected_data/BDI+-+עברית+template+-+Copy_October+1,+2025_09.59_values.tsv", locale = locale(encoding = "UTF-16")) |>
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

bdi_tshpd <- if (file.exists("data/תשפד/collected_data/BDI_July+1,+2025_10.58.tsv")) {
  read_tsv("data/תשפד/collected_data/BDI_July+1,+2025_10.58.tsv", locale = locale(encoding = "UTF-16")) |>
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

bdi_tshpe <- if (file.exists("data/תשפה/collected_data/BDI_תשפה-values.tsv")) {
  read_tsv("data/תשפה/collected_data/BDI_תשפה-values.tsv", locale = locale(encoding = "UTF-16")) |>
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

bdi <- bind_rows(bdi_tshpg, bdi_tshpd, bdi_tshpe)

#### STEP 2: DATE ----
bdi <- bdi |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 3: HANDLE ITEM RATINGS AND SCORES ----
# bdi from SC0 (Qualtrics total). No reversed items.

bdi <- bdi |>
  filter(Finished == 1) |>
  rename(bdi = SC0) |>
  mutate(across(c(bdi, BDI1:BDI21), as.numeric)) |>
  mutate(bdi_sum = rowSums(across(BDI1:BDI21), na.rm = FALSE))

# Flag inconsistency
bdi_mismatch <- bdi |> filter(!is.na(bdi) & !is.na(bdi_sum) & bdi != bdi_sum)
if (nrow(bdi_mismatch) > 0) {
  message("BDI: raw total (bdi) differs from item sum (bdi_sum) for ", nrow(bdi_mismatch), " row(s). subjectid: ", paste(bdi_mismatch$subjectid, collapse = ", "))
}

#### STEP 4: HANDLE DUPLICATE SUBJECTID ----
item_cols <- paste0("BDI", 1:21)
bdi <- bdi |>
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
bdi <- bdi |>
  select(subjectid, date_recorded, bdi, bdi_sum, BDI1:BDI21)

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(bdi, file = "data/all_cohorts_raw_data/bdi.Rdata")
tryCatch(
  write_xlsx(bdi, path = "data/all_cohorts_raw_data/bdi.xlsx"),
  error = function(e) warning("Could not write bdi.xlsx (close file if open): ", conditionMessage(e))
)
