library(readr)
library(dplyr)
library(writexl)

#### CREATE PQB RAW ####

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

pqb <- if (file.exists("data/תשפד/collected_data/PQ-B_July+1,+2025_11.21.tsv")) {
  read_tsv("data/תשפד/collected_data/PQ-B_July+1,+2025_11.21.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else {
  tibble()
}

#### STEP 2: DATE ----
pqb <- pqb |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 3: HANDLE SCORES AND ITEMS ----
# Keep finished responses; pqb = SC0 (total), pqb_distress = SC1 (distress). PQ-B has 21 items: PQ-B1..PQ-B21 (frequency), PQ-B1.1..PQ-B21.1 (distress).

pqb <- pqb |>
  filter(Finished == 1) |>
  rename(pqb = SC0, pqb_distress = SC1)

# Rename item columns to pqb1:pqb21 and pqb1_d:pqb21_d (distress)
pqb_freq_cols <- names(pqb)[grepl("^PQ-B\\d+$", names(pqb))]
pqb_freq_cols <- pqb_freq_cols[order(readr::parse_number(pqb_freq_cols))]
pqb_dist_cols <- names(pqb)[grepl("^PQ-B\\d+\\.1$", names(pqb))]
pqb_dist_cols <- pqb_dist_cols[order(readr::parse_number(pqb_dist_cols))]
if (length(pqb_freq_cols) == 21) {
  pqb <- pqb |> rename(!!!setNames(pqb_freq_cols, paste0("pqb", 1:21)))
}
if (length(pqb_dist_cols) == 21) {
  pqb <- pqb |> rename(!!!setNames(pqb_dist_cols, paste0("pqb", 1:21, "_d")))
}

pqb_item_cols <- c(paste0("pqb", 1:21), paste0("pqb", 1:21, "_d"))
pqb_item_cols <- intersect(pqb_item_cols, names(pqb))
pqb_dist_item_cols <- intersect(paste0("pqb", 1:21, "_d"), names(pqb))
pqb <- pqb |>
  mutate(across(c(pqb, pqb_distress, any_of(pqb_item_cols)), as.numeric)) |>
  mutate(across(any_of(pqb_dist_item_cols), ~ na_if(., 0)))

#### STEP 4: HANDLE DUPLICATE SUBJECTID ----
pqb <- pqb |>
  mutate(
    .has_data = if (length(pqb_item_cols) > 0) {
      !is.na(pqb) | !is.na(pqb_distress) | rowSums(!is.na(pick(all_of(pqb_item_cols)))) > 0
    } else {
      !is.na(pqb) | !is.na(pqb_distress)
    },
    .date_order = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p"))
  ) |>
  arrange(subjectid, desc(.has_data), .date_order) |>
  group_by(subjectid) |>
  slice(1) |>
  ungroup() |>
  select(-.has_data, -.date_order)

#### STEP 5: COLUMN ORDER AND EXPORT ----
pqb <- pqb |>
  select(subjectid, date_recorded, pqb, pqb_distress, any_of(paste0("pqb", 1:21)), any_of(paste0("pqb", 1:21, "_d")))

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(pqb, file = "data/all_cohorts_raw_data/pqb.Rdata")
tryCatch(
  write_xlsx(pqb, path = "data/all_cohorts_raw_data/pqb.xlsx"),
  error = function(e) warning("Could not write pqb.xlsx (close file if open): ", conditionMessage(e))
)
