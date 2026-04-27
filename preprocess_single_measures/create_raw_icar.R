library(readr)
library(dplyr)
library(writexl)

#### CREATE ICAR RAW ####

icar_cols <- c("ICAR_ln1", "ICAR_ln2", "ICAR_ln3", "ICAR_ln4", "ICAR_mx1", "ICAR_mx2", "ICAR_mx3", "ICAR_mx4", "ICAR_vr1", "ICAR_vr2", "ICAR_vr3", "ICAR_vr4", "ICAR_r3d1", "ICAR_r3d2", "ICAR_r3d3", "ICAR_r3d4")

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

icar_tshpg <- if (file.exists("data/collected_data/תשפג/ICAR16+Hebrew+-+Copy_October+1,+2025_10.03_values.tsv")) {
  read_tsv("data/collected_data/תשפג/ICAR16+Hebrew+-+Copy_October+1,+2025_10.03_values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

icar_tshpd <- if (file.exists("data/collected_data/תשפד/ICAR16_July+1,+2025_11.17.tsv")) {
  read_tsv("data/collected_data/תשפד/ICAR16_July+1,+2025_11.17.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 8, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

icar_tshpe <- if (file.exists("data/collected_data/תשפה_תשפו/ICAR16_תשפה - values.tsv")) {
  read_tsv("data/collected_data/תשפה_תשפו/ICAR16_תשפה - values.tsv", locale = locale(encoding = "UTF-16")) |>
    slice(-1, -2) |>
    rename(subjectid = shahar_id, date_recorded = RecordedDate) |>
    filter(nchar(subjectid) == 6, grepl("^[A-Za-z0-9]+$", subjectid), !grepl("example", subjectid, ignore.case = TRUE))
} else { tibble() }

icar <- bind_rows(icar_tshpg, icar_tshpd, icar_tshpe)

#### STEP 2: DATE ----
icar <- icar |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 3: HANDLE ITEM RATINGS AND SCORES ----
icar <- icar |>
  filter(Finished == 1) |>
  rename(icar = SC0)

if ("ICAR_lmx2" %in% names(icar) && "ICAR_mx2" %in% names(icar)) {
  icar <- icar |> mutate(ICAR_mx2 = coalesce(ICAR_mx2, ICAR_lmx2))
} else if ("ICAR_lmx2" %in% names(icar) && !("ICAR_mx2" %in% names(icar))) {
  icar <- icar |> rename(ICAR_mx2 = ICAR_lmx2)
}

icar <- icar |>
  mutate(across(c(icar, all_of(icar_cols)), as.numeric)) |>
  mutate(icar_sum = rowSums(pick(all_of(icar_cols)), na.rm = FALSE))

# Flag inconsistency
icar_mismatch <- icar |> filter(!is.na(icar) & !is.na(icar_sum) & icar != icar_sum)
if (nrow(icar_mismatch) > 0) {
  message("ICAR: raw total (icar) differs from item sum (icar_sum) for ", nrow(icar_mismatch), " row(s). subjectid: ", paste(icar_mismatch$subjectid, collapse = ", "))
}

#### STEP 4: HANDLE DUPLICATE SUBJECTID ----
icar <- icar |>
  mutate(
    .has_data = rowSums(!is.na(pick(all_of(icar_cols)))) > 0,
    .date_order = as.POSIXct(as.character(date_recorded), tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p"))
  ) |>
  arrange(subjectid, desc(.has_data), .date_order) |>
  group_by(subjectid) |>
  slice(1) |>
  ungroup() |>
  select(-.has_data, -.date_order)

#### STEP 5: COLUMN ORDER AND EXPORT ----
icar <- icar |>
  select(subjectid, date_recorded, icar, icar_sum, all_of(icar_cols))

dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
save(icar, file = "data/raw_data/icar.Rdata")
tryCatch(
  write_xlsx(icar, path = "data/raw_data/icar.xlsx"),
  error = function(e) warning("Could not write icar.xlsx (close file if open): ", conditionMessage(e))
)
