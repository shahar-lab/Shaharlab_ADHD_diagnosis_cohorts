library(readr)
library(dplyr)
library(writexl)

#### CREATE AQ RAW ####

#### STEP 1: LOAD ----
# Validate shaharID and date_recorded (keep cohort-specific checks)

aq_tshpg <- if (file.exists("data/collected_data/תשפג/AQ+template+-+עברית+-+Copy_October+1,+2025_10.01_values.tsv")) {
  read_tsv("data/collected_data/תשפג/AQ+template+-+עברית+-+Copy_October+1,+2025_10.01_values.tsv", locale = locale(encoding = "UTF-16")) |>
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

aq_tshpd <- if (file.exists("data/collected_data/תשפד/AQ_July+9,+2025_11.56.tsv")) {
  read_tsv("data/collected_data/תשפד/AQ_July+9,+2025_11.56.tsv", locale = locale(encoding = "UTF-16")) |>
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

aq_tshpe <- if (file.exists("data/collected_data/תשפה_תשפו/AQ_תשפה-values.tsv")) {
  read_tsv("data/collected_data/תשפה_תשפו/AQ_תשפה-values.tsv", locale = locale(encoding = "UTF-16")) |>
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

aq <- bind_rows(aq_tshpg, aq_tshpd, aq_tshpe)

#### STEP 2: DATE ----
# Keep date_recorded as a proper date type

aq <- aq |>
  mutate(
    date_recorded = as.POSIXct(
      as.character(date_recorded),
      tz = "UTC",
      tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y/%m/%d %H:%M:%OS", "%d/%m/%Y %H:%M:%OS", "%d/%m/%Y %H:%M", "%m/%d/%Y %I:%M:%OS %p")
    )
  )

#### STEP 3: HANDLE ITEM RATINGS AND SCORES ----
# Keep finished responses; SC0 is the AQ total (aq). Items already reverse-coded in data: mark with * in name.

aq <- aq |>
  filter(Finished == 1) |>
  rename(aq = SC0) |>
  rename_with(.cols = matches("^AQ\\d+$"), .fn = ~ paste0("aq", seq_along(.))) |>
  mutate(across(c(aq, aq1:aq50), as.numeric))

reverse_item_numbers <- c(1, 3, 8, 10, 11, 14, 15, 17, 24, 25, 27, 28, 29, 30, 31, 32, 34, 36, 37, 38, 40, 44, 47, 48, 49, 50)
reverse_items <- paste0("aq", reverse_item_numbers)

aq <- aq |>
  rename_with(.fn = ~ paste0(., "*"), .cols = all_of(reverse_items))

aq_item_names <- ifelse(1:50 %in% reverse_item_numbers, paste0("aq", 1:50, "*"), paste0("aq", 1:50))

# Subscales (Baron-Cohen et al.)
social_comm_items      <- c(1, 11, 13, 15, 22, 36, 44, 45, 47, 48)
atten_switching_items  <- c(2, 4, 10, 16, 25, 32, 34, 37, 43, 46)
atten_to_details_items <- c(5, 6, 9, 12, 19, 23, 28, 29, 30, 49)
communication_items    <- c(7, 17, 18, 26, 27, 31, 33, 35, 38, 39)
imagination_items      <- c(3, 8, 14, 20, 21, 24, 40, 41, 42, 50)

aq <- aq |>
  mutate(
    aq_sum = rowSums(pick(all_of(aq_item_names)), na.rm = FALSE),
    social_comm     = rowSums(pick(all_of(aq_item_names[social_comm_items])), na.rm = FALSE),
    atten_switching = rowSums(pick(all_of(aq_item_names[atten_switching_items])), na.rm = FALSE),
    atten_to_details = rowSums(pick(all_of(aq_item_names[atten_to_details_items])), na.rm = FALSE),
    communication   = rowSums(pick(all_of(aq_item_names[communication_items])), na.rm = FALSE),
    imagination     = rowSums(pick(all_of(aq_item_names[imagination_items])), na.rm = FALSE)
  )

# Flag inconsistency between raw total (aq) and computed sum (aq_sum)
aq_mismatch <- aq |> filter(!is.na(aq) & !is.na(aq_sum) & aq != aq_sum)
if (nrow(aq_mismatch) > 0) {
  message("AQ: raw total (aq) differs from item sum (aq_sum) for ", nrow(aq_mismatch), " row(s). subjectid: ", paste(aq_mismatch$subjectid, collapse = ", "))
}

#### STEP 4: HANDLE DUPLICATE SUBJECTID ----
# Keep first entry that has data (inline: no custom function)

item_cols <- aq_item_names
aq <- aq |>
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
# subjectid, date_recorded, sum/subscales, then items. Same content to .Rdata and Excel.

aq <- aq |>
  select(
    subjectid,
    date_recorded,
    aq,
    social_comm, atten_switching, atten_to_details, communication, imagination,
    aq_sum,
    all_of(aq_item_names)
  )

dir.create("data/raw_data", showWarnings = FALSE, recursive = TRUE)
save(aq, file = "data/raw_data/aq.Rdata")
tryCatch(
  write_xlsx(aq, path = "data/raw_data/aq.xlsx"),
  error = function(e) warning("Could not write aq.xlsx (close file if open): ", conditionMessage(e))
)
