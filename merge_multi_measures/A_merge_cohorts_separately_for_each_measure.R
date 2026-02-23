# A: Source preprocess scripts, then column-bind by subjectid into one df

library(dplyr)
options(readr.show_progress = FALSE, readr.show_col_types = FALSE)

#### Source single measure scripts (silent) ----
invisible(suppressMessages(suppressWarnings({
  source("preprocess_single_measures/parse_cohort_date.R")
  source("preprocess_single_measures/create_raw_hp1.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_diva.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_asrs.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_WURS.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_bdi.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_STAI.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_AQ.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_icar.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_ocir.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_patas.R", echo = FALSE)
  source("preprocess_single_measures/create_raw_PQB.R", echo = FALSE)
})))

#### Column-bind by subjectid (one df) ----
dedup_by_subject <- function(df) {
  if (nrow(df) == 0) return(df)
  if ("date_recorded" %in% names(df)) {
    df |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()
  } else {
    df |> group_by(subjectid) |> slice_head(n = 1) |> ungroup()
  }
}

drop_from_join <- c("cohort", "date_recorded")

diva <- diva |> mutate(
  diva_group = case_when(
    diva_diagnosis == "meet_diva_criteria"  ~ "ADHD",
    diva_diagnosis == "below_diva_criteria" ~ "TD",
    TRUE ~ NA_character_
  ) |> factor(levels = c("TD", "ADHD"))
)

df_agg <- hp1 |> dedup_by_subject() |>
  left_join(diva |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_diva")) |>
  left_join(asrs |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_asrs")) |>
  left_join(wurs |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_wurs")) |>
  left_join(bdi |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_bdi")) |>
  left_join(stai |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_stai")) |>
  left_join(aq |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_aq")) |>
  left_join(icar |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_icar")) |>
  left_join(ocir |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_ocir"))

if (nrow(patas) > 0) df_agg <- df_agg |> left_join(patas |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_patas"))
if (nrow(pqb) > 0) df_agg <- df_agg |> left_join(pqb |> dedup_by_subject() |> select(-any_of(drop_from_join)), by = "subjectid", suffix = c("", "_pqb"))

dir.create("data/all_cohorts_raw_data", showWarnings = FALSE, recursive = TRUE)
save(df_agg, file = "data/all_cohorts_raw_data/df_agg.Rdata")

