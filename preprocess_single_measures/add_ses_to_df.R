# Add city_eng, ses_rank, and ses_cluster from CBS 2006 SES data to a dataframe
# by matching df$City1 to ses$city_eng. Uses a vectorized join (no loop).
#
# Usage:
#   source("preprocess_single_measures/add_ses_to_df.R")
#   add_ses_to_df(ses_path = "data/ses_cbs_2006.csv", df_path = "df.csv")
#   # In-memory by Hebrew city (for HP1 etc.):
#   df <- add_ses_to_df_by_hebrew(df, city_hebrew_col = "place_of_residence_until12yo")

library(dplyr)

# Load SES from CSV or XLS
load_ses <- function(ses_path) {
  ext <- tolower(tools::file_ext(ses_path))
  if (ext == "xls" || ext == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl required for xls/xlsx")
    ses <- readxl::read_excel(ses_path)
  } else {
    ses <- read.csv(ses_path, fileEncoding = "UTF-8")
  }
  ses
}

add_ses_to_df <- function(ses_path = "data/ses_cbs_2006.csv", df_path = "df.csv") {
  ses <- load_ses(ses_path)
  df  <- read.csv(df_path)

  # One row per city (avoid duplicating df rows if ses has duplicates)
  ses_lookup <- ses %>%
    select(city_eng, RANK, cluster) %>%
    distinct(city_eng, .keep_all = TRUE) %>%
    rename(ses_rank = RANK, ses_cluster = cluster)

  df <- df %>%
    left_join(ses_lookup, by = c("City1" = "city_eng"))

  # Add city_eng: matched rows get the standard name (same as City1); unmatched get NA
  df$city_eng <- ifelse(!is.na(df$ses_rank), df$City1, NA_character_)

  write.csv(df, file = df_path, row.names = FALSE)
  invisible(df)
}

# In-memory: add city_hebrew, city_english, SES_rank, SES_cluster by matching
# a Hebrew city column to ses$city_heb. Unmatched -> city_english = "CHECK".
add_ses_to_df_by_hebrew <- function(df, city_hebrew_col = "place_of_residence_until12yo",
                                   ses_path = "data/ses_cbs_2006.xls") {
  city_sym <- rlang::sym(city_hebrew_col)
  if (!file.exists(ses_path)) {
    return(dplyr::mutate(df, city_hebrew = !!city_sym, city_english = "CHECK", SES_rank = NA_real_, SES_cluster = NA_character_))
  }
  ses <- load_ses(ses_path)
  # Expect columns city_heb, city_eng, RANK, cluster (xls has city_heb)
  ses_lookup <- ses %>%
    select(city_heb, city_eng, RANK, cluster) %>%
    distinct(city_heb, .keep_all = TRUE)
  df %>%
    left_join(ses_lookup, by = setNames("city_heb", city_hebrew_col)) %>%
    mutate(
      city_hebrew = !!city_sym,
      city_english = if_else(is.na(RANK), "CHECK", as.character(city_eng)),
      SES_rank = RANK,
      SES_cluster = as.character(cluster)
    ) %>%
    select(-city_eng, -RANK, -cluster)
}
