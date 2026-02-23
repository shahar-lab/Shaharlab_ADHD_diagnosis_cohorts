# Helper: parse RecordedDate and assign cohort for תשפה (before 2025-09-01) vs תשפו (from 2025-09-01)
# Source from create_raw_*.R when cohort_fixed is NULL

library(lubridate)

cohort_from_date <- function(date_recorded, cutoff = "2025-09-01") {
  if (inherits(date_recorded, "POSIXct")) {
    dt <- date_recorded
  } else {
    x <- as.character(date_recorded)
    dt <- parse_date_time(x, orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM", "mdy HMS", "mdy HM", "ymd", "dmy"), quiet = TRUE)
    dt <- as.POSIXct(dt, tz = "UTC")
  }
  if_else(as.Date(dt) < as.Date(cutoff), "תשפה", "תשפו")
}
