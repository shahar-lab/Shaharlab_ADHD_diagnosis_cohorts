# Load notion data from collected_data and standardize subject id.
# Uses same subjectid convention as other preprocess scripts.

library(dplyr)

notion_path <- "data/collected_data/notion.csv"
notion <- read.csv(notion_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Standardize ID column and rename group-by-referral to declared_group
notion <- notion |>
  rename(subjectid = shahar_id, declared_group = `גיוס_קבוצה_לפי_פנייה`,
  source_of_referral = `גיוס_מקור_ההפנייה`,
  psych_treatment = `ראיון_טיפול_פסיכיאטרי_.כן.לא.`,
  psych_treatment_type = `ראיון_טיפול_פסיכיאטרי_סוג`,
  adhd_treatment_medication = `ראיון_תרופת_קשב_.כן.לא.`,
  adhd_treatment_medication_notes = `ראיון_תרופת_קשב_הערות`,
  adhd_treatment_medication_type = `ראיון_תרופת_קשב_סוג`) |>
  select(subjectid, declared_group, source_of_referral, psych_treatment, psych_treatment_type, adhd_treatment_medication, adhd_treatment_medication_notes, adhd_treatment_medication_type)

View(notion)
save(notion, file = "data/raw_data/notion.Rdata")
