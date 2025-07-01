library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)

#### CREATE ICAR RAW ####

#load 
icar <- read_tsv("data/תשפה/collected_data/ICAR16_תשפה_July+1,+2025_11.26.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
icar <- icar |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  rename(subjectid = shahar_id) |>
  filter(
    nchar(subjectid) == 6 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)
  ) |>
  
  # rename survey data
  mutate (cohort = "תשפה") |>
  rename(date_recorded      = RecordedDate) |>   
  
  #rename icar sumscore
  rename(icar = 'SC0') |> filter(!is.na(icar)) |> mutate() |>
  
  #filter out cases that did not finsh the icar
  filter(Finished == 1) |>
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         icar,
         ICAR_ln1, ICAR_ln2, ICAR_ln3, ICAR_ln4,
         ICAR_mx1, ICAR_mx2, ICAR_mx3, ICAR_mx4,
         ICAR_vr1, ICAR_vr2, ICAR_vr3, ICAR_vr4,
         ICAR_r3d1, ICAR_r3d2, ICAR_r3d3, ICAR_r3d4)



#### ENSURING CLASS OF COLUMNS ----

icar <- icar |>
  
  mutate(across(c(icar, 
                  ICAR_ln1, ICAR_ln2, ICAR_ln3, ICAR_ln4,
                  ICAR_mx1, ICAR_mx2, ICAR_mx3, ICAR_mx4,
                  ICAR_vr1, ICAR_vr2, ICAR_vr3, ICAR_vr4,
                  ICAR_r3d1, ICAR_r3d2, ICAR_r3d3, ICAR_r3d4),
                as.numeric))



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
dup_count <- icar |>
  count(subjectid) |>
  filter(n > 1) |>
  summarise(subjectid_duplicate_count = n())

print(dup_count)

icar |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()

# 2. Retain only the first line for those how complete the whole thing more then once
icar = icar |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()

# 3. Validate sumscore
icar_sum_check = icar |> 
  mutate(icar_sum = rowSums(across(starts_with("ICAR_")))) |> 
  filter(icar_sum != icar)

print(nrow(icar_sum_check))

# 4. Hist of duration
icar |> 
  ggplot(aes(x = icar)) + 
  geom_dots() + labs(x = "ICAR Score") + theme_minimal()

#### SAVE ----
save(icar, file = 'data/תשפה/raw_data/icar.Rdata')
