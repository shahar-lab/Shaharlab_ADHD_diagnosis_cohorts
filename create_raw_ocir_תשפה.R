library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE PATAS RAW ####

#load 
ocir <- read_tsv("data/תשפה/collected_data/OCI-R_תשפה_July+1,+2025_11.28.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
ocir <- ocir |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  rename(subjectid = shahar_id) |>
  filter(
    nchar(subjectid) == 6 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)
  ) |>
  
  #  survey data
  mutate (cohort = "תשפה") |>
  rename(date_recorded      = RecordedDate) |>   
  
  
  #rename asrs 
  rename(ocir_hoarding = 'SC0', 
         ocir_checking = 'SC1',
         ocir_ordering = 'SC2',
         ocir_neutralizing = 'SC3',
         ocir_washing = 'SC4',
         ocir_obsessing = 'SC5',
         ocir = 'SC6') |> 
  
  rename_with(.cols = matches("^OCI-R\\d+$"), .fn = ~ paste0("ocir", seq_along(.))) |>
  
  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  filter(!is.na(ocir), !is.na(ocir1)) |> 
  
  
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         ocir, ocir_hoarding, ocir_checking, ocir_ordering, ocir_neutralizing, ocir_washing, ocir_obsessing,
         ocir1,ocir2,ocir3,ocir4,ocir5,ocir6,
         ocir7,ocir8,ocir9,ocir10,ocir11,ocir12,
         ocir13,ocir14,ocir15,ocir16,ocir17,ocir18)




#### ENSURING CLASS OF COLUMNS ----

ocir <- ocir |>
  
  mutate(across(c(ocir, ocir_hoarding, ocir_checking, ocir_ordering, ocir_neutralizing, ocir_washing, ocir_obsessing,
                  ocir1,ocir2,ocir3,ocir4,ocir5,ocir6,
                  ocir7,ocir8,ocir9,ocir10,ocir11,ocir12,
                  ocir13,ocir14,ocir15,ocir16,ocir17,ocir18),
                as.numeric))



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
dup_count <- ocir |>
  count(subjectid) |>
  filter(n > 1) |>
  summarise(subjectid_duplicate_count = n())

print(dup_count)

#ocir |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()

# 2. Retain only the first line for those how complete the whole thing more then once
ocir = ocir |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()


# 2. Validate sumscore
#note that reversed items where already revered in qualtrics
sum_check = ocir |> 
  mutate(sum_ocir    = ocir1 + ocir2  + ocir3 + ocir4 + ocir5 + ocir6 + 
           ocir7 + ocir8 + ocir9 + ocir10 + ocir11 + ocir12 + 
           ocir13 + ocir14 + ocir15 + ocir16 + ocir17 + ocir18) |>
  filter(ocir != sum_ocir )

print(nrow(sum_check))

# 4. Hist of sumscore
ocir |> 
  ggplot(aes(x = ocir)) + 
  geom_dots() + labs(x = "ocir Score") + theme_minimal()

#### SAVE ----
save(ocir, file = 'data/תשפה/raw_data/ocir.Rdata')
    