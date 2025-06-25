library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE PATAS RAW ####

#load 
patas <- read_tsv("data/תשפד/collected_data/PATAS_June+1,+2025_11.51_values.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
patas <- patas |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  filter(
    nchar(subjectid) == 8 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)
  ) |>
  
  #  survey data
  mutate (cohort = "תשפד") |>
  rename(date_recorded      = RecordedDate) |>   
  
  #rename patas 
  rename_with(.cols = matches("^PATAS\\d+$"), .fn = ~ paste0("patas", seq_along(.))) |>
  rename(patas = 'SC0') |> filter(!is.na(patas)) |> 
  
  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         patas,
         patas1, patas2, patas3, patas4, patas5, patas6, 
         patas7, patas8, patas9, patas10, patas11, patas12)




#### ENSURING CLASS OF COLUMNS ----
  
patas <- patas |>
  
  mutate(across(c(patas,
                  patas1, patas2, patas3, patas4, patas5, patas6, 
                  patas7, patas8, patas9, patas10, patas11, patas12),
                as.numeric))



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
    dup_count <- patas |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())

    print(dup_count)


# 3. Validate sumscore
    #note that reversed items where already revered in qualtrics
    sum_check = patas |> 
      mutate(patas_sum = rowSums(across(matches("^patas([1-9]|1[0-2])$")))) |> 
      filter(patas_sum != patas)
    
    print(nrow(sum_check))

# 4. Hist of sumscore
    patas |> 
      ggplot(aes(x = patas)) + 
      geom_dots() + labs(x = "PATAS Score") + theme_minimal()

#### SAVE ----
save(patas, file = 'data/תשפד/raw_data/patas.Rdata')
    