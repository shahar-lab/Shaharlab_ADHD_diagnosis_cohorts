library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE ASRS RAW ####

#load 
asrs <- read_tsv("data/תשפג/collected_data/ASRS+בעברית_October+1,+2025_09.57_values.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
asrs <- asrs |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  filter(
    nchar(subjectid) == 8 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)
  ) |>
  
  #  survey data
  mutate (cohort = "תשפג") |>
  rename(date_recorded      = RecordedDate) |>   
  
  #rename asrs 
  rename(asrs_ia = 'SC0', 
         asrs_hi = 'SC1',
         asrs    = 'SC2') |> 
  
  mutate(
    asrs_ia_count = 
      (ASRS1 >= 2) * 1 + 
      (ASRS2 >= 2) * 1 + 
      (ASRS3 >= 2) * 1 + 
      (ASRS4 >= 3) * 1 + 
      (ASRS7 >= 3) * 1 + 
      (ASRS8 >= 3) * 1 + 
      (ASRS9 >= 2) * 1 + 
      (ASRS10 >= 3) * 1 + 
      (ASRS11 >= 3) * 1,
    
    asrs_hi_count = 
      (ASRS5 >= 3) * 1 + 
      (ASRS6 >= 3) * 1 + 
      (ASRS12 >= 2) * 1 + 
      (ASRS13 >= 3) * 1 + 
      (ASRS14 >= 3) * 1 + 
      (ASRS15 >= 3) * 1 + 
      (ASRS16 >= 2) * 1 + 
      (ASRS17 >= 3) * 1 + 
      (ASRS18 >= 2) * 1) |>
  
  rename_with(.cols = matches("^ASRS\\d+$"), .fn = ~ paste0("asrs", seq_along(.))) |>
  

  
  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  filter(!is.na(asrs), !is.na(asrs1)) |> 
  
  select(
    subjectid, cohort, date_recorded,
    asrs, asrs_ia, asrs_hi, asrs_ia_count, asrs_hi_count,
    asrs1:asrs18
  )




#### ENSURING CLASS OF COLUMNS ----
  
asrs <- asrs |>
  
  mutate(across(c(asrs, asrs_ia, asrs_hi, asrs_ia_count, asrs_hi_count,
                  asrs1, asrs2 , asrs3 , asrs4 , asrs5 , asrs6 , 
                  asrs7, asrs8 , asrs9 , asrs10 , asrs11 , asrs12 ,
                  asrs13 , asrs14 , asrs15 , asrs16 , asrs17 , asrs18),
                as.numeric)) 
  
# |>
  # mutate(
  #   asrs      = asrs + 18,
  #   asrs_ia   = asrs_ia + 18,
  #   asrs_hi   = asrs_hi + 18,
  #   asrs1  = asrs1  + 1, asrs2  = asrs2  + 1, asrs3  = asrs3  + 1,
  #   asrs4  = asrs4  + 1, asrs5  = asrs5  + 1, asrs6  = asrs6  + 1,
  #   asrs7  = asrs7  + 1, asrs8  = asrs8  + 1, asrs9  = asrs9  + 1,
  #   asrs10 = asrs10 + 1, asrs11 = asrs11 + 1, asrs12 = asrs12 + 1,
  #   asrs13 = asrs13 + 1, asrs14 = asrs14 + 1, asrs15 = asrs15 + 1,
  #   asrs16 = asrs16 + 1, asrs17 = asrs17 + 1, asrs18 = asrs18 + 1
  # ) 



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
    dup_count <- asrs |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())

    print(dup_count)
    
    
    asrs |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()
    
    # 2. Retain only the first line for those how complete the whole thing more then once
    asrs = asrs |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()

# 2. Validate sumscore
    #note that reversed items where already revered in qualtrics
    sum_check = asrs |> 
      mutate(sum_asrs    = asrs1 + asrs2 + asrs3 + asrs4 + asrs5 + asrs6 + asrs7 + asrs8 + asrs9 + asrs10 + asrs11 + asrs12 + asrs13 + asrs14 + asrs15 + asrs16 + asrs17 + asrs18,
             sum_asrs_ia = asrs1 + asrs2 + asrs3 + asrs4 + asrs7 + asrs8 + asrs9 + asrs10 + asrs11,
             sum_asrs_hi = asrs5 + asrs6 + asrs12 + asrs13 + asrs14 + asrs15 + asrs16 + asrs17 + asrs18) |>
      filter(asrs != sum_asrs | 
             asrs_ia != sum_asrs_ia |
             asrs_hi != sum_asrs_hi)
    
    print(nrow(sum_check))

# 4. Hist of sumscore
    asrs |> 
      ggplot(aes(x = asrs)) + 
      geom_dots() + labs(x = "ASRS Score") + theme_minimal()

#### SAVE ----
save(asrs, file = 'data/תשפג/raw_data/asrs.Rdata')
    