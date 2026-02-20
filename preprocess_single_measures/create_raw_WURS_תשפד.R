library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE ASRS RAW ####

#load 
wurs <- read_tsv("data/תשפד/collected_data/WURS_July+1,+2025_11.23.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
wurs <- wurs |>
  
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
  
  #rename asrs 
  rename(wurs = 'SC0') |> 
  
    rename_with(.cols = matches("^WURS\\d+$"), .fn = ~ paste0("wurs", seq_along(.))) |>
  
  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  filter(!is.na(wurs), !is.na(wurs1)) |> 
  
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         wurs, 
         wurs1, wurs2, wurs3, wurs4, wurs5, wurs6, wurs7, wurs8, wurs9, wurs10, 
         wurs11, wurs12, wurs13, wurs14, wurs15, wurs16, wurs17, wurs18, wurs19, wurs20, 
         wurs21, wurs22, wurs23, wurs24, wurs25)




#### ENSURING CLASS OF COLUMNS ----
  
wurs <- wurs |>
  
  mutate(across(c(wurs, 
                  wurs1, wurs2, wurs3, wurs4, wurs5, wurs6, wurs7, wurs8, wurs9, wurs10, 
                  wurs11, wurs12, wurs13, wurs14, wurs15, wurs16, wurs17, wurs18, wurs19, wurs20, 
                  wurs21, wurs22, wurs23, wurs24, wurs25),
                as.numeric))



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
    dup_count <- wurs |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())

    print(dup_count)

#    wurs |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()
    
# 2. Retain only the first line for those how complete the whole thing more then once
wurs = wurs |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()
    
# 3. Validate sumscore
    #note that reversed items where already revered in qualtrics
    sum_check = wurs |> 
      mutate(sum_wurs    = wurs1 + wurs2 + wurs3 + wurs4 + wurs5 + wurs6 + wurs7 + wurs8 + wurs9 + wurs10 + 
               wurs11 + wurs12 + wurs13 + wurs14 + wurs15 + wurs16 + wurs17 + wurs18 + wurs19 + wurs20 + 
               wurs21 + wurs22 + wurs23 + wurs24 + wurs25
      ) |>
      filter(wurs != sum_wurs )
    
    print(nrow(sum_check))

# 4. Hist of sumscore
    wurs |> 
      ggplot(aes(x = wurs)) + 
      geom_dots() + labs(x = "WURS Score") + theme_minimal()

#### SAVE ----
save(wurs, file = 'data/תשפד/raw_data/wurs.Rdata')
    