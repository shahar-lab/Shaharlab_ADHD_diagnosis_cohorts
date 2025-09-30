library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE RAW ####

#load 
stai <- read_tsv("data/תשפה/collected_data/STAI_תשפה_July+1,+2025_11.30.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
stai <- stai |>
  
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
  mutate (cohort = "תשפד") |>
  rename(date_recorded      = RecordedDate) |>   
  
  #rename  
  rename(stai_state = 'SC0', 
         stai_trait = 'SC1',
         stai       = 'SC2') |> 
  

  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  filter(!is.na(stai), !is.na(SAI1)) |> 
  
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         stai_state, stai_trait, stai,
         SAI1, SAI2, SAI3, SAI4, SAI5, SAI6, SAI7, SAI8, SAI9, SAI10,
         SAI11, SAI12, SAI13, SAI14, SAI15, SAI16, SAI17, SAI18, SAI19, SAI20,
         TAI1, TAI2, TAI3, TAI4, TAI5, TAI6, TAI7, TAI8, TAI9, TAI10,
         TAI11, TAI12, TAI13, TAI14, TAI15, TAI16, TAI17, TAI18, TAI19, TAI20
        )




#### ENSURING CLASS OF COLUMNS ----
  
stai <- stai |>
  
  mutate(across(c(stai_state, stai_trait, stai,
                  SAI1, SAI2, SAI3, SAI4, SAI5, SAI6, SAI7, SAI8, SAI9, SAI10,
                  SAI11, SAI12, SAI13, SAI14, SAI15, SAI16, SAI17, SAI18, SAI19, SAI20,
                  TAI1, TAI2, TAI3, TAI4, TAI5, TAI6, TAI7, TAI8, TAI9, TAI10,
                  TAI11, TAI12, TAI13, TAI14, TAI15, TAI16, TAI17, TAI18, TAI19, TAI20),
                as.numeric))



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
    dup_count <- stai |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())

    print(dup_count)
    
    stai |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()
    
# 2. Retain only the first line for those how complete the whole thing more then once
    stai = stai |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()
    
    

# 2. Validate sumscore
    #note that reversed items where already revered in qualtrics
    sum_check <- stai |>
      mutate(
            # --- STATE total ---
            sum_stai_state =
              SAI1 + SAI2 + SAI3 + SAI4 +
              SAI5 + SAI6 + SAI7 + SAI8 +
              SAI9 + SAI10 + SAI11 + SAI12 +
              SAI13 + SAI14 + SAI15 + SAI16 +
              SAI17 + SAI18 + SAI19 + SAI20,
            
            # --- TRAIT total ---
            sum_stai_trait =
              TAI1 + TAI2 + TAI3 + TAI4 +
              TAI5 + TAI6 + TAI7 + TAI8 +
              TAI9 + TAI10 + TAI11 + TAI12 +
              TAI13 + TAI14 + TAI15 + TAI16 +
              TAI17 + TAI18 + TAI19 + TAI20,
            
            # --- OVERALL total ---
            sum_stai = sum_stai_state + sum_stai_trait
      ) |>
      filter(stai != sum_stai | 
             stai_state != sum_stai_state |
             stai_trait != sum_stai_trait)
    
    print(nrow(sum_check))

# 4. Hist of sumscore
    stai |> 
      ggplot(aes(x = stai)) + 
      geom_dots() + labs(x = "STAI Score") + theme_minimal()

#### SAVE ----
save(stai, file = 'data/תשפה/raw_data/stai.Rdata')
    