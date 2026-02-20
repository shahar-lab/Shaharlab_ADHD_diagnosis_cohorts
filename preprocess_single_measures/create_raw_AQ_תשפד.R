library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE AQ RAW ####

#load 
aq <- read_tsv("data/תשפד/collected_data/AQ_July+9,+2025_11.56.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
aq <- aq |>
  
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
  rename(aq = 'SC0') |> 
  

  rename_with(.cols = matches("^AQ\\d+$"), .fn = ~ paste0("aq", seq_along(.))) |>
  

  
  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  filter(!is.na(aq), !is.na(aq1)) |> 
  
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         aq, aq1, aq2, aq3, aq4, aq5, aq6, aq7, aq8, aq9, aq10,
         aq11, aq12, aq13, aq14, aq15, aq16, aq17, aq18, aq19, aq20,
         aq21, aq22, aq23, aq24, aq25, aq26, aq27, aq28, aq29, aq30,
         aq31, aq32, aq33, aq34, aq35, aq36, aq37, aq38, aq39, aq40,
         aq41, aq42, aq43, aq44, aq45, aq46, aq47, aq48, aq49, aq50)




#### ENSURING CLASS OF COLUMNS ----
  
aq <- aq |>
  
  mutate(across(c(aq, aq1, aq2, aq3, aq4, aq5, aq6, aq7, aq8, aq9, aq10,
                  aq11, aq12, aq13, aq14, aq15, aq16, aq17, aq18, aq19, aq20,
                  aq21, aq22, aq23, aq24, aq25, aq26, aq27, aq28, aq29, aq30,
                  aq31, aq32, aq33, aq34, aq35, aq36, aq37, aq38, aq39, aq40,
                  aq41, aq42, aq43, aq44, aq45, aq46, aq47, aq48, aq49, aq50),
                as.numeric))



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
    dup_count <- aq |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())

    print(dup_count)


# 2. Validate sumscore
    #note that reversed items where already revered in qualtrics
    sum_check = aq |> 
      mutate(sum_aq     = 
               aq1 + aq2 + aq3 + aq4 + aq5 + aq6 + aq7 + aq8 + aq9 + aq10 
             + aq11 + aq12 + aq13 + aq14 + aq15 + aq16 + aq17 + aq18 
             + aq19 + aq20 + aq21 + aq22 + aq23 + aq24 + aq25 + aq26 
             + aq27 + aq28 + aq29 + aq30 + aq31 + aq32 + aq33 + aq34 
             + aq35 + aq36 + aq37 + aq38 + aq39 + aq40 + aq41 + aq42 
             + aq43 + aq44 + aq45 + aq46 + aq47 + aq48 + aq49 + aq50) |>
      filter(aq != sum_aq )
    
    print(nrow(sum_check))

# 4. Hist of sumscore
    aq |> 
      ggplot(aes(x = aq)) + 
      geom_dots() + labs(x = "AQ Score") + theme_minimal()

#### SAVE ----
save(aq, file = 'data/תשפד/raw_data/aq.Rdata')
    