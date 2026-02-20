library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE ASRS RAW ####

#load 
pqb <- read_tsv("data/תשפד/collected_data/PQ-B_July+1,+2025_11.21.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
pqb <- pqb |>
  
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
  rename(pqb = 'SC0', 
         pqb_distress = 'SC1') |> 
  
  
  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  filter(!is.na(pqb)) |> 
  
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         pqb, pqb_distress)




#### ENSURING CLASS OF COLUMNS ----
  
pqb <- pqb |>
  
  mutate(across(c(pqb, pqb_distress),
                as.numeric)) |>
  mutate(
    pqb_distress = pqb_distress + 21*1 #בגלל ש"לא" נסכם כאפס ו"מאד לא מסכים" כאחד אבל בקוולטריקס זה קודד מאפס עבור מאד לא מסכים
  )



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
    dup_count <- asrs |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())

    print(dup_count)


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
save(asrs, file = 'data/תשפד/raw_data/asrs.Rdata')
    