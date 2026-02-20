
library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)



#### CREATE BDI RAW ####

#load 
bdi <- read_tsv("data/תשפג/collected_data/BDI+-+עברית+template+-+Copy_October+1,+2025_09.59_values.tsv",
                 locale = locale(encoding = "UTF-16")) 

#housekeeping
bdi <- bdi |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  filter(
    nchar(subjectid) == 8 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)
  ) |>
  
  #  survey data
  mutate(cohort = "תשפג") |>
  rename(date_recorded      = RecordedDate) |>   
  
  #rename bdi 
  rename(bdi = 'SC0') |> 

  #filter out cases that did not finish the survey 
  filter(Finished == 1) |>
  filter(!is.na(bdi)) |> 
  
  
  #columns to keep
  select(subjectid,
         cohort,date_recorded,
         bdi, BDI1, BDI2, BDI3, BDI4, BDI5, BDI6, BDI7, BDI8, BDI9, BDI10,
         BDI11, BDI12, BDI13, BDI14, BDI15, BDI16, BDI17, BDI18, BDI19, BDI20, BDI21)




#### ENSURING CLASS OF COLUMNS ----
  
bdi <- bdi |>
  
  mutate(across(c(bdi, BDI1, BDI2, BDI3, BDI4, BDI5, BDI6, BDI7, BDI8, BDI9, BDI10,
                  BDI11, BDI12, BDI13, BDI14, BDI15, BDI16, BDI17, BDI18, BDI19, BDI20, BDI21),
                as.numeric))



#### VALIDATIONS ----

# 1. Examine subjectid duplicates
    dup_count <- 
    bdi |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())

    print(dup_count)

    #bdi |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |> View()
    
    
# 2. Retain only the first line for those how complete the whole thing more then once
    bdi = bdi |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()
    
# 3. Validate sumscore
    #note that reversed items where already revered in qualtrics
    sum_check = 
      bdi |> 
      mutate(sum_bdi    = 
               BDI1+BDI2+BDI3+BDI4+BDI5+BDI6+BDI7+BDI8+BDI9+BDI10+
               BDI11+BDI12+BDI13+BDI14+BDI15+BDI16+BDI17+BDI18+BDI19+BDI20+BDI21) |>
      filter(bdi != sum_bdi )
    
    print(nrow(sum_check))

# 4. Hist of sumscore
    bdi |> 
      ggplot(aes(x = bdi)) + 
      geom_dots() + labs(x = "BDI Score") + theme_minimal()

#### SAVE ----
save(bdi, file = 'data/תשפג/raw_data/bdi.Rdata')
    