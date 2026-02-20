library(readr)
library(dplyr)
library(ggdist)
library(ggplot2)
library(hms)

#### CREATE ASRS RAW ####

#load 
wurs <- read_tsv("data/תשפג/collected_data/WURS+-+עברית+-+template+-+Copy_October+1,+2025_10.06_values.tsv",
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
  mutate (cohort = "תשפג") |>
  rename(date_recorded      = RecordedDate) |>   
  
  #rename  
  rename(wurs = 'SC0') |> 
  
  #rename to short version
  mutate(
    wurs1_new  = WURS3,   wurs2_new  = WURS4,   wurs3_new  = WURS5,   wurs4_new  = WURS6,   wurs5_new  = WURS7,
    wurs6_new  = WURS9,   wurs7_new  = WURS10,  wurs8_new  = WURS11,  wurs9_new  = WURS12,  wurs10_new = WURS15,
    wurs11_new = WURS16,  wurs12_new = WURS17,  wurs13_new = WURS20,  wurs14_new = WURS21,  wurs15_new = WURS24,
    wurs16_new = WURS25,  wurs17_new = WURS26,  wurs18_new = WURS27,  wurs19_new = WURS28,  wurs20_new = WURS29,
    wurs21_new = WURS40,  wurs22_new = WURS41,  wurs23_new = WURS51,  wurs24_new = WURS56,  wurs25_new = WURS59
  ) |>

  select(-matches("^wurs[0-9]+$")) |>

  rename(
    wurs1  = wurs1_new,   wurs2  = wurs2_new,   wurs3  = wurs3_new,   wurs4  = wurs4_new,   wurs5  = wurs5_new,
    wurs6  = wurs6_new,   wurs7  = wurs7_new,   wurs8  = wurs8_new,   wurs9  = wurs9_new,   wurs10 = wurs10_new,
    wurs11 = wurs11_new,  wurs12 = wurs12_new,  wurs13 = wurs13_new,  wurs14 = wurs14_new,  wurs15 = wurs15_new,
    wurs16 = wurs16_new,  wurs17 = wurs17_new,  wurs18 = wurs18_new,  wurs19 = wurs19_new,  wurs20 = wurs20_new,
    wurs21 = wurs21_new,  wurs22 = wurs22_new,  wurs23 = wurs23_new,  wurs24 = wurs24_new,  wurs25 = wurs25_new
  ) |>

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
      mutate(sum_wurs    = 
               wurs1 + wurs2 + wurs3 + wurs4 + wurs5 + wurs6 + wurs7 + wurs8 + wurs9 + wurs10 + 
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
save(wurs, file = 'data/תשפג/raw_data/wurs.Rdata')
    