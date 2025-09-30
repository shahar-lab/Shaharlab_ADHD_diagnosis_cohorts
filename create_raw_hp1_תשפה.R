library(readr)
library(dplyr)
library(lubridate)

#### CREATE HP1 RAW ####

# load 
hp1 <- read_tsv("data/תשפה/collected_data/home-pack-01_תשפה_June+1,+2025_09.12_labels.tsv",
                 locale = locale(encoding = "UTF-16")) 

#house keeping
hp1 <- hp1 |>
  
  # Remove the first two rows
  slice(-1, -2) |>
  
  # subjectid: keep valid IDs
  rename(subjectid = shahar_id...18) |>
  filter(
    nchar(subjectid) == 6 &
      grepl("^[A-Za-z0-9]+$", subjectid) &
      !grepl("example", subjectid, ignore.case = TRUE)
  ) |>
  
  
  #  survey data
  mutate (cohort = "תשפה") |>
  rename(date_recorded      = RecordedDate) |>   
  
  # Demographic
  rename(DOB_month = demographic2_DOB_month,
         DOB_year  = demographic2_DOB_year,
         gender    = demographic3_gender,
         education = demographic4_educati,
         place_of_residence_until12yo = demographic7_residan,
         vision_correction  = demographic8_vision,
         hearing_correction = demographic9_hearing,
         epilepsy = demographic10_ep,
         psychotic = demographic11_scizo,
         asd  = demographic12_asd,
         genetic = demographic13_geneti
        ) |>
  
  #Get age when completing survey
  mutate(DOB_month   = recode(DOB_month,
                              "ינואר"   = "January",
                              "פברואר"  = "February",
                              "מרץ"     = "March",
                              "אפריל"   = "April",
                              "מאי"     = "May",
                              "יוני"    = "June",
                              "יולי"    = "July",
                              "אוגוסט"  = "August",
                              "ספטמבר"  = "September",
                              "אוקטובר" = "October",
                              "נובמבר"  = "November",
                              "דצמבר"   = "December")) |> 
  mutate(DOB_day     = 15,
         DOB         = parse_date_time(paste(DOB_day, DOB_month, DOB_year, sep = "-"), orders = "d-B-Y"),
         date_recorded = dmy_hm(date_recorded),
         age         = interval(DOB, date_recorded) / years(1)) |> 
  select(-DOB_day) |>

  # EHI
  rename(EHI   = SC0,
         EHI4 = EHDI4) |>
  mutate(
    EHI_hand_dominance = case_when(
      EHI <= -61           ~ "Left handers",
      EHI >=  61           ~ "Right handers",
      EHI >  -61 & EHI < 61 ~ "Mixed handers",
      TRUE                 ~ NA_character_
    ) |> 
      factor(levels = c("Left handers", "Mixed handers", "Right handers"))
  ) |>
  
  # CUDIT / AUDIT 
  rename(
    CUDIT_USE = `CUDIT-RYES/NO`,
    CUDIT1  = `CUDIT-R1`,
    CUDIT2  = `CUDIT-R2`,
    CUDIT3  = `CUDIT-R3`,
    CUDIT4  = `CUDIT-R4`,
    CUDIT5  = `CUDIT-R5`,
    CUDIT6  = `CUDIT-R6`,
    CUDIT7  = `CUDIT-R7`,
    CUDIT8  = `CUDIT-R8`
  ) |>

  rename(AUDIT = SC1,
         CUDIT = SC2) |>
    
   select(subjectid,
         cohort,date_recorded,
         age,
         gender,
         EHI_hand_dominance,
         AUDIT, CUDIT, EHI,
         education, place_of_residence_until12yo, vision_correction,
         EHI1,EHI2,EHI3,EHI4,
         AUDIT1, AUDIT2, AUDIT3, AUDIT4, AUDIT5, AUDIT6, AUDIT7, AUDIT8, AUDIT9, AUDIT10,
         CUDIT_USE,CUDIT1, CUDIT2, CUDIT3, CUDIT4, CUDIT5, CUDIT6, CUDIT7, CUDIT8)



#### ENSURING CLASS OF COLUMNS ----

hp1 <- hp1 |>
  mutate(
    across(
      c(age, 
        starts_with("EH"), 
        CUDIT, CUDIT1, CUDIT2, CUDIT3, CUDIT4, CUDIT5, CUDIT6, CUDIT7, CUDIT8,
        AUDIT, AUDIT1, AUDIT2, AUDIT3, AUDIT4, AUDIT5, AUDIT6, AUDIT7, AUDIT8, AUDIT9, AUDIT10 ),
      as.numeric
    ),
    gender = as.factor(gender)
  ) |>
  
  # Addiction cutoffs
  mutate(alcohol_use_cutoff = ifelse(AUDIT < 8, "below_audit_cutoff", "above_audit_cutoff" ),
         cannabis_use_cutoff = ifelse(CUDIT < 8, "below_cudit_cutoff", "above_cudit_cutoff" )) 

#### VALIDATION ----

# 1. Examine subjectid duplicates

  dup_count <- hp1 |>
    count(subjectid) |>
    filter(n > 1) |>
    summarise(subjectid_duplicate_count = n())
  print(dup_count)
  
  #hp1 |> group_by(subjectid) |> filter(n() > 1) |> ungroup() |>  arrange(subjectid) |> View()

# 2. Retain only the first line for those how complete the whole thing more then once
  hp1 = hp1 |> group_by(subjectid) |> slice_min(order_by = date_recorded, n = 1) |> ungroup()
  
  
  
# 3. Manually compute CUDIT and AUDIT sums and compare with provided totals
hp1_validation = hp1 |>
  mutate(
    CUDIT_sum_manual = rowSums(across(CUDIT1:CUDIT8), na.rm = TRUE),
    AUDIT_sum_manual = rowSums(across(AUDIT1:AUDIT10), na.rm = TRUE),
    CUDIT_check      = CUDIT == CUDIT_sum_manual,
    AUDIT_check      = AUDIT == AUDIT_sum_manual
  )

hp1_validation_summary =
  hp1_validation |>
  summarize(
    CUDIT_mismatch_n = sum(!CUDIT_check, na.rm = TRUE),
    AUDIT_mismatch_n = sum(!AUDIT_check, na.rm = TRUE)
  )

print(hp1_validation_summary)


#### SAVE ----
save(hp1, file = 'data/תשפה/raw_data/hp1.Rdata')

       
       
       
       
       