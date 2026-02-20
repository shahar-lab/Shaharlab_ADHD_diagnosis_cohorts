library(dplyr)

#### DEMOGRAPHIC  ----



load('data/תשפג/raw_data/hp1.Rdata')
hp1_תשפג = hp1
rm(hp1)

load('data/תשפד/raw_data/hp1.Rdata')
hp1_תשפד = hp1
rm(hp1)

load('data/תשפה/raw_data/hp1.Rdata')
hp1_תשפה = hp1
rm(hp1)

#merge
common_cols = c("subjectid", "cohort", "age", "gender",
                "alcohol_use_cutoff", "cannabis_use_cutoff",
                "AUDIT", "CUDIT", "EHI", "EHI_hand_dominance")

hp1 = bind_rows(
  select(hp1_תשפג, all_of(common_cols)),
  select(hp1_תשפד, all_of(common_cols)),
  select(hp1_תשפה, all_of(common_cols))
)

rm(hp1_תשפג, hp1_תשפד, hp1_תשפה)

#duplicates
hp1 |> group_by(subjectid) |> filter(n() > 1) |> ungroup() 

save(hp1, file = 'data/all_cohorts_raw_data/hp1.Rdata')



#### DIVA  ----

load('data/תשפג/raw_data/diva.Rdata')
diva_תשפג <- diva

load('data/תשפד/raw_data/diva.Rdata')
diva_תשפד = diva

load('data/תשפה/raw_data/diva.Rdata')
diva_תשפה = diva
rm(diva)

#merge
common_cols = c("subjectid", "cohort", "declared_group",
                "diva_IA_symptoms", "diva_HI_symptoms",
                "diva_childhood_symptoms", "diva_function_adulthood", "diva_function_childhood",
                "DSM_criteria_A1", "DSM_criteria_A2", "DSM_criteria_B",
                "DSM_criteria_C_D", "diva_diagnosis", "diva_diagnosis_type",
                "community_diagnosis_meds",
                "community_diagnosis_meds_type", 
                "community_diagnosis_meds_dosage",
                "community_diagnosis_meds_freq")

diva <- bind_rows(
  
  select(diva_תשפג, all_of(common_cols)),
  select(diva_תשפד, all_of(common_cols)),
  select(diva_תשפה, all_of(common_cols))
  
)

diva = diva |> 
  mutate(
    diva_group = case_when(
      diva_diagnosis == "meet_diva_criteria"  ~ "ADHD",
      diva_diagnosis == "below_diva_criteria" ~ "TD",
      TRUE                                    ~ NA_character_
    ),
    diva_group = factor(diva_group, levels = c("TD", "ADHD"))
  )

contrasts(diva$diva_group)
rm(diva_תשפה, diva_תשפד, diva_תשפג)
save(diva, file = 'data/all_cohorts_raw_data/diva.Rdata')



#### ASRS ----

# Load each cohort
load('data/תשפג/raw_data/asrs.Rdata')
asrs_תשפג <- asrs
rm(asrs)

load('data/תשפד/raw_data/asrs.Rdata')
asrs_תשפד <- asrs
rm(asrs)

load('data/תשפה/raw_data/asrs.Rdata')
asrs_תשפה <- asrs
rm(asrs)

# Merge in chronological order: תשפג → תשפד → תשפה
asrs <- bind_rows(
  select(asrs_תשפג, subjectid, asrs, asrs_ia, asrs_hi, asrs_ia_count, asrs_hi_count),
  select(asrs_תשפד, subjectid, asrs, asrs_ia, asrs_hi, asrs_ia_count, asrs_hi_count),
  select(asrs_תשפה, subjectid, asrs, asrs_ia, asrs_hi, asrs_ia_count, asrs_hi_count)
)

# Clean up
rm(asrs_תשפג, asrs_תשפד, asrs_תשפה)

# Save merged file
save(asrs, file = 'data/all_cohorts_raw_data/asrs.Rdata')



#### WURS ----

# Load WURS from תשפג
load('data/תשפג/raw_data/wurs.Rdata')
x_תשפג <- wurs
rm(wurs)

# Load WURS from תשפד
load('data/תשפד/raw_data/wurs.Rdata')
x_תשפד <- wurs
rm(wurs)


# Load WURS from תשפה
load('data/תשפה/raw_data/wurs.Rdata')
x_תשפה <- wurs
rm(wurs)

# Merge in chronological order: תשפג → תשפד → תשפה
wurs <- bind_rows(
  select(x_תשפג, subjectid, wurs),
  select(x_תשפד, subjectid, wurs),
  select(x_תשפה, subjectid, wurs)
)

# Clean up
rm(x_תשפג, x_תשפד, x_תשפה)

# Save merged file
save(wurs, file = 'data/all_cohorts_raw_data/wurs.Rdata')

#### ICAR ----

# Load ICAR from תשפג 
load('data/תשפג/raw_data/icar.Rdata')
icar_תשפג <- icar
rm(icar)

# Load ICAR from תשפד 
load('data/תשפד/raw_data/icar.Rdata')
icar_תשפד <- icar
rm(icar)

# Load ICAR from תשפה 
load('data/תשפה/raw_data/icar.Rdata')
icar_תשפה <- icar
rm(icar)

# Merge in chronological order: תשפג → תשפד → תשפה
icar <- bind_rows(
  select(icar_תשפג, subjectid, icar),
  select(icar_תשפד, subjectid, icar),
  select(icar_תשפה, subjectid, icar)
)

# Clean up temporary objects
rm(icar_תשפג, icar_תשפד, icar_תשפה)

# Save merged file
save(icar, file = 'data/all_cohorts_raw_data/icar.Rdata')



#### BDI ----

load('data/תשפג/raw_data/bdi.Rdata')
bdi_תשפג <- bdi
rm(bdi)

load('data/תשפד/raw_data/bdi.Rdata')
bdi_תשפד <- bdi
rm(bdi)

load('data/תשפה/raw_data/bdi.Rdata')
bdi_תשפה <- bdi
rm(bdi)


bdi <- bind_rows(
  select(bdi_תשפג, subjectid, bdi),
  select(bdi_תשפד, subjectid, bdi),
  select(bdi_תשפה, subjectid, bdi)
)

rm(bdi_תשפג, bdi_תשפד, bdi_תשפה)
save(bdi, file = 'data/all_cohorts_raw_data/bdi.Rdata')


#### STAI ----

load('data/תשפג/raw_data/stai.Rdata')
x_תשפג <- stai
rm(stai)

load('data/תשפד/raw_data/stai.Rdata')
x_תשפד <- stai
rm(stai)

load('data/תשפה/raw_data/stai.Rdata')
x_תשפה <- stai
rm(stai)

stai <- bind_rows(
  select(x_תשפג, subjectid, stai, stai_state, stai_trait),
  select(x_תשפד, subjectid, stai, stai_state, stai_trait),
  select(x_תשפה, subjectid, stai, stai_state, stai_trait)
)

rm(x_תשפג, x_תשפד, x_תשפה)
save(stai, file = 'data/all_cohorts_raw_data/stai.Rdata')


#### OCIR ----

load('data/תשפג/raw_data/ocir.Rdata')
x_תשפג <- ocir
rm(ocir)

load('data/תשפד/raw_data/ocir.Rdata')
x_תשפד <- ocir
rm(ocir)

load('data/תשפה/raw_data/ocir.Rdata')
x_תשפה <- ocir
rm(ocir)

ocir <- bind_rows(
  select(x_תשפג, subjectid, ocir, ocir_hoarding, ocir_checking, ocir_ordering, ocir_neutralizing, ocir_washing, ocir_obsessing),
  select(x_תשפד, subjectid, ocir, ocir_hoarding, ocir_checking, ocir_ordering, ocir_neutralizing, ocir_washing, ocir_obsessing),
  select(x_תשפה, subjectid, ocir, ocir_hoarding, ocir_checking, ocir_ordering, ocir_neutralizing, ocir_washing, ocir_obsessing)
)

rm(x_תשפג, x_תשפד, x_תשפה)
save(ocir, file = 'data/all_cohorts_raw_data/ocir.Rdata')


#### AQ ----

load('data/תשפג/raw_data/aq.Rdata')
aq_תשפג <- aq
rm(aq)

load('data/תשפד/raw_data/aq.Rdata')
aq_תשפד <- aq
rm(aq)

load('data/תשפה/raw_data/aq.Rdata')
aq_תשפה <- aq
rm(aq)

aq <- bind_rows(
  select(aq_תשפג, subjectid, aq),
  select(aq_תשפד, subjectid, aq),
  select(aq_תשפה, subjectid, aq)
)

rm(aq_תשפג, aq_תשפד, aq_תשפה)
save(aq, file = 'data/all_cohorts_raw_data/aq.Rdata')



#### PATAS ----
# load patas from תשפד 
load('data/תשפד/raw_data/patas.Rdata')
patas_תשפד = patas
rm(patas)

# load patas from תשפה 
load('data/תשפה/raw_data/patas.Rdata')
patas_תשפה = patas
rm(patas)

#merge
patas = bind_rows(
  select(patas_תשפד, subjectid, patas, 
         patas1, patas2, patas3, patas4, patas5, patas6, 
         patas7, patas8, patas9, patas10, patas11, patas12),
  select(patas_תשפה, subjectid, patas,
         patas1, patas2, patas3, patas4, patas5, patas6, 
         patas7, patas8, patas9, patas10, patas11, patas12)
)
rm(patas_תשפד, patas_תשפה)
save(patas, file = 'data/all_cohorts_raw_data/patas.Rdata')




