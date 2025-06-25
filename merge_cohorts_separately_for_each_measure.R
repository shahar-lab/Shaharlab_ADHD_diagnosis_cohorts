library(dplyr)

#### DEMOGRAPHIC  ----

# load hp1 from תשפד 
load('data/תשפד/raw_data/hp1.Rdata')
hp1_תשפד = hp1
rm(hp1)

# load hp1 from תשפה 
load('data/תשפה/raw_data/hp1.Rdata')
hp1_תשפה = hp1
rm(hp1)

#merge
common_cols = c("subjectid", "cohort", "age", "gender",
                "alcohol_use_cutoff", "cannabis_use_cutoff",
                "AUDIT", "CUDIT", "EHI", "EHI_hand_dominance")

hp1 = bind_rows(
  select(hp1_תשפד, all_of(common_cols)),
  select(hp1_תשפה, all_of(common_cols))
)

rm(hp1_תשפד, hp1_תשפה)

save(hp1, file = 'data/all_cohorts_raw_data/hp1.Rdata')

#### DIVA  ----

#load diva from תשפד 
load('data/תשפד/raw_data/diva.Rdata')
diva_תשפד = diva

#load diva from תשפה
load('data/תשפה/raw_data/diva.Rdata')
diva_תשפה = diva
rm(diva)

#merge
common_cols = c("subjectid", "cohort", "declared_group",
              "diva_IA_symptoms", "diva_HI_symptoms",
               "diva_childhood_symptoms", "diva_function_adulthood", "diva_function_childhood",
               "DSM_criteria_A1", "DSM_criteria_A2", "DSM_criteria_B",
               "DSM_criteria_C_D", "diva_diagnosis", "diva_diagnosis_type")

diva = bind_rows(
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
rm(diva_תשפה, diva_תשפד)
save(diva, file = 'data/all_cohorts_raw_data/diva.Rdata')



#### ICAR ----
# load icar from תשפד 
load('data/תשפד/raw_data/icar.Rdata')
icar_תשפד = icar
rm(icar)

# load icar from תשפה 
load('data/תשפה/raw_data/icar.Rdata')
icar_תשפה = icar
rm(icar)

#merge
icar = bind_rows(
  select(icar_תשפד, subjectid, icar),
  select(icar_תשפה, subjectid, icar)
)
rm(icar_תשפד, icar_תשפה)
save(icar, file = 'data/all_cohorts_raw_data/icar.Rdata')

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




#### ASRS ----
# load asrs from תשפד 
load('data/תשפד/raw_data/asrs.Rdata')
asrs_תשפד = asrs
rm(asrs)

# load patas from תשפה 
load('data/תשפה/raw_data/asrs.Rdata')
asrs_תשפה = asrs
rm(asrs)

#merge
asrs = bind_rows(
  select(asrs_תשפד, subjectid, asrs, asrs_ia, asrs_hi),
  select(asrs_תשפה, subjectid, asrs, asrs_ia, asrs_hi)
)
rm(asrs_תשפד, asrs_תשפה)
save(asrs, file = 'data/all_cohorts_raw_data/asrs.Rdata')

