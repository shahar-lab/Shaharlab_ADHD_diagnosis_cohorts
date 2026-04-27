# THIS FILE ADD SUMSCORES TO THE DIVA AFTER DRUG AND DIAGNOSIS MISMATCH EXCLUSION
library(dplyr)

#### ADDING PATAS ----

# loading DIVA for תשפג תשפד תשפה
load('data/raw_data/diva_before_exclusion.Rdata')

df = diva 
df = df %>%
  filter(
    subjectid %in% c(
      "gov33P",
      "5FdTAz",
      "5gINAmxj",
      "omJVPUea",
      "aQAV3g2N",
      "fQGDx0f1",
      "EVhG6aHF",
      "onRQQXIH",
      "RGwrg4dh",
      "fKsxuz",
      "XpJhGr",
      "emzHyS",
      "ChXIQE",
      "obHWs2",
      "NqOPTt",
      "4KxP86",
      "Zg3xvI",
      "jUleiG"
    )
  )

df = df |> select(subjectid, age, gender, diva_IA_symptoms, diva_HI_symptoms,
                  community_diagnosis_meds,
                  community_diagnosis_meds_type, 
                  community_diagnosis_meds_dosage,
                  community_diagnosis_meds_freq)



write.csv(df , file =  'data/raw_data/נתונים_לעידו_מלול.csv')

