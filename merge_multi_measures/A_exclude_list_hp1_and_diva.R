# A: Load hp1 and diva; define included participants (pass CUDIT + AUDIT + diva match).
# Saves subjectid_included to filtered_data folder and prints omission counts.

library(dplyr)

if (basename(getwd()) == "merge_multi_measures") setwd("..")

load("data/raw_data/hp1.Rdata")
load("data/raw_data/diva.Rdata")

#### One row per subject (keep first row per subjectid so we don't drop anyone) ----
hp1_one  <- hp1  |> mutate(subjectid = trimws(as.character(subjectid))) |> group_by(subjectid) |> slice_head(n = 1) |> ungroup()
diva_one <- diva |> mutate(subjectid = trimws(as.character(subjectid))) |> group_by(subjectid) |> slice_head(n = 1) |> ungroup()

diva_one <- diva_one |>
  mutate(
    diva_group = case_when(
      diva_diagnosis == "meet_diva_criteria"  ~ "ADHD",
      diva_diagnosis == "below_diva_criteria" ~ "TD",
      TRUE ~ NA_character_
    )
  )

#### Merge: only participants who have BOTH hp1 and diva (inner_join) ----
hp1_diva <- hp1_one |>
  select(subjectid, cohort, alcohol_use_cutoff, cannabis_use_cutoff) |>
  inner_join(
    diva_one |> select(subjectid, declared_group, diva_group),
    by = "subjectid"
  )

n_total <- nrow(hp1_diva)

#### Compare declared vs diva as character (avoid factor level mismatch) ----
declared_chr <- as.character(hp1_diva$declared_group)
diva_chr     <- as.character(hp1_diva$diva_group)
diva_match   <- !is.na(declared_chr) & !is.na(diva_chr) & (declared_chr == diva_chr)

#### Included: pass both CUDIT and AUDIT, and declared_group == diva_group ----
pass_audit <- is.na(hp1_diva$alcohol_use_cutoff) | (hp1_diva$alcohol_use_cutoff != "above_audit_cutoff")
pass_cudit <- is.na(hp1_diva$cannabis_use_cutoff) | (hp1_diva$cannabis_use_cutoff != "above_cudit_cutoff")

subjectid_included <- hp1_diva$subjectid[pass_audit & pass_cudit & diva_match]
subjectid_included <- as.character(subjectid_included)

#### Omission counts ----
# CUDIT/AUDIT/both: over ALL hp1 subjects (whether or not they appear in diva)
n_omit_CUDIT <- sum(hp1_one$cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE)
n_omit_AUDIT <- sum(hp1_one$alcohol_use_cutoff == "above_audit_cutoff", na.rm = TRUE)
n_omit_both  <- sum(hp1_one$alcohol_use_cutoff == "above_audit_cutoff" &
                    hp1_one$cannabis_use_cutoff == "above_cudit_cutoff", na.rm = TRUE)
# TD/ADHD diva mismatch: only among participants who have both hp1 and diva
n_omit_TD_high_diva  <- sum(declared_chr == "TD" & diva_chr == "ADHD", na.rm = TRUE)
n_omit_ADHD_low_diva <- sum(declared_chr == "ADHD" & diva_chr == "TD", na.rm = TRUE)

# Denominators for percentages: all hp1 for CUDIT/AUDIT/both; n_total for diva mismatches
n_hp1 <- nrow(hp1_one)
pct_CUDIT         <- if (n_hp1 > 0) round(100 * n_omit_CUDIT / n_hp1, 1) else 0
pct_AUDIT         <- if (n_hp1 > 0) round(100 * n_omit_AUDIT / n_hp1, 1) else 0
pct_both          <- if (n_hp1 > 0) round(100 * n_omit_both / n_hp1, 1) else 0
pct_TD_high_diva  <- if (n_total > 0) round(100 * n_omit_TD_high_diva / n_total, 1) else 0
pct_ADHD_low_diva <- if (n_total > 0) round(100 * n_omit_ADHD_low_diva / n_total, 1) else 0

#### Print to console ----
message("")
message("Raw: hp1 ", nrow(hp1), " rows, diva ", nrow(diva), " rows.")
message("After one row per subject: hp1 ", nrow(hp1_one), ", diva ", nrow(diva_one), ".")
message("Participants with BOTH hp1 and diva: ", n_total)
message("")
message("Participants omitted (CUDIT/AUDIT/both = % of all hp1; TD/ADHD = % of those with diva):")
message("  Due to CUDIT:              ", n_omit_CUDIT, " (", pct_CUDIT, "%)")
message("  Due to AUDIT:              ", n_omit_AUDIT, " (", pct_AUDIT, "%)")
message("  Due to both (CUDIT+AUDIT): ", n_omit_both, " (", pct_both, "%)")
message("  TD with high DIVA:         ", n_omit_TD_high_diva, " (", pct_TD_high_diva, "%)  [declared TD, diva = ADHD]")
message("  ADHD with low DIVA:        ", n_omit_ADHD_low_diva, " (", pct_ADHD_low_diva, "%)  [declared ADHD, diva = TD]")
message("")
message("Participants included (pass CUDIT + AUDIT + diva match): ", length(subjectid_included))

#### Save subjectid_included to filtered_data folder ----
dir.create("data/filtered_data", showWarnings = FALSE, recursive = TRUE)
save(subjectid_included, file = "data/filtered_data/subjectid_included.Rdata")
write.csv(data.frame(subjectid = subjectid_included), file = "data/filtered_data/subjectid_included.csv", row.names = FALSE)
