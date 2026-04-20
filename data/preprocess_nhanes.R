# Preprocess NHANES 2015-2016 data → clean nhanes.rds for WebR
# Outcome: mean systolic blood pressure
# Predictors: age, sex, BMI, waist, total cholesterol, diabetes, smoking, income

library(haven)
library(dplyr)

data_dir <- "C:/Users/nepet/Documents/Studier/Computer labs/STA230 R/Exercise 1/data"
out_path <- "C:/Users/nepet/Documents/Studier/Computer labs/STA230 R/App/data/nhanes.rds"

demo  <- read_xpt(file.path(data_dir, "DEMO_I.xpt"))
bpx   <- read_xpt(file.path(data_dir, "BPX_I.xpt"))
bmx   <- read_xpt(file.path(data_dir, "BMX_I.xpt"))
tchol <- read_xpt(file.path(data_dir, "TCHOL_I.xpt"))
diq   <- read_xpt(file.path(data_dir, "DIQ_I.xpt"))
smq   <- read_xpt(file.path(data_dir, "SMQ_I.xpt"))

# Select relevant columns
demo_s  <- demo  |> select(SEQN, Age=RIDAGEYR, Sex=RIAGENDR,
                            Race=RIDRETH3, Income_pov=INDFMPIR)
bpx_s   <- bpx   |> select(SEQN, BPXSY1, BPXSY2, BPXSY3,
                                   BPXDI1, BPXDI2, BPXDI3)
bmx_s   <- bmx   |> select(SEQN, BMI=BMXBMI, Waist=BMXWAIST, Height=BMXHT, Weight=BMXWT)
tchol_s <- tchol |> select(SEQN, Cholesterol=LBXTC)
diq_s   <- diq   |> select(SEQN, Diabetes_raw=DIQ010)
smq_s   <- smq   |> select(SEQN, Smoke_raw=SMQ040)

# Merge all on SEQN
nhanes <- demo_s |>
  left_join(bpx_s,   by="SEQN") |>
  left_join(bmx_s,   by="SEQN") |>
  left_join(tchol_s, by="SEQN") |>
  left_join(diq_s,   by="SEQN") |>
  left_join(smq_s,   by="SEQN")

# Create clean variables
nhanes <- nhanes |>
  mutate(
    # Mean systolic BP from up to 3 readings
    SBP = rowMeans(cbind(BPXSY1, BPXSY2, BPXSY3), na.rm=TRUE),
    # Mean diastolic BP
    DBP = rowMeans(cbind(BPXDI1, BPXDI2, BPXDI3), na.rm=TRUE),
    # Sex as factor
    Sex = factor(ifelse(Sex==1, "Male", "Female")),
    # Race/ethnicity
    Race = factor(case_when(
      Race == 1 ~ "Mexican American",
      Race == 2 ~ "Other Hispanic",
      Race == 3 ~ "Non-Hispanic White",
      Race == 4 ~ "Non-Hispanic Black",
      Race == 6 ~ "Non-Hispanic Asian",
      TRUE      ~ "Other"
    )),
    # Diabetes: 1=Yes, 2=No, 3=Borderline → NA
    Diabetes = case_when(
      Diabetes_raw == 1 ~ 1L,
      Diabetes_raw == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    # Current smoker: 1=every day, 2=some days → 1; 3=not at all → 0
    Smoker = case_when(
      Smoke_raw %in% c(1, 2) ~ 1L,
      Smoke_raw == 3          ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  # Keep adults 18+ with complete BP measurement
  filter(Age >= 18, !is.nan(SBP), !is.na(SBP)) |>
  select(SEQN, Age, Sex, Race, Income_pov,
         BMI, Waist, Height, Weight,
         Cholesterol, Diabetes, Smoker,
         SBP, DBP)

# Remove rows with NAs in key predictors
nhanes_clean <- nhanes |>
  filter(!is.na(BMI), !is.na(Age), !is.na(Sex),
         !is.na(Cholesterol), !is.na(Waist))

cat("Rows in clean dataset:", nrow(nhanes_clean), "\n")
cat("Variables:", paste(names(nhanes_clean), collapse=", "), "\n")
cat("\nSummary SBP:\n")
print(summary(nhanes_clean$SBP))

saveRDS(nhanes_clean, out_path)
cat("\nSaved to:", out_path, "\n")
