# Preprocess NHANES 2015-2016 data → clean nhanes.rds
library(haven)
library(dplyr)

data_dir <- "C:/Users/nepet/Documents/Studier/Computer labs/STA230 R/Assignments/Assignment 1 - Supervised Regression Numerical outcome/data"
out_path <- "C:/Users/nepet/Documents/Studier/Computer labs/STA230 R/App/data/nhanes.rds"

demo  <- read_xpt(file.path(data_dir, "DEMO_I.xpt"))
bpx   <- read_xpt(file.path(data_dir, "BPX_I.xpt"))
bmx   <- read_xpt(file.path(data_dir, "BMX_I.xpt"))
tchol <- read_xpt(file.path(data_dir, "TCHOL_I.xpt"))
diq   <- read_xpt(file.path(data_dir, "DIQ_I.xpt"))
smq   <- read_xpt(file.path(data_dir, "SMQ_I.xpt"))
dxa   <- read_xpt(file.path(data_dir, "DXX_I.xpt"))
rhq   <- read_xpt(file.path(data_dir, "RHQ_I.xpt"))
cbc   <- read_xpt(file.path(data_dir, "CBC_I.xpt"))
fertn <- read_xpt(file.path(data_dir, "FERTIN_I.xpt"))
bio   <- read_xpt(file.path(data_dir, "BIOPRO_I.xpt"))

# Select relevant columns
demo_s  <- demo  |> select(SEQN, Age=RIDAGEYR, Sex=RIAGENDR,
                            Race=RIDRETH3, Income_pov=INDFMPIR)
bpx_s   <- bpx   |> select(SEQN, BPXSY1, BPXSY2, BPXSY3,
                                   BPXDI1, BPXDI2, BPXDI3)
bmx_s   <- bmx   |> select(SEQN, BMI=BMXBMI, Waist=BMXWAIST,
                            Height=BMXHT, Weight=BMXWT)
tchol_s <- tchol |> select(SEQN, Cholesterol=LBXTC)
diq_s   <- diq   |> select(SEQN, Diabetes_raw=DIQ010)
smq_s   <- smq   |> select(SEQN, Smoke_raw=SMQ040)
dxa_s   <- dxa   |> select(SEQN, BodyFatPct=DXDTOPF)
rhq_s   <- rhq   |> select(SEQN, HadPeriod_raw=RHQ031,
                            PeriodsPerYear=RHQ166)
cbc_s   <- cbc   |> select(SEQN, Hemoglobin=LBXHGB, Hematocrit=LBXHCT)
fertn_s <- fertn |> select(SEQN, Ferritin=LBXFER)
bio_s   <- bio   |> select(SEQN, Albumin=LBXSAL, TotalProtein=LBXSTP,
                            Iron=LBXSIR)

# Merge all on SEQN
nhanes <- demo_s |>
  left_join(bpx_s,   by="SEQN") |>
  left_join(bmx_s,   by="SEQN") |>
  left_join(tchol_s, by="SEQN") |>
  left_join(diq_s,   by="SEQN") |>
  left_join(smq_s,   by="SEQN") |>
  left_join(dxa_s,   by="SEQN") |>
  left_join(rhq_s,   by="SEQN") |>
  left_join(cbc_s,   by="SEQN") |>
  left_join(fertn_s, by="SEQN") |>
  left_join(bio_s,   by="SEQN")

# Create clean variables
nhanes <- nhanes |>
  mutate(
    SBP = rowMeans(cbind(BPXSY1, BPXSY2, BPXSY3), na.rm=TRUE),
    DBP = rowMeans(cbind(BPXDI1, BPXDI2, BPXDI3), na.rm=TRUE),
    Sex = factor(ifelse(Sex==1, "Male", "Female")),
    Race = factor(case_when(
      Race == 1 ~ "Mexican American",
      Race == 2 ~ "Other Hispanic",
      Race == 3 ~ "Non-Hispanic White",
      Race == 4 ~ "Non-Hispanic Black",
      Race == 6 ~ "Non-Hispanic Asian",
      TRUE      ~ "Other"
    )),
    Diabetes = case_when(
      Diabetes_raw == 1 ~ 1L,
      Diabetes_raw == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),
    Smoker = case_when(
      Smoke_raw %in% c(1, 2) ~ 1L,
      Smoke_raw == 3          ~ 0L,
      TRUE ~ NA_integer_
    ),
    # 1=had period, 0=no period, NA=men/missing
    HadPeriod = case_when(
      HadPeriod_raw == 1 ~ 1L,
      HadPeriod_raw == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) |>
  filter(Age >= 18, !is.nan(SBP), !is.na(SBP)) |>
  select(SEQN, Age, Sex, Race, Income_pov,
         BMI, Waist, Height, Weight,
         Cholesterol, Diabetes, Smoker,
         SBP, DBP, BodyFatPct,
         HadPeriod, PeriodsPerYear,
         Hemoglobin, Hematocrit, Ferritin,
         Albumin, TotalProtein, Iron)

nhanes_clean <- nhanes |>
  filter(!is.na(BMI), !is.na(Age), !is.na(Sex),
         !is.na(Cholesterol), !is.na(Waist))

cat("Rows:", nrow(nhanes_clean), "\n")
cat("Variables:", paste(names(nhanes_clean), collapse=", "), "\n")

saveRDS(nhanes_clean, out_path)
cat("Saved to:", out_path, "\n")
