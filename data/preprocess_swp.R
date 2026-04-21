# Preprocess SWP_CTW.rds → clean swp.rds for WebR
# Outcome: SWP (sustainable work participation, 1=yes/0=no)
# Predictors: age, sex, employment type, CTW, job demands/control/support,
#             mental wellbeing, demand-control category

d <- readRDS("C:/Users/nepet/Documents/Studier/Computer labs/STA230 R/Exercise 3/data/SWP_CTW.rds")

swp <- data.frame(
  SWP          = as.integer(as.character(d$SWP)),   # outcome: 0/1
  Age          = d$AgeYears,
  Sex          = factor(ifelse(d$V1 == 1, "Female", "Male")),
  Fulltime     = factor(ifelse(d$V5 == "Fulltime", "Yes", "No")),
  CTW          = d$CTW,              # Capacity to Work (T-score, mean≈100)
  JobDemands   = d$job_demands,
  JobControl   = d$job_control,
  JobSupport   = d$job_support,
  MentalWB     = d$MentalWellbeing,
  DemandCtrl   = factor(d$DemandControl)  # 0=low/low, 1=low/high, 2=high/low, 3=high/high
)

# Remove any remaining NAs
swp <- swp[complete.cases(swp), ]

cat("Rows:", nrow(swp), "\n")
cat("Outcome SWP:\n"); print(table(swp$SWP))
cat("\nClass balance:", round(mean(swp$SWP == 1) * 100, 1), "% SWP=1\n")
cat("\nVariables:", paste(names(swp), collapse=", "), "\n")
cat("\nSummary:\n"); print(summary(swp[, c("CTW","JobDemands","JobControl","MentalWB","Age")]))

out_path <- "C:/Users/nepet/Documents/Studier/Computer labs/STA230 R/App/data/swp.rds"
saveRDS(swp, out_path)
cat("\nSaved to:", out_path, "\n")
