load("data/data_PimaIndiansDiabetes.rda")

data_diabetes_mediation <- data_PimaIndiansDiabetes |>
  transform(
    diabetes = factor(diabetes, levels = c("neg", "pos"), labels = c("No", "Yes")),
    obesity = factor(ifelse(mass >= 30, "Yes", "No"), levels = c("No", "Yes")),
    bmi = mass,
    blood_pressure = pressure,
    pregnancies = pregnant,
    diabetes_pedigree = pedigree
  )

keep <- c(
  "diabetes", "obesity", "glucose", "bmi", "age", "blood_pressure",
  "pregnancies", "diabetes_pedigree"
)
data_diabetes_mediation <- data_diabetes_mediation[stats::complete.cases(data_diabetes_mediation[, keep]), keep]

attr(data_diabetes_mediation$diabetes, "label") <- "Diabetes"
attr(data_diabetes_mediation$obesity, "label") <- "Obesity"
attr(data_diabetes_mediation$glucose, "label") <- "Plasma glucose"
attr(data_diabetes_mediation$bmi, "label") <- "Body mass index"
attr(data_diabetes_mediation$age, "label") <- "Age"
attr(data_diabetes_mediation$blood_pressure, "label") <- "Diastolic blood pressure"
attr(data_diabetes_mediation$pregnancies, "label") <- "Number of pregnancies"
attr(data_diabetes_mediation$diabetes_pedigree, "label") <- "Diabetes pedigree function"

save(data_diabetes_mediation, file = "data/data_diabetes_mediation.rda", compress = "xz")
