gender_Fluidity_measure <- lm(averaged_Fluidity~EverDiverse+Age_4+truesex_1+race_ethnicity_1, data = AHHHHflipped)
summary(gender_Fluidity_measure)
lm.beta(gender_Fluidity_measure)
print(tab_model(gender_Fluidity_measure,show.stat = TRUE,string.stat = "T-Value", auto.label = FALSE, pred.labels = c("Intercept","Gender Diverse Identity", "Age", "Sex", "Race/Ethnicity")))
