martin = lmer(Gender_Dissonance~kbi_y_trans_id+Age+truesex+race_ethnicity+(1|src_subject_id), data = Glep_only_one_from_pair)
summary(martin)

print(tab_model(martin,show.stat = TRUE,string.stat = "T-Value", auto.label = FALSE, pred.labels = c("Intercept","Gender Diverse Identity", "Age", "Sex", "Race/Ethnicity")))
