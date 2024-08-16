historya3_Bianarized_GD <- historya3
historya3_Bianarized_GD$Gender_Dissonance[historya3_Bianarized_GD$ABS_GD<=1] <- 0
historya3_Bianarized_GD$Gender_Dissonance[historya3_Bianarized_GD$ABS_GD>1] <-1
historya3_Bianarized_GD_trans <- filter(historya3_Bianarized_GD, kbi_y_trans_id == "No")

fit1 <- lm(cbcl_scr_syn_internal_r~Gender_Dissonance+race_ethnicity+truesex+Age , data= historya3_Bianarized_GD_trans)
summary(fit1)
print(tab_model(fit1,show.stat = TRUE,string.stat = "T-Value", auto.label = FALSE, pred.labels = c("Intercept","Gender Dissonance", "Race/Ethnicity", "Sex", "Age")))
