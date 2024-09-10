library(magrittr)
library(dplyr)
library(data.table)
library(writexl)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(reshape2)
library(lme4)
library(lmerTest)
library(stringr)
library(QuantPsyc)
library(cowplot)
library(ggsignif) 
library(lme4)
install.packages("devtools") # Devtools is a package which allows to do this.
devtools::install_github("dustinfife/flexplot")
library(flexplot)
library(nlme)
library(tidyverse)
library(lme4)
library(ggsci)
library(cowplot)
library(sjPlot)
set_theme(base = theme_bw())
historya1r = historya1
historya1r$Gender_Dissonance = historya1r$expressed_gender -historya1r$felt_gender
historya1r <- filter(historya1r, src_subject_id %in% List_of_include)
historya1r$felt_gender_Dissonance = -1*historya1r$felt_gender_Dissonance 
historya1r <- filter(historya1r, Gender_Dissonance>=-4)
historya1r <- filter(historya1r, Gender_Dissonance<= 4)
historya1r <- filter(historya1r, felt_gender_Dissonance>=-4)
historya1r <- filter(historya1r, felt_gender_Dissonance<= 4)


fitSRS_below0 <- lm(srsScores~Gender_Dissonance+I(Gender_Dissonance^2)+truesex+Age+race_ethnicity+cbcl_scr_syn_totprob_r, data=  historya1r)
print(summary(fitSRS_below0))


print(sjPlot::plot_model(fitSRS_below0,type = "pred",terms = "Gender_Dissonance[all]",show.data = FALSE,show.p = TRUE,colors = "#00BFC4", plot.background = "white", ci_level = 0.95))
print(summ(fitSRS_below0))
print(tab_model(fitSRS_below0,show.stat = TRUE,string.stat = "T-Value", auto.label = FALSE, pred.labels = c("Intercept","Gender Dissonance","Gender Dissonance Squared", "Age", "Sex", "Race/Ethnicity", "CBCL Total Problem Score")))

print(lm.beta(fitSRS_below0))
