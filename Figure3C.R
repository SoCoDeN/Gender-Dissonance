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

historya3$Gender_Dissonance <- -1*historya3$Gender_Dissonance
historya3$kbi_y_trans_id[historya3$kbi_y_trans_id==1] <- "Yes"
historya3$kbi_y_trans_id[historya3$kbi_y_trans_id==2] <- "Maybe"
historya3$kbi_y_trans_id[historya3$kbi_y_trans_id==3] <- "No"
historya3$kbi_y_trans_id[historya3$kbi_y_trans_id==4] <- "I Don't Understand"
historya3$kbi_y_trans_id <- factor(historya3$kbi_y_trans_id, levels = c ("No","Maybe","Yes", "I Don't Understand"))
historya31 <- filter(historya3, !is.na(historya3$kbi_y_trans_id))

print(
historya31 %>%
  group_by(kbi_y_trans_id) %>%
  summarise(mean_GD = mean(Gender_Dissonance), standard_error = sd(Gender_Dissonance)/sqrt(length(Gender_Dissonance)),
            n = length(Gender_Dissonance), standarddev = sd(Gender_Dissonance)) %>%
  ggplot(aes(x= kbi_y_trans_id, fill = kbi_y_trans_id, ymin = mean_GD-(1.96*standard_error), ymax = mean_GD+(1.96*standard_error)))+
  scale_fill_manual(values = c("#00BFC4","#7CAE00","#C77CFF","#F8766D"))+
  geom_bar(position = "dodge", aes(y = mean_GD), stat="identity")+
  geom_errorbar(position=position_dodge(0.9),color="black")+
  theme_bw()+
  ylim(-0.6,0.7)+
  geom_point(position = position_dodge(0.9), aes(y = mean_GD))
)