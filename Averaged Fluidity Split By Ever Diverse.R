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
print(
AHHHH2 %>%
  group_by(ever_diverse_status) %>%
  summarise(mean_fluidity = mean(averaged_Fluidity), standard_error = sd(averaged_Fluidity)/sqrt(length(averaged_Fluidity)),
            n = length(averaged_Fluidity), standarddev = sd(averaged_Fluidity)) %>%
  ggplot(aes(x= ever_diverse_status, fill = ever_diverse_status, ymin = mean_fluidity-(1.96*standard_error), ymax = mean_fluidity+(1.96*standard_error)))+
  scale_fill_manual(values = c("#00BFC4","#7CAE00","#C77CFF","#F8766D"))+
  geom_bar(position = "dodge", aes(y = mean_fluidity), stat="identity")+
  geom_errorbar(position=position_dodge(0.9),color="black")+
  geom_point(position = position_dodge(0.9), aes(y = mean_fluidity))
)