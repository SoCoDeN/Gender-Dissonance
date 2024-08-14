library(magrittr)
library(dplyr)
library(data.table)
library(writexl)
library(ggplot2)
library(ggpubr)
library(tidyr)



#create histogram using GGPLOT
Graph1 <- ggplot(DFmaleyear3, aes(x = Gender_Dissonance)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7,fill="gray", color = "black") +
  labs(title = "Gender Dissonance Histogram", x = "Gender Dissonance") +
  theme_minimal()+ ylim(0,4500)
Graph1