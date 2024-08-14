library(magrittr)
library(dplyr)
library(data.table)
library(writexl)
library(ggplot2)
library(ggpubr)
library(tidyr)


tenth_percentile1 <- quantile(historya1$srsScores, probs = c(0.9))
# Create histogram using ggplot
Graph1 <- ggplot(DFfemaleyear3, aes(x = Gender_Dissonance)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7,fill="#C77CFF",color= "black") +
  labs(title = "Gender Dissonance Histogram", x = "Gender Dissonance") +
  theme_minimal()+ ylim(0,4500)
Graph1