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


#You can Skip Document_Processing after you've run it for the first time, 
#as "Dataframe_Restructuring_For_analysis will return to you an excel file 
#that can be used to skip the Document_Processing step
#in future variants (good to do cuz it takes a WHILE to run)
source("Document_Processing.R")
#Start Here if you have already run Document_Processing once
source("Dataframe_Restructuring_For_analysis.R")


#if you want to make the histogram of 3rd year FU AMAB gender dissonance scores 
#AKA (Figure 2A), run the following line of code 
source("Figure2A_AMAB.R")
#if you want to make the histogram of 3rd year FU AFAB gender dissonance scores 
#AKA (Figure 2A), run the following line of code 
source("Figure2A_AFAB.R")

#Below is the code needed to get the test of if gender diver children 
#have more gender dissonance than their gender congruent peers
source("Do Diverse Kids have more Gender Dissonance.R")


#below is the code needed to recreate figure 3. If the histograms are upside
#down, please run that line of code a second time. 
#You will know if the figures are upside down 
#if the error bars are invisible for any line
source("Figure3A.R")

source("Figure3B.R")

source("Figure3C.R")

source("Figure3D.R")

#If you want to make the demographics table (Table 1), run one of the following 
#four lines for each of the years included in the table
source("Demo_Table_Year_1.R")
source("Demo_Table_Year_2.R")
source("Demo_Table_Year_3.R")
source("Demo_Table_Year_4.R")

#Below is the code needed to get the test of if gender diver children 
#have more gender fluidity than their gender congruent peers
source("Do Diverse Kids Have More Fluidity.R.R")

#Next is the code needed to recreate figure 6
source("Averaged Fluidity Split By Ever Diverse.R")

#Next is the code needed to recreate figure 5's violin plots 
source("Figure5.R")

#Next is the code to run the linear mixed effects model comparing internalizing
#symptoms to gender dissonance. It will graph the model and export the model 
#summary. The plot can be found under the plots tab, and the model summary can 
#be found both in the console and the viewer tabs
source("LME_Internalizing.R")

#Next is the code to run the linear mixed effects model comparing externalizing
#symptoms to gender dissonance. It will graph the model and export the model 
#summary. The plot can be found under the plots tab, and the model summary can 
#be found both in the console and the viewer tabs
source("LME_Externalizing.R")



#Next is the code that runs the suicidality logistic regressions there is one
#line for each type of suicidality data
source("Suicidality Ideation Logistic Regression No Diverse control.R")
source("Suicidality Ideation Logistic Regressions.R")


source("Suicidality Plan Logistic Regression no diverse control.R")
source("Suicidality Plan Logistic Regression.R")

source("Suicidality Attempt Logistic Regression No Diverse Control.R")
source("Suicidality Attempt Logistic Regression.R")

#Next comes the code needed to run the autism linear regressions
#run base for the analysis in the paper
source("Autism Linear Regression Base.R")
#run total problem to get the total problem included model found in supplement
source("Autism Linear Regression With Problem Score.R")
#run the potter version to get the model using 
#an adaptation of potters felt gender metric as seen in supplement
source("Autism Linear Regression Potter.R")

#next is the code needed to run the gender fluidity internalizing regression
source("Gender Fluidity Internal without diverse controls.R")

source("Gender Fluidity Internal.R")



#next is the code needed to run the gender fluidity externalizing regression
source("Gender Fluidity External without diverse controls.R")

source("Gender Fluidity External.R")

#Below is the code needed to run the tails analysis found in figure S8
source("Tails_Analysis_Internalizing_Gender_Congruent.R")
source("Tails_Analysis_Externalizing_Gender_Congruent.R")
source("Tails_Analysis_Internalizing_Gender_Diverse.R")
source("Tails_Analysis_Externalizing_Gender_Diverse.R")

