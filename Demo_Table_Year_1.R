library(gtsummary)
library(magrittr)
library(dplyr)
library(data.table)
library(writexl)
library(ggplot2)
library(ggpubr)
library(tidyr)


table_one <- historya1 %>% select(Age,truesex,race_ethnicity,kbi_y_trans_id,demo_prnt_ed_v2_2yr_l, demo_prnt_ed_v2_l,eventname)


table_one$truesex[table_one$truesex==1]<-"Male"
table_one$truesex[table_one$truesex==2]<-"Female"
table_one$truesex[table_one$truesex==3]<-"Other"

table_one$race_ethnicity[table_one$race_ethnicity==1]<- "White"
table_one$race_ethnicity[table_one$race_ethnicity==2]<- "Black"
table_one$race_ethnicity[table_one$race_ethnicity==3]<- "Hispanic"
table_one$race_ethnicity[table_one$race_ethnicity==4]<- "Asian"
table_one$race_ethnicity[table_one$race_ethnicity==5]<- "Other"

table_one$kbi_y_trans_id[table_one$kbi_y_trans_id==1] <- "Yes"
table_one$kbi_y_trans_id[table_one$kbi_y_trans_id==2] <- "Maybe"
table_one$kbi_y_trans_id[table_one$kbi_y_trans_id==3] <- "No"
table_one$kbi_y_trans_id[table_one$kbi_y_trans_id==4] <- "I Don't Understand"
table_one$kbi_y_trans_id[table_one$kbi_y_trans_id==777] <- "Refuse to Answer"

table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==0] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==1] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==2] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==3] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==4] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==5] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==6] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==7] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==8] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==9] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==10] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==11] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==12] <- "< High School Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==13] <- "High School Graduate"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==14] <- "GED or equivalent Diploma"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==15] <- "Some College/Associate/Vocational"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==16] <- "Associate's Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==17] <- "Associate's Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==18] <- "Bachelor's Degree"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==19] <- "Advanced Degrees"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==20] <- "Advanced Degrees"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==21] <- "Advanced Degrees"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==777] <- "Refuse To Answer"
table_one$demo_prnt_ed_v2_l[table_one$demo_prnt_ed_v2_l==999] <- "Don't Know"

table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==0] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==1] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==2] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==3] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==4] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==5] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==6] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==7] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==8] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==9] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==10] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==11] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==12] <- "< High School Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==13] <- "High School Graduate"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==14] <- "GED or equivalent Diploma"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==22] <- "Some College/Associate/Vocational"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==23] <- "Some College/Associate/Vocational"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==16] <- "Some College/Associate/Vocational"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==17] <- "Some College/Associate/Vocational"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==18] <- "Bachelor's Degree"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==19] <- "Advanced Degrees"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==20] <- "Advanced Degrees"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==21] <- "Advanced Degrees"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==777] <- "Refuse To Answer"
table_one$demo_prnt_ed_v2_2yr_l[table_one$demo_prnt_ed_v2_2yr_l==999] <- "Don't Know"

table_one %>% tbl_summary(label = list(Age ~"Patient Age", truesex~"Sex Assigned at Birth", kbi_y_trans_id~"Are you Transgender?")) %>% modify_header(label = "**1st Year Follow up**")
