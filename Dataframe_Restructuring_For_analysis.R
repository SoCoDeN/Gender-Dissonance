library(magrittr)
library(dplyr)
library(data.table)
library(writexl)
library(ggplot2)
library(ggpubr)
library(tidyr)


DF4 <- data_analysis_4_projectversion3
DF4$felt_gender_Dissonance = numeric(nrow(DF4))
DF4year1 <- filter(DF4, eventname == "baseline_year_1_arm_1")
DF4year2 <- filter(DF4, eventname == "1_year_follow_up_y_arm_1")
DF4year3 <- filter(DF4, eventname == "2_year_follow_up_y_arm_1")
DF4year4 <- filter(DF4, eventname == "3_year_follow_up_y_arm_1")
DF4year5 <- filter(DF4, eventname == "4_year_follow_up_y_arm_1")
#With the above skeleton of how to populate SRS scores into the spreadsheet, 
#I can add in data for mental health outcomes, and later sum terms. 
for (Subject in DF4$src_subject_id){
  e = which(DF4$src_subject_id == Subject)
  for (i in e){
    if (DF4[i,"truesex"] ==1 ){
      DF4[i, "felt_gender_Dissonance"] <- (pull(DF4[i, "gish_m1_y"])- pull(DF4[i, "gish_m2_y"]))
    }
    if (DF4[i,"truesex"] ==2 ){
      DF4[i, "felt_gender_Dissonance"] <- (pull(DF4[i, "gish_f1_y"])- pull(DF4[i, "gish_f2_y"]))
    }}}
DF4 <- DF4 %>% drop_na(Gender_Dissonance)

DFfemale = DF4
#DFfemale$sex = numeric(nrow(DFfemale))
#for (Subject in DFfemale$src_subject_id){
#  e = which(DFfemale$src_subject_id == Subject)
#  for (i in e){
#    DFfemale[i, "sex"] <- sum(pull(DFfemale[i,"gish_f1_y"]), pull(DFfemale[i,"gish_f2_y"]),pull(DFfemale[i,"gish_f3_y"]),pull(DFfemale[i,"gish_f4_y"]))
#  }
#}

DFmale <- filter(DFfemale, truesex ==1)
is_empty = cbind(DFmale$gish_m1_y, DFmale$gish_m2_y,DFmale$gish_m3_y,DFmale$gish_m4_y)
rs = rowSums(is_empty)
#DFmale <-cbind(DFmale,rs)

DFfemale <- filter(DFfemale, truesex != 1)
DFmaletotal <-filter(DFmale, eventname != "baseline_year_1_arm_1")
DFfemaletotal <- filter(DFfemale, eventname != "baseline_year_1_arm_1")

#Here I am splitting DF male and DF female by year


DFfemalebaseyear <- filter(DFfemale, eventname == "baseline_year_1_arm_1")
DFfemaleyear1 <- filter(DFfemale, eventname == "1_year_follow_up_y_arm_1")
DFfemaleyear2 <- filter(DFfemale, eventname == "2_year_follow_up_y_arm_1")
DFfemaleyear3 <- filter(DFfemale, eventname == "3_year_follow_up_y_arm_1")
DFfemaleyear4 <- filter(DFfemale, eventname == "4_year_follow_up_y_arm_1")

DFmalebaseyear <- filter(DFmale, eventname == "baseline_year_1_arm_1")
DFmaleyear1 <- filter(DFmale, eventname == "1_year_follow_up_y_arm_1")
DFmaleyear2 <- filter(DFmale, eventname == "2_year_follow_up_y_arm_1")
DFmaleyear3 <- filter(DFmale, eventname == "3_year_follow_up_y_arm_1")
DFmaleyear4 <- filter(DFmale, eventname == "4_year_follow_up_y_arm_1")
write_xlsx(DF4,"/Users/swiggardlo/Downloads/data_analysis_4_projectversion3.xlsx")

#From here, I am now removing zeros from each year of DFmale and DF female, 
#as zeros are the main contributor to the histograms 
DFfemaleyear1no0 <-filter(DFfemaleyear1, Gender_Dissonance != 0)
DFfemaleyear2no0 <-filter(DFfemaleyear2, Gender_Dissonance != 0)
DFfemaleyear3no0 <-filter(DFfemaleyear3, Gender_Dissonance != 0)
DFfemaleyear4no0 <-filter(DFfemaleyear4, Gender_Dissonance != 0)
DFfemaletotalno0 <-filter(DFfemaletotal, Gender_Dissonance != 0)
DFmaleyear1no0 <-filter(DFmaleyear1, Gender_Dissonance != 0)
DFmaleyear2no0 <-filter(DFmaleyear2, Gender_Dissonance != 0)
DFmaleyear3no0 <-filter(DFmaleyear3, Gender_Dissonance != 0)
DFmaleyear4no0 <-filter(DFmaleyear4, Gender_Dissonance != 0)
DFmaletotalno0 <-filter(DFmaletotal, Gender_Dissonance != 0)
#Here is where I start plotting histograms. First set are the the raw histograms
#split by gender and year (zero is included)
#par(mfrow = c(2,5))
#hist(DFfemaletotal$Gender_Dissonance, main = "DFfemaletotal", labels = T)
#hist(DFfemaleyear1$Gender_Dissonance, main= "DFfemaleyear1", labels = T)
#hist(DFfemaleyear2$Gender_Dissonance, main= "DFfemaleyear2",labels = T)
#hist(DFfemaleyear3$Gender_Dissonance, main= "DFfemaleyear3",labels = T)
#hist(DFfemaleyear4$Gender_Dissonance, main= "DFfemaleyear4",labels = T)
#hist(DFmaletotal$Gender_Dissonance, main = "DFmaletotal", labels = T)
#hist(DFmaleyear1$Gender_Dissonance, main= "DFmaleyear1",labels = T)
#hist(DFmaleyear2$Gender_Dissonance, main= "DFmaleyear2",labels = T)
#hist(DFmaleyear3$Gender_Dissonance, main= "DFmaleyear3",labels = T)
#hist(DFmaleyear4$Gender_Dissonance, main= "DFmaleyear4",labels = T)
#Here is where I plot the histograms split by gender and year, but exclude 0
#par(mfrow = c(2,5))
#hist(DFfemaletotalno0$Gender_Dissonance, main = "DFfemaletotalNO0", labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFfemaleyear1no0$Gender_Dissonance, main= "DFfemaleyear1NO0", labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFfemaleyear2no0$Gender_Dissonance, main= "DFfemaleyear2NO0",labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFfemaleyear3no0$Gender_Dissonance, main= "DFfemaleyear3NO0",labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFfemaleyear4no0$Gender_Dissonance, main= "DFfemaleyear4NO0",labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFmaletotalno0$Gender_Dissonance, main = "DFmaletotalNO0", labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFmaleyear1no0$Gender_Dissonance, main= "DFmaleyear1NO0",labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFmaleyear2no0$Gender_Dissonance, main= "DFmaleyear2NO0",labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFmaleyear3no0$Gender_Dissonance, main= "DFmaleyear3NO0",labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))
#hist(DFmaleyear4no0$Gender_Dissonance, main= "DFmaleyear4NO0",labels = T,breaks = seq(-5,5,0.25), xlim = c(-5,5), xaxp = c(-5,5,40))





# Combine data frames
history1 <- rbind(DFfemaleyear1no0, DFmaleyear1no0, fill = TRUE)
history2 <- rbind(DFfemaleyear2no0, DFmaleyear2no0, fill = TRUE)
history3 <- rbind(DFfemaleyear3no0, DFmaleyear3no0, fill = TRUE)
history4 <- rbind(DFfemaleyear4no0, DFmaleyear4no0, fill = TRUE)
historytotal <- rbind(DFfemaletotalno0, DFmaletotalno0, fill = TRUE)
#Combining dataframes with zeros
historya1 <- rbind(DFfemaleyear1, DFmaleyear1, fill = TRUE)
historya2 <- rbind(DFfemaleyear2, DFmaleyear2, fill = TRUE)
historya3 <- rbind(DFfemaleyear3, DFmaleyear3, fill = TRUE)
historya4 <- rbind(DFfemaleyear4, DFmaleyear4, fill = TRUE)
historyatotal <- rbind(DFfemaletotal, DFmaletotal, fill = TRUE)


names(historyatotal)[names(historyatotal)=="eventname.x"]<- "eventname"
historyatotaltest <-  merge(historyatotal, Liam_mh_y_ksads_si, 
                            by = c("src_subject_id","eventname"), all.x = TRUE)

historyatotaltest <-  merge(historyatotal, mh_y_ksads_ss, 
                            by = c("src_subject_id","eventname"), all.x = TRUE)
Historyatotal_sexuality <- merge(historyatotal, )
historyatotaltest$ksads_23_147_t[is.na(historyatotaltest$ksads_23_147_t)]<- 7
historyatotaltest$ksads_23_148_t[is.na(historyatotaltest$ksads_23_148_t)]<- 7
historyatotaltest$ksads2_23_905_t[is.na(historyatotaltest$ksads2_23_905_t)]<- 7
historyatotaltest$ever_ideation <- (historyatotaltest$ksads_23_147_t==1|historyatotaltest$ksads_23_148_t==1|historyatotaltest$ksads2_23_905_t==1)
historyatotaltest$ever_ideation <- as.factor(historyatotaltest$ever_ideation)
#now comes plan
historyatotaltest$ksads_23_809_t[is.na(historyatotaltest$ksads_23_809_t)]<- 7
historyatotaltest$ksads_23_810_t[is.na(historyatotaltest$ksads_23_810_t)]<- 7
historyatotaltest$ksads_23_811_t[is.na(historyatotaltest$ksads_23_811_t)]<- 7
historyatotaltest$ksads_23_812_t[is.na(historyatotaltest$ksads_23_812_t)]<- 7
historyatotaltest$ksads_23_818_t[is.na(historyatotaltest$ksads_23_818_t)]<- 7
historyatotaltest$ksads_23_819_t[is.na(historyatotaltest$ksads_23_819_t)]<- 7
historyatotaltest$ksads_23_820_t[is.na(historyatotaltest$ksads_23_820_t)]<- 7
historyatotaltest$ksads_23_821_t[is.na(historyatotaltest$ksads_23_821_t)]<- 7
historyatotaltest$ksads2_23_906_t[is.na(historyatotaltest$ksads2_23_906_t)]<- 7
historyatotaltest$ksads2_23_907_t[is.na(historyatotaltest$ksads2_23_907_t)]<- 7
historyatotaltest$ksads2_23_908_t[is.na(historyatotaltest$ksads2_23_908_t)]<- 7
historyatotaltest$ksads2_23_909_t[is.na(historyatotaltest$ksads2_23_909_t)]<- 7
historyatotaltest$ksads2_23_910_t[is.na(historyatotaltest$ksads2_23_910_t)]<- 7
historyatotaltest$ksads2_23_911_t[is.na(historyatotaltest$ksads2_23_911_t)]<- 7
historyatotaltest$ksads2_23_922_t[is.na(historyatotaltest$ksads2_23_922_t)]<- 7
historyatotaltest$ever_suicide_plan <- (historyatotaltest$ksads_23_809_t==1|historyatotaltest$ksads_23_810_t==1|historyatotaltest$ksads_23_811_t==1|historyatotaltest$ksads_23_812_t==1|historyatotaltest$ksads_23_818_t==1|historyatotaltest$ksads_23_819_t==1|historyatotaltest$ksads_23_820_t==1|historyatotaltest$ksads_23_821_t==1|historyatotaltest$ksads2_23_906_t==1|historyatotaltest$ksads2_23_907_t==1|historyatotaltest$ksads2_23_908_t==1|historyatotaltest$ksads2_23_909_t==1|historyatotaltest$ksads2_23_910_t==1|historyatotaltest$ksads2_23_911_t==1|historyatotaltest$ksads2_23_922_t==1)
#ever plan encompasses suicidal ideation with an active plan, wether descript or non descript up to sepseif preperations made for the attempt 

#attempt will cover interrupted attempt, abordted attempt and suicide attempt.
historyatotaltest$ksads_23_822_t[is.na(historyatotaltest$ksads_23_822_t)]<- 7
historyatotaltest$ksads_23_149_t[is.na(historyatotaltest$ksads_23_149_t)]<- 7
historyatotaltest$ksads_23_150_t[is.na(historyatotaltest$ksads_23_150_t)]<- 7
historyatotaltest$ksads2_23_912_t[is.na(historyatotaltest$ksads2_23_912_t)]<- 7
historyatotaltest$ksads2_23_913_t[is.na(historyatotaltest$ksads2_23_913_t)]<- 7
historyatotaltest$ksads2_23_914_t[is.na(historyatotaltest$ksads2_23_914_t)]<- 7
historyatotaltest$ksads2_23_923_t[is.na(historyatotaltest$ksads2_23_923_t)]<- 7
historyatotaltest$ksads2_23_924_t[is.na(historyatotaltest$ksads2_23_924_t)]<- 7
historyatotaltest$ksads2_23_925_t[is.na(historyatotaltest$ksads2_23_925_t)]<- 7
historyatotaltest$ever_attempt <- (historyatotaltest$ksads_23_822_t==1|historyatotaltest$ksads_23_149_t==1|historyatotaltest$ksads_23_150_t==1|historyatotaltest$ksads2_23_912_t==1|historyatotaltest$ksads2_23_913_t==1|historyatotaltest$ksads2_23_914_t==1|historyatotaltest$ksads2_23_923_t==1|historyatotaltest$ksads2_23_924_t==1|historyatotaltest$ksads2_23_925_t==1)


Data_I_need1 <- historyatotal %>% dplyr::select(src_subject_id,eventname, cbcl_scr_syn_internal_r,cbcl_scr_syn_external_r,Gender_Dissonance,Age,truesex,race_ethnicity,kbi_y_trans_id)
Data_I_need1 <- na.omit(Data_I_need1)
##need to problemsolve here, 
Data_I_need <- historyatotaltest %>% dplyr::select(src_subject_id,eventname, cbcl_scr_syn_internal_r,cbcl_scr_syn_external_r,Gender_Dissonance,Age,truesex,race_ethnicity,kbi_y_trans_id)
Data_I_need <- na.omit(Data_I_need)
Data_I_need$race_ethnicity <- Data_I_need$race_ethnicity
Data_I_need$eventname[Data_I_need$eventname=="1_year_follow_up_y_arm_1"] <- 1
Data_I_need$eventname[Data_I_need$eventname=="2_year_follow_up_y_arm_1"] <- 2
Data_I_need$eventname[Data_I_need$eventname=="3_year_follow_up_y_arm_1"] <- 3
Data_I_need$eventname[Data_I_need$eventname=="4_year_follow_up_y_arm_1"] <- 4
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==1] <- 1
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==2] <- 2
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==3] <- 3
Data_I_need <- Data_I_need[!Data_I_need$kbi_y_trans_id == 777,]
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==4] <- 4
#if youre analyzing how gender identity changes over time, run this
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==1] <- 1
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==2] <- 1
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==3] <- 0
Data_I_need <- Data_I_need[!Data_I_need$kbi_y_trans_id == 777,]
Data_I_need$kbi_y_trans_id[Data_I_need$kbi_y_trans_id==4] <- NA
Data_I_need
Data_I_need$eventname[Data_I_need$eventname=="TRUE"] <- 0
Data_I_need <- filter(Data_I_need, eventname  != 0 )
DT_for_melt <- as.data.table(Data_I_need)
DT_for_melt1 <- as.data.table(Data_I_need1)
DTL1 <- melt(DT_for_melt1, id.vars=c("src_subject_id","eventname","cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","Gender_Dissonance","Age","truesex","race_ethnicity","kbi_y_trans_id"))
DTL <- melt(DT_for_melt, id.vars=c("src_subject_id","eventname","cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","Gender_Dissonance","Age","truesex","race_ethnicity","kbi_y_trans_id"))
AHHHH3 <- data.table::dcast(DTL1,src_subject_id~eventname,value.var =c("cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","Gender_Dissonance","Age","truesex","race_ethnicity","kbi_y_trans_id"))

AHHHH <- data.table::dcast(DTL,src_subject_id~eventname,value.var =c("cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","Gender_Dissonance","Age","truesex","race_ethnicity","kbi_y_trans_id"))
AHHHH$truesex_1[AHHHH$truesex_1==1]<- 0
AHHHH$truesex_1[AHHHH$truesex_1==2]<- 1
AHHHH$truesex_2[AHHHH$truesex_2==1]<- 0
AHHHH$truesex_2[AHHHH$truesex_2==2]<- 1
AHHHH$truesex_3[AHHHH$truesex_3==1]<- 0
AHHHH$truesex_3[AHHHH$truesex_3==2]<- 1
AHHHH$truesex_4[AHHHH$truesex_4==1]<- 0
AHHHH$truesex_4[AHHHH$truesex_4==2]<- 1
AHHHH <- filter(AHHHH, truesex_1 <2)

AHHHHflipped <- AHHHH

AHHHHflipped$Gender_Dissonance_1 <- -1*AHHHHflipped$Gender_Dissonance_1 
AHHHHflipped$Gender_Dissonance_2 <- -1*AHHHHflipped$Gender_Dissonance_2
AHHHHflipped$Gender_Dissonance_3 <- -1*AHHHHflipped$Gender_Dissonance_3 
AHHHHflipped$Gender_Dissonance_4 <- -1*AHHHHflipped$Gender_Dissonance_4


AHHHHflipped$averaged_Fluidity <-(abs(AHHHHflipped$Gender_Dissonance_1-AHHHHflipped$Gender_Dissonance_2)/3+abs(AHHHHflipped$Gender_Dissonance_2-AHHHHflipped$Gender_Dissonance_3)/3+abs(AHHHHflipped$Gender_Dissonance_3-AHHHHflipped$Gender_Dissonance_4)/3)
AHHHHflipped$averaged_FluiditytoT3 <-(abs(AHHHHflipped$Gender_Dissonance_1-AHHHHflipped$Gender_Dissonance_2)/2+abs(AHHHHflipped$Gender_Dissonance_2-AHHHHflipped$Gender_Dissonance_3)/2)

AHHHHflipped$EverDiverse <- (AHHHHflipped$kbi_y_trans_id_1 ==1|AHHHHflipped$kbi_y_trans_id_2 ==1|AHHHHflipped$kbi_y_trans_id_3 ==1|AHHHHflipped$kbi_y_trans_id_4 ==1)
AHHHHflipped$EverDiverse[is.na(AHHHHflipped$EverDiverse)] <- 0

AHHH1 <- AHHHH

AHHH1$kbi_y_trans_id_1[AHHH1$kbi_y_trans_id_1 == 2] <-1 
AHHH1$kbi_y_trans_id_1[AHHH1$kbi_y_trans_id_1 == 1] <-1
AHHH1$kbi_y_trans_id_1[AHHH1$kbi_y_trans_id_1 == 3] <-0
AHHH1 <- filter(AHHH1, kbi_y_trans_id_1 <2)
#stop here for analyzing year 1 vs year 2
AHHH1$kbi_y_trans_id_2[AHHH1$kbi_y_trans_id_2 == 2] <-1 
AHHH1$kbi_y_trans_id_2[AHHH1$kbi_y_trans_id_2 == 1] <-1
AHHH1$kbi_y_trans_id_2[AHHH1$kbi_y_trans_id_2 == 3] <-0
AHHH1 <- filter(AHHH1, kbi_y_trans_id_2 <2)
#stop here for analyzing year 2 vs year 3
AHHH1$kbi_y_trans_id_3[AHHH1$kbi_y_trans_id_3 == 2] <-1 
AHHH1$kbi_y_trans_id_3[AHHH1$kbi_y_trans_id_3 == 1] <-1
AHHH1$kbi_y_trans_id_3[AHHH1$kbi_y_trans_id_3 == 3] <-0
AHHH1 <- filter(AHHH1, kbi_y_trans_id_3 <2)
#stop here for analyzing year 3 vs year 4
AHHH1$kbi_y_trans_id_4[AHHH1$kbi_y_trans_id_4 == 2] <-1 
AHHH1$kbi_y_trans_id_4[AHHH1$kbi_y_trans_id_4 == 1] <-1
AHHH1$kbi_y_trans_id_4[AHHH1$kbi_y_trans_id_4 == 3] <-0
AHHH1 <- filter(AHHH1, kbi_y_trans_id_4 <2)
#run all of the above for analyzing the average 
AHHH1$averaged_Fluidity <-(abs(AHHH1$Gender_Dissonance_1-AHHH1$Gender_Dissonance_2)/3+abs(AHHH1$Gender_Dissonance_2-AHHH1$Gender_Dissonance_3)/3+abs(AHHH1$Gender_Dissonance_3-AHHH1$Gender_Dissonance_4)/3)

AHHHHflipped$Gender_Dissonance_Cluster <- 0



AHHHHflipped_to_T3 <- filter(AHHHHflipped, !is.na(Gender_Dissonance_1))
AHHHHflipped_to_T3 <- filter(AHHHHflipped_to_T3, !is.na(Gender_Dissonance_2))
AHHHHflipped_to_T3 <- filter(AHHHHflipped_to_T3, !is.na(Gender_Dissonance_3))

AHHHHflipped_to_T3_suicide <- filter(AHHHH_suicide, !is.na(Gender_Dissonance_1))
AHHHHflipped_to_T3_suicide <- filter(AHHHHflipped_to_T3_suicide, !is.na(Gender_Dissonance_2))
AHHHHflipped_to_T3_suicide <- filter(AHHHHflipped_to_T3_suicide, !is.na(Gender_Dissonance_3))











AHHHH2 <-data.table::dcast(DTL,src_subject_id~eventname,value.var =c("cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","Gender_Dissonance","Age","truesex","race_ethnicity","kbi_y_trans_id"))
AHHHH2$truesex_1[AHHHH2$truesex_1==1]<- 0
AHHHH2$truesex_1[AHHHH2$truesex_1==2]<- 1
AHHHH2$truesex_2[AHHHH2$truesex_2==1]<- 0
AHHHH2$truesex_2[AHHHH2$truesex_2==2]<- 1
AHHHH2$truesex_3[AHHHH2$truesex_3==1]<- 0
AHHHH2$truesex_3[AHHHH2$truesex_3==2]<- 1
AHHHH2$truesex_4[AHHHH2$truesex_4==1]<- 0
AHHHH2$truesex_4[AHHHH2$truesex_4==2]<- 1
AHHHH2 <- filter(AHHHH2, truesex_1 <2)

AHHHH2$Gender_Dissonance_1 <- -1*AHHHH2$Gender_Dissonance_1 
AHHHH2$Gender_Dissonance_1 <- -1*AHHHH2$Gender_Dissonance_1 
AHHHH2$Gender_Dissonance_1 <- -1*AHHHH2$Gender_Dissonance_1 
AHHHH2$Gender_Dissonance_1 <- -1*AHHHH2$Gender_Dissonance_1 
AHHHH2$Everyes <- (AHHHH2$kbi_y_trans_id_1 ==1|AHHHH2$kbi_y_trans_id_2 ==1|AHHHH2$kbi_y_trans_id_3 ==1|AHHHH2$kbi_y_trans_id_4 ==1)
AHHHH2$Everyes[is.na(AHHHH2$Everyes)] <- 0
AHHHH2$Evermaybe <- (AHHHH2$kbi_y_trans_id_1 ==2|AHHHH2$kbi_y_trans_id_2 ==2|AHHHH2$kbi_y_trans_id_3 ==2|AHHHH2$kbi_y_trans_id_4 ==2)
AHHHH2$Evermaybe[is.na(AHHHH2$Evermaybe)] <- 0
AHHHH2$Always_IDK <- (AHHHH2$kbi_y_trans_id_1 ==4&AHHHH2$kbi_y_trans_id_2 ==4&AHHHH2$kbi_y_trans_id_3 ==4&AHHHH2$kbi_y_trans_id_4 ==4)
AHHHH2$Always_IDK[is.na(AHHHH2$Always_IDK)] <- 0
AHHHH2$Always_IDK[AHHHH2$Always_IDK==1] <- 4

AHHHH2$Everyes[AHHHH2$Everyes==1] <- 2

AHHHH2$ever_diverse_status <- (AHHHH2$Everyes+AHHHH2$Evermaybe+AHHHH2$Always_IDK)
AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==3] <- 2
AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==4] <- 3 
AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==5] <- 3 
AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==6] <- 3 


AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==3] <- "I Don't Understand"
AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==2] <- "Yes"
AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==1] <- "Maybe"
AHHHH2$ever_diverse_status[AHHHH2$ever_diverse_status==0] <- "No"
AHHHH2$ever_diverse_status <- factor(AHHHH2$ever_diverse_status, levels = c('No', 'Maybe', 'Yes',"I Don't Understand"))

AHHHH2$averaged_Fluidity <-(abs(AHHHH2$Gender_Dissonance_1-AHHHH2$Gender_Dissonance_2)/3+abs(AHHHH2$Gender_Dissonance_2-AHHHH2$Gender_Dissonance_3)/3+abs(AHHHH2$Gender_Dissonance_3-AHHHH2$Gender_Dissonance_4)/3)
AHHHH2$averaged_GD <-(AHHHH2$Gender_Dissonance_1/4+AHHHH2$Gender_Dissonance_2/4+AHHHH2$Gender_Dissonance_3/4+AHHHH2$Gender_Dissonance_4/4)

AHHHH2$averaged_GD <-(abs(AHHHH2$Gender_Dissonance_1)/4+abs(AHHHH2$Gender_Dissonance_2)/4+abs(AHHHH2$Gender_Dissonance_3)/4+abs(AHHHH2$Gender_Dissonance_4)/4)


AHHHH2 <- filter(AHHHH2, !is.na(averaged_GD))













Data_I_need_sucide <- historyatotaltest %>% dplyr::select(src_subject_id,eventname, cbcl_scr_syn_internal_r,cbcl_scr_syn_external_r,Gender_Dissonance,Age,truesex,race_ethnicity,kbi_y_trans_id,ever_ideation,ever_suicide_plan,ever_attempt)
Data_I_need_sucide <- na.omit(Data_I_need_sucide)
Data_I_need_sucide$race_ethnicity <- Data_I_need_sucide$race_ethnicity
Data_I_need_sucide$eventname[Data_I_need_sucide$eventname=="1_year_follow_up_y_arm_1"] <- 1
Data_I_need_sucide$eventname[Data_I_need_sucide$eventname=="2_year_follow_up_y_arm_1"] <- 2
Data_I_need_sucide$eventname[Data_I_need_sucide$eventname=="3_year_follow_up_y_arm_1"] <- 3
Data_I_need_sucide$eventname[Data_I_need_sucide$eventname=="4_year_follow_up_y_arm_1"] <- 4
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==1] <- 1
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==2] <- 2
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==3] <- 3
Data_I_need_sucide <- Data_I_need_sucide[!Data_I_need_sucide$kbi_y_trans_id == 777,]
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==4] <- 4
#if youre analyzing how gender identity changes over time, run this
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==1] <- 1
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==2] <- 1
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==3] <- 0
Data_I_need_sucide <- Data_I_need_sucide[!Data_I_need_sucide$kbi_y_trans_id == 777,]
Data_I_need_sucide$kbi_y_trans_id[Data_I_need_sucide$kbi_y_trans_id==4] <- NA
Data_I_need_sucide
Data_I_need_sucide$eventname[Data_I_need_sucide$eventname=="TRUE"] <- 0
Data_I_need_sucide <- filter(Data_I_need_sucide, eventname  != 0 )
DT_for_melt_suicide <- as.data.table(Data_I_need_sucide)
DTL_suicide <- melt(DT_for_melt_suicide, id.vars=c("src_subject_id","eventname","cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","Gender_Dissonance","Age","truesex","race_ethnicity","kbi_y_trans_id","ever_ideation","ever_suicide_plan","ever_attempt" ))

AHHHH_suicide <- data.table::dcast(DTL_suicide,src_subject_id~eventname,value.var =c("cbcl_scr_syn_internal_r","cbcl_scr_syn_external_r","Gender_Dissonance","Age","truesex","race_ethnicity","kbi_y_trans_id","ever_ideation","ever_suicide_plan","ever_attempt"))
AHHHH_suicide$truesex_1[AHHHH_suicide$truesex_1==1]<- 0
AHHHH_suicide$truesex_1[AHHHH_suicide$truesex_1==2]<- 1
AHHHH_suicide$truesex_2[AHHHH_suicide$truesex_2==1]<- 0
AHHHH_suicide$truesex_2[AHHHH_suicide$truesex_2==2]<- 1
AHHHH_suicide$truesex_3[AHHHH_suicide$truesex_3==1]<- 0
AHHHH_suicide$truesex_3[AHHHH_suicide$truesex_3==2]<- 1
AHHHH_suicide$truesex_4[AHHHH_suicide$truesex_4==1]<- 0
AHHHH_suicide$truesex_4[AHHHH_suicide$truesex_4==2]<- 1
AHHHH_suicide <- filter(AHHHH_suicide, truesex_1 <2)
AHHHH_suicide$averaged_Fluidity <-(abs(AHHHH_suicide$Gender_Dissonance_1-AHHHH_suicide$Gender_Dissonance_2)/3+abs(AHHHH_suicide$Gender_Dissonance_2-AHHHH_suicide$Gender_Dissonance_3)/3+abs(AHHHH_suicide$Gender_Dissonance_3-AHHHH_suicide$Gender_Dissonance_4)/3)
AHHHH_suicide$averaged_FluiditytoT3 <-(abs(AHHHH_suicide$Gender_Dissonance_1-AHHHH_suicide$Gender_Dissonance_2)/2+abs(AHHHH_suicide$Gender_Dissonance_2-AHHHH_suicide$Gender_Dissonance_3)/2)

AHHHH_suicide$EverDiverse <- (AHHHH_suicide$kbi_y_trans_id_1 ==1|AHHHH_suicide$kbi_y_trans_id_2 ==1|AHHHH_suicide$kbi_y_trans_id_3 ==1|AHHHH_suicide$kbi_y_trans_id_4 ==1)
AHHHH_suicide$EverDiverse[is.na(AHHHH_suicide$EverDiverse)] <- 0


Table_for_figure_bargraphs <- historyatotal %>% dplyr::select (kbi_y_trans_id,Gender_Dissonance,eventname,felt_gender_Dissonance, truesex)
Table_for_figure_bargraphs$eventname[Table_for_figure_bargraphs$eventname == "1_year_follow_up_y_arm_1"] <- "1 year follow up"
Table_for_figure_bargraphs$eventname[Table_for_figure_bargraphs$eventname == "2_year_follow_up_y_arm_1"] <- "2 year follow up"
Table_for_figure_bargraphs$eventname[Table_for_figure_bargraphs$eventname == "3_year_follow_up_y_arm_1"] <- "3 year follow up"
Table_for_figure_bargraphs$eventname[Table_for_figure_bargraphs$eventname == "4_year_follow_up_y_arm_1"] <- "4 year follow up"
Table_for_figure_bargraphs$kbi_y_trans_id[Table_for_figure_bargraphs$kbi_y_trans_id==1] <- "Yes"
Table_for_figure_bargraphs$kbi_y_trans_id[Table_for_figure_bargraphs$kbi_y_trans_id==2] <- "Maybe"
Table_for_figure_bargraphs$kbi_y_trans_id[Table_for_figure_bargraphs$kbi_y_trans_id==3] <- "No"
Table_for_figure_bargraphs <- Table_for_figure_bargraphs[!Table_for_figure_bargraphs$kbi_y_trans_id == 777,]
Table_for_figure_bargraphs$kbi_y_trans_id[Table_for_figure_bargraphs$kbi_y_trans_id==4] <- "I Don't Understand"
Table_for_figure_bargraphs$kbi_y_trans_id[Table_for_figure_bargraphs$kbi_y_trans_id==777] <- "Refuse to Answer"
Table_for_figure_bargraphs <- Table_for_figure_bargraphs[!is.na(Table_for_figure_bargraphs$kbi_y_trans_id),]
Table_for_figure_bargraphs <- Table_for_figure_bargraphs[!Table_for_figure_bargraphs$eventname==TRUE,]
Table_for_figure_bargraphs$kbi_y_trans_id <- factor(Table_for_figure_bargraphs$kbi_y_trans_id, levels = c('No', 'Maybe', 'Yes',"I Don't Understand"))



Glep <- historyatotal
Glep$Gender_Dissonance <- -1*Glep$Gender_Dissonance
Glep$kbi_y_trans_id[Glep$kbi_y_trans_id == 2] <-1 
Glep$kbi_y_trans_id[Glep$kbi_y_trans_id == 1] <-1
Glep$kbi_y_trans_id[Glep$kbi_y_trans_id == 3] <-0
Glep <- filter(Glep, kbi_y_trans_id <2)
Glep$truesex[Glep$truesex == 1] <- 0 
Glep$truesex[Glep$truesex == 2] <- 1 
Glep$truesex <- as.numeric(Glep$truesex)
Glep <- Glep %>% filter(truesex==0|truesex==1)
Glep <- Glep %>% filter(Gender_Dissonance<=4&Gender_Dissonance>=-4)


Glep_Year1 <-historya1
Glep_Year1 <- filter(Glep_Year1, !is.na(kbi_y_trans_id))
Glep_Year1$kbi_y_trans_id[Glep_Year1$kbi_y_trans_id == 2] <-1 
Glep_Year1$kbi_y_trans_id[Glep_Year1$kbi_y_trans_id == 1] <-1
Glep_Year1$kbi_y_trans_id[Glep_Year1$kbi_y_trans_id == 3] <-0
Glep_Year1 <- filter(Glep_Year1, kbi_y_trans_id <2)

Glep_Year2 <-historya2
Glep_Year2 <- filter(Glep_Year2, !is.na(kbi_y_trans_id))
Glep_Year2$kbi_y_trans_id[Glep_Year2$kbi_y_trans_id == 2] <-1 
Glep_Year2$kbi_y_trans_id[Glep_Year2$kbi_y_trans_id == 1] <-1
Glep_Year2$kbi_y_trans_id[Glep_Year2$kbi_y_trans_id == 3] <-0
Glep_Year2 <- filter(Glep_Year2, kbi_y_trans_id <2)

Glep_Year3 <-historya3
Glep_Year3 <- filter(Glep_Year3, !is.na(kbi_y_trans_id))
Glep_Year3$kbi_y_trans_id[Glep_Year3$kbi_y_trans_id == 2] <-1 
Glep_Year3$kbi_y_trans_id[Glep_Year3$kbi_y_trans_id == 1] <-1
Glep_Year3$kbi_y_trans_id[Glep_Year3$kbi_y_trans_id == 3] <-0
Glep_Year3 <- filter(Glep_Year3, kbi_y_trans_id <2)

Glep_Year4 <-historya4
Glep_Year4 <- filter(Glep_Year4, !is.na(kbi_y_trans_id))
Glep_Year4$kbi_y_trans_id[Glep_Year4$kbi_y_trans_id == 2] <-1 
Glep_Year4$kbi_y_trans_id[Glep_Year4$kbi_y_trans_id == 1] <-1
Glep_Year4$kbi_y_trans_id[Glep_Year4$kbi_y_trans_id == 3] <-0
Glep_Year4 <- filter(Glep_Year4, kbi_y_trans_id <2)


Glep_Yes_Year1 <- historya1
Glep_Yes_Year1 <- filter(Glep_Yes_Year1, !is.na(kbi_y_trans_id))
Glep_Yes_Year1$kbi_y_trans_id[Glep_Yes_Year1$kbi_y_trans_id == 1] <-1
Glep_Yes_Year1$kbi_y_trans_id[Glep_Yes_Year1$kbi_y_trans_id == 3] <-0
Glep_Yes_Year1 <- filter(Glep_Yes_Year1, kbi_y_trans_id <2)
print(lm.beta(Glep_Yes_Year1))

Glep_Yes_Year2 <- historya2
Glep_Yes_Year2$kbi_y_trans_id[Glep_Yes_Year2$kbi_y_trans_id == 1] <-1
Glep_Yes_Year2$kbi_y_trans_id[Glep_Yes_Year2$kbi_y_trans_id == 3] <-0
Glep_Yes_Year2 <- filter(Glep_Yes_Year2, kbi_y_trans_id <2)


Glep_Yes_Year3 <- historya3
Glep_Yes_Year3$kbi_y_trans_id[Glep_Yes_Year3$kbi_y_trans_id == 1] <-1
Glep_Yes_Year3$kbi_y_trans_id[Glep_Yes_Year3$kbi_y_trans_id == 3] <-0
Glep_Yes_Year3 <- filter(Glep_Yes_Year3, kbi_y_trans_id <2)

Glep_Yes_Year4 <- historya4
Glep_Yes_Year4$kbi_y_trans_id[Glep_Yes_Year4$kbi_y_trans_id == 1] <-1
Glep_Yes_Year4$kbi_y_trans_id[Glep_Yes_Year4$kbi_y_trans_id == 3] <-0
Glep_Yes_Year4 <- filter(Glep_Yes_Year4, kbi_y_trans_id <2)

Glep_Maybe_Year1 <- historya1
Glep_Maybe_Year1$kbi_y_trans_id[Glep_Maybe_Year1$kbi_y_trans_id == 1] <-9
Glep_Maybe_Year1$kbi_y_trans_id[Glep_Maybe_Year1$kbi_y_trans_id == 2] <-1
Glep_Maybe_Year1$kbi_y_trans_id[Glep_Maybe_Year1$kbi_y_trans_id == 3] <-0
Glep_Maybe_Year1 <- filter(Glep_Maybe_Year1, kbi_y_trans_id <2)

Glep_Maybe_Year2 <- historya2
Glep_Maybe_Year2$kbi_y_trans_id[Glep_Maybe_Year2$kbi_y_trans_id == 1] <-9
Glep_Maybe_Year2$kbi_y_trans_id[Glep_Maybe_Year2$kbi_y_trans_id == 2] <-1
Glep_Maybe_Year2$kbi_y_trans_id[Glep_Maybe_Year2$kbi_y_trans_id == 3] <-0
Glep_Maybe_Year2 <- filter(Glep_Maybe_Year2, kbi_y_trans_id <2)


Glep_Maybe_Year3 <- historya3
Glep_Maybe_Year3$kbi_y_trans_id[Glep_Maybe_Year3$kbi_y_trans_id == 1] <-9
Glep_Maybe_Year3$kbi_y_trans_id[Glep_Maybe_Year3$kbi_y_trans_id == 2] <-1
Glep_Maybe_Year3$kbi_y_trans_id[Glep_Maybe_Year3$kbi_y_trans_id == 3] <-0
Glep_Maybe_Year3 <- filter(Glep_Maybe_Year3, kbi_y_trans_id <2)


Glep_Maybe_Year4 <- historya4
Glep_Maybe_Year4$kbi_y_trans_id[Glep_Maybe_Year4$kbi_y_trans_id == 1] <-9
Glep_Maybe_Year4$kbi_y_trans_id[Glep_Maybe_Year4$kbi_y_trans_id == 2] <-1
Glep_Maybe_Year4$kbi_y_trans_id[Glep_Maybe_Year4$kbi_y_trans_id == 3] <-0
Glep_Maybe_Year4 <- filter(Glep_Maybe_Year4, kbi_y_trans_id <2)

Glep_in_range <- filter(Glep, Gender_Dissonance <=4)
Glep_in_range <- filter(Glep_in_range, Gender_Dissonance>=-4)

Glep_in_range <- filter(Glep_in_range,src_subject_id != "TRUE")

