library(magrittr)
library(dplyr)
library(data.table)
library(writexl)
library(ggplot2)
library(ggpubr)
library(tidyr)
#Here I am Grabbing the Short SRS scores from participants to later combine them
#with gender information. In the future, I will also grab data related to mental
#health outcomes
newDF <-Liam_mh_p_ssrs51[,c("src_subject_id",'ssrs_p_ss_sum')]
newDF1 = na.omit(newDF)
NewDFCBCL <- Liam_mh_p_cbcl51
newDFPeer <- Liam_mh_y_peq51
newDFcyber <- Liam_mh_y_cbb51
newDFparentBehavior <- Liam_ce_y_crpbi51
NewDFParentGenderInfo <- Liam_gish_p_gi51
NewDFAGE <- Liam_abcd_p_demo51
DF2 = Liam_gish_y_gi51
#I SHOULD RESTRUCTURE ALL OTHER INFO TO WORK LIKE AGE THAT IM DOING HERE!!!

#This is how to properly  NAs and combine longitudinal data with baseline collection data!
NewDFAGE$demo_brthdat_v2[is.na(NewDFAGE$demo_brthdat_v2)] <- 0
NewDFAGE$demo_brthdat_v2_l[is.na(NewDFAGE$demo_brthdat_v2_l)] <- 0 
NewDFAGE$Age = pmax(NewDFAGE$demo_brthdat_v2,NewDFAGE$demo_brthdat_v2_l)

#I NEED TO FIGURE OUT HOW TO COMBINE ALL THE ETHNICITY DATA AND MAKE IS STABLE, THEN I CAN JUST MERGE!!!
NewDFAGE$race_ethnicity[is.na(NewDFAGE$race_ethnicity)]<- 0
NewDFAGE <- NewDFAGE %>%
  group_by(src_subject_id) %>%
  mutate(race_ethnicity = max(race_ethnicity))
#NOW I NEED TO ADD IN PARENT GI 
NewDFParentGenderInfo$kbi_p_c_trans[is.na(NewDFParentGenderInfo$kbi_p_c_trans)] <- 0
NewDFParentGenderInfo <- NewDFParentGenderInfo%>%
  group_by(src_subject_id) %>%
  mutate(kbi_p_c_trans = max(kbi_p_c_trans))
newDFsex <-Liam_abcd_p_demo51[,c("src_subject_id","eventname", "demo_sex_v2")]
newDFsex <- filter(newDFsex, eventname  == "baseline_year_1_arm_1" )
#Importing gender questionnaire

#matching the gender questionnaire to the SRS questionnaire (makes removes any 
#participant who did not fill in SRS data and vice versa
DF4 = setDT(DF2)[src_subject_id %chin% newDF1$src_subject_id]
#DF4 <- DF4 %>% na.replace(0)
#I am generating a new row called srs scores that we will populate with the SRS
#scores of our participants
DF4$srsScores = numeric(nrow(DF4))
DF4$felt_gender = numeric(nrow(DF4))
DF4$expressed_gender = numeric(nrow(DF4))
DF4$Gender_Dissonance = numeric(nrow(DF4))
DF4$ABS_GD = numeric(nrow(DF4))
DF4$truesex =  numeric(nrow(DF4))

Q = newDF1$ssrs_p_ss_sum
for (Subject in DF4$src_subject_id){
  c = which(newDF1$src_subject_id == Subject)
  e = which(DF4$src_subject_id == Subject)
  Q = newDF1$ssrs_p_ss_sum
  d = which(newDFsex$src_subject_id == Subject)
  for (i in e){
    DF4[i, "srsScores"] <- pull(newDF1[c, "ssrs_p_ss_sum"])
    DF4[i, "truesex"] <- pull(newDFsex[d,"demo_sex_v2"])
  }
}


for (Subject in DF4$src_subject_id){
  e = which(DF4$src_subject_id == Subject)
  for (i in e){
    if (DF4[i,truesex] ==1 ){
      DF4[i, "felt_gender"] <- (1/2)*((1/2)*(pull(DF4[i, "gish_m1_y"])+ pull(DF4[i, "gish_m2_y"]))+pull(DF4[i, "gish_m3_y"]))
    }
    if (DF4[i,truesex] ==2 ){
      DF4[i, "felt_gender"] <- (1/2)*((1/2)*(pull(DF4[i, "gish_f1_y"])+ pull(DF4[i, "gish_f2_y"]))+pull(DF4[i, "gish_f3_y"]))
    }
    if (DF4[i,truesex] ==1 ){
      DF4[i, "expressed_gender"] <- pull(DF4[i, "gish_m4_y"])
    }
    if (DF4[i,truesex]==2 ){
      DF4[i, "expressed_gender"] <- pull(DF4[i, "gish_f4_y"])
    }
    #DF4[i, "felt_gender"] <- (1/2)*((1/2)*(pull(DF4[i, "gish_m1_y"])+pull(DF4[i, "gish_f1_y"])+ pull(DF4[i, "gish_m2_y"])+pull(DF4[i, "gish_f2_y"]))+pull(DF4[i, "gish_m3_y"])+pull(DF4[i, "gish_f3_y"]))
    #DF4[i, "expressed_gender"] <- sum(pull(DF4[i, "gish_m4_y"]),pull(DF4[i, "gish_f4_y"]))
  }
}
for (Subject in DF4$src_subject_id){
  e = which(DF4$src_subject_id == Subject)
  for (i in e){
    DF4[i, "Gender_Dissonance"] <- (pull(DF4[i, "felt_gender"])- pull(DF4[i, "expressed_gender"]))
    DF4[i, "ABS_GD"] <- (abs(pull(DF4[i, "felt_gender"])- pull(DF4[i, "expressed_gender"])))
  }
}
#TODo MERGE DF4 and MY AGE/DEMO DF, then put them at the very front!!!!
DF4 <- merge(DF4, NewDFAGE,  by = c("src_subject_id","eventname"),all = TRUE)
DF4 <- merge(DF4, NewDFParentGenderInfo,by = c("src_subject_id","eventname"),all = TRUE)
DF4 <- merge(DF4, NewDFCBCL, by = c("src_subject_id","eventname"),all = TRUE)
DF4 <- merge(DF4, newDFPeer, by = c("src_subject_id","eventname"),all = TRUE)
DF4 <- merge(DF4, newDFcyber, by = c("src_subject_id","eventname"),all = TRUE)
DF4 <- merge(DF4, newDFparentBehavior, by = c("src_subject_id","eventname"),all = TRUE)
DF4 <- merge(DF4, LIAM_abcd_y_lt, by = c("src_subject_id","eventname"),all = TRUE)

