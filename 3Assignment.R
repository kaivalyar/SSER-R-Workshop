read.fwf("R6810L02.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,5,2,5,3,1,1,1,8,8,8,1,2,1,2,41,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","hhd_size","principal_ind_code","principal_occ_code","hhd_type","religion","social_group","land_owned","land_possessed","land_cultivated","if_nrega_jobcard","number_jobcard","if_bank_account","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level2
dim(level2)

length(unique(level2$fsunumber))


read.fwf("R6810L03.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,1,1,3,1,2,2,2,1,1,1,2,1,1,2,63,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","relation_to_head","sex","age","marital_status","gnrl_edu","tech_edu","current_attendance","inst_type","if_placement_agency","voc_training","field_training","if_regd_nrega","if_worked_nrega","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level3
ifelse(level3$NSS==level3$NSC,level3$MLT/100,level3$MLT/200)->level3$weight
dim(level3)[1]

aggregate(level3$sector, list(level3$sector), FUN=length)

aggregate(level3$sector, list(level3$sector), FUN=length)
aggregate(level3$sector, list(level3$sector), FUN=length)

#cumsum(subset(level3$weight))
library(data.table)
as.data.table(level3) -> lvl3

lvl3[,.(pop=sum(weight)),.(sector)]
lvl3[,.(totalpop=sum(weight)),]


(lvl3[sex==2,.(sexratio=sum(weight)),]/lvl3[sex==1,.(sexratio=sum(weight)),])*1000

(lvl3[sex==2&sector==1,.(ruralsexratio=sum(weight)),]/lvl3[sex==1&sector==1,.(ruralsexratio=sum(weight)),])*1000
(lvl3[sex==2&sector==2,.(urbansexratio=sum(weight)),]/lvl3[sex==1&sector==2,.(urbansexratio=sum(weight)),])*1000

lvl3[,.(pop=sum(weight)),by=.(sector,sex)]




