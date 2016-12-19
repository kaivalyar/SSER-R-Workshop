setwd("~/Documents/Work/NEW/SSERWorkshop/nss68_10/Data/")
library(data.table)
library(Hmisc)


## Level 3
read.fwf("R6810L03.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,1,1,3,1,2,2,2,1,1,1,2,1,1,2,63,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","relation_to_head","sex","age","marital_status","gnrl_edu","tech_edu","current_attendance","inst_type","if_placement_agency","voc_training","field_training","if_regd_nrega","if_worked_nrega","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level3
ifelse(level3$NSS==level3$NSC,level3$MLT/100,level3$MLT/200)->level3$weight

## Level 4
read.fwf("R6810L04.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,3,2,5,3,1,2,1,1,1,1,1,1,1,1,2,58,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","age","upa_status","upa_nic2008","upa_nco2004","if_subsidiary_activity","palwork_location","palenterprise_type","pal_if_electricity","palworkers_number","paljob_contract_type","pal_ifeligible_leave","palsoc_sec_benefits","palpayment_method","seeking_work","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level4
ifelse(level4$NSS==level4$NSC,level4$MLT/100,level4$MLT/200)->level4$weight

## Level 5
read.fwf("R6810L05.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,3,2,5,3,2,1,1,1,1,1,1,1,2,60,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","age","usa_status","usa_nic2008","usa_nco2004","subwork_location","subenterprise_type","sub_if_electricity","subworkers_number","subjob_contract_type","sub_ifeligible_leave","subsoc_sec_benefits","subpayment_method","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level5
ifelse(level5$NSS==level5$NSC,level5$MLT/100,level5$MLT/200)->level5$weight

merge(level3, level4, by = c("fsunumber", "hamlet_group", "second_stage_stratum", "hhs_no", "person_srl_no"), all = TRUE) -> mergedlevel1
merge(mergedlevel1, level5, by = c("fsunumber", "hamlet_group", "second_stage_stratum", "hhs_no", "person_srl_no"), all = TRUE) -> alldata

sort(names(alldata))
#age
#sector - rural/urban
#sex
ifelse((!is.na(alldata$upa_status))&(alldata$upa_status < 60), 1,
       ifelse((!is.na(alldata$usa_status))&(alldata$usa_status < 60), 1,
              0)
       ) -> alldata$is_working #uxa_status < 60 implies working.

ifelse(!is.na(alldata$age)&as.numeric(alldata$age)>=15&as.numeric(alldata$age)<=59,1,0) -> alldata$correct_age_range

#aggregate(alldata$isWorking, list(sex=alldata$sex,sect=alldata$sector), FUN=length)
as.data.table(alldata) -> dt


dt[is_working==1&correct_age_range==1,.(employedtotal=sum()),by=.(sector,sex)]
dt[is_working==0&correct_age_range==1,.(total=sum(weight.x)),by=.(sector,sex)]
dt[isWorking==1,.(total=sum(weight.x)),by=.(sector,sex)]

