setwd("~/Documents/Work/NEW/SSERWorkshop/nss68_10/Data/")
library(data.table)
library(Hmisc)

read.fwf("R6810L02.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,5,2,5,3,1,1,1,8,8,8,1,2,1,2,41,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","hhd_size","principal_ind_code","principal_occ_code","hhd_type","religion","social_group","land_owned","land_possessed","land_cultivated","if_nrega_jobcard","number_jobcard","if_bank_account","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level2
ifelse(level2$NSS==level2$NSC,level2$MLT/100,level2$MLT/200)->level2$weight

read.fwf("R6810L03.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,1,1,3,1,2,2,2,1,1,1,2,1,1,2,63,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","relation_to_head","sex","age","marital_status","gnrl_edu","tech_edu","current_attendance","inst_type","if_placement_agency","voc_training","field_training","if_regd_nrega","if_worked_nrega","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level3
ifelse(level3$NSS==level3$NSC,level3$MLT/100,level3$MLT/200)->level3$weight

as.data.table(level2) -> lvl2

##landowned
ifelse(is.na(lvl2$land_owned),0,lvl2$land_owned) -> lvl2$land_owned_nona # convert na to zero
lvl2[,.(landown_natozero=weighted.mean(land_owned_nona,weight)),]
lvl2[!is.na(land_owned),.(landown_nadropped=weighted.mean(land_owned,weight)),]
lvl2[,.(landown_natozero_dev=sqrt(wtd.var(land_owned_nona,weight))),]
lvl2[!is.na(land_owned),.(landown_nadropped_dev=sqrt(wtd.var(land_owned,weight))),]

###landowned by rural/urban
lvl2[,.(landown_natozero=weighted.mean(land_owned_nona,weight)),by=.(sector)] -> dt1
lvl2[!is.na(land_owned),.(landown_nadropped=weighted.mean(land_owned,weight)),by=.(sector)] -> dt2
lvl2[,.(landown_natozero_dev=sqrt(wtd.var(land_owned_nona,weight))),by=.(sector)] -> dt3
lvl2[!is.na(land_owned),.(landown_nadropped_dev=sqrt(wtd.var(land_owned,weight))),by=.(sector)] -> dt4
merge(dt1, dt3, by="sector") -> dt5
merge(dt2, dt4, by="sector") -> dt6
merge(dt5, dt6, by="sector")

###landowned by state
floor(lvl2$state_region/10) -> lvl2$state
lvl2[,.(landown_natozero=weighted.mean(land_owned_nona,weight)),by=.(state)] -> dt1
lvl2[!is.na(land_owned),.(landown_nadropped=weighted.mean(land_owned,weight)),by=.(state)] -> dt2
lvl2[,.(landown_natozero_dev=sqrt(wtd.var(land_owned_nona,weight))),by=.(state)] -> dt3
lvl2[!is.na(land_owned),.(landown_nadropped_dev=sqrt(wtd.var(land_owned,weight))),by=.(state)] -> dt4
merge(dt1, dt3, by="state") -> dt5
merge(dt2, dt4, by="state") -> dt6
merge(dt5, dt6, by="state")



