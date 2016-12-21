setwd("~/Documents/Work/NEW/SSERWorkshop/nss68_10/Data/")
library(data.table)

##
##COMMON TO ALL TABLES
##

## Read all data, define weights:
### Level 1:
read.fwf("R6810L01.TXT", 
         c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,5,2,1,1,1,6,6,3,3,1,1,1,1,2,55,3,3,10,3,3,10), 
         col.names = c(
           "roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","informant_sno","response_code","survey_code","subsitution_code","survey_date","despatch_date","canvas_time","canvas_time_block8","remarks1_block9","remarks2_block9","remarks1_elsewhere","remarks2_elsewhere","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"
         )
)->level1
ifelse(level1$NSS==level1$NSC,level1$MLT/100,level1$MLT/200)->level1$weight
### Level 2:
read.fwf("R6810L02.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,5,2,5,3,1,1,1,8,8,8,1,2,1,2,41,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","hhd_size","principal_ind_code","principal_occ_code","hhd_type","religion","social_group","land_owned","land_possessed","land_cultivated","if_nrega_jobcard","number_jobcard","if_bank_account","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level2
ifelse(level2$NSS==level2$NSC,level2$MLT/100,level2$MLT/200)->level2$weight
### Level 3:
read.fwf("R6810L03.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,1,1,3,1,2,2,2,1,1,1,2,1,1,2,63,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","relation_to_head","sex","age","marital_status","gnrl_edu","tech_edu","current_attendance","inst_type","if_placement_agency","voc_training","field_training","if_regd_nrega","if_worked_nrega","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level3
ifelse(level3$NSS==level3$NSC,level3$MLT/100,level3$MLT/200)->level3$weight
### Level 4:
read.fwf("R6810L04.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,3,2,5,3,1,2,1,1,1,1,1,1,1,1,2,58,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","age","upa_status","upa_nic2008","upa_nco2004","if_subsidiary_activity","palwork_location","palenterprise_type","pal_if_electricity","palworkers_number","paljob_contract_type","pal_ifeligible_leave","palsoc_sec_benefits","palpayment_method","seeking_work","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level4
ifelse(level4$NSS==level4$NSC,level4$MLT/100,level4$MLT/200)->level4$weight
### Level 5:
read.fwf("R6810L05.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,3,2,5,3,2,1,1,1,1,1,1,1,2,60,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","age","usa_status","usa_nic2008","usa_nco2004","subwork_location","subenterprise_type","sub_if_electricity","subworkers_number","subjob_contract_type","sub_ifeligible_leave","subsoc_sec_benefits","subpayment_method","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level5
ifelse(level5$NSS==level5$NSC,level5$MLT/100,level5$MLT/200)->level5$weight

## further data prep/munging:
as.data.table(level1) -> lvl1
as.data.table(level2) -> lvl2
as.data.table(level3) -> lvl3
lvl1$state <- floor(lvl1$state_region/10)
lvl2$state <- floor(lvl2$state_region/10)
lvl3$state <- floor(lvl3$state_region/10)



##
## TABLE (1)
##
##calculate each column
###number of surveyed fsu:
lvl1[sector==1,.(villages=length(unique(fsunumber))),by=.(state)] -> vil
lvl1[sector==2,.(UFS_Blocks=length(unique(fsunumber))),by=.(state)] -> ufs
merge(vil, ufs, by=c("state")) -> sfsu
###number of surveyed households:
lvl2[sector==1,.(rural=length(hhs_no)),by=.(state)] -> rural
lvl2[sector==2,.(urban=length(hhs_no)),by=.(state)] -> urban
merge(rural, urban, by=c("state")) -> sh
names(sh) <- c("state", "rural.household","urban.household")
###number of surveyed persons:
lvl3[sex==1,.(male=length(person_srl_no)),by=.(state, sector)] -> male
lvl3[sex==2,.(female=length(person_srl_no)),by=.(state, sector)] -> female
lvl3[,.(person=length(person_srl_no)),by=.(state, sector)] -> total
merge(male, female, by=c("state", "sector")) -> sp
merge(sp, total, by=c("state", "sector")) -> sp
merge(sp[sector==1],sp[sector==2],by=c("state")) -> sp
sp[,c("state","male.x","female.x","person.x","male.y","female.y","person.y")] -> sp
names(sp) <- c("state", "rural.male","rural.female","rural.person", "urban.male","urban.female","urban.person")

##merge all columns so created, add margin manually
rt1 <- merge(sfsu, sh, by=c("state"))
rt1 <- merge(rt1, sp, by=c("state"))
margin = data.frame("state"="ALL",
                    "villages"=sum(rt1$villages),
                    "UFS_Blocks"=sum(rt1$UFS_Blocks),
                    "rural.household"=sum(rt1$rural.household),
                    "urban.household"=sum(rt1$urban.household),
                    "rural.male"=sum(rt1$rural.male),
                    "rural.female"=sum(rt1$rural.female),
                    "rural.person"=sum(rt1$rural.person),
                    "urban.male"=sum(rt1$urban.male),
                    "urban.female"=sum(rt1$urban.female),
                    "urban.person"=sum(rt1$urban.person)
                    )
rbind(as.data.frame(rt1), margin) -> rt1
rt1



##
## TABLE (2): RURAL+URBAN
##
##calculate each column (RURAL+URBAN):
###number of surveyed households (RURAL+URBAN):
lvl2[,.(households=length(hhs_no)),by=.(state)] -> sh #lvl2[sector==X,.(households=length(hhs_no)),by=.(state)] -> sh
###number of surveyed persons (RURAL+URBAN):
lvl3[,.( #lvl3[sector==X,.(
  "male.5p"=length(person_srl_no[sex==1&age>=5]),
  "female.5p"=length(person_srl_no[sex==2&age>=5]),
  "person.5p"=length(person_srl_no[age>=5]),
  "male.15to59"=length(person_srl_no[sex==1&age>=15&age<=59]),
  "female.15to59"=length(person_srl_no[sex==2&age>=15&age<=59]),
  "person.15to59"=length(person_srl_no[age>=15&age<=59]),
  "male.15p"=length(person_srl_no[sex==1&age>=15]),
  "female.15p"=length(person_srl_no[sex==2&age>=15]),
  "person.15p"=length(person_srl_no[age>=15]),
  "all.male"=length(person_srl_no[sex==1]),
  "all.female"=length(person_srl_no[sex==2]),
  "all.person"=length(person_srl_no)
),by=.(state)] -> sp

##merge all columns so created, add margin manually
rt2 <- merge(sh, sp, by=c("state"))
margin = data.frame("state"="ALL",
                    "households"=sum(rt2$households),
                    "male.5p"=sum(rt2$male.5p),
                    "female.5p"=sum(rt2$female.5p),
                    "person.5p"=sum(rt2$person.5p),
                    "male.15to59"=sum(rt2$male.15to59),
                    "female.15to59"=sum(rt2$female.15to59),
                    "person.15to59"=sum(rt2$person.15to59),
                    "male.15p"=sum(rt2$male.15p),
                    "female.15p"=sum(rt2$female.15p),
                    "person.15p"=sum(rt2$person.15p),
                    "all.male"=sum(rt2$all.male),
                    "all.female"=sum(rt2$all.female),
                    "all.person"=sum(rt2$all.person)
                    )
rbind(as.data.frame(rt2), margin) -> rt2
rt2



##
## TABLE (3): RURAL+URBAN  ALL HOUSEHOLDS
##
##calculate each column (RURAL+URBAN  ALL HOUSEHOLDS):
###estimated number of households (RURAL+URBAN  ALL HOUSEHOLDS):
lvl2[,.(households=round(sum(weight)/100)),by=.(state)] -> eh
###estimated number of persons (RURAL+URBAN  ALL HOUSEHOLDS):
lvl3[,.(
  "male.5p"=round(sum(weight[sex==1&age>=5])/1000),
  "female.5p"=round(sum(weight[sex==2&age>=5])/1000),
  "person.5p"=round(sum(weight[age>=5])/1000),
  "male.15to59"=round(sum(weight[sex==1&age>=15&age<=59])/1000),
  "female.15to59"=round(sum(weight[sex==2&age>=15&age<=59])/1000),
  "person.15to59"=round(sum(weight[age>=15&age<=59])/1000),
  "male.15p"=round(sum(weight[sex==1&age>=15])/1000),
  "female.15p"=round(sum(weight[sex==2&age>=15])/1000),
  "person.15p"=round(sum(weight[age>=15])/1000),
  "all.male"=round(sum(weight[sex==1])/1000),
  "all.female"=round(sum(weight[sex==2])/1000),
  "all.person"=round(sum(weight)/1000)
),by=.(state)] -> ep

##merge all columns so created
rt3 <- merge(eh, ep, by=c("state"))

##add remaining household columns
round(((rt3$all.person*10)/rt3$households), 1) -> rt3$avg_hh_size
round(((rt3$all.female*1000)/rt3$all.male)) -> rt3$sex_ratio

##add margin manually
margin = data.frame("state"="ALL",
                    "households"=sum(rt3$households),
                    "male.5p"=sum(rt3$male.5p),
                    "female.5p"=sum(rt3$female.5p),
                    "person.5p"=sum(rt3$person.5p),
                    "male.15to59"=sum(rt3$male.15to59),
                    "female.15to59"=sum(rt3$female.15to59),
                    "person.15to59"=sum(rt3$person.15to59),
                    "male.15p"=sum(rt3$male.15p),
                    "female.15p"=sum(rt3$female.15p),
                    "person.15p"=sum(rt3$person.15p),
                    "all.male"=sum(rt3$all.male),
                    "all.female"=sum(rt3$all.female),
                    "all.person"=sum(rt3$all.person),
                    "avg_hh_size"=round((sum(rt3$all.person)*10)/sum(rt3$households), 1),
                    "sex_ratio"=round((sum(rt3$all.female)/sum(rt3$all.male))*1000)
                    )
rbind(as.data.frame(rt3), margin) -> rt3
rt3

