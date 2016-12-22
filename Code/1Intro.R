setwd("~/Documents/Work/NEW/SSERWorkshop/nss68_10/Data/")

## Level 1
read.fwf("R6810L01.TXT", 
         c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,5,2,1,1,1,6,6,3,3,1,1,1,1,2,55,3,3,10,3,3,10), 
         col.names = c(
           "roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","informant_sno","response_code","survey_code","subsitution_code","survey_date","despatch_date","canvas_time","canvas_time_block8","remarks1_block9","remarks2_block9","remarks1_elsewhere","remarks2_elsewhere","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"
           )
         )->level1
ifelse(level1$NSS==level1$NSC,level1$MLT/100,level1$MLT/200)->level1$weight

## Level 2
read.fwf("R6810L02.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,5,2,5,3,1,1,1,8,8,8,1,2,1,2,41,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","hhd_size","principal_ind_code","principal_occ_code","hhd_type","religion","social_group","land_owned","land_possessed","land_cultivated","if_nrega_jobcard","number_jobcard","if_bank_account","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level2
ifelse(level2$NSS==level2$NSC,level2$MLT/100,level2$MLT/200)->level2$weight

## Level 3
read.fwf("R6810L03.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,1,1,3,1,2,2,2,1,1,1,2,1,1,2,63,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","relation_to_head","sex","age","marital_status","gnrl_edu","tech_edu","current_attendance","inst_type","if_placement_agency","voc_training","field_training","if_regd_nrega","if_worked_nrega","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level3
ifelse(level3$NSS==level3$NSC,level3$MLT/100,level3$MLT/200)->level3$weight

## Level 4
read.fwf("R6810L04.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,3,2,5,3,1,2,1,1,1,1,1,1,1,1,2,58,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","age","upa_status","upa_nic2008","upa_nco2004","if_subsidiary_activity","palwork_location","palenterprise_type","pal_if_electricity","palworkers_number","paljob_contract_type","pal_ifeligible_leave","palsoc_sec_benefits","palpayment_method","seeking_work","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level4
ifelse(level4$NSS==level4$NSC,level4$MLT/100,level4$MLT/200)->level4$weight

## Level 5
read.fwf("R6810L05.TXT", c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,2,3,2,3,2,5,3,2,1,1,1,1,1,1,1,2,60,3,3,10,3,3,10), col.names = c("roundcode","fsunumber","round","schedule_number","sample","sector","state_region","district","stratum_number","sub_stratum","filler1","sub_round","sub_sample","fod_sub_region","hamlet_group","second_stage_stratum","hhs_no","level","filler2","person_srl_no","age","usa_status","usa_nic2008","usa_nco2004","subwork_location","subenterprise_type","sub_if_electricity","subworkers_number","subjob_contract_type","sub_ifeligible_leave","subsoc_sec_benefits","subpayment_method","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"))->level5
ifelse(level5$NSS==level5$NSC,level5$MLT/100,level5$MLT/200)->level5$weight


#getwd()
#view(data_frame_object)
#ls()
#class(data_frame_object)
#dim(data_frame_object)
#names(data_frame_object)
#head(data_frame_object)
#head(data_frame_object, n=2)
#data_frame_object[,]
#data_frame_object[,c(2:6)]
#data_frame_object[c(2000:2100),]
#data_frame_object[100,1]
#rm()

### 2.cw

summary(level2$hhd_size)
subset(level2,social_group==1) -> level2a
subset(level2,social_group==2) -> level2b
subset(level2,social_group==3) -> level2c
subset(level2,social_group==9) -> level2i

dim(level2a)
str(level2a)
table(head(level2a))

summary(level2a$hhd_size)
summary(level2b$hhd_size)
summary(level2c$hhd_size)
summary(level2i$hhd_size)

aggregate(level2$hhd_size, list(level2$social_group), FUN=mean)
aggregate(level2$hhd_size, list(SocGroup=level2$social_group), FUN=mean)
aggregate(level2$hhd_size, list(level2$social_group, level2$religion), FUN=mean)
aggregate(level2$hhd_size, list(SocGroup=level2$social_group, Religion=level2$religion), FUN=mean) -> hhsizedata
round(hhsizedata$x, 3) -> hhsizedata$roundX
names(hhsizedata) <- c("Rel", "SocGrp", "Mean", "RoundedMean")

c("Rel", "SocGrp", "Mean", "RoundedMean") -> names(hhsizedata)
hhsizedata


#inner join : intersection. incomplete records are dropped
merge(level3,level2, by=c("fsnumber", "hamlet_group", "second_stage_staratum", "hhs_no"))

#select all from x (level3)
merge(level3,level2, by=c("fsnumber", "hamlet_group", "second_stage_staratum", "hhs_no"), all.x = T)

#select all from y (level2)
merge(level3,level2, by=c("fsnumber", "hamlet_group", "second_stage_staratum", "hhs_no"), all.y = T)

#outer join : union. no records are dropped
merge(level3,level2, by=c("fsnumber", "hamlet_group", "second_stage_staratum", "hhs_no"), all = T)
merge(level3,level2, by=c("fsnumber", "hamlet_group", "second_stage_staratum", "hhs_no"), all.x = T, all.y = T)


# on average, sample mean equals population mean. therefore, average of averages(sample means) is exaclty equal to population mean.
# weighted averages, multipliers

ifelse(level2$social_group==2, "SC",
       ifelse(level2$social_group==1, "ST", 
              ifelse(level2$religion==2, "MUSLIM",
                     "OTHER"))) -> level2$vikas_categorisation

aggregate(level2$vikas_categorisation, list(level2$hhd_size), FUN=table) -> vcat_by_hhd

names(vcat_by_hhd) <- c("HouseHold Size", "")

vcat_by_hhd


### 2.hw LAWRENCE CURVE

subset(level2, !is.na(level2$land_owned)) -> level2sansNA # drop NA
ifelse(is.na(level2$land_owned), 0, level2$land_owned) -> level2$land_owned_noNA # consider NA as 0
#level2$land_owned[is.na(level2$land_owned)] <- 0


level2sansNA[order(level2sansNA$land_owned),] -> level2sorted_sansNA # sort the data
level2[order(level2$land_owned_noNA),] -> level2sorted

as.numeric(level2sorted$MLT) -> level2sorted$hhno
as.numeric(level2sorted_sansNA$MLT) -> level2sorted_sansNA$hhno

as.numeric(level2sorted$MLT)*level2sorted$land_owned_noNA -> level2sorted$land_owned_noNA_mlt
as.numeric(level2sorted_sansNA$MLT)*level2sorted_sansNA$land_owned -> level2sorted_sansNA$land_owned_mlt

cumsum(level2sorted$land_owned_noNA_mlt) -> level2sorted$cum_land_owned # calculate cumulative land owned
cumsum(level2sorted$hhno) -> level2sorted$cum_hhno
cumsum(level2sorted_sansNA$land_owned_mlt) -> level2sorted_sansNA$cum_land_owned # calculate cumulative land owned
cumsum(level2sorted_sansNA$hhno) -> level2sorted_sansNA$cum_hhno

# UGLY HACK
ifelse( level2sorted$cum_land_owned == 0, 
        level2sorted$cum_land_owned/2.425672e+13, #58290713, 
        level2sorted$cum_land_owned/2.425672e+13) -> level2sorted$cum_land_owned_proportion
ifelse( level2sorted$cum_hhno == 0, 
        level2sorted$cum_hhno/50068663319, #/101724, number changed bc multiplier
        level2sorted$cum_hhno/50068663319) -> level2sorted$cum_hhno_proportion

ifelse( level2sorted_sansNA$cum_land_owned == 0, 
        level2sorted_sansNA$cum_land_owned/2.425672e+13, #58290713, 
        level2sorted_sansNA$cum_land_owned/2.425672e+13) -> level2sorted_sansNA$cum_land_owned_proportion
ifelse( level2sorted_sansNA$cum_hhno == 0, 
        level2sorted_sansNA$cum_hhno/42910736965, #/87637, number changed bc multiplier
        level2sorted_sansNA$cum_hhno/42910736965) -> level2sorted_sansNA$cum_hhno_proportion

plot(level2sorted$cum_hhno_proportion, level2sorted$cum_hhno_proportion, type="l")
par(new=T)
plot(level2sorted$cum_hhno_proportion, level2sorted$cum_land_owned_proportion, type="l")
par(new=T)
plot(level2sorted_sansNA$cum_hhno_proportion, level2sorted_sansNA$cum_land_owned_proportion, type="l")


### 3.cw
#library(c("reshape2", "dplyr", "ggplot2", "Hmisc", "data.table"))
