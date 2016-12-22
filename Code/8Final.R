setwd("~/Documents/Work/NEW/SSERWorkshop/FinalClass/")
library(readr)
library(data.table)
library(Hmisc)

##
##COMMON TO ALL TABLES
##

## Read all data, define weights:
### Level 3:
read.fwf("R6801T2L03.TXT", 
         #c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2,  2,5,2,1,1,1,6,6,3,3,1,1,1,1,2,55,3,3,10,3,3,10), 
         c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2, 2,5,2,1,1,1,1,4,1,1,9,2,61,3,3,10),
         col.names = c(
           "roundcode",
           "fsunumber",
           "round",
           "schedule_number",
           "sample",
           "sector",
           "state_region",
           "district",
           "stratum_number",
           "sub_stratum",
           "schedule_type",
           "sub_round",
           "sub_sample",
           "fod_subregion",
           "hamlet_group",
           "second_stage_stratum",
           "hhs_no",
           "level",
           "filler",
           "cooking_code",
           "lighting_code",
           "dwelling_unit_code",
           "regular_salary_earner",
           "perform_ceremony",
           "meals_seved_to_others",
           "possess_ration_card",
           "type_of_ration_card",
           "mpce",
           "special_characters",
           "blank",
           "NSS",
           "NSC",
           "MLT"
           #"level","filler2","informant_sno","response_code","survey_code","subsitution_code","survey_date","despatch_date","canvas_time","canvas_time_block8","remarks1_block9","remarks2_block9","remarks1_elsewhere","remarks2_elsewhere","special_characters","blank","NSS","NSC","MLT","nss_sr","nsc_sr","mlt_sr"
         )
)->level3
ifelse(level3$NSS==level3$NSC,level3$MLT/100,level3$MLT/200)->level3$weight
level3[,c("fsunumber", "second_stage_stratum", "hhs_no", "hamlet_group", "weight")] -> level3

### Level 4:
read.fwf("R6801T2L04.TXT", 
         c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2, 2,3,2,1,1,3,1,2,2,1,2,2,2,2,2,2,61,3,3,10),
         col.names = c(
           "roundcode",
           "fsunumber",
           "round",
           "schedule_number",
           "sample",
           "sector",
           "state_region",
           "district",
           "stratum_number",
           "sub_stratum",
           "schedule_type",
           "sub_round",
           "sub_sample",
           "fod_subregion",
           "hamlet_group",
           "second_stage_stratum",
           "hhs_no",
           "level",
           "filler",
           "person_srl_no",
           "relation",
           "sex",
           "age",
           "marital status",
           "education",
           "days_away_from_home",
           "meals_per_day",
           "meals_school",
           "meals_employer",
           "meals_others",
           "meals_payment",
           "meals_home",
           "special characters for ok stamp",
           "blank",
           "NSS",
           "NSC",
           "MLT"
         )
)->level4
ifelse(level4$NSS==level4$NSC,level4$MLT/100,level4$MLT/200)->level4$weight
level4[,c("fsunumber", "second_stage_stratum", "hhs_no", "hamlet_group", "weight", "person_srl_no")] -> level4

### extract hh_size
lvl4 <- as.data.table(level4)
lvl4[,.(hh_size=length(person_srl_no), weight=weight),by=.(hhs_no, fsunumber, second_stage_stratum, hamlet_group)] -> lvl3
lvl3 <- unique(lvl3)

### Level 5:
read_fwf("R6801T2L05.TXT", fwf_widths(c(3,5,2,3,1,1,3,2,2,2,1,1,1,4,1,1,2, 2,2,3,9,8,9,8,1,2,47,3,3,10), 
                                      c(
                                        "roundcode",
                                        "fsunumber",
                                        "round",
                                        "schedule_number",
                                        "sample",
                                        "sector",
                                        "state_region",
                                        "district",
                                        "stratum_number",
                                        "sub_stratum",
                                        "schedule_type",
                                        "sub_round",
                                        "sub_sample",
                                        "fod_subregion",
                                        "hamlet_group",
                                        "second_stage_stratum",
                                        "hhs_no",
                                        "level",
                                        "filler",
                                        "item_code",
                                        "home_produce_quantity",
                                        "home_produce_value",
                                        "total_consumption_quantity",
                                        "total_consumption_value",
                                        "source_code",
                                        "special_characters",
                                        "blank",
                                        "NSS",
                                        "NSC",
                                        "MLT"
                                      )
                                      )) -> level5
ifelse(level5$NSS==level5$NSC,level5$MLT/100,level5$MLT/200)->level5$weight
level5[,c("fsunumber", "second_stage_stratum", "hhs_no", "hamlet_group", "weight", "item_code", "total_consumption_quantity")] -> level5
as.data.frame(level5) -> level5

### read nutrition
read.csv("nutrition.csv", header = T, sep = "|") -> i2cal

### calculate calorie per item per household
merge(level5, i2cal, by=c("item_code"), all.x = T) -> level5_cal
level5_cal$total_calorie_consumed <- as.numeric(level5_cal$calories_per_unit_kcal) * as.numeric(level5_cal$total_consumption_quantity) /1000

### add household identifiers to item code data:
merge(lvl3, level5_cal, by=c("fsunumber", "hamlet_group", "second_stage_stratum", "hhs_no"), all = T) -> df
dt <- as.data.table(df)
dt[,.(total_kcal=sum(total_calorie_consumed)),by=.(fsunumber, hamlet_group, second_stage_stratum, hhs_no)] -> rdt

df[,c("fsunumber", "hamlet_group", "second_stage_stratum", "hhs_no", "item_code", "person_srl_no", "total_consumption_quantity", "weight")] -> df
merge(df, i2cal, by=c("item_code"), all.x = T) -> df2
df2[,c("fsunumber", "hamlet_group", "second_stage_stratum", "hhs_no", "item_code", "person_srl_no", "total_consumption_quantity", "weight", "calories_per_unit_kcal")] -> df2

