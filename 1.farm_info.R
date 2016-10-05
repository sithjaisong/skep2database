#
#
#
# delete the 

FieldData$status_panicle <- NULL
FieldData$id_farm <- NULL
FieldData$id_main.x <- NULL
FieldData$id_main.y <- NULL
FieldData$other_soil_prob <- NULL

FieldData$prev_crop <- tolower(FieldData$prev_crop)

rec_FieldData <- FieldData %>% left_join(., rice_var_type, by = c("rice_var_type" = "id_rice_var_type")) %>% 
        left_join(., cem_type, by = c("cem_type" = "id_cem_type")) %>%
        left_join(., soil_prop_type, by = c("soil_prob_id" = "id_soil_prob_type")) %>%
        left_join(., season_type, by = c("season_id" = "seasonid")) %>% 
        dplyr::select(id, date, country, year_season, tropical_season, field_num, village_name, district, province, latitude, longitude, farmer_name, field_area, cem_name, seed_rate, age_seed, cem_date, harvest_date,rice_var_name, rice_var_type.y , prev_crop, fallow_period, soil_prob)



# = yield part
yield <- YieldData %>% select(country,season_id, main_id, id_w_harv,farmer_name, fresh_w, moisture) %>% mutate(y.at.14kg.ha = (fresh_w*(100 - moisture))/84*2000)

all_yield <- yield %>% group_by(main_id) %>% summarise(yield = mean(y.at.14kg.ha))

# = combine the first part

Farmer_info <- left_join(rec_FieldData, all_yield, by = c("id" = "main_id")) 

# check which fields are incomplete

#Farmer_info[!complete.cases(Farmer_info),]
