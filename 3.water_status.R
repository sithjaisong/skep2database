# water status

water_s <- InjuriesData %>% select(id_ic, id_main, visit, WS) %>% mutate(DVS = ifelse(visit == 1, 40, 90))

water_s$WSC[water_s$WS == 1 & water_s$DVS == "100"] <- 0  # if WS = 1 at harvest stage
water_s$WSC[water_s$WS == 2 & water_s$DVS == "100"] <- 0 # if WS = 2 at harvest stage
water_s$WSC[water_s$WS == 3 & water_s$DVS %in% c("70", "80" ,"90", "100")] <- 0 # if WS = 3 at 
water_s$WSC[water_s$WS == 4 & water_s$DVS == "100"] <- 0 # if WS = 4 at harvest stage
water_s$WSC[water_s$WS == 5 & water_s$DVS %in% c("70", "80" ,"90", "100")] <- 0 # if WS = 5 at DVS = 70 , 80 , 90, 100  | WSC = 0
water_s$WSC[water_s$WS == 6 & water_s$DVS =="100"] <- 0 # if WS = 6 at harvest stage | WSC = 0
water_s$WSC[water_s$WS == 7] <- 0 # if WS = 7 at any stage | WSC = 0
water_s$WSC[water_s$WS == 8 & water_s$DVS %in% c("70", "80" ,"90", "100")] <- 0 # if WS =8 at DVS = 70 , 80 , 90, 100 | WSC = 0
water_s$WSC[water_s$WS == 8 & water_s$DVS %in% c( "10", "20", "30", "40", "50", "60")] <- 1 # if WS =8 at DVS = 10 , 20 , 30, 40, 50, 60 | WSC = 1
water_s$WSC[water_s$WS == 9] <- 0 # if WS = 9 at any stage | WSC = 0
water_s$WSC[water_s$WS == 10] <- 1 # if WS = 10 at any stage | WSC = 1
water_s$WSC[water_s$WS == 11 & water_s$DVS %in% c("30", "40", "50", "60")] <- 0 # if WS = 11 at DVS = 30, 40, 50, 60 | WSC = 0
water_s$WSC[water_s$WS == 11 & water_s$DVS %in% c("10", "20", "70", "80", "90", "100")] <- 1 # if WS = 11 at DVS = 10, 20, 70, 80 | WSC = 1

water_s$WSC[is.na(water_s$WSC)] <- -1

water_satus <- water_s %>% group_by(id_main) %>% summarise(DSCUM = mean(WSC == -1)*2, WSCUM = mean(WSC == 1)*2)


 