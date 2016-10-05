

weed_rate <- left_join(InjuriesData, Weedrating, by = c("id_ic" = "id_ci")) %>% 
        select(id_ic, id_main, visit, area, above_can, below_can) %>% 
        mutate(DVS = ifelse(visit == 1, 40, 90))

weed_rate[5:6][weed_rate[,5:6] == "0"] <- 0
weed_rate[5:6][weed_rate[,5:6] == "1"] <- 5
weed_rate[5:6][weed_rate[,5:6] == "2"] <- 20
weed_rate[5:6][weed_rate[,5:6] == "3"] <- 45
weed_rate[5:6][weed_rate[,5:6] == "4"] <- 80


Weed.synthesis <- weed_rate %>% group_by(id_ic, id_main) %>% summarise(ave_WA = mean(above_can),
                                                                       ave_WB = mean(below_can)) %>%
        group_by(id_main) %>% 
        summarise(WA = sum(ave_WA)*25, WB = sum(ave_WB)*25)
