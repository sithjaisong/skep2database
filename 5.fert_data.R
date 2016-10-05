
Fert_synthesis <- FertilizerData %>% select(id, n, p , k, amount.y) %>% mutate(N = n/100*amount.y,
                                                             P = p/100*amount.y,
                                                             K = k/100*amount.y) %>% group_by(id) %>%
        summarise_each(funs(sum), N, P, K)
