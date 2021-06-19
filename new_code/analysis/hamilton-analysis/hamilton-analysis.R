hamilton_analysis_data <- read_rds(here("inter_data","ipum_hamilton_for_analysis.Rds"))
#Approx cost in billions
#Unsure what weight I shoul use, probably spmweight because I use that or family structure
ipum %>% summarise(total_cost=sum(hamilton_money*spmwt)/1000000000) #876B estimate in Hamilton

#I'm getting 1.37T with adjusted gross income, which they don't have
# I'm getting 1.6T
#1.1 Billion when I use total income, which is I think inclusive of welfare stuff
#Check pop, 316M seems about right....
ipum %>% mutate(num_one=1) %>% 
  summarise(num_one=sum(num_one*spmwt))
ipum <- ipum %>% mutate(eitcred=ifelse(eitcred==9999,0,eitcred))
#Status quo EITC
ipum %>% summarise(total_eitc_cost=sum(eitcred*spmwt)/1000000000)