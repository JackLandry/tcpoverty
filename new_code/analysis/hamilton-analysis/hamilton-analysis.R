library(estimatr)
hamilton_analysis_data <- read_rds(here("inter_data","ipum_hamilton_for_analysis.Rds"))

#adjusted gross income is actually one less zero
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(inctot=ifelse(inctot==999999999,NA,inctot),
                                                            adjginc=ifelse(inctot==99999999,NA,adjginc))

hamilton_analysis_data %>% summarise(total_cost=sum(hamilton_money*asecwt)/1000000000) #876B estimate in Hamilton

hamilton_analysis_data %>% mutate(num_one=1) %>% 
  summarise(num_one=sum(num_one*spmwt))

#Want to see how many are getting benefits compared to SCF
hamilton_analysis_data %>% mutate(some_hamilton_money=ifelse(hamilton_money>0,1,0)) %>% 
  summarise(num_one=sum(some_hamilton_money*spmwt),
            frac_individuals_benefits=weighted.mean(some_hamilton_money,spmwt))

hamilton_analysis_data <- hamilton_analysis_data %>% mutate(eitcred=ifelse(eitcred==9999,0,eitcred))
#Status quo EITC
ipum %>% summarise(total_eitc_cost=sum(eitcred*spmwt)/1000000000)