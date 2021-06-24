library(estimatr)
hamilton_analysis_data <- read_rds(here("inter_data","ipum_hamilton_for_analysis.Rds"))

#adjusted gross income is actually one less zero
hamilton_analysis_data <- hamilton_analysis_data %>% mutate(inctot=ifelse(inctot==999999999,NA,inctot),
                                                            adjginc=ifelse(inctot==99999999,NA,adjginc))
lm_robust(inctot ~ adjginc, data = hamilton_analysis_data %>% filter(adjginc<=70000 | inctot<=70000)) %>% tidy()

hamilton_analysis_data <- hamilton_analysis_data %>% 
  mutate(adjusted_gross_tot_inc_diff=adjginc-inctot,
         large_adjusted_gross_tot_inc_diff=ifelse(adjusted_gross_tot_inc_diff > 50000 | adjusted_gross_tot_inc_diff < -50000,1,0))

summary(hamilton_analysis_data$adjusted_gross_tot_inc_diff, na.rm=TRUE)
#Trying to show relationship between adjusted gross income and total income but it doesn't show up well.... also data looks a bit weird
ggplot(hamilton_analysis_data %>% filter(large_adjusted_gross_tot_inc_diff!=1 & adjginc<100000), aes(x=inctot,y=adjginc)) +
  geom_point(alpha = .1) +
  geom_smooth(method='lm', formula= y~x)



#Approx cost in billions
#Unsure what weight I shoul use, probably spmweight because I use that or family structure
hamilton_analysis_data %>% summarise(total_cost=sum(hamilton_money*spmwt)/1000000000) #876B estimate in Hamilton

#I'm getting 1.37T with adjusted gross income, which they don't have
# I'm getting 1.6T
#1.1 Billion when I use total income, which is I think inclusive of welfare stuff
#Check pop, 316M seems about right....
hamilton_analysis_data %>% mutate(num_one=1) %>% 
  summarise(num_one=sum(num_one*spmwt))

#Want to see how many are getting benefits compared to SCF
hamilton_analysis_data %>% mutate(some_hamilton_money=ifelse(hamilton_money>0,1,0)) %>% 
  summarise(num_one=sum(some_hamilton_money*spmwt),
            frac_individuals_benefits=weighted.mean(some_hamilton_money,spmwt))

hamilton_analysis_data <- hamilton_analysis_data %>% mutate(eitcred=ifelse(eitcred==9999,0,eitcred))
#Status quo EITC
ipum %>% summarise(total_eitc_cost=sum(eitcred*spmwt)/1000000000)