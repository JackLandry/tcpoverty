

library(rio)
scf_2019 <- import("/Users/jacklandry/Documents/GitHub/tcpoverty/scf-data/summary-extracts/scfp2019s.zip")
scf_num_adults <- import('/Users/jacklandry/Documents/GitHub/tcpoverty/inter_data/scf-num-adults.dta')
scf_2019 <- scf_2019 %>% as_tibble()
scf_2019 <- scf_2019 %>% left_join(scf_num_adults)
nrow(scf_2019)
names(scf_2019)
head(scf_2019) ##yy1 and y1 together identify cases I think


#Can try to get HH level information from this

#Use this for variable definitions: https://sda.berkeley.edu/sdaweb/analysis/?dataset=scfcomb2019
# Household income for previous calendar year.  Inlcudes wages, 
# self-employment and business income, taxable and tax-exempt 
# interest, dividends, realized capital gains, food stamps and 
# other support programs provided by the government, pension income 
# and withdrawals from retirement accounts, Social Security income, 
# alimony and other support payments, and miscellaneous sources of 
# income.

scf_2019 %>% filter(married==2) %>% count(tot_extra_adults)
scf_2019 %>% filter(married==1) %>% count(tot_extra_adults)
#When there's an extra adult in an unmarried/not living with partner household, I should count it as married
#There's no procudure for two unmarried adults, just 3+
#Rather than change the values of married, lets just do it in a different line

scf_2019 <- scf_2019 %>% mutate(income_measure=income)
#Hamilton money singles below phaseout start point
scf_2019 <- scf_2019 %>% mutate(dummy_data=0)
scf_2019 <- scf_2019 %>% add_row(income_measure=40000,married=2,age=18, kids = 0, dummy_data=1, tot_extra_adults=0) %>% 
  add_row(income_measure=40000,married=2,age=18, kids = 1, dummy_data=1, tot_extra_adults=0) %>% 
  add_row(income_measure=40000,married=2,age=18, kids = 2, dummy_data=1, tot_extra_adults=0) %>% 
  add_row(income_measure=40000,married=1,age=18, kids = 0, dummy_data=1, tot_extra_adults=0) %>% 
  add_row(income_measure=40000,married=1,age=18, kids = 1, dummy_data=1, tot_extra_adults=0) %>% 
  add_row(income_measure=40000,married=1,age=18, kids = 2, dummy_data=1, tot_extra_adults=0)
scf_2019 <- scf_2019 %>% mutate(hamilton_money=0)
#Going though each HH unit and doing hamilton money the way it's described in the report
scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure<10000 & married==2 & age>=18 & tot_extra_adults==0,
                                              12500+(kids*4500),hamilton_money))


scf_2019 <- scf_2019 %>% mutate(hamilton_benefit=0,
                        hamilton_phaseout_percentage=0,
                        hamilton_phaseout_amount=0,
                        hamilton_phaseout=0)

#Benefit = base + 4500*num_kis
scf_2019 <- scf_2019 %>% mutate(hamilton_benefit=ifelse(income_measure>10000 & income_measure<=50000 &
                                                  married==2 & age>=18 & tot_extra_adults==0,
                                                12500+4500*kids,hamilton_benefit))

#Phaseout = (total_income_measure-phaseout_start)*phaseout_rate
#Phaseout rate should be depend on number of children, so I don't think I have the formula right... 
#Need to review that
scf_2019 <- scf_2019 %>% mutate(hamilton_phaseout_percentage=(12500+4500*kids)/-40000,
                        hamilton_phaseout_amount=(income_measure-10000)*hamilton_phaseout_percentage)


#Putting it together
scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure>10000 & income_measure<=50000 &
                                                married==2 & age>=18 & tot_extra_adults==0,
                                              hamilton_benefit+hamilton_phaseout_amount,hamilton_money))



#Not dividing by 2 for spouses because things are at the household level now
#Spouses below phaseout start point
scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure<15000 & married==1 & age>=18 & tot_extra_adults==1,
                                              (25000+(kids*4500)),hamilton_money))
scf_2019 %>% filter(income_measure<15000 & 
                  married==1 & age>=18 & tot_extra_adults==1) %>% 
  select(hamilton_money,income_measure,kids)

#Spouses above phaseout start point
#Benefit
scf_2019 <- scf_2019 %>% mutate(hamilton_benefit=ifelse(income_measure>=15000 & income_measure<=70000 & 
                                                  married==1 & age>=18 & tot_extra_adults==1,
                                                25000+4500*kids,hamilton_benefit))
#Phaseout
scf_2019 <- scf_2019 %>% mutate(hamilton_phaseout=ifelse(income_measure>=15000 & income_measure<=70000 & 
                                                   married==1 & age>=18 & tot_extra_adults==1,
                                                 (income_measure-15000)*(25000+(4500*kids))/55000,hamilton_phaseout))

scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure>15000 & income_measure<=70000 & 
                                                married==1 & age>=18 & tot_extra_adults==1, #is married married or 
                                              (hamilton_benefit-hamilton_phaseout),hamilton_money))

###add benefit calculation for 3+ people
#NEE TO CHECK THAT MARRIED includes cohabitating
#need to check household strutue between CPS and this
#households with three or more adults receive up to \$30,000 total for those households with income up to \$30,000, 
#declining at a rate of 50 percent above \$30,000 to fully phase out at $80,000 in total household income across all adults. 
#Because this category only represents a small proportion of total households, 
#the choice of how to estimate the grant amounts for these households will not significantly affect the overall program cost. 
#Under an alternative assumption that every non-married adult (with two or more adults in the household) file separately, 
#that divides household income in a way that maximizes the refund value, 
#our guaranteed income program cost still remains under \$1 trillion.

#Need to add two adults not married/cohabbitating, think I should implement as if marriedd

#Benefit <30k 3+ adults
scf_2019 <- scf_2019 %>% 
  mutate(hamilton_money=ifelse(income_measure<30000 & tot_extra_adults>=2 & age>=18,
                               30000+(4500*kids),hamilton_money)) #+4500*kids assume this still holds, but it's not 100% clear

#Benefit for 30-80k 3+ adults
scf_2019 <- scf_2019 %>% 
  mutate(hamilton_benefit=ifelse(income_measure>=30000 & income_measure<=80000 & tot_extra_adults>=2 & age>=18,
                               30000+(4500*kids),hamilton_benefit))

#Phaseout for 30-80k 3+ adults
scf_2019 <- scf_2019 %>% mutate(hamilton_phaseout=ifelse(income_measure>=30000 & income_measure<=80000 & 
                                                           age>=18 & tot_extra_adults>=2,
                                                         #Need to calculate this
                                                         #income - phase out start * max benefit/phaseout start-phaseout end = 30k - 80k
                                                         (income_measure-30000)*(30000+(4500*kids))/50000,hamilton_phaseout))

#Putting it together
scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure>30000 & income_measure<=80000 & 
                                                        tot_extra_adults>=2 & age>=18, #is married married or 
                                                      (hamilton_benefit-hamilton_phaseout),hamilton_money))

#Need to try alternative way that maximizes benefit the way they describe
#Under an alternative assumption that every non-married adult (with two or more adults in the household) file separately, 
#that divides household income in a way that maximizes the refund value, 
#our guaranteed income program cost still remains under \$1 trillion.
#Divide income by number of adults. If under 10k, give max benefit to everyone. 
#If above 10k, assign all the income to one person and give max benefits to num_adults-1
#Assigning kids like single even if married, generally ignoring marrige, maybe need to go back to that
scf_2019 <- scf_2019 %>% mutate(income_ratio_large_hh=ifelse(tot_extra_adults>=2,income_measure/(tot_extra_adults+1),NA),
                                hamilton_alt_assignment=ifelse(tot_extra_adults>=2 & income_ratio_large_hh<=10000,tot_extra_adults+1*12500+(4500*kids),0),
                                hamilton_alt_assignment=ifelse(tot_extra_adults>=2 & income_ratio_large_hh>10000,tot_extra_adults*12500+(4500*kids),0))


scf_2019 <- scf_2019 %>% 
  mutate(hamilton_money_alt=ifelse(tot_extra_adults>=2,hamilton_alt_assignment,hamilton_money))


#Treated 2 adults not cohabitating/married the same as married

scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure<15000 & married==2 & age>=18 & tot_extra_adults==1,
                                                      (25000+(kids*4500)),hamilton_money))

#Benefit
scf_2019 <- scf_2019 %>% mutate(hamilton_benefit=ifelse(income_measure>=15000 & income_measure<=70000 & 
                                                          married==2 & age>=18 & tot_extra_adults==1,
                                                        25000+4500*kids,hamilton_benefit))
#Phaseout
scf_2019 <- scf_2019 %>% mutate(hamilton_phaseout=ifelse(income_measure>=15000 & income_measure<=70000 & 
                                                           married==2 & age>=18 & tot_extra_adults==1,
                                                         (income_measure-15000)*(25000+(4500*kids))/55000,hamilton_phaseout))

scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure>15000 & income_measure<=70000 & 
                                                        married==2 & age>=18 & tot_extra_adults==1, #is married married or 
                                                      (hamilton_benefit-hamilton_phaseout),hamilton_money))



#Checking  
scf_2019 %>% 
  select(hamilton_money,hamilton_benefit,hamilton_phaseout,income_measure,married,kids)
#Have negatives corrossed again, need to check the other data
#Note how the phaseout is the same
scf_2019 %>% filter(dummy_data==1) %>% 
  select(hamilton_money,hamilton_benefit,hamilton_phaseout,hamilton_phaseout_amount,income_measure,married,kids)
scf_2019 <- scf_2019 %>% mutate(`Num Kids`=as.factor(kids))

ggplot(scf_2019 %>% filter(married==2  & income_measure<100000 & tot_extra_adults==0), 
       aes(x = income_measure, y = hamilton_money, group = `Num Kids`)) +
  geom_point(aes(color = `Num Kids`), alpha = .25) +
  theme_bw() +
  labs(x = "Income", y = "Benefit", title = "Hamilton Benefits by Income for Singles")

ggplot(scf_2019 %>% filter(married==1  & income_measure<100000 & tot_extra_adults==1), 
       aes(x = income_measure, y = hamilton_money, group = kids)) +
  geom_point(aes(color = `Num Kids`), alpha = .25) +
  theme_bw() +
  labs(x = "Income", y = "Benefit", title = "Hamilton Benefits by Income \nfor Couples")

#9601  40725.
#Approx cost in billions

scf_2019 <- scf_2019 %>% filter(dummy_data!=1) 
scf_2019 %>%   summarise(total_cost=sum(hamilton_money*wgt)/1000000000)
#Recall diddferences with total income and actual total income

scf_2019 %>%   summarise(total_cost=sum(hamilton_money_alt*wgt)/1000000000) #876B estimate in Hamilton, I'm getting 975
#Now im getting 975 accounting the way they do for extra adults

#Total number of filers... is this right?
scf_2019 %>%  
  mutate(num_filers=1) %>% 
  summarise(total_filers=sum(num_filers*wgt)) #876B estimate in Hamilton, I'm getting 767

#Fraction of people getting benefits
scf_2019 %>% 
  mutate(some_hamilton_money=ifelse(hamilton_money>0,1,0)) %>% 
  summarise(frac_getting_benefit=weighted.mean(some_hamilton_money,wgt)) #876B estimate in Hamilton, I'm getting 767

#Average total filer income
scf_2019 %>% 
  summarise(avg_income=weighted.mean(income_measure,wgt))
#For families with kids
scf_2019 %>% filter(kids>0) %>% 
  summarise(avg_income=weighted.mean(income_measure,wgt))
#Families without kids
scf_2019 %>% filter(kids==0) %>% 
  summarise(avg_income=weighted.mean(income_measure,wgt))
  



