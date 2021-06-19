

library(rio)
scf_2019 <- import("/Users/jacklandry/Documents/GitHub/tcpoverty/scf-data/summary-extracts/scfp2019s.zip")
scf_2019 <- scf_2019 %>% as_tibble()
#Use this for variable definitions: https://sda.berkeley.edu/sdaweb/analysis/?dataset=scfcomb2019
# Household income for previous calendar year.  Inlcudes wages, 
# self-employment and business income, taxable and tax-exempt 
# interest, dividends, realized capital gains, food stamps and 
# other support programs provided by the government, pension income 
# and withdrawals from retirement accounts, Social Security income, 
# alimony and other support payments, and miscellaneous sources of 
# income.


scf_2019 <- scf_2019 %>% mutate(income_measure=income)
#Hamilton money singles below phaseout start point
scf_2019 <- scf_2019 %>% mutate(dummy_data=0)
scf_2019 <- scf_2019 %>% add_row(income_measure=40000,married=2,age=18, kids = 0, dummy_data=1) %>% 
  add_row(income_measure=40000,married=2,age=18, kids = 1, dummy_data=1) %>% 
  add_row(income_measure=40000,married=2,age=18, kids = 2, dummy_data=1) %>% 
  add_row(income_measure=40000,married=1,age=18, kids = 0, dummy_data=1) %>% 
  add_row(income_measure=40000,married=1,age=18, kids = 1, dummy_data=1) %>% 
  add_row(income_measure=40000,married=1,age=18, kids = 2, dummy_data=1)
scf_2019 <- scf_2019 %>% mutate(hamilton_money=0)
#Going though each HH unit and doing hamilton money the way it's described in the report
scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure<10000 & married==2 & age>=18,
                                              12500+(kids*4500),hamilton_money))


scf_2019 <- scf_2019 %>% mutate(hamilton_benefit=0,
                        hamilton_phaseout_percentage=0,
                        hamilton_phaseout_amount=0,
                        hamilton_phaseout=0)

#Benefit = base + 4500*num_kis
scf_2019 <- scf_2019 %>% mutate(hamilton_benefit=ifelse(income_measure>10000 & income_measure<=50000 &
                                                  married==2 & age>=18,
                                                12500+4500*kids,hamilton_benefit))

#Phaseout = (total_income_measure-phaseout_start)*phaseout_rate
#Phaseout rate should be depend on number of children, so I don't think I have the formula right... 
#Need to review that
scf_2019 <- scf_2019 %>% mutate(hamilton_phaseout_percentage=(12500+4500*kids)/-40000,
                        hamilton_phaseout_amount=(income_measure-10000)*hamilton_phaseout_percentage)


#Putting it together
scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure>10000 & income_measure<=50000 &
                                                married==2 & age>=18,
                                              hamilton_benefit+hamilton_phaseout_amount,hamilton_money))



#Not dividing by 2 for spouses because things are at the household level now
#Spouses below phaseout start point
scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure<15000 & married==1 & age>=18,
                                              (25000+(kids*4500)),hamilton_money))
scf_2019 %>% filter(income_measure<15000 & 
                  married==1 & age>=18) %>% 
  select(hamilton_money,income_measure,kids)

#Spouses above phaseout start point
#Benefit
scf_2019 <- scf_2019 %>% mutate(hamilton_benefit=ifelse(income_measure>=15000 & income_measure<=70000 & 
                                                  married==1 & age>=18,
                                                25000+4500*kids,hamilton_benefit))
#Phaseout
scf_2019 <- scf_2019 %>% mutate(hamilton_phaseout=ifelse(income_measure>=15000 & income_measure<=70000 & 
                                                   married==1 & age>=18,
                                                 (income_measure-15000)*(25000+(4500*kids))/55000,hamilton_phaseout))

scf_2019 <- scf_2019 %>% mutate(hamilton_money=ifelse(income_measure>15000 & income_measure<=70000 & 
                                                married==1 & age>=18,
                                              (hamilton_benefit-hamilton_phaseout),hamilton_money))


#Checking  
#Seems like it's wrong for singles
scf_2019 %>% 
  select(hamilton_money,hamilton_benefit,hamilton_phaseout,income_measure,married,kids)
#Have negatives corrossed again, need to check the other data
#Note how the phaseout is the same
scf_2019 %>% filter(dummy_data==1) %>% 
  select(hamilton_money,hamilton_benefit,hamilton_phaseout,hamilton_phaseout_amount,income_measure,married,kids)
scf_2019 <- scf_2019 %>% mutate(`Num Kids`=as.factor(kids))

ggplot(scf_2019 %>% filter(married==2  & income_measure<100000), 
       aes(x = income_measure, y = hamilton_money, group = `Num Kids`)) +
  geom_point(aes(color = `Num Kids`)) +
  theme_bw() +
  labs(x = "Income", y = "Benefit", title = "Hamilton Benefits by Income for Singles")

#This is still off by a bit...
ggplot(scf_2019 %>% filter(married==1  & income_measure<100000), aes(x = income_measure, y = hamilton_money, group = kids)) +
  geom_point(aes(color = as.factor(kids))) +
  theme_bw() +
  labs(x = "Income", y = "Benefit", title = "Hamilton Benefits by Income for Couples")

#9601  40725.
#Approx cost in billions

scf_2019 <- scf_2019 %>% filter(dummy_data!=1) 
scf_2019 %>%   summarise(total_cost=sum(hamilton_money*wgt)/1000000000) #876B estimate in Hamilton, I'm getting 767

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
  



