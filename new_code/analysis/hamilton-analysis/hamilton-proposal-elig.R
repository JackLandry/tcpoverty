library(tidyverse)
library(here)
library(tidylog)
ipum <- read_rds(here("inter_data","ipum_prepped.Rds"))

####################################################
####################################################
####################################################
############Hamilton benefits programming###########
####################################################
####################################################

#income-phaseout_start * phaseout_rate
#(total_income-start_phase_out)*phase_out_rate
#Phase_out_rate=slope... (y2-y1)/(x2-x1) == max_eitc_benefit-0/phase_out_begin-phase_out_end == max_eitc_benefit/10000-50000
#phase_out_rate=base_benefit+child_benefit*num_children/-40000

ipum <- ipum %>% mutate(dummy_data=0)
ipum <- ipum %>% add_row(single=1, total_personal_income=40000, num_eitc_elig_children_single=0, age=30, dummy_data=1) %>% 
  add_row(single=1, total_personal_income=40000, num_eitc_elig_children_single=1, age=30, dummy_data=1) %>% 
  add_row(single=1, total_personal_income=40000, num_eitc_elig_children_single=2, age=30, dummy_data=1)

#Hamilton money singles below phaseout start point
ipum <- ipum %>% mutate(hamilton_money=0)
ipum <- ipum %>% mutate(hamilton_money=ifelse(total_personal_income<=10000 & single==1 & age>=18,
                                              12500+(num_eitc_elig_children_single*4500),hamilton_money))


#Hamilton money singles above phaseout start point
ipum <- ipum %>% mutate(hamilton_benefit=0,
                        hamilton_phaseout_percentage=0,
                        hamilton_phaseout_amount=0,
                        hamilton_phaseout=0)

#Benefit = base + 4500*num_kis
ipum <- ipum %>% mutate(hamilton_benefit=ifelse(total_personal_income>10000 & total_personal_income<=50000 &
                                                  single==1 & age>=18,
                                                12500+4500*num_eitc_elig_children_single,hamilton_benefit))
ipum %>% filter(dummy_data==1 & single==1) %>% #Need to double spouses for the exsample
  select(hamilton_phaseout_percentage,hamilton_phaseout_amount,hamilton_benefit,total_personal_income,hamilton_money,num_eitc_elig_children_single)


ipum <- ipum %>% mutate(hamilton_phaseout_percentage=(12500+4500*num_eitc_elig_children_single)/-40000,
                        hamilton_phaseout_amount=(total_personal_income-10000)*hamilton_phaseout_percentage)

#Putting it together
ipum <- ipum %>% mutate(hamilton_money=ifelse(total_personal_income>10000 & total_personal_income<=50000 &
                                                single==1 & age>=18,
                                              hamilton_benefit+hamilton_phaseout_amount,hamilton_money))


singles_data <- ipum %>% filter(dummy_data==1 & single==1) %>% #Need to double spouses for the exsample
  select(hamilton_money,total_personal_income,num_eitc_elig_children_single)

write_rds(singles_data, here("inter_data","singles_hamilton.rds"))

ipum <- ipum %>% filter(dummy_data!=1)
#Example for spouses
ipum <- ipum %>% add_row(spouse=1, total_income_couple=40000, num_eitc_elig_children_two_parents=0, age=30, dummy_data=1) %>% 
  add_row(spouse=1, total_income_couple=40000, num_eitc_elig_children_two_parents=1, age=30, dummy_data=1) %>% 
  add_row(spouse=1, total_income_couple=40000, num_eitc_elig_children_two_parents=2, age=30, dummy_data=1)

#For spouses, divide the benefit in 2 and split between spouses
#Spouses below phaseout start point
ipum <- ipum %>% mutate(hamilton_money=ifelse(total_income_couple<=15000 & spouse==1 & age>=18,
                                              (25000+(num_eitc_elig_children_two_parents*4500))*.5,hamilton_money))
ipum %>% filter(total_income_couple<15000 & 
                  spouse==1 & age>=18) %>% 
  select(hamilton_money,total_income_couple,num_eitc_elig_children_two_parents)



#Spouses above phaseout start point
#Benefit
ipum <- ipum %>% mutate(hamilton_benefit=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                  spouse==1 & age>=18,
                                                25000+4500*num_eitc_elig_children_two_parents,hamilton_benefit))


#Phaseout
ipum <- ipum %>% mutate(hamilton_phaseout=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                   spouse==1 & age>=18,
                                                 (total_income_couple-15000)*(25000+(4500*num_eitc_elig_children_two_parents))/55000,hamilton_phaseout))



#Checking                     
ipum %>% filter(hamilton_benefit!=0 & total_income_couple>10000 & total_income_couple<=70000 & 
                  spouse==1 & age>=18) %>% 
  select(hamilton_benefit,hamilton_phaseout,total_income_couple)
#This should be 0, and in gneral phaseout is conditional on benefit size
ipum %>% filter(hamilton_benefit!=0 & total_income_couple==70000 & 
                  spouse==1 & age>=18) %>% 
  select(hamilton_benefit,hamilton_phaseout,total_income_couple)




#Putting it together, dividing by 2 because split between two groups
ipum <- ipum %>% mutate(hamilton_money=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                spouse==1 & age>=18,
                                              (hamilton_benefit-hamilton_phaseout)*.5,hamilton_money))

#Getting the 40k exsample fpr spouses
couples_data <- ipum %>% filter(dummy_data==1) %>% #Need to double spouses for the exsample
  mutate(hamilton_money=ifelse(spouse==1 & dummy_data==1,hamilton_money*2,hamilton_money)) %>% 
  select(total_income_couple,hamilton_money,num_eitc_elig_children_two_parents)
write_rds(couples_data, here("inter_data","couples_hamilton.rds"))


#Getting rid of dummy data
ipum <- ipum %>% filter(dummy_data!=1)

ipum %>% filter(spmfamunit==9001 & age>=18) %>% 
  select(hamilton_money,total_personal_income,single,spouse,num_eitc_elig_children_single)

#Checking the phaseouts
ggplot(data = ipum %>% filter(spouse==1 & total_income_couple<=100000 & age>=18), 
       aes(x = total_income_couple, y = hamilton_money)) +
  geom_point(alpha = .1) +
  labs(y = "Benefit", x = "Total couple income")


ggplot(data = ipum %>% filter(single==1 & total_personal_income<=100000 & age>=18), 
       aes(x = total_personal_income, y = hamilton_money)) +
  geom_point(alpha = .1) +
  labs(y = "Benefit", x = "Labor Income")


write_rds(ipum, here("inter_data","ipum_hamilton_for_analysis.Rds"))




