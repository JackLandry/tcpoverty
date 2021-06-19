
#################################################################################
#################################################################################
#################################################################################
###########################Boost Act Programming#################################
#################################################################################
#################################################################################
#################################################################################

#Single below phase out
ipum <- ipum %>% mutate(boost_money=0)
ipum <- ipum %>% mutate(boost_money=ifelse(total_personal_income<30000 & single==1 & age>=18 & num_eitc_elig_children_single==0,
                                           3000,boost_money))

#Couple below phase out
ipum <- ipum %>% mutate(boost_money=ifelse(total_income_couple<=80000  & spouse==1 & age>=18,
                                           6000*.5,boost_money))

#Single with kids below phase out
ipum <- ipum %>% mutate(boost_money=ifelse(total_personal_income<60000 & single==1 & age>=18 & num_eitc_elig_children_single>0,
                                           3000,boost_money))

#Single above phaseout start no kids
ipum <- ipum %>% mutate(boost_money=ifelse(total_personal_income>=30000 & total_personal_income<=50000 &
                                             single==1 & age>=18 & num_eitc_elig_children_single==0,
                                           3000-((total_personal_income-30000)*.15),boost_money))
#Couple above phaseout point
ipum <- ipum %>% mutate(boost_money=ifelse(total_income_couple>=60000 & total_income_couple<=100000 &
                                             spouse==1 & age>=18,
                                           (6000-((total_income_couple-60000)*.15))*.5,boost_money))
#Single with kids above phaseout point
ipum <- ipum %>% mutate(boost_money=ifelse(total_personal_income>=60000 & total_personal_income<=80000 &
                                             single==1 & age>=18 & num_eitc_elig_children_single>0,
                                           3000-((total_personal_income-60000)*.15),boost_money))


#Seems right
ipum %>% filter(age>=18) %>% 
  select(spmfamunit,boost_money,total_income_couple,total_personal_income,single,spouse)

ggplot(ipum %>% filter(single==1 & age>=18 & total_personal_income<100000 & num_eitc_elig_children_single==0), 
       aes(x = total_personal_income, y = boost_money)) +
  geom_point()

ggplot(ipum %>% filter(single==1 & age>=18 & total_personal_income<100000 & num_eitc_elig_children_single>0), 
       aes(x = total_personal_income, y = boost_money)) +
  geom_point()

ggplot(ipum %>% filter(spouse==1 & age>=18 & total_income_couple<100000), 
       aes(x = total_income_couple, y = boost_money)) +
  geom_point()




#BOOST Act total cost: 451 billion vs. ITEP of $380 billion in 2020... 
ipum %>% summarise(total_cost=sum(boost_money*spmwt)/1000000000)
ipum %>% filter(filestat!=6) %>% 
  summarise(total_cost=sum(boost_money*spmwt)/1000000000)
#Checking number of beneficaries
ipum <- ipum %>% mutate(some_boost_money=ifelse(boost_money>0,1,0))
#70% of adults benefit vs. ITEP 65%
ipum %>% filter(age>=18) %>% 
  summarise(num_benefeciaries=weighted.mean(some_boost_money,spmwt))
#


############
#General checking
ipum %>% filter(hamilton_money!=0) %>% 
  select(hamilton_money,total_income_couple,
         spouse,total_personal_income,num_eitc_elig_children_two_parents,num_eitc_elig_children_single)
#Want to look at personal income X benefit amont
ipum <- ipum %>% group_by(spmfamunit) %>% 
  mutate(hamilton_spu_family_benefit=sum(hamilton_money,na.rm=TRUE),
         spu_family_income=sum(total_personal_income, na.rm=TRUE))

#Looking at family impact by family income
ggplot(data = ipum %>% filter(spu_family_income<=100000), aes(x = spu_family_income, y = hamilton_spu_family_benefit)) +
  geom_point(alpha = .1) +
  labs(y = "Benefit", x = "Labor Income")

#Why are these people getting no money
ipum %>% filter(hamilton_money==0 & total_personal_income<10000) %>% select(age,spouse,total_personal_income)
#This is just weird....
ipum %>% filter(spmfamunit==2001) %>% select(hamilton_money,age,spouse,total_personal_income,num_eitc_elig_children_single)
#This mightbe a family type situation, these 18+ kids should be getting money, maybe they aren't because same family unit as parents?
ipum %>% filter(spmfamunit==9001) %>% select(hamilton_money,age,spouse,total_personal_income,num_eitc_elig_children_single)


#Average boost across the family income distribution maybe?



#)-(total_income_couple-15000)*(25000+(4500*num_eitc_elig_children_two_parents)/-60000))*.5

# ipum <- ipum %>% mutate(hamilton_money=ifelse(total_personal_income>10000 & total_personal_income<=70000 & 
#                                                 spouse==1 & age>=18,
#                                               ((25000+4500*num_eitc_elig_children_two_parents)-(total_income_couple-15000)*(25000+(4500*num_eitc_elig_children_two_parents)/-60000))*.5,hamilton_money))






#Think about effective marginal tax rate for large number of children, I think it's really high
#How would I actually analyze this relative to standard EITC + state-top ups. I don't think the tax calculator does the top ups as is



#base_benefit+child_benefit*num_children-(total_income-start_phase_out)*phase_out_rate
#Phase_out_rate=slope... (y2-y1)/(x2-x1) == max_eitc_benefit-0/phase_out_begin-phase_out_end == max_eitc_benefit/10000-50000
#phase_out_rate=base_benefit+child_benefit*num_children/-40000



#Might be nice to get this: https://www.cchcpelink.com/book/state-tax-handbook-2021-10034384-0012/18482/

