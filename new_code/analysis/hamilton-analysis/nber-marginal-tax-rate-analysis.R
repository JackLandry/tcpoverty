
#Analyzing marginal tax rates using NBER data
nber_input_data <- read_csv(here("inter_data","marginal_tax_rate_nber_test_data.csv"))
nber_input_data %>% mutate(sage=ifelse(mstat!=2,0,sage))
#Need to do something about different states

# Eliminate variables that are always 0 to simplify things

state_pop_data <- read_csv("/Users/jacklandry/Downloads/2019_Census_US_Population_Data_By_State_Lat_Long.csv")
state_pop_data #Need to crosswalk with state codes
names(state_pop_data) <- tolower(names(state_pop_data))
state_pop_data <- state_pop_data %>% 
  rename(state_name=state, pop_state=popestimate2019) %>% 
  select(state_name,pop_state)
state_pop_data <- state_pop_data %>% mutate(total_pop=sum(pop_state),
                          frac_pop=pop_state/total_pop)

state_crosswalk_soi <- read_csv(here("random","state-code-crosswalk-clean.csv"))
state_crosswalk_soi <- state_crosswalk_soi %>% mutate(state_name=ifelse(state_name=="DC","District of Columbia",state_name))

state_pop_data <- state_pop_data %>% left_join(state_crosswalk_soi)

nber_data <- import(here("inter_data","taxsim_out.dta"))
nber_data <- nber_data %>% left_join(nber_input_data)
#Removing columns that are always 0
nber_data <- nber_data %>% select(which(!colSums(nber_data, na.rm=TRUE) %in% 0))
nber_data <- nber_data %>% rename(tax_id=state) %>% left_join(state_pop_data) %>% as_tibble()
nber_data %>% filter(income==40000 & children==2 & mstat==2) 

#How do I weight by state
nber_data <- nber_data %>% select(-state_name,-census_id) %>% 
  group_by(mstat,children,income) %>% 
  summarise_all(~(weighted.mean(., frac_pop))) #Not sure if this is the propper weight
#Would also be nice to have workflows where I didn't need to weight by state


nber_data <- nber_data %>% rename(eitc=v25)
nber_data <- nber_data %>% 
  mutate(single=ifelse(mstat!=2,1,0),
         spouse=ifelse(mstat==2,1,0),
         age=20,
         total_personal_income=pwages,
         total_income_couple=pwages+swages, #Is this correc
         num_eitc_elig_children_single=ifelse(single==1,children,0),
         num_eitc_elig_children_two_parents=ifelse(mstat==2,children,0))

#Hamilton money singles below phaseout start point
nber_data <- nber_data %>% mutate(hamilton_money=0)
nber_data <- nber_data %>% mutate(hamilton_money=ifelse(total_personal_income<=10000 & single==1 & age>=18,
                                                    12500+(num_eitc_elig_children_single*4500),hamilton_money))


#Hamilton money singles above phaseout start point
nber_data <- nber_data %>% mutate(hamilton_benefit=0,
                              hamilton_phaseout_percentage=0,
                              hamilton_phaseout_amount=0,
                              hamilton_phaseout=0)

#Benefit = base + 4500*num_kis
nber_data <- nber_data %>% mutate(hamilton_benefit=ifelse(total_personal_income>10000 & total_personal_income<=50000 &
                                                        single==1 & age>=18,
                                                      12500+4500*num_eitc_elig_children_single,hamilton_benefit))


#Phaseout = (total_income-phaseout_start)*phaseout_rate

nber_data <- nber_data %>% mutate(hamilton_phaseout_percentage=(12500+4500*num_eitc_elig_children_single)/-40000,
                              hamilton_phaseout_amount=(total_personal_income-10000)*hamilton_phaseout_percentage)

#Putting it together
nber_data <- nber_data %>% mutate(hamilton_money=ifelse(total_personal_income>10000 & total_personal_income<=50000 &
                                                      single==1 & age>=18,
                                                    hamilton_benefit+hamilton_phaseout_amount,hamilton_money))


#For spouses, divide the benefit in 2 and split between spouses
#Not doing that in the tax data which is at the filing unit level
#Spouses below phaseout start point
nber_data <- nber_data %>% mutate(hamilton_money=ifelse(total_income_couple<=15000 & spouse==1 & age>=18,
                                                    (25000+(num_eitc_elig_children_two_parents*4500)),hamilton_money))
nber_data %>% filter(total_income_couple<15000 & 
                     spouse==1 & age>=18) %>% 
  select(hamilton_money,total_income_couple,num_eitc_elig_children_two_parents)



#Spouses above phaseout start point
#Benefit
nber_data <- nber_data %>% mutate(hamilton_benefit=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                        spouse==1 & age>=18,
                                                      25000+4500*num_eitc_elig_children_two_parents,hamilton_benefit))


#Phaseout
nber_data <- nber_data %>% mutate(hamilton_phaseout=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                         spouse==1 & age>=18,
                                                       (total_income_couple-15000)*(25000+(4500*num_eitc_elig_children_two_parents))/55000,hamilton_phaseout))



#Checking                     
nber_data %>% filter(hamilton_benefit!=0 & total_income_couple>10000 & total_income_couple<=70000 & 
                     spouse==1 & age>=18) %>% 
  select(hamilton_benefit,hamilton_phaseout,total_income_couple)
#This should be 0, and in general phaseout is conditional on benefit size
nber_data %>% filter(hamilton_benefit!=0 & total_income_couple==70000 & 
                     spouse==1 & age>=18) %>% 
  select(hamilton_benefit,hamilton_phaseout,total_income_couple)




#Putting it together, dividing by 2 because split between two groups
nber_data <- nber_data %>% mutate(hamilton_money=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                      spouse==1 & age>=18,
                                                    (hamilton_benefit-hamilton_phaseout),hamilton_money))

#####################################################################
#####################################################################
nber_data <- nber_data %>% mutate(total_taxes=fiitax+siitax+(fica*.5), #federal taxes, state taxes, and emplyee side of fica taxes (rather than sum)
                                  total_federal_taxes=fiitax+(fica*.5),
                                  aftertax_income=pwages+swages-total_taxes,
                                  aftertax_income_federal_taxes=pwages+swages-total_federal_taxes,
                                  pretax_income=pwages+swages) 

#The issue is EIC with Hamilon
# nber_data %>% select(aftertax_income_no_eitc,aftertax_income,marginal_tax_rate,marginal_tax_rate_no_eitc) %>% View()
# nber_data %>% select(aftertax_hamilton_no_eitc,aftertax_hamilton_eitc,eitc,
#                      marginal_tax_rate_hamilton_eitc,marginal_tax_rate_hamilton_no_eitc) %>% View()
#Need to make some kind of function here
nber_data <- nber_data %>% 
  mutate(aftertax_income_no_eitc=aftertax_income-eitc, #Need to
         tax_rate=(pretax_income-aftertax_income)/pretax_income,
         tax_rate_no_eitc=(pretax_income-aftertax_income_no_eitc)/pretax_income,
         aftertax_hamilton_eitc=aftertax_income+hamilton_money,
         aftertax_fed_only_no_eitc=aftertax_income_federal_taxes-eitc,
         aftertax_hamilton_eitc_fed_only=aftertax_income_federal_taxes+hamilton_money,
         aftertax_hamilton_no_eitc=aftertax_income_no_eitc+hamilton_money,
         aftertax_hamilton_no_eitc_fed_only=aftertax_fed_only_no_eitc+hamilton_money,
         tax_rate_hamilton=(pretax_income-(aftertax_income+hamilton_money))/pretax_income,
         tax_rate_hamilton_no_eitc=(pretax_income-(aftertax_income_no_eitc+hamilton_money))/pretax_income) 

#Calculating effective marginal tax rate for the phase out
#For example, a single parent with one child and income between $10,000 and $50,000 would face a benefit reduction rate of 42.5 percent 
#plus a payroll tax of 7.65 percent, for an effective marginal tax rate of 50.15 percent. 

#For example, households with two parents and two children, and an income approaching $70,000, would probably face an income tax rate of 12 percent. 
#Their combined EMTR would thus be 61.8 + 7.65 + 12, or 81.45 percent.

#Calculating MTR over the phase out for singles
nber_mt_singles_1 <- nber_data %>% filter(income==10000 & mstat==1) %>% select(income,aftertax_hamilton_no_eitc) %>% mutate(pre_post=0)
nber_mt_singles_2 <- nber_data %>% filter(income==50000 & mstat==1) %>% select(income,aftertax_hamilton_no_eitc) %>% mutate(pre_post=1)
nber_mt_singles_1 %>% bind_rows(nber_mt_singles_2) %>% arrange(children,pre_post) %>% 
  mutate(change_pretax_income=income-lag(income),
         change_aftertax_income_hamilton_no_eitc=aftertax_hamilton_no_eitc-lag(aftertax_hamilton_no_eitc),
         marginal_tax_rate_hamilton_no_eitc=(change_pretax_income-change_aftertax_income_hamilton_no_eitc)/change_pretax_income) %>% 
  filter(income==50000) %>% select(income,marginal_tax_rate_hamilton_no_eitc)

#Same thing for couples
nber_mt_couples_1 <- nber_data %>% filter(income==15000 & mstat==2) %>% select(income,aftertax_hamilton_no_eitc) %>% mutate(pre_post=0)
nber_mt_couples_2 <- nber_data %>% filter(income==70000 & mstat==2) %>% select(income,aftertax_hamilton_no_eitc) %>% mutate(pre_post=1)

nber_mt_couples_1 %>% bind_rows(nber_mt_couples_2) %>% arrange(children,pre_post) %>% 
  mutate(change_pretax_income=income-lag(income),
         change_aftertax_income_hamilton_no_eitc=aftertax_hamilton_no_eitc-lag(aftertax_hamilton_no_eitc),
         marginal_tax_rate_hamilton_no_eitc=(change_pretax_income-change_aftertax_income_hamilton_no_eitc)/change_pretax_income) %>% 
  filter(income==70000) %>% select(income,marginal_tax_rate_hamilton_no_eitc)


#Need to write the correct MTR calculation here
#Probably should write some kind of fucntion in the future here...
nber_data <- nber_data %>% 
  mutate(change_aftertax_income=aftertax_income-lag(aftertax_income),
         change_aftertax_income_no_eitc=aftertax_income_no_eitc-lag(aftertax_income_no_eitc),
         change_aftertax_income_federal_only=aftertax_income_federal_taxes-lag(aftertax_income_federal_taxes),
         change_aftertax_income_hamilton_eitc=aftertax_hamilton_eitc-lag(aftertax_hamilton_eitc),
         change_aftertax_income_hamilton_eitc_fed_only=aftertax_hamilton_eitc_fed_only-lag(aftertax_hamilton_eitc_fed_only),
         change_aftertax_income_hamilton_no_eitc=aftertax_hamilton_no_eitc-lag(aftertax_hamilton_no_eitc),
         change_aftertax_income_hamilton_no_eitc_fed_only=aftertax_hamilton_no_eitc_fed_only-lag(aftertax_hamilton_no_eitc_fed_only),
         change_pretax_income=pretax_income-lag(pretax_income),
         marginal_tax_rate=(change_pretax_income-change_aftertax_income)/change_pretax_income,
         marginal_tax_rate_no_eitc=(change_pretax_income-change_aftertax_income_no_eitc)/change_pretax_income,
         marginal_tax_rate_federal_only=(change_pretax_income-change_aftertax_income_federal_only)/change_pretax_income,
         marginal_tax_rate_hamilton_eitc=(change_pretax_income-change_aftertax_income_hamilton_eitc)/change_pretax_income,
         marginal_tax_rate_hamilton_eitc_fed_only=(change_pretax_income-change_aftertax_income_hamilton_eitc_fed_only)/change_pretax_income,
         marginal_tax_rate_hamilton_no_eitc=(change_pretax_income-change_aftertax_income_hamilton_no_eitc)/change_pretax_income,
         marginal_tax_rate_hamilton_no_eitc_fed_only=(change_pretax_income-change_aftertax_income_hamilton_no_eitc)/change_pretax_income)


nber_data <- nber_data %>% mutate(couple_status=ifelse(single==1,"Single",
                                                   ifelse(spouse==1,"Couple",NA)),
                              num_children=ifelse(num_eitc_elig_children_single==0,
                                                  num_eitc_elig_children_two_parents,num_eitc_elig_children_single))

#For a given level of income, what is the tax stuff for different family types\
#Wait, isn't it better to do this goup r
# nber_data_wide <- nber_data %>% select(pretax_income,couple_status,num_children,marginal_tax_rate,marginal_tax_rate_hamilton_eitc,marginal_tax_rate_hamilton_no_eitc) %>% 
#   pivot_wider(id_cols = pretax_income,
#               names_from = c(couple_status,num_children), 
#               values_from = c(marginal_tax_rate,marginal_tax_rate_hamilton_eitc,marginal_tax_rate_hamilton_no_eitc))
# 

nber_data <- nber_data %>% mutate(`Number of Children`=as.factor(num_children))

nber_data <- nber_data %>% mutate(num_children_clean=str_glue("{num_children} Children")) 

write_rds(nber_data, here("inter_data","nber_data_hamilton_for_analysis.Rds"))





ggplot(data= nber_data %>% filter(couple_status=="Single" & num_children!=6 & pretax_income!=0), 
       aes(x = pretax_income, y = marginal_tax_rate_federal_only)) +
  geom_line() +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 6) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))


ggplot(data= nber_data %>% filter(couple_status=="Single" & num_children!=6 & pretax_income!=0), 
       aes(x = pretax_income, y = marginal_tax_rate)) +
  geom_line() +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 6) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

ggplot(data= nber_data %>% filter(couple_status=="Couple" & num_children!=6 & pretax_income!=0), 
       aes(x = pretax_income, y = marginal_tax_rate)) +
  geom_line() +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income",
       title = "Marginal Tax Rates for Married Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 6) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))






#######################
nber_data <- read_rds(here("inter_data","nber_data_hamilton_for_analysis.Rds"))
#Want to show the difference between 
nber_data_long <- nber_data %>% 
  select(pretax_income,couple_status,num_children,num_children_clean,marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc,
         marginal_tax_rate,marginal_tax_rate_federal_only,marginal_tax_rate_hamilton_no_eitc_fed_only) %>% 
  pivot_longer(cols = c(marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc,marginal_tax_rate,
                        marginal_tax_rate_federal_only,marginal_tax_rate_hamilton_no_eitc_fed_only),
               names_to = "tax_rate_type",
               values_to = "marginal_tax_rate")

#Want to do aftertax income here, not sure why it's not working
nber_data_long_aftertax_inc <- nber_data %>% 
  select(pretax_income,couple_status,num_children,num_children_clean,
         aftertax_income,
         aftertax_hamilton_eitc,
         aftertax_hamilton_eitc_fed_only,
         aftertax_hamilton_no_eitc) %>% 
  pivot_longer(cols = c(pretax_income,aftertax_hamilton_eitc,
                        aftertax_hamilton_eitc_fed_only,aftertax_income),
               names_to = "tax_rate_type",
               values_to = "aftertax_income")


#Need to fix this for the new tax rate types
nber_data_long <- nber_data_long %>% 
  mutate(tax_rate_type=ifelse(tax_rate_type=="marginal_tax_rate", "Status Quo",
                              ifelse(tax_rate_type=="marginal_tax_rate_hamilton_no_eitc","Hamilton Reform\nEIC Repealed", 
                                     ifelse(tax_rate_type=="marginal_tax_rate_hamilton_eitc","Hamilton Reform\nKeep EIC",
                                            ifelse(tax_rate_type=="marginal_tax_rate_federal_only","Status Quo Federal Only",
                                                   ifelse(tax_rate_type=="marginal_tax_rate_hamilton_no_eitc_fed_only","Hamilton Reform\nEIC Repealed\nFed Only",NA))))))

#Don' understand why it's not showing a difference

#nber_data_long %>% filter(tax_rate_type=="Status Quo Federal Only"|tax_rate_type=="Status Quo") %>% View()
nber_data_long <- nber_data_long %>% filter(tax_rate_type!="Status Quo Federal Only" & tax_rate_type != "Hamilton Reform\nEIC Repealed\nFed Only")
#How can this be correct????

ggplot(data= nber_data_long %>% filter(couple_status=="Single" & num_children!=6 & pretax_income!=0 & !is.na(tax_rate_type)), 
       aes(x = pretax_income, y = marginal_tax_rate, group = tax_rate_type)) +
  geom_line(aes(color = tax_rate_type), size=linesize, alpha = line_trans) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate\nFor Additonal\n$100 Income",
       x = "Labor Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5, limits = c(-1,1.3), breaks = seq(-1,1,.5)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

#Issue with single mothers getting EIC
ggplot(data= nber_data_long %>% filter(couple_status=="Couple" & num_children!=6 & pretax_income!=0 & !is.na(tax_rate_type)), 
       aes(x = pretax_income, y = marginal_tax_rate, group = tax_rate_type)) +
  geom_line(aes(color = tax_rate_type), size=linesize, alpha = line_trans) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate\nFor Additonal\n$100 Income",
       x = "Labor Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5, limits = c(-1,1.3), breaks = seq(-1,1,.5)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))



nber_data_long_aftertax_inc
####
ggplot(data= nber_data_long_aftertax_inc %>% 
         filter(couple_status=="Couple" & num_children!=6 & pretax_income!=0 & !is.na(tax_rate_type)), 
       aes(x = pretax_income, y = aftertax_income, group = tax_rate_type)) +
  geom_line(aes(color = tax_rate_type), size=linesize, alpha = line_trans) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate\nFor Additonal\n$100 Income",
       x = "Labor Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5, limits = c(-1,1.3), breaks = seq(-1,1,.5)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))




#######################

#What are these weird blips
ggplot(data= nber_data %>% filter(couple_status=="Single" & num_children!=6 & pretax_income!=0), 
       aes(x = pretax_income, y = marginal_tax_rate_federal_only)) +
  geom_line() +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 6) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

