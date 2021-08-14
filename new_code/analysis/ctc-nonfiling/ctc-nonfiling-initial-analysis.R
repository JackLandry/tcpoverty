library(here)
library(tidyverse)
library(tidylog)
library(rio)

here()

new_ctc <- read_csv(here("tax-calculator-code","cps_2019_for_taxsim-21-#-ctc-new-current-law-#.csv"))
no_new_ctc <- read_csv(here("tax-calculator-code","cps_2019_for_taxsim-21-#-ctc-repealed-#.csv"))
below_max_credit <- read_csv(here("tax-calculator-code","full_credit_start.csv"))

#Getting tax variables for new 
tax_demos <- new_ctc %>% select(nu18,nu06,age_head,MARS,RECID)
new_ctc <- new_ctc %>% 
  select(ctc_new,aftertax_income,e00200,RECID,c00100
         ,combined,iitax,payrolltax,lumpsum_tax,c07220) %>% #Lets break this up into 
  rename_all( ~ paste0("ctc_expansion_", .x)) %>% 
  rename(aftertax_income_ctc=ctc_expansion_aftertax_income,
         RECID=ctc_expansion_RECID)
new_ctc <- new_ctc %>% left_join(tax_demos)
no_new_ctc <- no_new_ctc %>% 
  select(ctc_new,aftertax_income,RECID,combined,iitax,payrolltax,lumpsum_tax,c07220) %>% 
  rename_all( ~ paste0("status_quo_", .x)) %>% 
  rename(aftertax_income_no_ctc=status_quo_aftertax_income,
         RECID=status_quo_RECID)

tax_reform <- new_ctc %>% left_join(no_new_ctc)

tax_reform <- tax_reform %>% mutate(change_aftertax_income=aftertax_income_ctc-aftertax_income_no_ctc,
                                    total_ctc=ctc_expansion_c07220+ctc_expansion_ctc_new)
#If it doesn't match it's ok, maybe that child-marriage combo isn't in data
#Or the master doesn't have kids
# tax_reform <- tax_reform %>% left_join(below_max_credit)  #Ahh 
#Not sure if this is right, wouldn't it be full credit under old system
#This is receiving full credit under expansion, not just refundability
# #Want to make another set of jsons for refundability change only, keeping TCJA amounts
# tax_reform <- tax_reform %>% 
#   mutate(not_getting_full_credit=ifelse(c00100<start_full_credit & !is.na(start_full_credit),1,0))
# 


tax_reform <- tax_reform %>% select(nu18,nu06,c00100,age_head,MARS,
                      total_ctc,ctc_new,change_aftertax_income,e00200,RECID,not_getting_full_credit,combined,iitax,payrolltax,lumpsum_tax)


tax_reform <- tax_reform %>% mutate(`Children\nUnder 18`=as.factor(nu18),
       `Children\nUnder 6`=as.factor(nu06),
       `Marital Status` = ifelse(MARS==2,"Married Filing Jointly",
                                 ifelse(MARS==1,"Single",
                                        ifelse(MARS==4,"Head of Household",NA))))



ggplot(tax_reform %>% filter(nu18<6 & nu06<=3 & ctc_expansion_c00100<200000 & age_head!=18 & MARS!=1 & nu18!=0),
       aes(x = c00100, y = change_aftertax_income)) +
  geom_point(aes(color = `Children\nUnder 18`, shape = `Children\nUnder 6`)) +
  facet_grid(cols = vars(`Marital Status`)) +
  labs(y = "New\nCTC\nCredit", x = "Adjusted Gross Income") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 

#Prepping variables from HH cps
hh_cps <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2019/hhpub19.csv")
names(hh_cps) <- tolower(names(hh_cps))
hh_cps  <- hh_cps %>% mutate(snap=ifelse(hfoodsp==1,1,0),
                             free_lunch=ifelse(hflunch==1,1,0),
                             housing_money=ifelse(hlorent==1,1,0),
                             public_housing=ifelse(hpublic==1,1,0),
                             housing_assistance=ifelse(housing_money==1 | public_housing==1,1,0),
                             wic=ifelse(hrwicyn==1,1,0),
                             medicaid_chip=ifelse(hmcaid==1 | hmcaid==2,1,0),
                             social_security=ifelse(hss_yn==1,1,0)) %>% 
  select(c(h_seq,snap:social_security))


#Would be good to cut down the size of this cps sample by reducing number of variables
cps <- import("~/Dropbox/JFI/cps_data/clean/cps_pre_tax_filing_unit_collapse.dta")
cps <- cps %>% as_tibble()
names(tax_reform)
cps <- cps %>% rename(h_seq=ph_seq)
cps <- cps %>% left_join(hh_cps)
#Actually would be better to keep the variables from tax reform, because they are at the tax unit level
cps <- cps %>% select(-c(nu18,nu06,age_head,MARS,e00200))
#tax_reform_small <- tax_reform %>% select(total_ctc,ctc_new,change_aftertax_income,RECID)

cps <- cps %>% left_join(tax_reform, by = "RECID") 



cps <- cps %>% mutate(poor_ctc=ifelse(spm_povthreshold>=spm_resources+change_aftertax_income,1,0), 
               poor_no_ctc=ifelse(spm_povthreshold>=spm_resources,1,0),
               out_of_poverty_ctc=ifelse(poor_no_ctc==1 & poor_ctc==0,1,0))

#Getting basic stats from the analysis
cps %>% summarise(pov_status_quo=weighted.mean(poor_ctc,spm_weight_r),
                  pov_ctc=weighted.mean(poor_no_ctc,spm_weight_r),
                  out_of_poverty_ctc=weighted.mean(out_of_poverty_ctc,spm_weight_r))

cps %>% filter(a_age<18) %>% 
  summarise(pov_status_quo=weighted.mean(poor_ctc,spm_weight_r),
            pov_ctc=weighted.mean(poor_no_ctc,spm_weight_r),
            out_of_poverty_ctc=weighted.mean(out_of_poverty_ctc,spm_weight_r))

#Getting number of people in each tax unit
cps <- cps %>% add_count(RECID) %>% mutate(n_tax_id=n)

#Fixing a few more varaibles
cps <- cps %>% mutate(num_one=1,
                      child=ifelse(a_age<18,1,0), #In the raw data I count permently disabled as children for EITC calc, this fixes
                      spm_weight_r=spm_weight/100,
                      asec_ind_weight=marsupwt/100) 
#From codebookValues: 2 implied decimals (example: 255212=2552.12)



#How many children not eligible according to CPS-ASEC data

cps <- cps %>% mutate(getting_ctc=ifelse(total_ctc>0,1,0),
                      monthly_ctc_payment=total_ctc/12)

#Confirms that just about 2% of people are not eligable, and missing ~12 million kids
cps %>% filter(child==1) %>% 
  summarise(num_children_getting_ctc=sum(getting_ctc*asec_ind_weight),
                  frac_children_getting_ctc=weighted.mean(getting_ctc,asec_ind_weight))

#Checking child pop
cps %>% filter(child==1) %>% summarise(sum(asec_ind_weight)) 
#Making sure IRS pop matches my pop, they say 65 mil children is 88%
65/73.79 #Matches the IRS pop
num_children_not_getting_ctc_infer <- 72110947-65000000
num_children_not_getting_ctc_infer #7.1 M
#Number of poor children = 10 mil, so that's enough to work with for bounds
cps %>% filter(child==1 & poor_no_ctc==1) %>% summarise(sum(asec_ind_weight))
cps %>% filter(child==1) %>% summarise(child_pov=weighted.mean(poor_no_ctc,asec_ind_weight))
  
#I estimate only 16.5 mil children not getting full credit now getting it
#Way less than WH estimate, so that's weird, I think my thing is wrong
cps %>% filter(not_getting_full_credit==1 & child==1) %>% summarise(sum(asec_ind_weight)) #27 million newly getting full credit


#How many nonfilers does Urban estimate?
#22 percent of families who are not required to file taxes, 
#do not currently qualify for the earned income tax credit or additional CTC, 
#and who have minimal (under $100) or no earnings do not receive the credit. 
#Should double check other earnings measures, but I think that is right
cps %>% mutate(no_earnings_child_hh=if_else(child==1 & ctc_expansion_c00100<=100,1,0)) %>% 
  filter(child==1) %>% 
  summarise(no_earnings_child_hh_mean=weighted.mean(no_earnings_child_hh,asec_ind_weight),
            sum(no_earnings_child_hh*asec_ind_weight)) 

6385087*.22
num_nonfilers_no_eips


#Lets automate this but you get the idea
#17 year olds that turn 18 not eligable, newly born kids are eligable, so ~evens out, right?
#PRobably more 17 year olds turning 18 than births
num_children <- 73792790
frac_nonfilers_tresury <- 2267562/num_children
num_nonfilers <- 2267562
frac_nonfilers_tresury
eip_children <- 720000
num_nonfilers_tresury_eips <- num_nonfilers-eip_children
frac_nonfilers_tresury_eips <- num_nonfilers_tresury_eips/73792790
num_ctc_elig_children <- cps %>% filter(child==1) %>% 
  summarise(num_children_getting_ctc=sum(getting_ctc*asec_ind_weight)) %>% pull()
irs_inital_number <- 65000000
irs_missing_children <- num_ctc_elig_children-irs_inital_number
frac_irs_missing_children <- irs_missing_children/num_children
#Need to add backing out method here




# cps <- cps %>% mutate(snap=ifelse(spm_snapsub!=0,1,0),
#                housing_assistance=ifelse(spm_caphousesub!=0,1,0),
#                wic=ifelse(spm_wicval!=0,1,0),
#                school_lunch=ifelse(spm_schlunch!=0,1,0),
#                tanf_or_afdc=ifelse(paw_typ==1 | paw_typ==3,1,0), #This is at individual level
#                total_num_benefits=snap+school_lunch+housing_assistance+wic,
#                at_least_one_benefit=ifelse(total_num_benefits>=1,1,0)) #Didn't put everything in here

cps <- cps %>% 
  mutate(tanf_or_afdc=ifelse(paw_typ==1 | paw_typ==3,1,0)) %>% 
  group_by(spm_id) %>% 
  mutate(tanf=max(tanf_or_afdc)) %>% 
  ungroup()

cps <- cps %>% mutate(total_num_benefits=snap+free_lunch+housing_assistance+wic+tanf+medicaid_chip,
                      at_least_one_benefit=ifelse(total_num_benefits>=1,1,0))
set.seed(5)
cps <- cps %>% mutate(id=row_number())

# nonfilers_func <- function(filter_poor,nonfiler_measure,variable_name) {
# cps %>% filter(child==1)
# }

#Can write a simple function for this in the future
#Think I should do this now because I am going to add more variables
#Probably want to do multiple different version here
no_ctc_random_1095 <- cps %>% filter(child==1) %>% 
  mutate(rand_num = runif(nrow(.)),
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=num_nonfilers/sample, 
         no_ctc_random_1095=ifelse(rand_num<new_frac_nonfilers,1,0)) %>% #Im doing fraction of what I sampled which is wrong
  filter(no_ctc_random_1095==1) %>% 
  select(no_ctc_random_1095,id)

#low benefits also poor
no_ctc_low_benefits_1095 <- cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.))) %>% 
  arrange(total_num_benefits,rand_num) %>% #Randomizing order within benefit groups
  mutate(selection_number = row_number(),
         max_row_number = max(selection_number),
         selection_number=selection_number/max_row_number,
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=num_nonfilers/sample, 
         no_ctc_random_low_benefits_1095=ifelse(selection_number<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_random_low_benefits_1095==1) %>% 
  select(no_ctc_random_low_benefits_1095,id,total_num_benefits)

no_ctc_high_benefits_1095 <- cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.))) %>% 
  arrange(desc(total_num_benefits,rand_num)) %>% #Randomizing order within benefit groups
  mutate(selection_number = row_number(),
         max_row_number = max(selection_number),
         selection_number=selection_number/max_row_number, #making row number 0-1
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=num_nonfilers/sample, 
         no_ctc_random_high_benefits_1095=ifelse(selection_number<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_random_high_benefits_1095==1) %>% 
  select(no_ctc_random_high_benefits_1095,id,total_num_benefits)

# 
# cps %>% filter(child==1 & poor_no_ctc==1) %>% 
#   ggplot(aes(x=total_num_benefits)) +
#   geom_bar()
# 
# 
# no_ctc_high_benefits %>% summarise(mean(total_num_benefits))
# no_ctc_low_benefits %>% summarise(mean(total_num_benefits)) #I think this is because


#Lets say I do 20% random onpoor and then 80% poor in the different benefit groups


no_ctc_random_among_ctc_recip_1095 <- cps %>% filter(child==1 & getting_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.)),
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=num_nonfilers/sample, 
         no_ctc_random_among_ctc_recip_1095=ifelse(rand_num<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_random_among_ctc_recip_1095==1) %>% 
  select(no_ctc_random_among_ctc_recip_1095,id)



no_ctc_poor_only_1095 <- cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.)),
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=num_nonfilers/sample, 
         no_ctc_poor_only_1095=ifelse(rand_num<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_poor_only_1095==1) %>% 
  select(no_ctc_poor_only_1095,id)

cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.)),
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=num_nonfilers/sample, 
         no_ctc_poor_only=ifelse(rand_num<new_frac_nonfilers,1,0)) %>% 
  summarise(sum(no_ctc_poor_only*asec_ind_weight)) #Confirming im taking benefits away from the right amount of people


#Nonfilers with no earnings
#Earnings are at the filing unit level
#Not enough of these to do for larger fractions of nonfilers
no_ctc_no_earnings_only_1095 <- cps %>% filter(child==1 & ctc_expansion_c00100==0) %>% 
  mutate(rand_num = runif(nrow(.)),
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=num_nonfilers/sample, 
         no_ctc_no_earnings_only_1095=ifelse(rand_num<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_no_earnings_only_1095==1) %>% 
  select(no_ctc_no_earnings_only_1095,id)

#Indicators for different scenarios taking away CTC receipt
#Want to turn off tidylog for this
cps <- cps %>% 
  left_join(no_ctc_random_1095) %>% 
  left_join(no_ctc_poor_only_1095) %>% 
  left_join(no_ctc_low_benefits_1095) %>% 
  left_join(no_ctc_high_benefits_1095) %>% 
  left_join(no_ctc_no_earnings_only_1095) %>% 
  mutate(across(no_ctc_random_1095:no_ctc_no_earnings_only_1095,~ifelse(is.na(.x),0,.x))) 


cps <- cps %>% mutate(across(no_ctc_random_1095:no_ctc_no_earnings_only_1095,
                             ~ifelse(.x==0,poor_ctc, #If we didn't declare you a nonfiler, we give you the poor_ctc designation
                                     #Otherwise, we say you are poor if ctc without 
                                         ifelse(.x==1 & spm_povthreshold>=spm_resources,1,0)), .names = "poor_{.col}")) 
# %>%   
#   mutate(across(poor_no_ctc_random:poor_no_ctc_no_earnings_only, #Creating the out of poverty variables, not working
#                 ~ifelse(poor_no_ctc==1 & .x==0,1,0), .names = "out_of_{.col}")) 



#Add more stats here
  #Could also try to do something like percentage change, p.p change
basic_bar_graph <- cps %>% filter(a_age<18) %>% #Maybe should change order here
  summarise(`Status Quo\nPoverty`=weighted.mean(poor_no_ctc,spm_weight_r),
            `CTC Full Uptake\nPoverty`=weighted.mean(poor_ctc,spm_weight_r),
            `CTC Poor Nonfilers\nPoverty`=weighted.mean(poor_no_ctc_poor_only_1095,spm_weight_r),
            `CTC Poor Nonfilers\nLow Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_low_benefits_1095,spm_weight_r),
            `CTC Poor Nonfilers\nHigh Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_high_benefits_1095,spm_weight_r)) #This is clearly messed up, not sure how

library(scales)
basic_bar_graph %>% pivot_longer(cols = everything()) %>% 
  mutate(row_num=row_number(),
         name=fct_reorder(name,row_num)) %>% #Getting info right order
  ggplot(aes(x = name, y = value)) +
  geom_col(fill = "#572dff") +
  geom_text(aes(x=name, y=value, label = percent(value), vjust=3.5), position = position_dodge(width=0.9),  family = "Unna") +
  labs(x = "", y = "Children\nIn\nPoverty", caption = "Jain Family Institute",
       title = "Percentage of Children in Poverty\nUnder Different Assumptions about Nonfilers\n") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L), limits = c(0,.15), breaks = seq(0,.15,.05)) +
  theme_jfi()

#Trying to add percentage change
cps %>% filter(a_age<18) %>% #Maybe should change order here
  summarise(`Status Quo\nPoverty`=weighted.mean(poor_no_ctc,spm_weight_r),
            `CTC Full Uptake\nPoverty`=weighted.mean(poor_ctc,spm_weight_r),
            `CTC Poor Nonfilers\nPoverty`=weighted.mean(poor_no_ctc_poor_only_1095,spm_weight_r),
            `CTC Poor Nonfilers\nLow Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_low_benefits_1095,spm_weight_r),
            `CTC Poor Nonfilers\nHigh Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_high_benefits_1095,spm_weight_r)) %>% 
  mutate(across(`CTC Full Uptake\nPoverty`:`CTC Poor Nonfilers\nHigh Benefits\nPoverty`,
                ~ (`Status Quo\nPoverty`-.x)/`Status Quo\nPoverty`)) %>% 
  select(`CTC Full Uptake\nPoverty`:`CTC Poor Nonfilers\nHigh Benefits\nPoverty`) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(row_num=row_number(),
         name=fct_reorder(name,row_num)) %>% #Getting info right order
  ggplot(aes(x = name, y = value)) +
  geom_col(fill = "#572dff") +
  geom_text(aes(x=name, y=value, label = percent(value), vjust=3.5), position = position_dodge(width=0.9),  family = "Unna") +
  labs(x = "", y = "Percentage\nReduction\nChild\nPoverty", caption = "Jain Family Institute",
       title = "Percentage Reduction in Child Poverty\nUnder Different Assumptions about Nonfilers\n") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L)) +
  theme_jfi()


cps %>% filter(a_age<18 & no_ctc_poor_only_1095==1) %>% 
  summarise(across(c(snap:medicaid_chip,tanf,at_least_one_benefit), ~weighted.mean(.x, spm_weight_r))) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(senario="no_ctc_poor_only") %>% 
  filter(name!="housing_money" & name!="public_housing")


#Wait how did I think this was going to work...
program_status_func <- function(nonfiling_variable,nonfiling_description) {
  cps %>% filter(a_age<18 & {{nonfiling_variable}}==1) %>% 
    summarise(across(c(snap:medicaid_chip,tanf,at_least_one_benefit), ~weighted.mean(.x, spm_weight_r))) %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(senario=nonfiling_description) %>% 
    filter(name!="housing_money" & name!="public_housing")
}
no_ctc_poor_only_benes_1095 <- program_status_func(no_ctc_poor_only_1095,"Random\nPoor Nonfilers") 
no_ctc_low_benefits_benes_1095 <- program_status_func(no_ctc_random_low_benefits_1095,"Low Benefit\nPoor Nonfilers") 
no_ctc_high_benefits_benes_1095 <- program_status_func(no_ctc_random_high_benefits_1095,"High Benefit\nPoor Nonfilers") 


no_ctc_poor_only_benes_1095 %>% 
  bind_rows(no_ctc_low_benefits_benes_1095) %>% 
  bind_rows(no_ctc_high_benefits_benes_1095) %>% 
  mutate(name=ifelse(name=="snap","SNAP",
                     ifelse(name=="housing_assistance", "Housing\nAssistance",
                            ifelse(name=="wic","WIC",
                                   ifelse(name=="free_lunch", "School Lunch",
                                          ifelse(name=="medicaid_chip", "Medicaid\nOr CHIP",
                                                 ifelse(name=="tanf","TANF",
                                                        ifelse(name=="at_least_one_benefit", "At Least\nOne Benefit",NA)))))))) %>% 
  mutate(senario=fct_relevel(senario, "Random\nPoor Nonfilers", after = 1)) %>% 
  rename(`Nonfiler\nImputation\nMethod`=senario) %>% 
  ggplot(aes(x = name, y = value, fill = `Nonfiler\nImputation\nMethod`)) +
    geom_bar(position="dodge", stat="identity", alpha = .5) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1L)) +
    labs(x = "\nBenefit Type", y = "Percentage\nReciving\nBenefit",
         caption = "Jain Family Institute", title = "Nonfiler Welfare Benefits\n") +
    theme_jfi()


#Want to repeat the process for the higher nonfiling number

################################################################################################
################################################################################################
################################################################################################
################################################################################################

#Need to change the nonfilers stuff here!!!!

#low benefits also poor
no_ctc_low_benefits_bo <- cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.))) %>% 
  arrange(total_num_benefits,rand_num) %>% #Randomizing order within benefit groups
  mutate(selection_number = row_number(),
         max_row_number = max(selection_number),
         selection_number=selection_number/max_row_number,
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=irs_missing_children/sample, 
         no_ctc_random_low_benefits_bo=ifelse(selection_number<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_random_low_benefits_bo==1) %>% 
  select(no_ctc_random_low_benefits_bo,id,total_num_benefits)

no_ctc_high_benefits_bo <- cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.))) %>% 
  arrange(desc(total_num_benefits,rand_num)) %>% #Randomizing order within benefit groups
  mutate(selection_number = row_number(),
         max_row_number = max(selection_number),
         selection_number=selection_number/max_row_number, #making row number 0-1
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=irs_missing_children/sample, 
         no_ctc_random_high_benefits_bo=ifelse(selection_number<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_random_high_benefits_bo==1) %>% 
  select(no_ctc_random_high_benefits_bo,id,total_num_benefits)

no_ctc_poor_only_bo <- cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.)),
         sample=sum(asec_ind_weight),
         new_frac_nonfilers=irs_missing_children/sample, 
         no_ctc_random_poor_only_bo=ifelse(rand_num<new_frac_nonfilers,1,0)) %>% 
  filter(no_ctc_random_poor_only_bo==1) %>% 
  select(no_ctc_random_poor_only_bo,id)


#Indicators for different scenarios taking away CTC receipt
#Want to turn off tidylog for this
cps <- cps %>% 
  left_join(no_ctc_poor_only_bo) %>% 
  left_join(no_ctc_low_benefits_bo) %>% 
  left_join(no_ctc_high_benefits_bo) %>% 
  mutate(across(no_ctc_random_poor_only_bo:no_ctc_random_high_benefits_bo,~ifelse(is.na(.x),0,.x))) #Need to do the variables here


cps <- cps %>% mutate(across(no_ctc_random_poor_only_bo:no_ctc_random_high_benefits_bo,
                             ~ifelse(.x==0,poor_ctc, #If we didn't declare you a nonfiler, we give you the poor_ctc designation
                                     #Otherwise, we say you are poor if ctc without 
                                     ifelse(.x==1 & spm_povthreshold>=spm_resources,1,0)), .names = "poor_{.col}")) 

#Add more stats here
#Could also try to do something like percentage change, p.p change
basic_bar_graph_bo <- cps %>% filter(a_age<18) %>% #Maybe should change order here
  summarise(`Status Quo\nPoverty`=weighted.mean(poor_no_ctc,spm_weight_r),
            `CTC Full Uptake\nPoverty`=weighted.mean(poor_ctc,spm_weight_r),
            `CTC Poor Nonfilers\nPoverty`=weighted.mean(poor_no_ctc_random_poor_only_bo,spm_weight_r),
            `CTC Poor Nonfilers\nLow Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_low_benefits_bo,spm_weight_r),
            `CTC Poor Nonfilers\nHigh Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_high_benefits_bo,spm_weight_r)) #This is clearly messed up, not sure how


basic_bar_graph_bo %>% pivot_longer(cols = everything()) %>% 
  mutate(row_num=row_number(),
         name=fct_reorder(name,row_num)) %>% #Getting info right order
  ggplot(aes(x = name, y = value)) +
  geom_col(fill = "#572dff") +
  geom_text(aes(x=name, y=value, label = percent(value), vjust=3.5), position = position_dodge(width=0.9),  family = "Unna") +
  labs(x = "", y = "Children\nIn\nPoverty", caption = "Jain Family Institute",
       title = "Percentage of Children in Poverty\nUnder Different Assumptions about Nonfilers\n") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L), limits = c(0,.15), breaks = seq(0,.15,.05)) +
  theme_jfi()

cps %>% filter(a_age<18) %>% #Maybe should change order here
  summarise(`Status Quo\nPoverty`=weighted.mean(poor_no_ctc,spm_weight_r),
            `CTC Full Uptake\nPoverty`=weighted.mean(poor_ctc,spm_weight_r),
            `CTC Poor Nonfilers\nPoverty`=weighted.mean(poor_no_ctc_random_poor_only_bo,spm_weight_r),
            `CTC Poor Nonfilers\nLow Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_low_benefits_bo,spm_weight_r),
            `CTC Poor Nonfilers\nHigh Benefits\nPoverty`=weighted.mean(poor_no_ctc_random_high_benefits_bo,spm_weight_r)) %>% 
  mutate(across(`CTC Full Uptake\nPoverty`:`CTC Poor Nonfilers\nHigh Benefits\nPoverty`,
                ~ (`Status Quo\nPoverty`-.x)/`Status Quo\nPoverty`)) %>% 
  select(`CTC Full Uptake\nPoverty`:`CTC Poor Nonfilers\nHigh Benefits\nPoverty`) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(row_num=row_number(),
         name=fct_reorder(name,row_num)) %>% #Getting info right order
  ggplot(aes(x = name, y = value)) +
  geom_col(fill = "#572dff") +
  geom_text(aes(x=name, y=value, label = percent(value), vjust=3.5), position = position_dodge(width=0.9),  family = "Unna") +
  labs(x = "", y = "Percentage\nReduction\nChild\nPoverty", caption = "Jain Family Institute",
       title = "Percentage Reduction in Child Poverty\nUnder Different Assumptions about Nonfilers\n") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L)) +
  theme_jfi()


no_ctc_poor_only_benes_bo <- program_status_func(no_ctc_random_poor_only_bo,"Random\nPoor Nonfilers") 
no_ctc_low_benefits_benes_bo <- program_status_func(no_ctc_random_low_benefits_bo,"Low Benefit\nPoor Nonfilers") 
no_ctc_high_benefits_benes_bo <- program_status_func(no_ctc_random_high_benefits_bo,"High Benefit\nPoor Nonfilers") 


no_ctc_poor_only_benes_bo %>% 
  bind_rows(no_ctc_low_benefits_benes_bo) %>% 
  bind_rows(no_ctc_high_benefits_benes_bo) %>% 
  mutate(name=ifelse(name=="snap","SNAP",
                     ifelse(name=="housing_assistance", "Housing\nAssistance",
                            ifelse(name=="wic","WIC",
                                   ifelse(name=="free_lunch", "School Lunch",
                                          ifelse(name=="medicaid_chip", "Medicaid\nOr CHIP",
                                                 ifelse(name=="tanf","TANF",
                                                        ifelse(name=="at_least_one_benefit", "At Least\nOne Benefit",NA)))))))) %>% 
  mutate(senario=fct_relevel(senario, "Random\nPoor Nonfilers", after = 1)) %>% 
  rename(`Nonfiler\nImputation\nMethod`=senario) %>% 
  ggplot(aes(x = name, y = value, fill = `Nonfiler\nImputation\nMethod`)) +
  geom_bar(position="dodge", stat="identity", alpha = .5) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L)) +
  labs(x = "\nBenefit Type", y = "Percentage\nReciving\nBenefit",
       caption = "Jain Family Institute", title = "Nonfiler Welfare Benefits\n") +
  theme_jfi()








cps %>% filter(a_age<18) %>% 
  summarise(out_of_poverty_ctc=weighted.mean(out_of_poverty_ctc,spm_weight))


cps %>% filter(a_age<18) %>% 
  summarise(across(c(out_of_poverty_ctc,out_of_poor_no_ctc_random:out_of_poor_no_ctc_no_earnings_only), 
                   ~weighted.mean(., w = spm_weight)))

#How did we get from taking away the CTC from 2.6 mil children to 8 mil kids no longer begin lifted out of poverty
#The issue, I think, is that I need to take away benefits at the filing unit level rather than the individual level
#

#Why does this yeild almost all the same numbers??
#Weird about the no earnings result, I guess they are in HHs with earnings?
cps %>% filter(a_age<18) %>% 
  summarise(across(c(out_of_poverty_ctc,out_of_poor_no_ctc_random:out_of_poor_no_ctc_no_earnings_only), 
                   ~sum(.x*asec_ind_weight)))




#How many people does this represent




cps %>% mutate(no_ctc_random_poor=ifelse(no_ctc_random==0,poor_ctc,
                                             ifelse(no_ctc_random==1 & spm_povthreshold>=spm_resources,1,0)),
               out_of_poverty_ctc_noncompliance=ifelse(poor_no_ctc==1 & no_ctc_random_poor==0,1,0))





cps %>% filter(a_age<18) %>% 
  summarise(out_of_poverty_ctc=weighted.mean(out_of_poverty_ctc,spm_weight),
            out_of_poverty_ctc_noncompliance=weighted.mean(out_of_poverty_ctc_noncompliance,spm_weight))




cps <- cps %>% mutate(poor_ctc=ifelse(spm_povthreshold>=spm_resources+change_aftertax_income,1,0), 
                      poor_no_ctc=ifelse(spm_povthreshold>=spm_resources,1,0),
                      out_of_poverty_ctc=ifelse(poor_no_ctc==1 & poor_ctc==0,1,0))




  summarise(num_not_getting_credit=sum(not_getting_credit*asec_ind_weight))




frac_nonfilers_8 <- frac_nonfilers*.8

cps %>% filter(child==1 & poor_no_ctc==1) %>% 
  mutate(rand_num = runif(nrow(.))) %>% 
  mutate(not_getting_credit=ifelse(rand_num<frac_nonfilers_8,1,0)) 


cps %>% filter(child==1) %>% 
  group_by(RECID) %>% 
  mutate(rand_num = runif(nrow(.))) %>% #How do I get this to be constistent (e.g. set seed)
  mutate(not_getting_credit=ifelse(rand_num<frac_nonfilers,1,0)) %>% 
  summarise(sum(num_not_getting_credit=not_getting_credit*asec_ind_weight))


#Lets say I said 80% children in poor HHs
#Specific numbers in specific bins are gonna matter a lot


cps %>% filter(child==1) %>% 
  sample_frac(.02) #Not the best apprach








cps %>%  
  summarise(total_pop=sum(asec_ind_weight)) #Pop 324 mil but the sampling frame isn't the entire pop
#Percentage of children who should be getting it (92.5%)


#Avg monthly payment among recepients... I think this is wrong compared to the IRS fig
#Because I am upweighting larger families, rather, than averaging over payments
#However, it's unclear how to do in the the tax reform file because that's not weighted at all
#Some observations in tax reform represent more people than other observations
cps %>% filter(child==1 & getting_ctc==1) %>% 
  summarise(avg_ctc_payment=weighted.mean(monthly_ctc_payment,asec_ind_weight))
cps <- cps %>% mutate(monthly_ctc_payment_person=monthly_ctc_payment/n_tax_id)
cps %>% filter(getting_ctc==1) %>% select(monthly_ctc_payment,monthly_ctc_payment_person,n_tax_id)

#Total estimated payments CPS 18B vs. 14.8B
cps %>% 
  summarise(total_ctc_payments=sum(monthly_ctc_payment_person*asec_ind_weight))





tax_reform <- tax_reform %>% mutate(getting_ctc=ifelse(total_ctc>0,1,0),
                      monthly_ctc_payment=total_ctc/12)
tax_reform %>% filter(getting_ctc==1) %>% summarise(avg_ctc_payment=mean(monthly_ctc_payment))
#423 in IRS


cps_child_ctc
#I belive this is correct
#IRS says 56 mil children, so a 
irs_ctc_child_num <- 56301000
irs_ctc_child_num
num_missing_children <- cps_child_ctc-irs_ctc_child_num
num_missing_children #17 Mil seems like a lot?



cps %>%
  summarise(out_of_poverty_ctc=weighted.mean(out_of_poverty_ctc,spm_weight),
            poverty_status_quo=weighted.mean(poor_no_ctc,spm_weight))

# This seems high status quo, and it's wrong because I'm not counting spm resouces, just aftertax income
cps %>% filter(a_age<18) %>% 
  summarise(poverty_status_quo=weighted.mean(poor_no_ctc,spm_weight),
            poverty_ctc=weighted.mean(poor_ctc,spm_weight),
            out_of_poverty_ctc=weighted.mean(out_of_poverty_ctc,spm_weight),
            ctc_bonus=weighted.mean(change_aftertax_income,spm_weight)) #The average child is in a family getting an extra $3355



cps %>% filter(a_age<18) %>% 

#With the weights, randomly taking away children receiving the CTC and matching a number is tricky
#Need to match 

#Need to basically get posttax income for both versions and compare
#Need to be able to link to cps-asec
  
  #Percentage of children getting credit  
  cps %>% filter(child==1) %>% 
  summarise(percentage_getting_ctc=weighted.mean(getting_ctc,asec_ind_weight))

#Making sure weird lone children aren't materally effecting anything
#still annyoing me
cps %>% filter(child==1 & age_head!=18) %>% 
  summarise(percentage_getting_ctc=weighted.mean(getting_ctc,asec_ind_weight))


#93% of tax units getting CTC if perfect compliance
#Why is that different from PPP, also need to address the fucked up tax units
#Need to look at income measure... Seems like this is where most of it is coming from
#Earnings from main job, other empoyers, buisness/self employment, and secondary souce, plus disability if age less than 56
# ern_val  + ws_val  + se_val  + frm_val + dis_val1

#The limits are 240 unmarried and 440 if married
#Need to get it aggregated over tax_id
cps %>% mutate(agi_over_400_taxcalc=ifelse(c00100>400000,1,0),
               agi_over_400_cps=ifelse(agi>400000,1,0)) %>% 
  summarise(across(agi_over_400_taxcalc:agi_over_400_cps,mean))

cps %>% select(agi,tax_id)


#Want to calculate change in refund, e.g. change in tax liability
#Number of children getting CTC
#
cps %>% filter(child==1) %>% 
  group_by(getting_ctc) %>% 
  summarise(num_children=sum(asec_ind_weight))

cps <- cps %>% mutate(num_one=1)
#Lets remove 2.6M children and see what happens

