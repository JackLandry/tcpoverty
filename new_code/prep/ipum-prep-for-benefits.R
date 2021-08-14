
library(tidylog)
ddi <- read_ipums_ddi(here("cps_data","cps_00046.xml"))
ipum_raw <-  read_ipums_micro(ddi)
names(ipum_raw) <- tolower(names(ipum_raw))

#The CPS ASEC data has information on every individual in a household
#Each individual in a household is identified by a person number, pernum
#Family units for supplemental poverty calculations are identified by spmfamunit
#Variables momloc and poploc identify the person number of a child's mom and dad in the household
#sploc is the person number of the spouse


ipum <- ipum_raw %>% mutate(single=ifelse(sploc==0,1,0),
                            spouse=ifelse(sploc!=0,1,0))
#Need to identify parents of the children for determining eligibility.....

#Within a given family, I want to know how many times a EITC eligible child's momloc matches pernum
#Want to break up the dataframes and match
etic_children <- ipum %>% filter(age<18) %>% select(momloc,poploc,momloc2,poploc2,spmfamunit) #momloc2,poploc2, need to add those to the data
etic_children %>% filter(momloc2!=0)
etic_children %>% filter(poploc2!=0)
#Getting the person number indicators for moms with no dads in HH the number of kids for that mom
#Subsequently merge back by person number, adding the number of kids in
#Need to adjust this for same-sex parents, but that should have a pretty negligible effect

eitc_children_moms <- etic_children %>% group_by(momloc,poploc,poploc2,momloc2,spmfamunit) %>% #Unique combo of family, mom person number and dad person number
  count(name="num_eitc_elig_children_one_parent_mom") %>% #Count number of instances for each unique combo
  filter(momloc!=0 & poploc==0 & momloc2==0) %>% ungroup() %>% 
  select(-poploc) %>% 
  rename(pernum=momloc) 
eitc_children_moms
#Confirming sometimes there are multiple moms in one family unit and I'm still getting them
eitc_children_moms %>% count(spmfamunit) %>% arrange(desc(n))

#Vis versa, person number and number of kids for dads with kid(s) and no mom
eitc_children_dads <- etic_children %>% group_by(momloc,poploc,poploc2,momloc2,spmfamunit) %>% 
  count(name="num_eitc_elig_children_one_parent_dad") %>% 
  filter(poploc!=0 & momloc==0 & poploc2==0) %>% ungroup() %>% 
  select(-momloc) %>% 
  rename(pernum=poploc) 

#Getting mom and dads person number and number of kids for two-parent families
#Think it's easest to do in seperate dataframes for gay couples
eitc_children_2_parents <- etic_children %>% group_by(momloc,poploc,spmfamunit) %>% 
  count(name="num_eitc_elig_children_two_parents") %>% 
  filter(poploc!=0 & momloc!=0 ) %>% ungroup()
eitc_children_2_parents

#Reshape this for joining
eitc_children_2_parents <- eitc_children_2_parents %>% 
  pivot_longer(cols = momloc:poploc, values_to = "pernum") %>% select(-name)
eitc_children_2_parents 


#Some moms are not in the master data but it's just a weird data thing, see below
ipum <- ipum %>% left_join(eitc_children_moms) #For these, calculate eitc based on individual income only

####Testing where the small number of unmatched are coming from... Seems ok...
eitc_children_moms_test <- eitc_children_moms %>% mutate(mom_data=1)
test_df <- ipum %>% full_join(eitc_children_moms_test)
test_df %>% mutate(mom_data_only=ifelse(!is.na(mom_data) & is.na(year),1,0)) %>% 
  filter(mom_data_only==1) %>% select(pernum,spmfamunit,num_eitc_elig_children_one_parent_mom)
#All these missings seem ok, there is a person number for a mom but they are not in the household
#They are under 18, so maybe should be clasified as a dependent, but it's a small number and ambiguous under Hamilton program rules
ipum %>% filter(spmfamunit==4397001) %>% select(pernum,momloc,poploc,relate,age)
ipum %>% filter(spmfamunit==24716001) %>% select(pernum,momloc,poploc,relate,age)  

ipum <- ipum %>% left_join(eitc_children_dads)
#Making missings 0 here then getting a combined variable for number of eligible, single parent
#more efficient way to do this would be an update values merge replacing NA
ipum <- ipum %>% 
  mutate(num_eitc_elig_children_one_parent_dad=
           ifelse(is.na(num_eitc_elig_children_one_parent_dad),0,num_eitc_elig_children_one_parent_dad),
         num_eitc_elig_children_one_parent_mom=
           ifelse(is.na(num_eitc_elig_children_one_parent_mom),0,num_eitc_elig_children_one_parent_mom),
         num_eitc_elig_children_single=num_eitc_elig_children_one_parent_dad+num_eitc_elig_children_one_parent_mom)


ipum <- ipum %>% left_join(eitc_children_2_parents) #For these, need to calculate based on mom+dad income

#adjginc is adjusted gross income for tax purposes, what Hamilton bases eligability on
ipum <- ipum %>% mutate(total_personal_income=ifelse(adjginc==99999999,NA,adjginc))
#Hamilton uses total income even though they know that's wrong
# ipum <- ipum %>% mutate(total_personal_income=ifelse(inctot==999999999,NA,inctot))


#Getting combined income for couples to determine eligibility
#Keeping rows where number of children for two parens is not missing --> only rows that matched to the two parent dataset
combo_eitc_income_2_parents <- ipum %>% filter(!is.na(num_eitc_elig_children_two_parents)) %>% 
  group_by(spmfamunit) %>% 
  mutate(combo_eitc_income=sum(total_personal_income, na.rm=T)) %>% ungroup() %>% 
  select(spmfamunit,combo_eitc_income)


#One row per family unit.... I think this messes things up because there could be multiple sets of parents in one HH
#Need to adjust this in the future, but minimal cases
#Simplifies merge because I merge on family unit ID
#Just 39 families, less than 1%
combo_eitc_income_2_parents %>% count(spmfamunit) %>% filter(n>2)

combo_eitc_income_2_parents <- combo_eitc_income_2_parents %>% unique()
#Checking first few cases are right
ipum %>% select(spmfamunit,total_personal_income,relate)
combo_eitc_income_2_parents

ipum <- ipum %>% left_join(combo_eitc_income_2_parents)
ipum %>% select(starts_with("num_eitc_elig"))

#Setting number of eligible children to 0 if NA from no match
ipum <- ipum %>% mutate(across(starts_with("num_eitc_elig"), replace_na,0))
#creating indicator for any children
ipum <- ipum %>% 
  mutate(eitc_children_inc=ifelse(num_eitc_elig_children_one_parent_mom+num_eitc_elig_children_one_parent_dad+num_eitc_elig_children_two_parents>=1,1,0))

#Need combined earnings for couples without children
#Need to use person number of spouse, because I need to get the total earning between spouses
#Make a separate dataframe with the earnings of spouse
spousal_income <- ipum %>% filter(sploc!=0) %>% select(spmfamunit,sploc,total_personal_income) %>% 
  rename(pernum=sploc,
         total_spousal_income=total_personal_income)


#Merging spousal income for each family
ipum <- ipum %>% left_join(spousal_income) 

ipum <- ipum %>% mutate(total_income_couple=total_personal_income+total_spousal_income) 

ipum %>% filter(age>18) %>% 
  select(total_income_couple,total_personal_income,total_spousal_income,sploc,pernum,spmfamunit)

write_rds(ipum, here("inter_data","ipum_prepped.Rds"))


library(tidylog)
ddi <- read_ipums_ddi(here("cps_data","cps_00042.xml"))
ipum_raw <-  read_ipums_micro(ddi)
names(ipum_raw) <- tolower(names(ipum_raw))

#The CPS ASEC data has information on every individual in a household
#Each individual in a household is identified by a person number, pernum
#Family units for supplemental poverty calculations are identified by spmfamunit
#Variables momloc and poploc identify the person number of a child's mom and dad in the household
#sploc is the person number of the spouse


ipum <- ipum_raw %>% mutate(single=ifelse(sploc==0,1,0),
                            spouse=ifelse(sploc!=0,1,0))
#Need to identify parents of the children for determining eligibility.....

#Within a given family, I want to know how many times a EITC eligible child's momloc matches pernum
#Want to break up the dataframes and match
etic_children <- ipum %>% filter(age<18) %>% select(momloc,poploc,spmfamunit) #momloc2,poploc2, need to add those to the data

#Getting the person number indicators for moms with no dads in HH the number of kids for that mom
#Subsequently merge back by person number, adding the number of kids in
#Need to adjust this for same-sex parents, but that should have a pretty negligible effect

eitc_children_moms <- etic_children %>% group_by(momloc,poploc,spmfamunit) %>% #Unique combo of family, mom person number and dad person number
  count(name="num_eitc_elig_children_one_parent_mom") %>% #Count number of instances for each unique combo
  filter(momloc!=0 & poploc==0) %>% ungroup() %>% 
  select(-poploc) %>% 
  rename(pernum=momloc) 

#Confirming sometimes there are multiple moms in one family unit and I'm still getting them
eitc_children_moms %>% count(spmfamunit) %>% arrange(desc(n))

#Vis versa, person number and number of kids for dads with kid(s) and no mom
eitc_children_dads <- etic_children %>% group_by(momloc,poploc,spmfamunit) %>% 
  count(name="num_eitc_elig_children_one_parent_dad") %>% 
  filter(poploc!=0 & momloc==0) %>% ungroup() %>% 
  select(-momloc) %>% 
  rename(pernum=poploc) 

#Getting mom and dads person number and number of kids for two-parent families
eitc_children_2_parents <- etic_children %>% group_by(momloc,poploc,spmfamunit) %>% 
  count(name="num_eitc_elig_children_two_parents") %>% 
  filter(poploc!=0 & momloc!=0) %>% ungroup()
eitc_children_2_parents

#Reshape this for joining
eitc_children_2_parents <- eitc_children_2_parents %>% 
  pivot_longer(cols = momloc:poploc, values_to = "pernum") %>% select(-name)
eitc_children_2_parents 


#Some moms are not in the master data but it's just a weird data thing, see below
ipum <- ipum %>% left_join(eitc_children_moms) #For these, calculate eitc based on individual income only

####Testing where the small number of unmatched are coming from... Seems ok...
eitc_children_moms_test <- eitc_children_moms %>% mutate(mom_data=1)
test_df <- ipum %>% full_join(eitc_children_moms_test)
test_df %>% mutate(mom_data_only=ifelse(!is.na(mom_data) & is.na(year),1,0)) %>% 
  filter(mom_data_only==1) %>% select(pernum,spmfamunit,num_eitc_elig_children_one_parent_mom)
#All these missings seem ok, there is a person number for a mom but they are not in the household
#They are under 18, so maybe should be clasified as a dependent, but it's a small number and ambiguous under Hamilton program rules
ipum %>% filter(spmfamunit==4397001) %>% select(pernum,momloc,poploc,relate,age)
ipum %>% filter(spmfamunit==24716001) %>% select(pernum,momloc,poploc,relate,age)  

ipum <- ipum %>% left_join(eitc_children_dads)
#Making missings 0 here then getting a combined variable for number of eligible, single parent
#more efficient way to do this would be an update values merge replacing NA
ipum <- ipum %>% 
  mutate(num_eitc_elig_children_one_parent_dad=
           ifelse(is.na(num_eitc_elig_children_one_parent_dad),0,num_eitc_elig_children_one_parent_dad),
         num_eitc_elig_children_one_parent_mom=
           ifelse(is.na(num_eitc_elig_children_one_parent_mom),0,num_eitc_elig_children_one_parent_mom),
         num_eitc_elig_children_single=num_eitc_elig_children_one_parent_dad+num_eitc_elig_children_one_parent_mom)


ipum <- ipum %>% left_join(eitc_children_2_parents) #For these, need to calculate based on mom+dad income

#adjginc is adjusted gross income for tax purposes, what Hamilton bases eligability on
ipum <- ipum %>% mutate(total_personal_income=ifelse(adjginc==99999999,NA,adjginc))
#Hamilton uses total income even though they know that's wrong
# ipum <- ipum %>% mutate(total_personal_income=ifelse(inctot==999999999,NA,inctot))


#Getting combined income for couples to determine eligibility
#Keeping rows where number of children for two parens is not missing --> only rows that matched to the two parent dataset
combo_eitc_income_2_parents <- ipum %>% filter(!is.na(num_eitc_elig_children_two_parents)) %>% 
  group_by(spmfamunit) %>% 
  mutate(combo_eitc_income=sum(total_personal_income, na.rm=T)) %>% ungroup() %>% 
  select(spmfamunit,combo_eitc_income)


#One row per family unit.... I think this messes things up because there could be multiple sets of parents in one HH
#Need to adjust this in the future, but minimal cases
#Simplifies merge because I merge on family unit ID
#Just 39 families, less than 1%
combo_eitc_income_2_parents %>% count(spmfamunit) %>% filter(n>2)

combo_eitc_income_2_parents <- combo_eitc_income_2_parents %>% unique()
#Checking first few cases are right
ipum %>% select(spmfamunit,total_personal_income,relate)
combo_eitc_income_2_parents

ipum <- ipum %>% left_join(combo_eitc_income_2_parents)
ipum %>% select(starts_with("num_eitc_elig"))

#Setting number of eligible children to 0 if NA from no match
ipum <- ipum %>% mutate(across(starts_with("num_eitc_elig"), replace_na,0))
#creating indicator for any children
ipum <- ipum %>% 
  mutate(eitc_children_inc=ifelse(num_eitc_elig_children_one_parent_mom+num_eitc_elig_children_one_parent_dad+num_eitc_elig_children_two_parents>=1,1,0))

#Need combined earnings for couples without children
#Need to use person number of spouse, because I need to get the total earning between spouses
#Make a separate dataframe with the earnings of spouse
spousal_income <- ipum %>% filter(sploc!=0) %>% select(spmfamunit,sploc,total_personal_income) %>% 
  rename(pernum=sploc,
         total_spousal_income=total_personal_income)


#Merging spousal income for each family
ipum <- ipum %>% left_join(spousal_income) 

ipum <- ipum %>% mutate(total_income_couple=total_personal_income+total_spousal_income) 

ipum %>% filter(age>18) %>% 
  select(total_income_couple,total_personal_income,total_spousal_income,sploc,pernum,spmfamunit)

ipum %>% filter(age>18 & total_personal_income>0 & total_spousal_income>0) %>% 
  select(total_income_couple,total_personal_income,total_spousal_income,sploc,pernum,spmfamunit)


write_rds(ipum, here("inter_data","ipum_prepped.Rds"))

