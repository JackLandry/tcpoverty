
library(tidylog)
# ddi <- read_ipums_ddi(here("cps_data","cps_00042.xml"))
# ipum_raw <-  read_ipums_micro(ddi)
# names(ipum_raw) <- tolower(names(ipum_raw))

ipum <- ipum_raw %>% mutate(single=ifelse(sploc==0,1,0),
                            spouse=ifelse(sploc!=0,1,0))
#Need to identify parents of the children.....

#Within a given family, I want to know how many times a EITC eligable child's momloc matches pernum
#Want to break up the dataframes and match
etic_children <- ipum %>% filter(age<18) %>% select(momloc,poploc,spmfamunit) #momloc2,poploc2, need to add those to the data

#Getting the person number indicators for moms with no dads in HH the number of kids for that mom
#Subsequently merge back by person number, adding the number of kids in
#Need to adjust this for same-sex parents
eitc_children_moms <- etic_children %>% group_by(momloc,poploc,spmfamunit) %>% #Unique combo of family, mompernum and data pernum
  count(name="num_eitc_elig_children_one_parent_mom") %>% 
  filter(momloc!=0 & poploc==0) %>% ungroup() %>% 
  select(-poploc) %>% 
  rename(pernum=momloc) 

#Confirming sometimes there are multiple moms in one family unit and I'm still getting them
eitc_children_moms %>% count(spmfamunit) %>% arrange(desc(n))

#Vis versa, person number and number of kids for dads with kid(s) and no mom
eitc_children_dads <- etic_children %>% group_by(momloc,poploc,spmfamunit) %>% 
  count(name="num_eitc_elig_children_one_parent_dad") %>% #Need to make sure this includes non-biological children
  filter(poploc!=0 & momloc==0) %>% ungroup() %>% 
  select(-momloc) %>% 
  rename(pernum=poploc) 

#Getting mom and dads person number and number of kids fo two-parent families
eitc_children_2_parents <- etic_children %>% group_by(momloc,poploc,spmfamunit) %>% 
  count(name="num_eitc_elig_children_two_parents") %>% 
  filter(poploc!=0 & momloc!=0) %>% ungroup()
eitc_children_2_parents

#Trying to reshape this for joining
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
ipum %>% filter(spmfamunit==4397001) %>% select(pernum,momloc,poploc,relate)
ipum %>% filter(spmfamunit==24716001) %>% select(pernum,momloc,poploc,relate) #All these missings seem ok

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

#Currently basing everything based on wage income, might want to revise slightly in the future
#adjginc is adjusted gross income for tax purposes
ipum <- ipum %>% mutate(total_personal_income=ifelse(adjginc==99999999,NA,adjginc))
# ipum <- ipum %>% mutate(total_personal_income=ifelse(inctot==999999999,NA,inctot))

#Cleaning income so missing/weird alues don't mess with things
#Look at my other script to get at that stuff

#Definitly should include
#incwage+incbus+incfarm+
#Probably should include
#incss incretir incdivid and other investiment catagories
#Include social securty in income now
#


# #Changing this to total income, probably want a mix of counting some benefits and not others
# ipum <- ipum %>% mutate(total_personal_income=ifelse(inctot==999999999,NA,inctot)) #Might want to change to wage income
#Creating an indicator for no income
ipum <- ipum %>% mutate(no_income=ifelse(total_personal_income==0,1,0))
#Trying to see how many people have no income an thus are getting benefits
#This is among head of households, huge number....
#Maybe there's an NA problem? Not sure
ipum %>% filter(age>=18 & relate==101) %>% 
  summarise(frac_no_income=mean(no_income, na.rm=TRUE))

#Need to filter by two parents because can't be summing everyone up
#Not sure what I'm doing here??
combo_eitc_income_2_parents <- ipum %>% filter(!is.na(num_eitc_elig_children_two_parents)) %>% 
  group_by(spmfamunit) %>% 
  mutate(combo_eitc_income=sum(total_personal_income, na.rm=T)) %>% ungroup() %>% 
  select(spmfamunit,combo_eitc_income)


#One row per family unit.... I think this messes things up because there could be multiple sets of parents in one HH
#Simplifies merge because I merge on family unit ID
#If it wasn't unique within a strata, would it throw errors?
combo_eitc_income_2_parents <- combo_eitc_income_2_parents %>% unique()
#Checking first few cases are right
ipum %>% select(spmfamunit,total_personal_income,relate)
combo_eitc_income_2_parents

ipum <- ipum %>% left_join(combo_eitc_income_2_parents)
ipum %>% select(starts_with("num_eitc_elig"))

#Setting number of eligible children to 0 if NA from no match
ipum <- ipum %>% mutate(across(starts_with("num_eitc_elig"), replace_na,0))
#Combining moms and dads
ipum <- ipum %>% 
  mutate(eitc_children_inc=ifelse(num_eitc_elig_children_one_parent_mom+num_eitc_elig_children_one_parent_dad+num_eitc_elig_children_two_parents>=1,1,0))

#Need to write the functions for increasing income now....
#If no children, 12.5k base, start phase out at 10k, end phase out at 50k, 31.9% apparently.
#2 adults, 25k base phase out at 15k end phase out at 70k 46.4% phaseout apparently


#Need to use person number of spouse, because I need to get the total earning between spouses
#Make a separate dataframe with the earnings of spouse
spousal_income <- ipum %>% filter(sploc!=0) %>% select(spmfamunit,sploc,total_personal_income) %>% 
  rename(pernum=sploc,
         total_spousal_income=total_personal_income)
#Again need to look into duplicates here... 
spousal_income <- spousal_income %>% unique() #Why are there like 4 duplicates....

ipum <- ipum %>% left_join(spousal_income) #Very small number of duplicates can investigate later later
ipum <- ipum %>% mutate(total_income_couple=total_personal_income+total_spousal_income) #total_spousal_income should rename

ipum %>% 
  select(total_income_couple,total_personal_income,total_spousal_income,sploc,pernum,spmfamunit)

write_rds(ipum, here("inter_data","ipum_prepped.Rds"))

