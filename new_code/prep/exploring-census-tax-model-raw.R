
#Want to show that the census tax model just assume nonfiling for people with very low AGI/labor earnings
#Want to reproduce some of the PPP analysis
cps_raw <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2020/pppub20.csv")
# cps_raw <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2020/asecpub20csv/pppub20.csv")
#Selecting relevent variables, need to add income variables
cps_raw <- cps_raw %>% select(TAX_ID,PERIDNUM,SPM_POVTHRESHOLD,SPM_RESOURCES,FILESTAT,WSAL_VAL,PTOTVAL,SPM_WEIGHT,SPM_ID,
                                 A_FNLWGT,A_AGE,AGI,TAX_INC,MARSUPWT) #Want to not keep the huge thing for momeory reasons
cps_select <- cps_raw
names(cps_select) <- tolower(names(cps_select))

cps_select <- cps_select %>% 
  mutate(nonfiler=ifelse(filestat==6,1,0),
         wage_earnings=ifelse(wsal_val==9999999,0,wsal_val),
         total_income=ifelse(ptotval==99999999,0,ptotval),
         filer=ifelse(filestat!=6,1,0),
         poor=ifelse(spm_resources<spm_povthreshold,1,0),
         child=ifelse(a_age<18,1,0),
         spm_weight_r=spm_weight/100,
         asec_ind_weight=marsupwt/100,
         agi_clean=ifelse(agi==9999999,0,agi),
         taxable_income_clean=ifelse(tax_inc==9999999,0,tax_inc)) 


income_filing_status <- cps_select %>% group_by(tax_id) %>% 
  mutate(tax_unit_filer=max(filer),
         nonfiler=ifelse(tax_unit_filer==0,1,0)) %>% 
  group_by(tax_id,nonfiler) %>% 
  summarise(across(c(wage_earnings:total_income,asec_ind_weight),~sum(.x))) %>% ungroup()

#So many people at 0 for nonfiling it doesn't work if you include that
income_filing_status %>% 
  filter(wage_earnings!=0 & wage_earnings<50000) %>% 
  mutate(nonfiler=ifelse(nonfiler==1,"Nonfiler","Filer")) %>% 
  ggplot(aes(x = wage_earnings, fill = nonfiler)) +
  geom_density()


#Need to make a quick bar graph with different bins, like 0, 1-250,250-500,500+
income_filing_status %>% 
  mutate(income_bin=ifelse(wage_earnings==0,"$0",
                           ifelse(wage_earnings>0 & wage_earnings<250,"$1-$250",
                                  ifelse(wage_earnings>=250 & wage_earnings<500,"$251-$500",
                                         ifelse(wage_earnings>=501, "$500+", NA))))) %>% 
  mutate(nonfiler=ifelse(nonfiler==1,"Nonfiler","Filer")) %>% 
  group_by(income_bin,nonfiler) %>% 
  summarise(n=sum(asec_ind_weight)) %>%
  mutate(freq = n / sum(n)) %>% 
  filter(!is.na(income_bin)) %>% 
  rename(`Filing Status`=nonfiler) %>% 
  ggplot(aes(x = income_bin, y = freq, fill = `Filing Status`)) +
    geom_bar(position="dodge", stat="identity", alpha = .5) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1L)) +
    labs(y = "Percentage in\nIncome Bin", x = "\nWage Earnings", title = "Wage Earnings\nBy Filing Status\n") +
    theme_jfi()
    
income_filing_status %>% 
  mutate(income_bin=ifelse(wage_earnings==0,"$0",
                           ifelse(wage_earnings>0,"$1+",NA))) %>% 
  mutate(nonfiler=ifelse(nonfiler==1,"Nonfiler","Filer")) %>% 
  group_by(income_bin,nonfiler) %>% 
  filter(!is.na(income_bin)) %>% 
  summarise(n=sum(asec_ind_weight)) %>%
  ungroup() %>% 
  group_by(nonfiler) %>% 
  mutate(freq = n / sum(n))  %>% 
  rename(`Filing Status`=nonfiler) %>% 
  ggplot(aes(x = income_bin, y = freq, fill = `Filing Status`)) +
  geom_bar(position="dodge", stat="identity", alpha = .5) +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1L)) +
  labs(y = "Percentage in\nIncome Bin\nBy Filing Status", x = "\nWage Earnings", title = "Wage Earnings\nBy Filing Status\n") +
  theme_jfi()




cps_select %>% select(tax_id,spm_id,a_age,filer)
#Want to know for each tax ID did someone file
#Also want to try to deconstruct how tax ids are created

#7 million children are in households that don't file taxes and are eligible for the credit
#I'm getting 6 mil from 2019 data, but maybe I could do it differently
#10 million children are in poverty, according to washington pot
tax_unit_filing_status <- cps_select %>% group_by(tax_id) %>% 
  summarise(tax_unit_filed=max(filer),
            any_child=max(child))
tax_unit_filing_status <- tax_unit_filing_status %>% mutate(tax_unit_nonfiler=ifelse(tax_unit_filed==0,1,0))

cps_select <- cps_select %>% left_join(tax_unit_filing_status)

#5.8 M nonfiler children in CPS-ASEC
cps_select %>% filter(child==1 & tax_unit_nonfiler==1) %>% summarise(sum(asec_ind_weight))
#41 M total nonfilers
cps_select %>% filter(tax_unit_nonfiler==1) %>% summarise(sum(asec_ind_weight))
#In SPM families, ofen some file and some don't
cps_spm_unit <- cps_select %>% group_by(spm_id,poor,spm_resources) %>% 
  summarise(total_agi=sum(agi),
            total_wage_earnings=sum(wage_earnings),
            spm_resources<spm_povthreshold,
            tax_filing_spm=mean(tax_unit_filed))
cps_spm_unit %>% group_by(poor) %>% 
  summarise(avg_tax_filing=mean(tax_filing_spm))

#Want to show avg income by filing unit or something
cps_select %>% group_by(tax_id,poor) %>% 
  summarise(tax_unit_filed=max(filer),
            total_agi=sum(agi),
            total_wage_earnings=sum(wage_earnings)) %>% 
  group_by(tax_unit_filed) %>% 
  summarise(avg_wage_earnings=mean(total_wage_earnings),
            frac_poor=mean(poor))

#This isn't even among nonfilers

filing_unit_wages <- cps_select %>% group_by(tax_id,poor) %>% 
  summarise(tax_unit_filed=max(filer),
            total_agi=sum(agi),
            total_wage_earnings=sum(wage_earnings)) 



#Don't understand what's going on with grouping here
cps_select %>% group_by(tax_id,poor) %>% 
  summarise(tax_unit_filed=max(filer),
            total_agi=sum(agi),
            total_wage_earnings=sum(wage_earnings))  %>% 
  ungroup() %>% 
  filter(tax_unit_filed==0) %>% 
  summarise(avg_wage_earnings=mean(total_wage_earnings))


#Cannot figure out shit, really frustrating
df <- filing_unit_wages %>% 
  filter(total_wage_earnings<10000 & tax_unit_filed==0) %>% ungroup()
ggplot(df, aes(x = total_wage_earnings)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  labs(x = "Total Wage Eanings", y = "", title = "Wage Earnings Among Nonfilers") +
  theme_jfi()






cps_select %>% group_by(tax_id,poor) %>% 
  summarise(tax_unit_filed=max(filer),
            total_agi=sum(agi),
            total_wage_earnings=sum(wage_earnings)) %>% 
  ungroup() %>% #Bassically everyone with
  filter(total_wage_earnings>0 & total_wage_earnings<5000) %>% #Everyone with no AGI didn't file, everyone with above 0 agi did file
  summarise(tax_unit_filed=mean(tax_unit_filed))

#Want to get frac filed by income groups, maybe groups of 10



cps_spm_unit %>% group_by(poor) %>% 
  summarise(avg_tax_filing=mean(tax_filing_spm))

#This is at the SPM unit level, not the tax unit level
#Probably a product of multiple tax units at the 
#SPM level and one poor/no earnings the other high, averaging their filing together
cps_spm_unit %>% filter(total_wage_earnings>50000) %>% ungroup() %>% #Don't understand why I have to ungroup
  summarise(avg_tax_filing=mean(tax_filing_spm))

#Want to get avg tax filing by income



cps_select %>% group_by(spm_id) %>% summarise(family_filed_taxes=mean(tax_unit_filed))
cps_select %>% filter(child==1) %>% 
  summarise(family_filed_taxes=mean(tax_unit_filed))
cps_select %>% filter(child==1) %>% 
  summarise(family_filed_taxes=sum(tax_unit_filed*spm_weight))
cps_select %>% filter(child==1) %>% 
  mutate(spm_weight_review=spm_weight/100) %>% 
  summarise(family_filed_taxes=sum(tax_unit_filed*spm_weight_review),
            family_nonfilers=sum(tax_unit_nonfiler*spm_weight_review))

cps_select %>% 
  mutate(spm_weight_review=spm_weight/100) %>% 
  summarise(family_filed_taxes=sum(tax_unit_filed*spm_weight_review),
            family_nonfilers=sum(tax_unit_nonfiler*spm_weight_review))

#This is reasonable close, maybe if I get 2019 data
cps_select %>% filter(child==1) %>% 
  mutate(spm_weight_review=spm_weight/100) %>% 
  summarise(num_poor_families=sum(poor*spm_weight_review))

#Here im getting 5.35 mil nonfilers
cps_select %>% filter(child==1) %>% 
  mutate(spm_weight_review=spm_weight/100) %>% 
  summarise(family_filed_taxes=sum(tax_unit_nonfiler*spm_weight_review))





#Among children
cps_select %>% filter(child==1) %>% 
  group_by(poor) %>% 
  summarise(mean(tax_unit_filed))
filing_status_by_tax_id <- cps_select %>% 
  group_by(tax_id,poor,spm_weight) %>% 
  summarise(anyone_file_taxes=max(filer),
            any_children=max(child),
            spm_resources=mean(spm_resources), #Mean because already summed overr spm unit
            total_agi=sum(agi_clean),
            total_taxable_inc=sum(taxable_income_clean),
            total_wage_earnings=sum(wage_earnings))


#Assume 
filing_status_by_tax_id %>% filter(anyone_file_taxes==0) %>% 
  ungroup() %>% 
  summarise(avg_agi=mean(total_agi), #Agi is assigned 0
            spm_resources=mean(spm_resources),  #spm resources are higher
            wage_earnings=mean(total_wage_earnings)) #Wage earnings are really low, how does spm resouces get so high

filing_status_by_tax_id %>% filter(anyone_file_taxes==1) %>% 
  ungroup() %>% 
  summarise(mean(total_agi, na.rm=T))

filing_status_by_tax_id %>% 
  filter(any_children==1 & poor==1) %>% 
  ungroup() %>% 
  summarise(filing_rate=weighted.mean(anyone_file_taxes,spm_weight)) #62% file by this measure