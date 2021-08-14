
# ####Trying to replace spm_resouces tax conponent with tax calculator calculations
# cps %>% select(fedtax_ac,combined,iitax,RECID) #Total federal tax liability
# cps %>% group_by(tax_id) %>% #Difficult to know which to replace
#   #Need to get status qup taxn code, not the reform tax code
#   summarise(census_fed_tax=sum(fedtax_ac),
#             sim_fed_tax=mean(iitax), #Not actially
#             sim_fed_tax_2=mean(payrolltax)) %>% 
#   summarise(across(census_fed_tax:sim_fed_tax_2,mean))
# #Not sure about how to compare
# cps %>% select(fedtax_ac,tax_id) #Not distributed by tax
# 
# #Want to try to get things at the family level, can't figure it out
# # cps %>% filter(child==1) %>% 
# #   group_by(RECID) %>% 
# #   mutate()

#Want to figure out how to select filing units for 
# cps %>% filter(child==1) %>% 
#   group_by(RECID) %>% 
#   mutate(rand_num=cur_group_id(), #This isn't random, can fix later
#          sample=sum(asec_ind_weight),
#          new_frac_nonfilers=num_nonfilers/sample, #Is this still right? In expectation, I think it should be
#          no_ctc_random=ifelse(rand_num<new_frac_nonfilers,1,0)) %>% 
#   filter(no_ctc_random==1) %>% ungroup() %>% 
#   summarise(sum(asec_ind_weight)) #This isn't right

new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income) %>% filter(nu18>=1) #Ok obviously not working
new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income) %>% filter(nu18>=1 & ctc_new>1) #Ok obviously not working

#What is the difference between these units... that's the key in finding the error...
#I don't think writing out all the logic will do it
#Maybe look at the filing unit number and go from there
new_ctc$e00200

#Tax units getting some CTC
new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income,RECID,XTOT,e00200,age_head) %>% 
  filter(nu18>=1 & ctc_new>0)
#Tax units not getting any ctc
new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income,RECID,XTOT,e00200,age_head) %>% 
  filter(nu18>=1 & ctc_new==0)
#Basically either a head age 18 related issue or high income I think (c00100=agi)
new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income,RECID,XTOT,e00200,age_head) %>% 
  filter(nu18>=1 & ctc_new==0 & age_head!=18)

new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income,RECID,XTOT,e00200,age_head) %>% 
  filter(nu18==1 & ctc_new>1 & c00100==0 & e00200==0)

#All had ages under 18, so that must be the issue
#I think even if these are wrong, it's a small number
new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income,RECID,XTOT,e00200,age_head) %>% #Ok so this is problematic
  filter(nu18==1 & ctc_new==0 & c00100==0 & e00200==0)

new_ctc %>% select(ctc_new,c00100,nu18,nu06,aftertax_income,RECID,XTOT,e00200,age_head) %>% #Ok so this is problematic
  filter(nu18==1 & ctc_new==0 & c00100==0 & e00200==0 & age_head==18) 

no_new_ctc <- read_csv(here("tax-calculator-code","cps_2019_for_taxsim-21-#-ctc-repealed-#.csv"))
no_new_ctc %>% summarize(mean(ctc_new)) #Good
new_ctc <- new_ctc %>% select(RECID,aftertax_income,ctc_new) %>% 
  rename(aftertax_income_ctc=aftertax_income)
no_new_ctc <- no_new_ctc %>% select(RECID,aftertax_income) %>% 
  rename(aftertax_income_no_ctc=aftertax_income)


tax_reform <- new_ctc %>% left_join(no_new_ctc)
tax_reform <- tax_reform %>% mutate(change_aftertax_income=aftertax_income_ctc-aftertax_income_no_ctc)
