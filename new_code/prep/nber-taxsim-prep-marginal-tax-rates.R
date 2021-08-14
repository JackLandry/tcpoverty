#Making data for marginal tax rates for NBER
#could try to do this for nber, which has state taxes in addition to federal

income <- seq(from = 0, to = 100000, by = 100)
state <- seq(from = 1, to = 51, by = 1)
married <- tibble(mstat = c(1,2))
children <- seq(from = 0, to = 6, by = 1)
marginal_tax_rate_diagram <- expand_grid(married,children,income,state)
marginal_tax_rate_diagram <- marginal_tax_rate_diagram %>% 
  mutate(depx=children, 
         dep13=children,
         dep17=children,
         dep18=children,
         taxsimid=row_number(),
         page=30,
         sage=30,
         year=2019) #No longer doing married seperate for people wiht kids

#Adding income
marginal_tax_rate_diagram <- marginal_tax_rate_diagram %>% mutate(pwages=ifelse(mstat!=2,income,income*.5), #Dividing income by 2 for married filing jointly
                                                                  swages=ifelse(mstat!=2,0,income*.5))
names(marginal_tax_rate_diagram)
#How about I try to get rent paid and property taxes empirically by looking at the CPS
marginal_tax_rate_diagram %>% select(-children,-income) %>% 
  select(taxsimid,year,state,mstat,page,sage,depx,dep13,dep17,dep18,pwages,swages)
#All oher fields 0 for now
marginal_tax_rate_diagram <- marginal_tax_rate_diagram %>% mutate(intrec=0,stcg=0,ltcg=0,otherprop=0,
                                     nonprop=0,pensions=0,gssi=0,
                                     ui=0,transfers=0,rentpaid=0,proptax=0,
                                     otheritem=0,childcare=0,mortgage=0,
                                     scorp=0,pbusinc=0,sbusinc=0,sprofinc=0)


marginal_tax_rate_diagram <- marginal_tax_rate_diagram %>% mutate(sage=ifelse(mstat!=2,0,sage)) 
write_csv(marginal_tax_rate_diagram, here("inter_data","marginal_tax_rate_nber_test_data.csv"))
# nrow(marginal_tax_rate_diagram)
# 
# 
# 
# 
# 
# 
# 
# test <- import("/Users/jacklandry/Documents/GitHub/tcpoverty/tax-calculator-code/c17_taxsim.csv")
# head(test) #These are the variables actually used
# test %>% count(MARS)


# MARS- Filing status [1=single, 2=joint, 3=separate, 4=household-head, 5=widow(er)]
#f2441 = number of children/dependent qualifying people
#n24 =  number of children qualifying tax credit eligible, must be less than 17
#EIC = number of EITC qualifying children max is 3
#XTOT = total number of exceptions for filing unit... need to look into this, but apparently they got rid of it
#with the Trump tax cutes...
#Maybe try to use the 
#e00200p = wage income after pension contributions for HH
#e00200s = wage income after pension contributions for spouse
##e00200p = wage income after pension contributions combined
# e00300 = taxable interest income
# e02400 = social securuty benefits
# e18400 = itemizable state/local taxes
