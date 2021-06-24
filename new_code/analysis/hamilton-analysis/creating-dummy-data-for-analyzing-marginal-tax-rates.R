#Making data for marginal tax rates

income <- seq(from = 0, to = 100000, by = 100)
married <- tibble(MARS = c(1,2))
children <- seq(from = 0, to = 6, by = 1)
marginal_tax_rate_diagram <- expand_grid(married,children,income)
marginal_tax_rate_diagram <- marginal_tax_rate_diagram %>% 
  mutate(EIC=ifelse(children>=3,3,children),
         MARS=ifelse(MARS==1 & children>=1,4,MARS),
         n24=children, 
         f2441=children,
         RECID=row_number(),
         FLPDYR=2020,
         age_head=30,
         age_spouse=30)

#Adding income
marginal_tax_rate_diagram <- marginal_tax_rate_diagram %>% mutate(e00200p=ifelse(MARS!=2,income,income*.5), #Dividing income by 2 for married filing jointly
                                     e00200s=ifelse(MARS!=2,0,income*.5),
                                     e00200=income)
#I think I can ignore XTOT if I use TJCA json

# marginal_tax_rate_diagram %>% mutate(sum_incomes=e00200p + e00200s,
#                                      incomes_not_equal=ifelse(sum_incomes!=e00200,1,0)) %>% 
#   filter(incomes_not_equal==1)

write_csv(marginal_tax_rate_diagram, "/Users/jacklandry/Documents/GitHub/tcpoverty/inter_data/marginal_tax_rate_test_data.csv")
nrow(marginal_tax_rate_diagram)







test <- import("/Users/jacklandry/Documents/GitHub/tcpoverty/tax-calculator-code/c17_taxsim.csv")
head(test) #These are the variables actually used
test %>% count(MARS)


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
