
library(rio)
taxsim_output <- import(here("outputs","webcalc_2.txt"))
head(taxsim_output)
# taxsim_output <- taxsim_output %>% filter(row_number()>1)
# #Need to change variables names to first row
# taxsim_output[1,]
# names(taxsim_output) <- taxsim_output[1,]
# taxsim_output <- taxsim_output %>% filter(row_number()>1)
# head(taxsim_output) #Now I need to send this to the tax calculator


write_csv(taxsim_output, here("outputs","a17_test.csv")) #Change to c to follow Ernie code
#Could call this with the tax calculator code, but threw errors


#Importing what was outputted from the NBER taxsim
test <- import(here("tax-calculator-code","test.tsv"))

write_csv(test, here("tax-calculator-code","c17_taxsim.csv"))

