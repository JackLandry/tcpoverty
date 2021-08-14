
#Trying to merge IPUMS data to raw census CPS-ASEC data to get tax_id in the data
library(ipumsr)
#
ddi <- read_ipums_ddi(here("cps_data","cps_00053.xml"))
ipum_raw <-  read_ipums_micro(ddi)
names(ipum_raw) <- tolower(names(ipum_raw))
ipum_raw %>% select(hseq,lineno)
ipum_raw %>% count(year) #why are so many not matching 
cps_raw <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2019/pppub19.csv")
cps_raw_small <- cps_raw %>% mutate(lineno=A_LINENO, serial=PH_SEQ, age_check=A_AGE, tax_id_census=TAX_ID) %>% 
  select(pernum,serial,age_check,tax_id_census)
write_csv(cps_raw_small, here("cps_data","csp_raw_2019.csv"))

cps_raw #PH_SEQ and PHF_SEQ
cps_raw %>% select(PH_SEQ,PHF_SEQ,PPPOS) %>% head() #First two rows are duplicates, can't work
cps_raw_small <- cps_raw %>% mutate(lineno=A_LINENO, serial=PH_SEQ, age_check=A_AGE, tax_id_census=TAX_ID) %>% 
  select(lineno,serial,age_check,tax_id_census)
#GOD DANM IT WTF why are these different
ipum_raw %>% left_join(cps_raw_small) %>% mutate(age_checking=age-age_check) %>% summarise(mean(age_checking, na.rm=T))

# HSEQ and LINENO, which correspond to the original variables PH_SEQ and A_LINENO
cps_raw <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2020/pppub20.csv")
cps_raw <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2020/pppub20.csv")

#This clearly isn't working, not sure how to get the data properly
library(rio)
asec_2018 <- import("~/Dropbox/JFI/cps_data/raw_census/2018/asec2018_pubuse.dat")
