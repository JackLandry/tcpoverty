

#Tax unit prep for tax calculator
cps_raw <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2020/pppub20.csv")

names(cps_raw) <- tolower(names(cps_raw))
#Just want tax id, age, spouse indicators
cps_raw %>% select(tax_id, a_age,A_ENRLW,A_EXPRRP,A_FAMREL,A_FAMTYP,
                   A_MARITL,PRCITSHP,AGI,DEP_STAT,FILESTAT,TAX_INC)