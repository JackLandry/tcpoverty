##Link CPS-ASEC individual and family files
#Restrict to key variables only

ff_cps <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2019/ffpub19.csv")
hh_cps <- read_csv("~/Dropbox/JFI/cps_data/raw_census/2019/hhpub19.csv")
hh_cps
# ff_cps
# #HRECORD
# H_IDNUM PERIDNUM.
# H_SEQ
# PF_SEQ
# PH_SEQ


cps %>% select(pf_seq,ph_seq) %>% tail()
names(hh_cps) <- tolower(names(hh_cps))
hh_cps %>% select(h_idnum,h_seq)
cps_test <- cps %>% rename(h_seq=ph_seq) %>% select(h_seq)
cps_test %>% left_join(hh_cps) #Some rows only in hh_cps, I think that's fine?


hh_cps  <- hh_cps %>% mutate(snap=ifelse(hfoodsp==1,1,0),
                  free_lunch=ifelse(hflunch==1,1,0),
                  housing_money=ifelse(hlorent==1,1,0),
                  public_housing=ifelse(hpublic==1,1,0),
                  housing_assistance=ifelse(housing_money==1 | public_housing==1,1,0),
                  wic=ifelse(hrwicyn==1,1,0),
                  medicaid_chip=ifelse(hmcaid==1 | hmcaid==2,1,0),
                  social_security=ifelse(hss_yn==1,1,0)) %>% 
  select(c(h_seq,snap:social_security))

