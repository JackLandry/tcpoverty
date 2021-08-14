library(rio)
scf_2019 <- import("/Users/jacklandry/Documents/GitHub/tcpoverty/scf-data/summary-extracts/scfp2019s.zip")
scf_num_adults <- import('/Users/jacklandry/Documents/GitHub/tcpoverty/inter_data/scf-num-adults.dta')
scf_2019 <- scf_2019 %>% as_tibble()
scf_2019 <- scf_2019 %>% left_join(scf_num_adults)
nrow(scf_2019)
names(scf_2019)
head(scf_2019) ##yy1 an

#lets do this in the cost estimate maybe?
scf_2019 <- scf_2019 %>% mutate(inc_mod=income-transfothinc,
                                full_elig=ifelse(income<10000 & married==2,1,0),
                                full_elig=ifelse(income<15000 & married==1,1,full_elig),
                                partial_elig=ifelse(income<50000 & married==2,1,0),
                                partial_elig=ifelse(income<70000 & married==1,1,partial_elig),
                                mod_full_elig=ifelse(inc_mod<10000 & married==2,1,0),
                                mod_full_elig=ifelse(inc_mod<15000 & married==1,1,mod_full_elig),
                                mod_partial_elig=ifelse(inc_mod<50000 & married==2,1,0),
                                mod_partial_elig=ifelse(inc_mod<70000 & married==1,1,mod_partial_elig))
scf_2019 %>% summarise(across(c(income,inc_mod,full_elig:mod_partial_elig), mean, na.rm=T))


