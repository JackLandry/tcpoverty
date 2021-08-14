

library(rio)
sipp_raw <- read_csv("~/Dropbox/JFI/sipp_data/2018_extract.csv")

sipp_clean <- sipp_raw %>% mutate(file_taxes=ifelse(efiling==1,1,0),
                                  file_or_plan_to_file=ifelse((efiling==1 | ewillfile==1),1,0),
                                  plan_to_file=ifelse(ewillfile==1,1,0),
                    hh_poor=ifelse(thincpov<1,1,0))

#Want to try and get the tax filing unit from relationship variables
#really tricky to resorect tax filing units, can I break income down at this level too? 
sipp_clean %>% filter(tage<10) %>% select(rrel1:rrel10)





sipp_clean <- sipp_clean %>% 
  mutate(poverty_threhold=ifelse(thincpov<.5,"Deep\nPoverty",
                                 ifelse(thincpov>=.5 & thincpov<1, "Poverty",
                                        ifelse(thincpov>=1 & thincpov<1.5, "100-150%\nPoverty",
                                               ifelse(thincpov>=1.5 & thincpov<2, "150-200%\nPoverty",
                                                      ifelse(thincpov>=2 & thincpov<3, "200-300%\nPoverty",
                                                             ifelse(thincpov>=3, "300%+\nPoverty",NA)))))))


tax_filing_hh_income_bins <- sipp_clean %>% 
  mutate(poverty_ratio_10_bins=ntile(thincpov,10),
         poverty_ratio_10_bins=as.factor(poverty_ratio_10_bins),
         poverty_threhold=as.factor(poverty_threhold),
         poverty_threhold=fct_reorder(poverty_threhold,thincpov)) %>% 
  group_by(poverty_threhold) %>% #can I get weights here
  summarise(tax_filing_rate=mean(file_taxes, na.rm=T),
            file_or_plan_to_rate=mean(file_or_plan_to_file, na.rm=T),
            plan_to_file_rate=mean(plan_to_file, na.rm=T)) #Want to get the CIs for this, maybe srvyr
sipp_clean %>% summarise(mean(file_taxes, na.rm=T))

ggplot(data = tax_filing_hh_income_bins %>% filter(!is.na(poverty_threhold)), 
       aes(x = poverty_threhold, y = file_or_plan_to_rate)) +
  geom_col(alpha = .5) +
  labs(y = "Tax\nFiling\nRate", x = "Poverty Status",
       title = "Self-Report Tax Filing Rate\nBy HH Income\n",
       caption = "Survey of Income and Program Participation, 2018") +
  scale_y_continuous(labels=scales::percent_format(),limits = c(0,1), breaks = seq(0,1,.20)) +
  theme_jfi() 


# 
# +
#   theme(axis.text.x = element_text(angle = 45, vjust = .5))
# sipp_raw %>% mutate()
# sipp_raw
# ggplot(sipp_raw, aes())