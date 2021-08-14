
#Creating the test file
#Maybe use this to find the cutoff points to "full" credit
library(tidyverse)
library(here)
df <- tibble(e00200=seq(from = 0, to = 500000, by = 1000))
children_under_18 <- tibble(nu18=seq(from = 1, to = 10, by = 1))
children_under_6 <-  tibble(nu06=seq(from = 0, to = 6, by = 1))
m_status <- tibble(MARS=c(2,4))
df <- expand_grid(df,children_under_18,children_under_6,m_status)
#Removing any rows where num children under 18 is less than children under 6
#Not getting just 1 kid under 6?
df %>% filter(nu18==1 & nu06==1)
df %>% filter(nu18==1 & nu06==0)
df <- df %>% mutate(bad_combo=ifelse(nu18<nu06,1,0)) %>% 
  filter(bad_combo!=1) %>% 
  select(-bad_combo)
df <- df %>% mutate(RECID=row_number(), n24=nu18, FLPDYR=2021, age_head=50,
                    e00200p=e00200, XTOT=ifelse(MARS==2,nu18+2,nu18+1)) #Not sure if I have exemptions right
write_csv(df, here("tax-calculator-code","test_ctc_file.csv"))

#Reading in results after running though tax calculator
#new_ctc <- read_csv(here("tax-calculator-code","test_ctc_file-21-#-ctc-refundability-only-#.csv"))
new_ctc <- read_csv(here("tax-calculator-code","test_ctc_file-21-#-ctc-new-current-law-#.csv"))
no_new_ctc <- read_csv(here("tax-calculator-code","test_ctc_file-21-#-ctc-repealed-#.csv"))


#Getting results setup for graphing
new_ctc <- new_ctc %>% 
  rename(aftertax_income_ctc=aftertax_income) %>% 
  mutate(total_ctc_new=c07220+ctc_new)
no_new_ctc <- no_new_ctc %>% select(RECID,aftertax_income,c07220,ctc_new) %>% 
  rename(aftertax_income_no_ctc=aftertax_income) %>% 
  mutate(total_ctc_old=c07220+ctc_new) %>% 
  select(-c07220,-ctc_new)

tax_reform <- new_ctc %>% left_join(no_new_ctc)
tax_reform <- tax_reform %>% mutate(change_aftertax_income=aftertax_income_ctc-aftertax_income_no_ctc)

tax_reform <- tax_reform %>% mutate(`Children\nUnder 18`=as.factor(nu18),
                                    `Children\nUnder 6`=as.factor(nu06),
                                    `Marital Status` = ifelse(MARS==2,"Married Filing Jointly",
                                                              ifelse(MARS==1,"Single",
                                                                     ifelse(MARS==4,"Head of Household",NA))))


# ggplot(tax_reform %>% filter(nu18<6 & nu06<=3 & c00100<200000 & age_head!=18 & MARS!=1 & nu18!=0),
#        aes(x = c00100, y = change_aftertax_income)) +
#   geom_point(aes(color = `Children\nUnder 18`)) +
#   facet_grid(cols = vars(`Marital Status`)) +
#   labs(y = "New\nCTC\nCredit", x = "Adjusted Gross Income") +
#   scale_y_continuous(labels=scales::dollar_format()) +
#   scale_x_continuous(labels=scales::dollar_format()) +
#   theme_jfi() 


#All types of kids, CTC expansion
ggplot(tax_reform %>% filter(nu18<6 & nu06<=3 & c00100<250000 & age_head!=18 & MARS!=1 & nu18!=0),
       aes(x = c00100, y = change_aftertax_income)) +
  geom_point(aes(color = `Children\nUnder 18`, shape = `Children\nUnder 6`)) +
  facet_grid(cols = vars(`Marital Status`)) +
  labs(y = "New\nCTC\nCredit", x = "Adjusted Gross Income") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 

#Total ctc under 18 only
ctc_totals_u18 <- ggplot(tax_reform %>% filter(nu18<6 & nu06==0 & c00100<500000 & age_head!=18 & MARS!=1 & nu18!=0),
       aes(x = c00100, y = total_ctc_new)) +
  geom_point(aes(color = `Children\nUnder 18`)) +
  facet_grid(cols = vars(`Marital Status`)) + #Want to filer and do this with cowplot instead and then manually add the labels
  labs(y = "Total CTC", x = "Adjusted Gross Income") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 




######Annotating total CTC

size_for_annotation <- 3.5
color_for_plots <- "#572dff"
ctc_totals_u18_married <- ggplot(tax_reform %>% 
                                   filter(nu18<6 & nu06==0 & c00100<500000 & 
                                            age_head!=18 & MARS!=1 & nu18!=0 & `Marital Status`=="Married Filing Jointly"),
       aes(x = c00100, y = total_ctc_new)) +
  geom_point(color = color_for_plots) + #Will adjust color
  labs(y = "Total\nCTC", x = "Adjusted Gross Income", caption = "Jain Family Institute",
       title = "Child Tax Credit For Married Parents\n") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 


ctc_totals_u18_married <- ctc_totals_u18_married + 
  annotate("label", label = "1 Child", x = 70000, y = 3000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "2 Children", x = 70000, y = 6000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3 Children", x = 70000, y = 9000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "4 Children", x = 70000, y = 12000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "5 Children", x = 70000, y = 15000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 
ctc_totals_u18_married

ctc_totals_u18_single <- ggplot(tax_reform %>% 
                                  filter(nu18<6 & nu06==0 & c00100<400000 & age_head!=18 & MARS!=1 & 
                                           nu18!=0 & `Marital Status`=="Head of Household" & total_ctc_new!=0),
                                aes(x = c00100, y = total_ctc_new)) +
  geom_point(color = color_for_plots) + #Will adjust color
  labs(y = "Total\nCTC", x = "Adjusted Gross Income", caption = "Jain Family Institute",
       title = "Child Tax Credit For Single Parents\n") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 
ctc_totals_u18_single


ctc_totals_u18_single <- ctc_totals_u18_single + 
  annotate("label", label = "1 Child", x = 60000, y = 3000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "2 Children", x = 60000, y = 6000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3 Children", x = 60000, y = 9000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "4 Children", x = 60000, y = 12000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "5 Children", x = 60000, y = 15000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 
ctc_totals_u18_single

library(patchwork)
ctc_totals_u18_single + ctc_totals_u18_married


######Annotating new CTC
ctc_new_u18_single <- ggplot(tax_reform %>% 
                                  filter(nu18<6 & nu06==0 & c00100<250000 & age_head!=18 & MARS!=1 & 
                                           nu18!=0 & `Marital Status`=="Head of Household" & change_aftertax_income!=0),
                                aes(x = c00100, y = change_aftertax_income)) +
  geom_point(color = color_for_plots) + #Will adjust color
  labs(y = "New\nCTC", x = "Adjusted Gross Income", caption = "Jain Family Institute",
       title = "New Expanded Child Tax Credit For Single Parents\n") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 
ctc_new_u18_single 

size_for_annotation <- 3
ctc_new_u18_single <- ctc_new_u18_single + 
  annotate("label", label = "1 Child", x = 75000, y = 1000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna")  +
  annotate("label", label = "2 Children", x = 75000, y = 2000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3 Children", x = 75000, y = 3000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "4 Children", x = 75000, y = 4000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "5 Children", x = 75000, y = 5000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 

ctc_new_u18_single
  
  
ctc_new_u18_married <- ggplot(tax_reform %>% 
                               filter(nu18<6 & nu06==0 & c00100<250000 & age_head!=18 & MARS!=1 & 
                                        nu18!=0 & `Marital Status`=="Married Filing Jointly" & change_aftertax_income!=0),
                             aes(x = c00100, y = change_aftertax_income)) +
  geom_point(color = color_for_plots) + #Will adjust color
  labs(y = "New\nCTC", x = "Adjusted Gross Income", caption = "Jain Family Institute",
       title = "New Expanded Child Tax Credit For Married Parents\n") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 
ctc_new_u18_married 

size_for_annotation <- 3
ctc_new_u18_married <- ctc_new_u18_married + 
  annotate("label", label = "1 Child", x = 110000, y = 1000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna")  +
  annotate("label", label = "2 Children", x = 110000, y = 2000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "3 Children", x = 110000, y = 3000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "4 Children", x = 110000, y = 4000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") +
  annotate("label", label = "5 Children", x = 110000, y = 5000, size = size_for_annotation, colour = color_for_plots,
           family= "Unna") 
ctc_new_u18_married

ctc_new_u18_single + ctc_new_u18_married

#Also want to do total CTC credit

  
#Should check if things are different for people with kids under 18
tax_reform <- tax_reform %>% 
  arrange(MARS,nu18,nu06,c00100) %>% 
  mutate(change_aftertax_income_change=change_aftertax_income-lag(change_aftertax_income)) 

below_max_credit <- tax_reform %>% 
  filter(c00100>21000 & change_aftertax_income_change==0) %>% 
  group_by(MARS,nu18,nu06) %>% #Why am I only getting one mars
  summarise(start_full_credit=min(c00100)) %>% 
  mutate(start_full_credit=start_full_credit-1000)

write_csv(below_max_credit,here("tax-calculator-code","full_credit_start.csv"))

#Now need to add number of children under 6

tax_reform %>% select(MARS,nu18,change_aftertax_income_change)


#Graph of result
ggplot(tax_reform,
       aes(x = c00100, y = change_aftertax_income)) +
  geom_point() +
  labs(y = "New\nCTC\nCredit", x = "Adjusted Gross Income") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_minimal()
tax_reform <- tax_reform %>% mutate(total_ctc=c07220+ctc_new)
ggplot(tax_reform,
       aes(x = c00100, y = total_ctc)) +
  geom_point() +
  labs(y = "New\nCTC\nCredit", x = "Adjusted Gross Income") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_minimal()


tax_reform %>% filter(c00100==120000 & nu18==3 & nu06==1) %>% select(total_ctc,MARS)
tax_reform %>% filter(c00100==20000 & nu18==1 & nu06==0) %>% select(total_ctc,MARS)


ggplot(tax_reform,
       aes(x = c00100, y = iitax)) +
  geom_point() +
  labs(y = "New\nCTC\nCredit", x = "Adjusted Gross Income") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 

ggplot(tax_reform,
       aes(x = c00100, y = payrolltax)) +
  geom_point() +
  labs(y = "New\nCTC\nCredit", x = "Adjusted Gross Income") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() 

