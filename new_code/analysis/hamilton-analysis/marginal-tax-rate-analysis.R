

mt_data <- read_csv(here("tax-calculator-outputs","marginal_tax_rate_test_data-20-#-TCJA-#.csv"))

#Trying to figure out tax calculator, right now I'm adding the EITC but I don't think that's right, the EITC is already accounted for
#aftertax_income = expanded_income - combined
# combined = Sum of iitax and payrolltax and lumpsum_tax
# iitax = c09200 - refund
# refund = (eitc + c11070 + c10960 + CDCC_refund + recovery_rebate_credit + personal_refundable_credit + ctc_new + rptc + ctc_refund)
#  c09200: Income tax liabilities (including othertaxes) after non-refundable credits are used, but before refundable credits are applied
# lumpsum_tax: Lumpsum (or head) tax if LST == 0.0 or DSI == 1: lumpsum_tax = 0. else: lumpsum_tax = LST * max(num, XTOT)

#Why is this a thing
mt_data %>% filter(aftertax_income==e00200 & eitc>0) %>% select(refund,eitc,aftertax_income,e00200) 
#EITC is designed to exactly offset normal taxes, that's why it looks like it's not being added
mt_data %>% filter(aftertax_income==e00200 & eitc>0) %>% select(aftertax_income,e00200,expanded_income,combined)
# Eliminate variables that are always 0 to simplify things
mt_data <- mt_data %>% select(which(!colSums(mt_data, na.rm=TRUE) %in% 0))
names(mt_data)
#Does seem to calculate this part correctly
mt_data %>% select(aftertax_income,expanded_income,combined)
mt_data %>% filter(refund==0 & eitc>0) 
mt_data %>% filter(refund>0) %>% select(refund,c09200,eitc,aftertax_income,e00200) #Refunds aren't getting added to aftertax income it seems
# Refund: "Total refundable income tax credits"
mt_data %>% filter(refund>0 & eitc>0) %>% select(refund,eitc,aftertax_income,e00200) #But aftertax income is often larger than pretax income
mt_data %>% filter(refund!=eitc) %>% select(refund,eitc,aftertax_income,e00200)

###Adding Hamilton Program Money
#Making some variables necessary for the Hamilton stuff
mt_data <- mt_data %>% 
  mutate(single=ifelse(MARS!=2,1,0),
         spouse=ifelse(MARS==2,1,0),
         age=20,
         total_personal_income=e00200p,
         total_income_couple=e00200,
         num_eitc_elig_children_single=ifelse(single==1,n24,0),
         num_eitc_elig_children_two_parents=ifelse(MARS==2,n24,0))

#Hamilton money singles below phaseout start point
mt_data <- mt_data %>% mutate(hamilton_money=0)
mt_data <- mt_data %>% mutate(hamilton_money=ifelse(total_personal_income<=10000 & single==1 & age>=18,
                                              12500+(num_eitc_elig_children_single*4500),hamilton_money))


#Hamilton money singles above phaseout start point
mt_data <- mt_data %>% mutate(hamilton_benefit=0,
                        hamilton_phaseout_percentage=0,
                        hamilton_phaseout_amount=0,
                        hamilton_phaseout=0)

#Benefit = base + 4500*num_kis
mt_data <- mt_data %>% mutate(hamilton_benefit=ifelse(total_personal_income>10000 & total_personal_income<=50000 &
                                                  single==1 & age>=18,
                                                12500+4500*num_eitc_elig_children_single,hamilton_benefit))


#Phaseout = (total_income-phaseout_start)*phaseout_rate

mt_data <- mt_data %>% mutate(hamilton_phaseout_percentage=(12500+4500*num_eitc_elig_children_single)/-40000,
                        hamilton_phaseout_amount=(total_personal_income-10000)*hamilton_phaseout_percentage)

#Putting it together
mt_data <- mt_data %>% mutate(hamilton_money=ifelse(total_personal_income>10000 & total_personal_income<=50000 &
                                                single==1 & age>=18,
                                              hamilton_benefit+hamilton_phaseout_amount,hamilton_money))


#For spouses, divide the benefit in 2 and split between spouses
#Not doing that in the tax data which is at the filing unit level
#Spouses below phaseout start point
mt_data <- mt_data %>% mutate(hamilton_money=ifelse(total_income_couple<=15000 & spouse==1 & age>=18,
                                              (25000+(num_eitc_elig_children_two_parents*4500)),hamilton_money))
mt_data %>% filter(total_income_couple<15000 & 
                  spouse==1 & age>=18) %>% 
  select(hamilton_money,total_income_couple,num_eitc_elig_children_two_parents)



#Spouses above phaseout start point
#Benefit
mt_data <- mt_data %>% mutate(hamilton_benefit=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                  spouse==1 & age>=18,
                                                25000+4500*num_eitc_elig_children_two_parents,hamilton_benefit))


#Phaseout
mt_data <- mt_data %>% mutate(hamilton_phaseout=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                   spouse==1 & age>=18,
                                                 (total_income_couple-15000)*(25000+(4500*num_eitc_elig_children_two_parents))/55000,hamilton_phaseout))



#Checking                     
mt_data %>% filter(hamilton_benefit!=0 & total_income_couple>10000 & total_income_couple<=70000 & 
                  spouse==1 & age>=18) %>% 
  select(hamilton_benefit,hamilton_phaseout,total_income_couple)
#This should be 0, and in general phaseout is conditional on benefit size
mt_data %>% filter(hamilton_benefit!=0 & total_income_couple==70000 & 
                  spouse==1 & age>=18) %>% 
  select(hamilton_benefit,hamilton_phaseout,total_income_couple)


mt_data <- mt_data %>% mutate(hamilton_money=ifelse(total_income_couple>15000 & total_income_couple<=70000 & 
                                                spouse==1 & age>=18,
                                              (hamilton_benefit-hamilton_phaseout),hamilton_money))




mt_data <- mt_data %>% 
  mutate(aftertax_income_no_eitc=aftertax_income-eitc,
         tax_rate=(e00200-aftertax_income)/e00200,
         tax_rate_no_eitc=(e00200-aftertax_income_no_eitc)/e00200,
         aftertax_hamilton_eitc=aftertax_income+hamilton_money,
         aftertax_hamilton_no_eitc=aftertax_income_no_eitc+hamilton_money,
         tax_rate_hamilton=(e00200-(aftertax_income+hamilton_money))/e00200,
         tax_rate_hamilton_no_eitc=(e00200-(aftertax_income_no_eitc+hamilton_money))/e00200) 

# mt_data %>% 
#   select(e00200,aftertax_income,aftertax_income,hamilton_money,tax_rate_hamilton_no_eitc,tax_rate) %>% 
#   View()

#Calculating MTR over the phase out for singles
mt_data <- mt_data %>% mutate(income=e00200)
mt_singles_1 <- mt_data %>% filter(income==10000 & (MARS==1 | MARS==4)) %>% select(income,aftertax_hamilton_no_eitc,aftertax_hamilton_eitc,num_eitc_elig_children_single) %>% mutate(pre_post=0)
mt_singles_2 <- mt_data %>% filter(income==50000 & (MARS==1 | MARS==4)) %>% select(income,aftertax_hamilton_no_eitc,aftertax_hamilton_eitc,num_eitc_elig_children_single) %>% mutate(pre_post=1)
mt_singles_1 %>% bind_rows(mt_singles_2) %>% arrange(num_eitc_elig_children_single,pre_post) %>% 
  mutate(change_pretax_income=income-lag(income),
         change_aftertax_income_hamilton_no_eitc=aftertax_hamilton_no_eitc-lag(aftertax_hamilton_no_eitc),
         change_aftertax_income_hamilton_eitc=aftertax_hamilton_eitc-lag(aftertax_hamilton_eitc),
         marginal_tax_rate_hamilton_eitc=(change_pretax_income-change_aftertax_income_hamilton_eitc)/change_pretax_income,
         marginal_tax_rate_hamilton_no_eitc=(change_pretax_income-change_aftertax_income_hamilton_no_eitc)/change_pretax_income) %>% 
  filter(income==50000) %>% select(income,num_eitc_elig_children_two_parents,marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc)

#Same thing for couples
mt_couples_1 <- mt_data %>% filter(income==15000 & MARS==2) %>% select(income,aftertax_hamilton_no_eitc,aftertax_hamilton_eitc,num_eitc_elig_children_two_parents) %>% mutate(pre_post=0)
mt_couples_2 <- mt_data %>% filter(income==70000 & MARS==2) %>% select(income,aftertax_hamilton_no_eitc,aftertax_hamilton_eitc,num_eitc_elig_children_two_parents) %>% mutate(pre_post=1)

#Maybe add status quo MTR over that period, other stuff
mt_couples_1 %>% bind_rows(mt_couples_2) %>% arrange(num_eitc_elig_children_two_parents,pre_post) %>% 
  mutate(change_pretax_income=income-lag(income),
         change_aftertax_income_hamilton_no_eitc=aftertax_hamilton_no_eitc-lag(aftertax_hamilton_no_eitc),
         change_aftertax_income_hamilton_eitc=aftertax_hamilton_eitc-lag(aftertax_hamilton_eitc),
         marginal_tax_rate_hamilton_eitc=(change_pretax_income-change_aftertax_income_hamilton_eitc)/change_pretax_income,
         marginal_tax_rate_hamilton_no_eitc=(change_pretax_income-change_aftertax_income_hamilton_no_eitc)/change_pretax_income) %>% 
  filter(income==70000) %>% select(income,num_eitc_elig_children_two_parents,marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc)


mt_data <- mt_data %>% 
  mutate(change_aftertax_income=aftertax_income-lag(aftertax_income),
         change_aftertax_income_no_eitc=aftertax_income_no_eitc-lag(aftertax_income_no_eitc),
         change_aftertax_income_hamilton_eitc=aftertax_hamilton_eitc-lag(aftertax_hamilton_eitc),
         change_aftertax_income_hamilton_no_eitc=aftertax_hamilton_no_eitc-lag(aftertax_hamilton_no_eitc),
         change_pretax_income=e00200-lag(e00200),
         marginal_tax_rate=(change_pretax_income-change_aftertax_income)/change_pretax_income,
         marginal_tax_rate_no_eitc=(change_pretax_income-change_aftertax_income_no_eitc)/change_pretax_income,
         marginal_tax_rate_hamilton_eitc=(change_pretax_income-change_aftertax_income_hamilton_eitc)/change_pretax_income,
         marginal_tax_rate_hamilton_no_eitc=(change_pretax_income-change_aftertax_income_hamilton_no_eitc)/change_pretax_income)



mt_data %>% filter(single==1 & num_eitc_elig_children_single==0) %>% 
  select(e00200,aftertax_income,tax_rate,marginal_tax_rate)
#I think I need to reshape to plot multiple lines
#Want to do this with one variable I think
mt_data <- mt_data %>% mutate(couple_status=ifelse(single==1,"Single",
                                        ifelse(spouse==1,"Couple",NA)),
                   num_children=ifelse(num_eitc_elig_children_single==0,
                                       num_eitc_elig_children_two_parents,num_eitc_elig_children_single))

#For a given level of income, what is the tax stuff for different family types\
#Wait, isn't it better to do this goup r
mt_data_wide <- mt_data %>% select(e00200,couple_status,num_children,marginal_tax_rate,marginal_tax_rate_hamilton_eitc,marginal_tax_rate_hamilton_no_eitc) %>% 
  pivot_wider(id_cols = e00200,
              names_from = c(couple_status,num_children), 
              values_from = c(marginal_tax_rate,marginal_tax_rate_hamilton_eitc,marginal_tax_rate_hamilton_no_eitc))


mt_data <- mt_data %>% mutate(`Number of Children`=as.factor(num_children))

mt_data <- mt_data %>% mutate(num_children_clean=str_glue("{num_children} Children")) 
mt_data <- mt_data %>% mutate(num_children_clean=ifelse(num_children_clean=="1 Children","1 Child",num_children_clean)) 
write_rds(mt_data, here("inter_data","mt_data_hamilton_for_analysis.Rds"))

############################

mt_data <- read_rds(here("inter_data","mt_data_hamilton_for_analysis.Rds"))

linesize <- 1.25
line_trans <- .5

mt_data_long_mt_rates <- mt_data %>% 
  select(e00200,couple_status,num_children,num_children_clean,marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc,
         marginal_tax_rate) %>% 
  pivot_longer(cols = c(marginal_tax_rate_hamilton_no_eitc,marginal_tax_rate_hamilton_eitc,marginal_tax_rate),
               names_to = "tax_rate_type",
               values_to = "marginal_tax_rate")

mt_data_long_mt_rates

mt_data_long_mt_rates <- mt_data_long_mt_rates %>% 
  mutate(tax_rate_type=ifelse(tax_rate_type=="marginal_tax_rate", "Status Quo",
                              ifelse(tax_rate_type=="marginal_tax_rate_hamilton_no_eitc","Hamilton Reform\nEIC Repealed", 
                                     ifelse(tax_rate_type=="marginal_tax_rate_hamilton_eitc","Hamilton Reform\nKeep EIC",NA))))

# mt_data_long_mt_rates %>% filter(tax_rate_type=="Hamilton Reform\nKeep EIC" & e00200>40000 & e00200<60000 & num_children_clean=="5 Children") %>% 
#   select(marginal_tax_rate) %>% View()
ggplot(data= mt_data_long_mt_rates %>% filter(couple_status=="Single" & num_children!=6 & e00200!=0), 
                                     aes(x = e00200, y = marginal_tax_rate, group = tax_rate_type)) +
  geom_line(aes(color = tax_rate_type), size=linesize, alpha = line_trans) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate\nFor Additonal\n$100 Income",
       x = "Labor Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5, limits = c(-1,1.3), breaks = seq(-1,1,.5)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))


ggplot(data= mt_data_long_mt_rates %>% filter(couple_status=="Couple" & num_children!=6 & e00200!=0), 
                                     aes(x = e00200, y = marginal_tax_rate, group = tax_rate_type)) +
  geom_line(aes(color=tax_rate_type),size=linesize, alpha = line_trans) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate\nFor Additonal\n$100 Income",
       x = "Labor Income",
       title = "Marginal Tax Rates for Married Filers\n",
       caption = "Jain Family Institute") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 5, limits = c(-1,1.3), breaks = seq(-1,1,.5)) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))



###########################

#Marginal tax rate singles
ggplot(data= mt_data %>% filter(couple_status=="Single" & num_children!=6), 
       aes(x = e00200, y = marginal_tax_rate_hamilton_eitc)) +
  geom_line() +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 6) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

#this is good, need to add post tax income
mt_data_long <- mt_data %>% 
  select(e00200,couple_status,num_children,num_children_clean,marginal_tax_rate_hamilton_no_eitc,aftertax_income) %>% 
  pivot_longer(cols = c(marginal_tax_rate_hamilton_no_eitc, marginal_tax_rate),
               names_to = "tax_rate_type",
               values_to = "marginal_tax_rate")


mt_data_long_posttax_income <- mt_data %>% 
  select(e00200,couple_status,num_children,num_children_clean,aftertax_hamilton_no_eitc,aftertax_income) %>% 
  pivot_longer(cols = c(aftertax_hamilton_no_eitc,aftertax_income),
               names_to = "aftertax_income_type",
               values_to = "aftertax_income")


mt_data_long <- mt_data_long %>% 
  mutate(tax_rate_type=ifelse(aftertax_income_type=="aftertax_income", "Status Quo",
                              ifelse(tax_rate_type=="aftertax_hamilton_no_eitc",
                                     "Hamilton Reform", NA)))

ggplot(data= mt_data_long_posttax_income %>% filter(couple_status=="Single" & num_children!=6), 
       aes(x = e00200, y = aftertax_income, group = aftertax_income_type)) +
  geom_line(aes(color=aftertax_income_type)) +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income",
       title = "Marginal Tax Rates for Single Filers\n") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 6) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        legend.title = element_blank(),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

mt_data %>% select(e00200,num_children,marginal_tax_rate_hamilton_eitc)

ggplot(data = mt_data_wide %>% filter(couple_status=="Single" & num_children==3),
                                      aes(x = e00200))


# ggsave(here("new_code","analysis","hamilton-analysis","plots","marginal_tax_rate_hamilton_no_eitc_singles.png"),
#        width = 4, height = 2, units = "cm", dpi = 1000)

#Margins tax rate couples
ggplot(data= mt_data %>% filter(couple_status=="Couple" & num_children!=6), 
       aes(x = e00200, y = marginal_tax_rate_hamilton_no_eitc)) +
  geom_line() +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Marginal\nTax Rate\nFor Additonal\n$100 Income",
       x = "Income",
       title = "Marginal Tax Rates for Married Filers\n",
       caption = "Jain Family Institute") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 8) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

# #Maybe try to 
# ggplot(data= mt_data %>% filter(couple_status=="Couple" & num_children!=6), 
#        aes(x = e00200, y = tax_rate_hamilton_no_eitc)) +
#   geom_line() +
#   facet_wrap(vars(num_children_clean)) +
#   labs(y = "Average Tax Rate",
#        x = "Labor Income",
#        title = "Hamilton Benefits for Married Filers",
#        caption = "Jain Family Institute") +
#   theme_bw() +
#   scale_y_continuous(labels=scales::percent_format(), n.breaks = 8) +
#   scale_x_continuous(labels=scales::dollar_format()) +
#   theme(plot.title = element_text(hjust = 0.5),
#         text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
#         panel.grid.minor = element_blank(),
#         axis.title.y = element_text(angle=0, vjust = 0.5),
#         panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
#         plot.background = element_rect(fill="#fffdfc"),
#         plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

#Average tax rate singles
ggplot(data= mt_data %>% filter(couple_status=="Single" & num_children!=6 & e00200>=5000), 
       aes(x = e00200, y = tax_rate_hamilton_no_eitc, group = num_children_clean)) +
  geom_line(aes(color=num_children_clean)) +
  labs(y = "Average Tax Rate",
       x = "Labor Income",
       title = "Average Tax Rates for Single Filers\nHamilton Guaranteed Income Progam",
       caption = "Jain Family Institute") +
  scale_color_discrete(name = "Number of Children") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 8) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))
#Average tax rate coupes
ggplot(data= mt_data %>% filter(couple_status=="Couple" & num_children!=6 & e00200>=5000), 
       aes(x = e00200, y = tax_rate_hamilton_no_eitc, group = num_children_clean)) +
  geom_line(aes(color=num_children_clean)) +
  labs(y = "Average Tax Rate",
       x = "Labor Income",
       title = "Average Tax Rates for Married Filers\nHamilton Guaranteed Income Progam",
       caption = "Jain Family Institute") +
  scale_color_discrete(name = "Number of Children") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 8) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

mt_data <- mt_data %>% mutate(tax_rate=ifelse(e00200==0,0,tax_rate))
ggplot(data= mt_data %>% filter(couple_status=="Couple" & num_children!=6), 
       aes(x = e00200, y = tax_rate, group = num_children_clean)) +
  geom_line(aes(color=num_children_clean)) +
  labs(y = "Average Tax Rate",
       x = "Labor Income",
       title = "Average Tax Rates for Married Filers\nStatus-Quo Tax System",
       caption = "Jain Family Institute") +
  scale_color_discrete(name = "Number of Children") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 8) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

ggplot(data= mt_data %>% filter(couple_status=="Single" & num_children!=6), 
       aes(x = e00200, y = tax_rate, group = num_children_clean)) +
  geom_line(aes(color=num_children_clean)) +
  labs(y = "Average Tax Rate",
       x = "Labor Income",
       title = "Average Tax Rates for Single Filers\nStatus-Quo Tax System",
       caption = "Jain Family Institute") +
  scale_color_discrete(name = "Number of Children") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 8) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))


ggplot(data= mt_data %>% filter(couple_status=="Single" & num_children!=6 & e00200>=5000), 
                               aes(x = e00200, y = tax_rate_hamilton_no_eitc, group = num_children_clean)) +
  geom_line(aes(color=num_children_clean)) +
  labs(y = "Average Tax Rate",
       x = "Labor Income",
       title = "Average Tax Rates for Single Filers\nHamilton Guaranteed Income Progam\n",
       caption = "Jain Family Institute") +
  # scale_color_discrete(name = "Number of Children") +
  scale_color_brewer(palette = "Set1", name = "Number of Children") +
  scale_y_continuous(labels=scales::percent_format(), n.breaks = 8) +
  scale_x_continuous(labels=scales::dollar_format(seq(from = 5000, to = 100000, by = 10000)),
                     breaks=seq(from = 5000, to = 100000, by = 10000)) +
  theme_jfi() +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust=1),
        # axis.text.y = element_text(angle = 20, vjust = 1, hjust=1),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))


###Graph for Steve
ggplot(data= mt_data %>% filter(couple_status=="Couple" & num_children!=6), 
       aes(x = e00200, y = hamilton_money)) +
  geom_line() +
  facet_wrap(vars(num_children_clean)) +
  labs(y = "Hamilton\nBenefit",
       x = "Labor Income",
       title = "Hamilton Benefits for Married Filers",
       caption = "Jain Family Institute") +
  theme_bw() +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 8) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

mt_data %>% select(hamilton_money,e00200,couple_status,num_children,aftertax_income,marginal_tax_rate,marginal_tax_rate_hamilton_eitc) %>% View()

ggplot(data= mt_data %>% filter(single==1), 
       aes(x = e00200, y = marginal_tax_rate_hamilton_no_eitc, 
           group = num_eitc_elig_children_single, color = `Number of Children`)) +
  geom_line(aes(group=`Number of Children`)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

ggplot(data= mt_data %>% filter(spouse==1), 
       aes(x = e00200, y = marginal_tax_rate_hamilton_no_eitc, 
           group = num_eitc_elig_children_single, color = `Number of Children`)) +
  geom_line(aes(group=`Number of Children`)) +
  labs(y = "Marginal\nTax Rate",
       x = "Income") +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

ggplot(data= mt_data %>% filter(spouse==1 & num_eitc_elig_children_two_parents==0), 
       aes(x = e00200, y = marginal_tax_rate_hamilton_no_eitc)) +
  geom_line() +
  labs(y = "Marginal\nTax Rate",
       x = "Income")

ggplot(data= mt_data %>% filter(single==1 & num_eitc_elig_children_single==4), 
       aes(x = e00200, y = aftertax_income)) +
  geom_line() +
  labs(y = "Marginal\nTax Rate",
       x = "Income")

mt_data %>% filter(single==1 & num_eitc_elig_children_single==4) %>% 
  select(aftertax_income,e00200,change_pretax_income,change_aftertax_income,marginal_tax_rate) %>%  View()

mt_data %>% filter(single==1 & num_eitc_elig_children_single==0) %>% 
  select(e00200,aftertax_income,tax_rate)
#I think because I didn't fill in age maybe EITC went to 0




