boost_act_single <-  tibble(eitc = c(3000,3000,0),
                            labor_income = c(0,30000,50000),
                            `Filing Status` = c("Single"))

boost_act_couple <-  tibble(eitc = c(6000,6000,0),
                            labor_income = c(0,80000,100000),
                            `Filing Status` = c("Married"))

combo_boost <- bind_rows(boost_act_single,boost_act_couple)
combo_boost
combo_boost <- combo_boost %>% mutate(total_earnings=eitc+labor_income)
combo_boost %>% mutate(tax_rate=(total_earnings-lag(total_earnings))/(labor_income-lag(labor_income)),
                       tax_rate=ifelse(labor_income==0,NA,tax_rate),
                       tax_rate=1-tax_rate) 


#Maybe add those old child labels
boost_plot <- ggplot(combo_boost, aes(x = labor_income, y = eitc, group =  `Filing Status`)) +
  geom_line(color = "#572dff") + #Need to control colors so the old is gray
  #scale_color_manual(values=c("#572dff","#572dff")) +
  theme_bw() + 
  labs(y = "BOOST  \nBenefit  ", x = "\nAdjusted  Gross  Income ", 
       title = "BOOST Act\nBenefits By Income",
       caption = "Jain Family Institute") +
  scale_y_continuous(limits = c(0,8000), labels=scales::dollar_format(), n.breaks = 5) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

#Adding labels
boost_plot <- boost_plot + 
  annotate("label", label = "Single", x = 15000, y = 4000, size = 3.5, colour = "#572dff",
           family= "Unna") +
  annotate("label", label = "Married", x = 15000, y = 7000, size = 3.5, colour = "#572dff",
           family= "Unna") 
boost_plot

ggplot(combo_boost, aes(x = labor_income, y = total_earnings, group =  `Filing Status`)) +
  geom_line(color = "#572dff") + #Need to control colors so the old is gray
  #scale_color_manual(values=c("#572dff","#572dff")) +
  theme_bw() + 
  labs(y = "BOOST  \nBenefit  ", x = "\nAdjusted  Gross  Income ", 
       title = "BOOST Act\nBenefits By Income",
       caption = "Jain Family Institute") +
  scale_y_continuous(labels=scales::dollar_format(), n.breaks = 5) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 
