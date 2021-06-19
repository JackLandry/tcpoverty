

#Making EITC diagram
#Maybe try the tools here: https://github.com/andrewheiss/reconPlots

## Make a grpah of the EITC changes showing how the trapazoid chnages (Changes in benefits at different levels of income), pre vs. post reform


# // Eliminates Phase-In (This caused problems in the past)
# // Phase in rate for 0 kids, 1 kid, 2 kids, 3+ kids (Changed to 0 because EITC_basic_frac + EITC_rt must be less than 1), maybe that will fix error. But if I get an error try eliminates both of these
# // Phase out thresehold for 0 kids, 1 kid, 2 kids, 3+ kids
# // Additional phase out for married filing jointly (didn't change status quo)
# // Max EITC credit for 0 kids, 1 kid, 2 kids, 3+ kids
# // Phase out rate (unchanged)
# // Starting with this, we'll see if it works
#  // need to check for married phaseout
#  {
# "EITC_basic_frac": {"2017": 1},
# "EITC_rt": {"2017": [0.0,0.0,0.0,0.0]},
# "EITC_ps": {"2017": [30000.0, 30000.0, 30000.0, 30000.0]},
# "EITC_ps_MarriedJ": {"2017": [5590.0, 5590.0, 5590.0, 5590.0]},
# "EITC_c": {"2017": [5000.0, 10000.0, 15000.0, 20000.0]},
# "EITC_prt": {"2017": [0.0765, 0.1598, 0.2106, 0.2106]} #Seems like a very low phase out rate
#  }
#                                                     
 #New EITC with no phase in
                                                    
# eitc = max_benefit + married_indicator*additional_married_benefit - [(phase_out_threshold - labor income) * phase_out_rate]
# 0 kids
# etic = 5000 + married_indicator * 5590.0 + ((30000 - labor_income)*.0765)
# eitc = 10000.0 +  + ((30000 - labor_income)*0.1598)



eitc <- tibble(eitc = c(5000,5000,0,
                        10000,10000,0,
                        15000,15000,0,
                        20000,20000,0), 
               labor_income = c(0,30000,95359,
                                0,30000,92578.2,
                                0,30000,101025,
                                0,30000,124967), 
               group = c("0 Children","0 Children","0 Children",
                         "1 Child", "1 Child", "1 Child",
                         "2 Children", "2 Children", "2 Children",
                         "3+ Children","3+ Children","3+ Children"))

library(ggrepel)
eitc_plot <- ggplot(eitc, aes(x = labor_income, y = eitc, group = group)) +
  geom_line(color = "#572dff") +
  theme_bw() + 
  labs(y = "EITC  \nBenefit  ", x = "\nLabor Income", 
       title = "New EITC Benefit For Single Filers",
       caption = "Jain Family Institute") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels=scales::dollar_format()) +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="LM Roman 10"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 

eitc_plot <- eitc_plot + 
  annotate("label", label = "0 Children", x = 15000, y = 6000, size = 3.5, colour = "#572dff",
           family= "LM Roman 10") +
  annotate("label", label = "1 Child", x = 15000, y = 11000, size = 3.5, colour = "#572dff",
           family= "LM Roman 10") +
  annotate("label", label = "2 Children", x = 15000, y = 16000, size = 3.5, colour = "#572dff",
           family= "LM Roman 10") +
  annotate("label", label = "3+ Children", x = 15000, y = 21000, size = 3.5, colour = "#572dff",
           family= "LM Roman 10") 

#If I wanted to add the jain logo somehow
#https://stackoverflow.com/questions/9917049/inserting-an-image-to-ggplot2

#Maybe want to compare to old EITC



library(extrafont) 
font_import()

#Want to try to do this for all the different filers, maybe programatically



