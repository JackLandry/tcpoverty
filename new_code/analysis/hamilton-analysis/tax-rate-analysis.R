

hamilton_analysis_data <- read_rds(here("inter_data","hamilton_tax_merged_for_analysis.rds"))

hamilton_analysis_data %>% select(aftertax_income_hamilton,aftertax_income_no_hamilton,hamilton_money,adjginc)
#How to calculate implicit taxes, e.g. for change in x dollars adjusted gross income how much does aftertax income change
#want to compare pre-post hamilton
#might be good reason to not tax-sim this and make a simple exsample with just labor income
#adjusted gross income might not e the best because diff aftertax income might include different things
hamilton_analysis_data <- hamilton_analysis_data %>% 
  mutate(tax_rate_hamilton=(aftertax_income_hamilton-adjginc)/adjginc,
         tax_rate_no_hamilton=(aftertax_income_no_hamilton-adjginc)/adjginc,
         diff_tax_rate=tax_rate_hamilton-tax_rate_no_hamilton) 

#what about marginal tax rates... 



hamilton_analysis_data %>%   filter(single==1 & age>=18 & adjginc>=0) %>% 
  select(tax_rate_hamilton,tax_rate_no_hamilton,diff_tax_rate,aftertax_income_hamilton,aftertax_income_no_hamilton,hamilton_money,adjginc)
#Ho
#think i need to o this differently for couples
tax_rate_by_income <- hamilton_analysis_data %>% 
  filter(single==1 & age>=18 & adjginc>=0 & adjginc<=200000) %>%  #why is this starting at -5001
  mutate(income_by_1000=cut_width(adjginc, width = 1000, center = 500)) %>% 
  group_by(income_by_1000) %>% 
  summarise(across(tax_rate_hamilton:diff_tax_rate,  mean, na.rm=TRUE, .names = "mean_{.col}")) %>% #Lets do this with a lot of stuff
  mutate(row=row_number())
tax_rate_by_income #NEED TO CENVERT TO NUMBERS, so annoying


tax_rate_by_income <- tax_rate_by_income %>%
  mutate(income_by_1000=as.character(income_by_1000),
         income_by_1000=str_remove(income_by_1000,"\\("), #GOTTA BE a way to o this in one line
         income_by_1000=str_remove(income_by_1000,"\\["),
         income_by_1000=str_remove(income_by_1000,"\\]"),
         income_by_1000=str_replace_all(income_by_1000,"e\\+03","000"), #why is this not working
         income_by_1000=str_replace_all(income_by_1000,"e\\+04","0000"),
         income_by_1000=str_replace_all(income_by_1000,"e\\+05","00000"), #maybe remove everything after the comma
         income_by_1000=as.factor(income_by_1000),
         income_by_1000=fct_reorder(income_by_1000,row))


tax_rate_by_income
#how to plot... this is a decent start if i cleaned it up one way or another
ggplot(tax_rate_by_income %>% filter(row<50), aes(x = income_by_1000, y = mean_tax_rate_hamilton)) +
  geom_col() +
  theme_bw() +
  scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
  scale_x_discrete(breaks=tax_rate_by_income$income_by_1000[seq(1,length(tax_rate_by_income$income_by_1000),by=5)]) +
  labs(y = "Average Tax Rate", x = "Income Bracket",
       title = "Average Tax Rates by Income") +
  theme(plot.title = element_text(hjust = 0.5),
        text=element_text(family="Unna"), #Want to change this to Jain font, need to ask for it
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle=0, vjust = 0.5),
        panel.background = element_rect(fill="#fffdfc"), #Not sure if this is actually modifying background
        plot.background = element_rect(fill="#fffdfc"),
        plot.margin=unit(c(.15,.75,.15,.15),"cm")) 




tax_rate_by_income %>% filter(single==1 & adjginc<100000)


ggplot(hamilton_analysis_data %>% filter(single==1 & adjginc<100000 & aftertax_income_hamilton<200000 & age>=18),
       aes(x = adjginc, y = aftertax_income_hamilton)) +
  geom_point(alpha=.1) +
  geom_smooth(method='loess', formula= y~x, color = 'red', se = FALSE) +
  geom_smooth(method='lm', formula= y~x, color = 'blue', se = FALSE)

#just doing people who are single with two kids, more tractable
#the slope of this line - 1 is the effective marginal tax rate....
#in other words, the derivative 
ggplot(hamilton_analysis_data %>% filter(single==1 & adjginc<100000 & aftertax_income_hamilton<200000 & age>=18 &
                                           num_eitc_elig_children_single==2),
       aes(x = adjginc, y = aftertax_income_hamilton)) +
  geom_point(alpha=.1) +
  geom_smooth(method='loess', formula= y~x, color = 'red', se = FALSE)

#estimating the lossess function, then calculating the deriviative to get effective marginal tax rate
test <- loess(aftertax_income_hamilton ~ adjginc, data = hamilton_analysis_data %>% filter(single==1 & adjginc<100000 & aftertax_income_hamilton<200000 & age>=18 &
                                                                                             num_eitc_elig_children_single==2))

x <- 1:100000
px <- predict(test, newdata=x)
px
px1 <- diff(px)
x <- x[-1]

marginal_tax_rate <- bind_cols(x,px1)
marginal_tax_rate <- marginal_tax_rate %>% rename(income=...1,
                                                  tax_rate=...2)

predict_seq <- seq(1,100000, by = 100)
predictions <- predict(test, newdata=predict_seq)
change_in_predictions <- diff(predictions)
predictions <- predictions[-1]

#this seems to not work... unclear as to why....
marginal_tax_rate <- bind_cols(predictions,change_in_predictions)
marginal_tax_rate <- marginal_tax_rate %>% rename(income=...1,
                                                  tax_rate=...2)

#plotting... what is going on at the tail, could that be a sample issue??
ggplot(marginal_tax_rate, aes(x = income, y = 1-tax_rate)) +
  geom_line() +
  theme_bw() +
  labs(y = 'Marginal tax rate',
       x = 'Adjusted Gross Income') +
  scale_y_continuous(labels=scales::percent_format(accuracy=1), limits = n.breaks = 12) 
  scale_x_continuous(labels=scales::dollar_format())