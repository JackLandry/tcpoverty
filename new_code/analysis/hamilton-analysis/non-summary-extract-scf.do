
*SCF data non-summary extract to get HH info
use "/Users/jacklandry/Documents/GitHub/tcpoverty/scf-data/summary-extracts/full-data/p19i6.dta", clear




*Case ID: yy1
*Number of people in HH: x7001
*They have info on all 12 people in the HH

*some really ugly stata code
gen extra_adult_2=1 if x104>=18 & x104<=99 
gen extra_adult_3=1 if x7006==1 
gen extra_adult_4=1 if x7007==1
gen extra_adult_5=1 if x7008==1
gen extra_adult_6=1 if x7009==1
gen extra_adult_7=1 if x7010==1
gen extra_adult_8=1 if x7013==1
gen extra_adult_9=1 if x7013==1
gen extra_adult_10=1 if x7013==1
gen extra_adult_11=1 if x7014==1
gen extra_adult_12=1 if x7035==1
foreach num of numlist 2/12 {
recode extra_adult_`num' (.=0)
}
gen tot_extra_adults=extra_adult_2+extra_adult_3+extra_adult_4+extra_adult_5+ ///
extra_adult_6+extra_adult_7+extra_adult_8+extra_adult_9+extra_adult_10+extra_adult_11+ ///
extra_adult_12
tab tot_extra_adults

keep tot_extra_adults yy1 y1

save "/Users/jacklandry/Documents/GitHub/tcpoverty/inter_data/scf-num-adults.dta", replace

*Now try to add the new imputation
