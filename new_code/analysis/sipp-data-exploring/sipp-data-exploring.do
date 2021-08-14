use "~/Dropbox/JFI/sipp_data/pu2018.dta", clear

*efiling ewillfile efstatus edepclm eeitc
*12 months for each person observations
*Multiple people in the HH multiple observations, differentiated by pnum
*HHs are ssuid


*Could potentially add the status flags for these

keep if monthcode==1
keep monthcode pnum ssuid rrel1 efiling ewillfile efstatus thincpov  tftotinc rrel* tage
export delimited "~/Dropbox/JFI/sipp_data/2018_extract.csv",  replace

tab edepclm if 

*One of the challenges of the SIPP is capturing data on individuals who were part of the household at some point in the reference year, but left the household before the interview. Data on these individuals, referred to as “Type 2” people, are important in order to understand how household composition and income change over the year.

*TFTOTINC
*TFTOTINC is all income
