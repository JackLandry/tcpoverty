* TCPOVERTY: STEP 4
* MERGE BACK POVERTY RESULTS 
* Ernie Tedeschi
* Last Updated: 21 March 2019
/*
set more off
clear all
pause on

* SETTINGS

local wd "~/Documents/data/cpsasec"								// Working directory
local asec asecmaster.dta										// CPS ASEC extract
local base "~/c17_taxsim-17-#-#-#.csv"							// Baseline tc CLI dump
local reform "~/c17_taxsim-17-#-reform-#.csv"					// Reform tc CLI dump
local ids "~/Tax-Calculator/taxcalc/validation/taxsim/ids.csv"	// ids.csv file created in Step 2a

*/
* END SETTINGS
******************************************************************************


* Merge ids, base, and reform files, and 
* create a single file with serial, pernum, 
* and the effects of the reform


*** = My comment
* = Ernie comment

cd "/Users/jacklandry/Documents/GitHub/tcpoverty"

import delimited "outputs/ids.csv", case(preserve) clear 

save "random/ids.dta",replace

import delimited "tax-calculator-code/c17_taxsim-17-#-#-#.csv", case(preserve) clear // need to fix
keep RECID aftertax_income
ren aftertax ati1
save "random/c17_base.dta", replace

import delimited "tax-calculator-outputs/earnie-reform.csv", case(preserve) clear 
merge 1:1 RECID using "random/c17_base.dta", nogen keep(master match)
merge 1:1 RECID using "random/ids.dta", nogen keep(master match)
gen dati = aftertax_income - ati1

keep serial pernum dati

save "random/dreform.dta", replace

* Merge the effects file into the CPS ASEC
***Need to get this in .dta form for this...
use year age serial pernum spmwt spmthresh spmtotres spmfamunit if year == 2018 using "random/cps_17.dta", clear

merge 1:1 serial pernum using "random/dreform.dta", nogen keep(master match)	

* Aggregate reform effects by SPM family unit

bysort spmfamunit: egen sum_dati = total(dati)

* Create baseline (spm1) and reform (spm2) SPM poverty indicators

gen spm1 = spmtotres < spmthresh
gen spm2 = (spmtotres + sum_dati) < spmthresh
gen out_of_spm = spm2 == 0 & spm1 == 1

* See SPM poverty results

tab spm1 [iw=spmwt]
tab spm2 [iw=spmwt]
tab out_of_spm [iw=spmwt]

tab spm1 if age < 18 [iw=spmwt]
tab spm2 if age < 18 [iw=spmwt]
tab out_of_spm if age < 18 [iw=spmwt]
