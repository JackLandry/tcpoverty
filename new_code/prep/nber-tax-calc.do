
cd "/Users/jacklandry/Documents/GitHub/tcpoverty/inter_data/"
import delimited "marginal_tax_rate_nber_test_data.csv", varn(1) clear


*Non-joint return with non-zero sage 
 
 taxsim32, f
 *dividends pprofinc are not found, and default to zero.

*import delimited "/Users/jacklandry/Documents/GitHub/tcpoverty/inter_data/results.raw", clear
*import delimited "/Users/jacklandry/Documents/GitHub/tcpoverty/inter_data/txpydata.raw", clear

use "taxsim_out.dta", clear
