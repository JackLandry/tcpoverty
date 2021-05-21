


library(readr)
library(rio)
#Can't open this for whatever reason, and doesn't read in correctly like this
cps_ex_from_docs <- utils::read.table("/Users/jacklandry/Documents/GitHub/tcpoverty/tax-calc-info/cps_data/cps.csv.gz")
head(cps_ex_from_docs)
#These variables match the docs
#e00200,e00200p,e00200s,e00900,e00900p,e00900s,e02100,e02100p,e02100s,e00600,e01500,e00800,e02400,e02300,
#mcaid_ben,mcare_ben,ssi_ben,tanf_ben,vet_ben,wic_ben,snap_ben,housing_ben,age_head,age_spouse,blind_head,
#fips,h_seq,a_lineno,ffpos,s006,FLPDYR,EIC,DSI,MARS,XTOT,nu18,n1820,n21,nu06,nu13,n24,elderly_dependents,
#f2441,e18400,blind_spouse,e00650,e00300,e00400,e01700,e01100,e01400,e03300,e03270,e32800,e17500,e20400,
#e03240,e19200,e18500,other_ben,e19800,e20100,e03210,e03150,RECID,agi_bin,pencon_p,pencon_s