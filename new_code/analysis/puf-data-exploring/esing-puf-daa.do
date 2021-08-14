use "~/Dropbox/JFI/irs-soi-data/saez-zucman/14rptaxexpendcapwealthdata/pufonline2016.dta", clear

*THere is a paper summarizing this data bu it's not very clear... prey sure i doesn' handle nonfilers
*https://www.irs.gov/pub/irs-soi/18rpsynthindtaxfiles.pdftt

*150 mil tax filers, average income 68k
su agi [fw=dweght]
*How does his compare to pop

su eictot [fw=dweght]
gen num_people=1
replace num_people=num_people+1 if married==1
replace num_people=num_people+xded 
su num_people [fw=dweght] // ~300 million (mean ~2, n = ~150 mil)
*Unclear how the weighs were developed
