import delimited "~/Dropbox/JFI/cps_data/raw_census/2019/hhpub19.csv", varn(1) clear


su hchcare_val

keep h_idnum hrecord h_seq hhchcare_val 

*Maybe can merge with ph_seq

*Want to merge this in to model child care tax credit
