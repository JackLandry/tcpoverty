Input variables
===============

This section contains documentation of input variables in a format that is easy to search and print.
The input variables are ordered alphabetically by name.
There are no subsections, just a long list of input variables that Tax-Calculator is programmed to use in its calculations.
The Availability information indicates which input data files contain the variable.


##  `DSI`  
_Description_: 1 if claimed as dependent on another return; otherwise 0  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 6a  


##  `EIC`  
_Description_: number of EIC qualifying children (range: 0 to 3)  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch EIC  


##  `FLPDYR`  
_Description_: Calendar year for which taxes are calculated  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040  


##  `MARS`  
**_Required Input Variable_**  
_Description_: Filing (marital) status: line number of the checked box [1=single, 2=joint, 3=separate, 4=household-head, 5=widow(er)]  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 lines 1-5  


##  `MIDR`  
_Description_: 1 if separately filing spouse itemizes; otherwise 0  
_Datatype_: int  
_Availability_: taxdata_puf  
_IRS Form Location:_  
2013-2016: 1040 line 39b  


##  `RECID`  
**_Required Input Variable_**  
_Description_: Unique numeric identifier for filing unit; appears as RECID variable in tc CLI minimal output  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: private info  


##  `XTOT`  
_Description_: Total number of exemptions for filing unit  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 6d  


##  `age_head`  
_Description_: Age in years of taxpayer (i.e. primary adult)  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `age_spouse`  
_Description_: Age in years of spouse (i.e. secondary adult if present)  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `agi_bin`  
_Description_: Historical AGI category used in data extrapolation  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: not used in tax calculations  


##  `blind_head`  
_Description_: 1 if taxpayer is blind; otherwise 0  
_Datatype_: int  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 39a  


##  `blind_spouse`  
_Description_: 1 if spouse is blind; otherwise 0  
_Datatype_: int  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 39a  



##  `e00200`  
_Description_: Wages, salaries, and tips for filing unit net of pension contributions  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 7  


##  `e00200p`  
_Description_: Wages, salaries, and tips for taxpayer net of pension contributions (pencon_p)  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 7 component  


##  `e00200s`  
_Description_: Wages, salaries, and tips for spouse net of pension contributions (pencon_s)  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 7 component  


##  `pencon_p`  
_Description_: Contributions to defined-contribution pension plans for taxpayer  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: Imputed using IRS tabulations of Form W-2 sample  


##  `pencon_s`  
_Description_: Contributions to defined-contribution pension plans for spouse  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: Imputed using IRS tabulations of Form W-2 sample  


##  `e00300`  
_Description_: Taxable interest income  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 8a  


##  `e00400`  
_Description_: Tax-exempt interest income  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 8b  


##  `e00600`  
_Description_: Ordinary dividends included in AGI  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 9a  


##  `e00650`  
_Description_: Qualified dividends included in ordinary dividends  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 9b  


##  `e00800`  
_Description_: Alimony received  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 11  


##  `e00900`  
_Description_: Sch C business net profit/loss for filing unit  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 12  


##  `e00900p`  
_Description_: Sch C business net profit/loss for taxpayer  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 12 component  


##  `e00900s`  
_Description_: Sch C business net profit/loss for spouse  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 12 component  


##  `e01100`  
_Description_: Capital gain distributions not reported on Sch D  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 13  


##  `e01400`  
_Description_: Taxable IRA distributions  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 15b  


##  `e01500`  
_Description_: Total pensions and annuities  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 16a  


##  `e01700`  
_Description_: Taxable pensions and annuities  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 16b  



##  `e02100`  
_Description_: Farm net income/loss for filing unit from Sch F  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 18  


##  `e02100p`  
_Description_: Farm net income/loss for taxpayer  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 18 component  


##  `e02100s`  
_Description_: Farm net income/loss for spouse  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 18 component  


##  `e02300`  
_Description_: Unemployment insurance benefits  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 19  


##  `e02400`  
_Description_: Total social security (OASDI) benefits  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 20a  


##  `e03150`  
_Description_: Total deductible IRA contributions  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 32  


##  `e03210`  
_Description_: Student loan interest  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 33  


##  `e03240`  
_Description_: Domestic production activities from Form 8903  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 35  


##  `e03270`  
_Description_: Self-employed health insurance deduction  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 29  


##  `e03300`  
_Description_: Contributions to SEP, SIMPLE and qualified plans  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 line 28  


##  `e17500`  
_Description_: Itemizable medical and dental expenses.  WARNING: this variable is zero below the floor in PUF data.  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch A line 1  


##  `e18400`  
_Description_: Itemizable state and local income/sales taxes  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch A line 5  


##  `e18500`  
_Description_: Itemizable real-estate taxes paid  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch A line 6  


##  `e19200`  
_Description_: Itemizable interest paid  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch A line 15  


##  `e19800`  
_Description_: Itemizable charitable giving: cash/check contributions.  WARNING: this variable is already capped in PUF data.  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch A line 16  


##  `e20100`  
_Description_: Itemizable charitable giving: other than cash/check contributions.  WARNING: this variable is already capped in PUF data.  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch A line 17  


##  `e20400`  
_Description_: Itemizable miscellaneous deductions.  WARNING: this variable is zero below the floor in PUF data.  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 1040 Sch A line 24  


##  `e32800`  
_Description_: Child/dependent-care expenses for qualifying persons from Form 2441  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 2441 line 3  

##  `elderly_dependents`  
_Description_: number of dependents age 65+ in filing unit excluding taxpayer and spouse  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data; not used in tax law  


##  `f2441`  
_Description_: number of child/dependent-care qualifying persons  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: 2441 line 2b  


##  `a_lineno`  
_Description_: CPS line number for the person record of the head of the tax filing unit (not used in tax-calculation logic)  
_Datatype_: int  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2013-2016: sample construction info  


##  `ffpos`  
_Description_: CPS family identifier within household (not used in tax-calculation logic)  
_Datatype_: int  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2013-2016: sample construction info  


##  `fips`  
_Description_: FIPS state code (not used in tax-calculation logic)  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: sample construction info  


##  `h_seq`  
_Description_: CPS household sequence number (not used in tax-calculation logic)  
_Datatype_: int  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2013-2016: sample construction info  


##  `data_source`  
_Description_: 1 if unit is created primarily from IRS-SOI PUF data; 0 if created primarily from CPS data (not used in tax-calculation logic)  
_Datatype_: int  
_Availability_: taxdata_puf  
_IRS Form Location:_  
2013-2016: sample construction info  


##  `mcaid_ben`  
_Description_: Imputed Medicaid benefits expressed as the actuarial value of Medicaid health insurance  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  


##  `mcare_ben`  
_Description_: Imputed Medicare benefits expressed as the actuarial value of Medicare health insurance  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  


##  `n24`  
_Description_: Number of children who are Child-Tax-Credit eligible, one condition for which is being under age 17  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `nu06`  
_Description_: Number of dependents under 6 years old  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `nu13`  
_Description_: Number of dependents under 13 years old  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `nu18`  
_Description_: Number of people under 18 years old in the filing unit  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `n1820`  
_Description_: Number of people age 18-20 years old in the filing unit  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `n21`  
_Description_: Number of people 21 years old or older in the filing unit  
_Datatype_: int  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: imputed from CPS data  


##  `other_ben`  
_Description_: Non-imputed benefits  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: determined using government benefit program data  



##  `s006`  
_Description_: Filing unit sampling weight; appears as WEIGHT variable in tc CLI minimal output  
_Datatype_: float  
_Availability_: taxdata_puf, taxdata_cps  
_IRS Form Location:_  
2013-2016: not used in filing unit tax calculations  


##  `snap_ben`  
_Description_: Imputed SNAP benefits  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  


##  `housing_ben`  
_Description_: Imputed housing benefits  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  


##  `ssi_ben`  
_Description_: Imputed SSI benefits  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  


##  `tanf_ben`  
_Description_: Imputed TANF benefits  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  


##  `vet_ben`  
_Description_: Imputed Veteran's benefits  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  


##  `wic_ben`  
_Description_: Imputed WIC benefits  
_Datatype_: float  
_Availability_: taxdata_cps  
_IRS Form Location:_  
2014-20??: imputed using the C-TAM model  

