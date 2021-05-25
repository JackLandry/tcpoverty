
### Earned Income Tax Credit

####  `EITC_c`  
_Description:_ This is the maximum amount of earned income credit taxpayers are eligible for; it depends on how many kids they have.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ True _Is Inflation Indexed:_ True  
_Value Type:_ float  
_Known Values:_  
 for: [0kids, 1kid, 2kids, 3+kids]  
2013: [487.0, 3250.0, 5372.0, 6044.0]  
2014: [496.0, 3305.0, 5460.0, 6143.0]  
2015: [503.0, 3359.0, 5548.0, 6242.0]  
2016: [506.0, 3373.0, 5572.0, 6269.0]  
2017: [510.0, 3400.0, 5616.0, 6318.0]  
2018: [519.0, 3461.0, 5716.0, 6431.0]  
2019: [529.0, 3526.0, 5828.0, 6557.0]  
_Valid Range:_ min = 0 and max = 9e+99  
_Out-of-Range Action:_ error  


####  `EITC_rt`  
_Description:_ Pre-phaseout credit is minimum of this rate times earnings and the maximum earned income credit.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ float  
_Known Values:_  
 for: [0kids, 1kid, 2kids, 3+kids]  
2013: [0.0765, 0.34, 0.4, 0.45]  
2014: [0.0765, 0.34, 0.4, 0.45]  
2015: [0.0765, 0.34, 0.4, 0.45]  
2016: [0.0765, 0.34, 0.4, 0.45]  
2017: [0.0765, 0.34, 0.4, 0.45]  
2018: [0.0765, 0.34, 0.4, 0.45]  
2019: [0.0765, 0.34, 0.4, 0.45]  
_Valid Range:_ min = 0 and max = 9e+99  
_Out-of-Range Action:_ error  


####  `EITC_basic_frac`  
_Description:_ This fraction of EITC_c is always paid as a credit and one minus this fraction is applied to the phasein rate, EITC_rt.  This fraction is zero under current law.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ float  
_Known Values:_  
2013: 0.0  
2014: 0.0  
2015: 0.0  
2016: 0.0  
2017: 0.0  
2018: 0.0  
2019: 0.0  
_Valid Range:_ min = 0.0 and max = 1.0  
_Out-of-Range Action:_ error  


####  `EITC_prt`  
_Description:_ Earned income credit begins to decrease at the this rate when AGI is higher than earned income credit phaseout start AGI.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ float  
_Known Values:_  
 for: [0kids, 1kid, 2kids, 3+kids]  
2013: [0.0765, 0.1598, 0.2106, 0.2106]  
2014: [0.0765, 0.1598, 0.2106, 0.2106]  
2015: [0.0765, 0.1598, 0.2106, 0.2106]  
2016: [0.0765, 0.1598, 0.2106, 0.2106]  
2017: [0.0765, 0.1598, 0.2106, 0.2106]  
2018: [0.0765, 0.1598, 0.2106, 0.2106]  
2019: [0.0765, 0.1598, 0.2106, 0.2106]  
_Valid Range:_ min = 0 and max = 9e+99  
_Out-of-Range Action:_ error  


####  `EITC_ps`  
_Description:_ If AGI is higher than this threshold, the amount of EITC will start to decrease at the phaseout rate.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ True _Is Inflation Indexed:_ True  
_Value Type:_ float  
_Known Values:_  
 for: [0kids, 1kid, 2kids, 3+kids]  
2013: [7970.0, 17530.0, 17530.0, 17530.0]  
2014: [8110.0, 17830.0, 17830.0, 17830.0]  
2015: [8250.0, 18150.0, 18150.0, 18150.0]  
2016: [8270.0, 18190.0, 18190.0, 18190.0]  
2017: [8340.0, 18340.0, 18340.0, 18340.0]  
2018: [8490.0, 18660.0, 18660.0, 18660.0]  
2019: [8650.0, 19030.0, 19030.0, 19030.0]  
_Valid Range:_ min = 0 and max = 9e+99  
_Out-of-Range Action:_ error  


####  `EITC_ps_MarriedJ`  
_Description:_ This is the additional amount added on the regular phaseout start amount for taxpayers with filling status of married filing jointly.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ True _Is Inflation Indexed:_ True  
_Value Type:_ float  
_Known Values:_  
 for: [0kids, 1kid, 2kids, 3+kids]  
2013: [5340.0, 5340.0, 5340.0, 5340.0]  
2014: [5430.0, 5430.0, 5430.0, 5430.0]  
2015: [5500.0, 5500.0, 5500.0, 5500.0]  
2016: [5550.0, 5550.0, 5550.0, 5550.0]  
2017: [5590.0, 5590.0, 5590.0, 5590.0]  
2018: [5680.0, 5690.0, 5690.0, 5690.0]  
2019: [5800.0, 5790.0, 5790.0, 5790.0]  
_Valid Range:_ min = 0 and max = 9e+99  
_Out-of-Range Action:_ error  


####  `EITC_MinEligAge`  
_Description:_ For a childless filing unit, at least one individual's age needs to be no less than this age (but no greater than the EITC_MaxEligAge) in order to be eligible for an earned income tax credit.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ int  
_Known Values:_  
2013: 25  
2014: 25  
2015: 25  
2016: 25  
2017: 25  
2018: 25  
2019: 25  
_Valid Range:_ min = 0 and max = 125  
_Out-of-Range Action:_ error  


####  `EITC_MaxEligAge`  
_Description:_ For a childless filing unit, at least one individual's age needs to be no greater than this age (but no less than the EITC_MinEligAge) in order to be eligible for an earned income tax credit.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ int  
_Known Values:_  
2013: 64  
2014: 64  
2015: 64  
2016: 64  
2017: 64  
2018: 64  
2019: 64  
_Valid Range:_ min = 0 and max = 125  
_Out-of-Range Action:_ error  


####  `EITC_InvestIncome_c`  
_Description:_ The EITC amount is reduced when investment income exceeds this ceiling.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ True _Is Inflation Indexed:_ True  
_Value Type:_ float  
_Known Values:_  
2013: 3300.0  
2014: 3350.0  
2015: 3400.0  
2016: 3400.0  
2017: 3450.0  
2018: 3500.0  
2019: 3600.0  
_Valid Range:_ min = 0 and max = 9e+99  
_Out-of-Range Action:_ error  


####  `EITC_excess_InvestIncome_rt`  
_Description:_ The EITC amount is reduced at this rate per dollar of investment income exceeding the ceiling.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ float  
_Known Values:_  
2013: 9e+99  
2014: 9e+99  
2015: 9e+99  
2016: 9e+99  
2017: 9e+99  
2018: 9e+99  
2019: 9e+99  
_Valid Range:_ min = 0 and max = 9e+99  
_Out-of-Range Action:_ error  


####  `EITC_indiv`  
_Description:_ Current-law value is false implying EITC is filing-unit based; a value of true implies EITC is computed for each individual wage earner.  The additional phaseout start for joint filers is not affected by this parameter, nor are investment income and age eligibilty rules.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ True  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ bool  
_Known Values:_  
2013: False  
2014: False  
2015: False  
2016: False  
2017: False  
2018: False  
2019: False  
_Valid Range:_ min = False and max = True  
_Out-of-Range Action:_ error  


####  `EITC_sep_filers_elig`  
_Description:_ Current-law value is false, implying ineligibility.  
_Has An Effect When Using:_ _PUF data:_ True _CPS data:_ False  
_Can Be Inflation Indexed:_ False _Is Inflation Indexed:_ False  
_Value Type:_ bool  
_Known Values:_  
2013: False  
2014: False  
2015: False  
2016: False  
2017: False  
2018: False  
2019: False  
_Valid Range:_ min = False and max = True  
_Out-of-Range Action:_ error  


