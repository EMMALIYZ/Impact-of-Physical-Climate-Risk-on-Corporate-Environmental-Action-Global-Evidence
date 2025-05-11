cd  "D:\\硕士毕业论文\\climate risk论文\\code_data_climate_risk_carbon"

// data preprocessing

*1.dependent variable
gen inflation = (CPI-100)/100
gen carbon =  (1+inflation)*carbon_intensity_one
replace carbon = 1 if carbon == 0
gen lncarbon = ln(carbon)

*2.independent variable
gen lnCRI = ln(CRI)
gen lnCRI_neg = -1*lnCRI

*3.control variables
gen age_0 = age
replace age_0 = 1 if age == 0
gen lnage_2 = ln(age_0)
gen capitalexat = capitalex/total_asset
gen lnPGDP = ln(PGDP)
winsor2 lncarbon ROA size cashflow leverage current capitalexat,cut(1 99)

*4.mechanism variables
by firm: ipolate envir_innovation year,gen(envir_innovation_2) epolate
winsor2 envir_innovation_2,cut(1 99)
replace envir_innovation_2_w = 0 if envir_innovation_2_w < 0
winsor2 fixed_assets_turnover,cut(1 99)
gen fix = fixed_assets_turnover_w/(1+inflation)

//global variable settings

global core "lncarbon_w lnCRI_neg"
global controls "ROA_w size_w cashflow_w lnage_2 leverage_w current_w capitalexat_w lnPGDP urban FDIGDP opengdp"
global fix "firm year"

//set panel data
encode SP_ENTITY_NAME, gen(firm)
xtset firm year

//相关性分析
logout, save(mytable5) word replace: pwcorr_a $core $controls

//descriptive statistics
sum $core $controls envir_innovation_2_w fix

//basic regression
reghdfe $core, ///
absorb($fix) vce (cluster firm)
est store m1

reghdfe $core ROA_w size_w cashflow_w lnage_2 leverage_w current_w capitalexat_w, ///
absorb($fix) vce (cluster firm)
est store m2

reghdfe $core $controls, ///
absorb($fix) vce (cluster firm)
est store m3

esttab m1 m2 m3 using "D:\\code_data_climate_risk_carbon\\table3.rtf",  star(* 0.1 ** 0.05 *** 0.01) b(%10.3f) nogaps ar2

//mechanism analysis
*1.environmental innovation
reghdfe envir_innovation_2_w lnCRI_neg $controls, absorb($fix) vce (cluster firm)
est store m4

*2.asset lightening
reghdfe fix lnCRI_neg $controls, absorb($fix) vce (cluster firm)
est store m5

esttab m4 m5 using "D:\\code_data_climate_risk_carbon\\table4.rtf",  star(* 0.1 ** 0.05 *** 0.01) b(%10.3f) nogaps ar2

//country heterogeneity
*1.resource endowment
summarize endowment if year == 2006,detail
replace endowment_ty = "large" if endowment >  .9133614
replace endowment_ty = "small" if endowment <=  .9133614
encode endowment_ty,gen(endowment_type)
label list endowment_type
*large
reghdfe  $core $controls ///
if endowment_type == 1 ///
,absorb($fix) vce (cluster firm)
est store m6
*small
reghdfe  $core $controls ///
if endowment_type == 2 ///
,absorb($fix) vce (cluster firm) 
est store m7

*2.income
*high
reghdfe $core $controls ///
if (income2006 == 1 | income2006 == 2) ///
,absorb($fix) vce (cluster firm)
est store m8
*low
reghdfe $core $controls ///
if (income2006 == 3 | income2006 == 4) ///
,absorb($fix) vce (cluster firm)
est store m9

*3.climate legislation
summarize cum_law_all if year == 2006,detail
gen law_all_ty = ""
replace law_all_ty = "high" if cum_law_all >  6
replace law_all_ty = "low" if cum_law_all <=  6
encode law_all_ty,gen(law_all_type)
label list law_all_type
codebook law_all_type
*high
reghdfe $core $controls ///
if law_all_type == 1 ///
,absorb($fix) vce (cluster firm)
est store m10
*low
reghdfe $core $controls ///
if law_all_type == 2 ///
,absorb($fix) vce (cluster firm)
est store m11

esttab m6 m7 m8 m9 m10 m11 using "D:\\code_data_climate_risk_carbon\\table5.rtf",  star(* 0.1 ** 0.05 *** 0.01) b(%10.3f) nogaps ar2

//corporate heterogeneity
*1.carbon-intensive and non-carbon-intensive
encode SECTOR,gen(industry)
//industry:
//            1 Communication Services 
//            2 Consumer Discretionary 
//            3 Consumer Staples 
//            4 Energy 
//            5 Financials
//            6 Health Care
//            7 Industrials 
//            8 Information Technology
//            9 Materials 
//           10 Real Estate
//           11 Utilities 
*carbon-intensive 
reghdfe $core $controls ///
if industry == 4 | industry == 7 | industry == 9 | industry == 11 ///
,absorb($fix) vce (cluster firm)
est store m12 
*non-carbon-intensive
reghdfe $core $controls ///
if industry == 1 | industry == 2 | industry == 3 | ///
industry == 6 | industry == 8 | industry == 10 ///
,absorb($fix) vce (cluster firm)
est store m13

*2.large and small firms
summarize size_w if year == 2006,detail
gen size_ty = ""
replace size_ty = "large" if size_w >  14.91719
replace size_ty = "small" if size_w <=   14.91719
encode size_ty,gen(size_type)
label list size_type
codebook size_type
*large
reghdfe $core $controls ///
if size_type == 1 ///
,absorb($fix) vce (cluster firm)
est store m14
*small
reghdfe $core $controls ///
if size_type == 2 ///
,absorb($fix) vce (cluster firm)
est store m15

*3.corporate environmental governance
*with
reghdfe $core $controls ///
if EMP >0 ///
,absorb($fix) vce (cluster firm)
est store m16
*without
reghdfe $core $controls ///
if EMP == 0 ///
,absorb($fix) vce (cluster firm)
est store m17

esttab m12 m13 m14 m15 m16 m17 using "D:\\code_data_climate_risk_carbon\\table6.rtf",  star(* 0.1 ** 0.05 *** 0.01) b(%10.3f) nogaps ar2

//robustness checks
*1.alternative measure of independent and dependent variables
gen industry_avg_intensity = mean(carbon_intensity_one), by(industry year)
gen c_industry = carbon_intensity_one - industry_avg_intensity
winsor2 c_industry,cut(1 99)
reghdfe c_industry_w lnCRI_neg $controls ///
,absorb($fix) vce (cluster firm)
est store m18
reghdfe lncarbon_w vulnerability $controls ///
,absorb($fix) vce (cluster firm)
est store m19
esttab m18 m19 using "D:\\code_data_climate_risk_carbon\\table7.rtf",  star(* 0.1 ** 0.05 *** 0.01) b(%10.3f) nogaps ar2

*2.multinational firms and lagging effect
reghdfe $core $controls ///
if multination == 0 ///
,absorb($fix) vce (cluster firm)
est store m20
sort firm year
reghdfe lncarbon_w L.lnCRI_neg  $controls ///
,absorb($fix) vce (cluster firm)
est store m21
esttab m20 m21 using "D:\\code_data_climate_risk_carbon\\table8.rtf",  star(* 0.1 ** 0.05 *** 0.01) b(%10.3f) nogaps ar2

*3.shocks of carbon policies
replace tax = 0 if tax == .
replace tran = 0 if tran == .
reghdfe $core $controls tax ///
,absorb($fix) vce (cluster firm)
est store m22
reghdfe $core $controls tran ///
,absorb($fix) vce (cluster firm)
est store m23
reghdfe $core $controls tax tran ///
,absorb($fix) vce (cluster firm)
est store m24
esttab m22 m23 m24 using "D:\\code_data_climate_risk_carbon\\table9.rtf",  star(* 0.1 ** 0.05 *** 0.01) b(%10.3f) nogaps ar2