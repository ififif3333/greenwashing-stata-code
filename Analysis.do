*****************************Part I: merge database*****************************
*greenwashing data
clear

*this greenwash dataset has deleted the data with ST or ST* and financial industry firms
use "greenwash.dta",clear

*merge the database which contain the green patent data 
merge 1:1 id year using "green_patent_data.dta", nogen keep(1 3) 

*merge with the database which contain the corporate governance data
merge 1:1 id year using "internal_control.dta", nogen keep(1 3) 

*merge with the database which contain all the company control variables
merge 1:1 id year using "firm_controls.dta", nogen keep(1 3) 

*merge with the database which contain the management attention to climate risks
merge 1:1 id year using "attention_data.dta", nogen keep(1 3) 

*generate the log level data for green invention patent, granted patent data
gen GTI11=ln(当年独立获得的绿色发明数量+1)
label variable GTI11 "greent patent granted invention patent"

*generate the log level data for green utility patent, granted patent data
gen GTI22=ln(当年独立获得的绿色实用新型数量+1)
label variable GTI22 "greent patent granted utility patent"

*IC is the degree of corporate internal governance 
destring IC,force replace

*delete the missing values 剔除缺失 
drop if missing(GB,MFC,IC,GTI11,GTI22,FirmAge,TOP1,Indep,Board,Dual,Opinion)

save "all_data.dta",replace

*------------------------------------------------------------------------------*
*****************************Part II:Descriptive Statistics********************* 
*Import Database
clear
use "all_data.dta" ,clear
encode indcode, generate(ind)
winsor2 GB MFC GTI11 GTI22 FirmAge TOP1 Indep Board Dual Opinion, replace cuts(1 99) by(year)

sum2docx GB MFC IC GTI11 GTI22 FirmAge TOP1 Indep Board Dual Opinion using summary.docx, replace ///
    stats(N mean(%9.4f) sd min(%9.2f) median(%9.2f) max(%9.2f)) ///
    title("Table 2: Summary Statistics")

*correlation analysis
*the following is reported in "Table S2: Pairwise correlation of main variables"
logout,save(相关性分析) word replace:pwcorr_a GB MFC IC GTI11 GTI22 FirmAge TOP1 Indep Board Dual Opinion, star1(.01) star5(.05) star10(.1) 
*VIF test, saved in MyFile.doc
reg GB MFC IC GTI11 GTI22 FirmAge TOP1 Indep Board Dual Opinion

asdoc vif, replace dec(4)

asdoc vif
*------------------------------------------------------------------------------*
*****************************Part III: Regression*******************************
*Baseline regression
*add fixed effects， by year, province, industry, year*province, year*industry 
encode Province, generate(province_num)
encode Industry2, generate(industry_num)
gen industry_first_num = substr(indcode, 1, 1)
encode industry_first_num, gen(industry_number)

reg GB MFC FirmAge TOP1 Indep Board Dual Opinion,r
est store a1

*year, province, industry fixed effect 
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num) vce(cluster id)
est store a2

*add year*industry fixed effect 
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#industry_num) vce(cluster id)
est store a3

reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store a4

*save results in word files
*These results correspond to the results shown in Table 2 
*in the Table 2 reporting, I have changed MFC to MAC
outreg2 [a1 a2 a3 a4] using baseline_regression.doc, replace tstat bdec(4) tdec(4) keep(GB MFC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)
*------------------------------------------------------------------------------*
******************************Part IV: Causal Mediation*************************
***Mediation effect***

*Direct effect of MFC (Managerial attention to climate risks) on GB (Greenwashing Behavior)
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r1

*Effect of MFC on GTI11 (Green Invention Patents Granted, log level)
reghdfe GTI11 MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r2

*Effect of GTI11 on GB, controlling for MFC
reghdfe GB GTI11 MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r3

*Effect of MFC on GTI22 (Green Utility Patents Granted, log level)
reghdfe GTI22 MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r4

*Effect of GTI22 on GB, controlling for MFC
reghdfe GB GTI22 MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r5

*the results correspond to results shown in Table 3 
outreg2 [r1 r2 r3 r4 r5] using mediation_effect.doc,replace tstat  bdec(4) tdec(4) keep(GB_normalized MFC GTI11 GTI22 FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES,Ind, YES)

*the following results correspond to results shown in Table 4
*the following part is for GTI11, green invention patent granted
*https://www.trentonmize.com/software/sgmediation2
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation2 GB, mv(MFC) iv(GTI11) cv(FirmAge TOP1 Indep Board Dual Opinion) 
estat bootstrap, percentile bc

*Indirect (mediated) effect ab = 
*"中介（间接）效应" means "Indirect (mediated effect)"
di "中介（间接）效应 ab = "_b[_bs_1]  

*Direct effect c
*"直接效应" means "Direct effect"
di "直接效应 c' = "_b[_bs_2]   

*Proportion of mediation effect ab/c
*"中介效应占比" means "proportion of mediation effect"
di "中介效应占比ab/c = "_b[_bs_1]/(_b[_bs_1]+_b[_bs_2])  

*the following part is for GTI22, green utility patent granted 
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation2 GB, mv(MFC) iv(GTI22) cv(FirmAge TOP1 Indep Board Dual Opinion) 
estat bootstrap, percentile bc

*"中介（间接）效应" means "Indirect (mediated effect)"
di "中介（间接）效应 ab = "_b[_bs_1]  

*"直接效应" means "Direct effect"
di "直接效应 c' = "_b[_bs_2]   

*"中介效应占比" means "proportion of mediation effect"
di "中介效应占比ab/c = "_b[_bs_1]/(_b[_bs_1]+_b[_bs_2])

*------------------------------------------------------------------------------*
********************************Part V:Moderating Effect************************
*Moderating effect estimation for corporate governance level 
*the following result is reported in "Table S3: Moderating effect Regression Result"
center MFC IC
gen MFC_IC=c_MFC*c_IC
reghdfe GB MFC MFC_IC IC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store r6
outreg2 [r6] using moderating_IC.doc,replace tstat  bdec(4) tdec(4) keep(GB MFC MFC_IC IC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES,Ind, YES)

*------------------------------------------------------------------------------*
*********************************Part VI: Robustness Check**********************
*the following results are the data used in "Figure 2:Combined Robustness Checks"
*1. Lag MAC by one, two, and three periods to verify the consistency of results 
xtset id year
reghdfe GB L.MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store b1
reghdfe GB L2.MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store b2
reghdfe GB L3.MFC FirmAge TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store b3
outreg2 [b1 b2 b3 ] using robustness_1.doc,replace tstat  bdec(4) tdec(4) keep(GB MFC L.MFC L2.MFC L3.MFC  FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)

*2. Excluding Special Years: Remove 2016 (a year of increased regulatory intensity in China).
preserve
drop if year == 2016
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store b4

outreg2 [b4] using robustness_2.doc, replace tstat bdec(4) tdec(4) keep(GB MFC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)
restore


*3.robustness check - 11 coastal provinces (coasttal vs. non-coastal)

*11 coastal provinces are : "辽宁省", "河北省", "山东省", "江苏省", "浙江省" "福建省", "广东省", "广西省", "海南省", "上海市". "天津市"
*the corresponding translations are: "Liaoning","Hebei","Shandong","Jiangsu","Zhejiang","Fujian","Guangdong","Guangxi","Hainan","Shanghai","Tianjin"
gen coastal = 0
replace coastal = 1 if inlist(Province, "辽宁省", "河北省", "山东省", "江苏省", "浙江省")
replace coastal = 1 if inlist(Province, "福建省", "广东省", "广西省", "海南省", "上海市")
replace coastal = 1 if inlist(Province, "天津市")

* 11 coastal provinces
reg GB MFC FirmAge TOP1 Indep Board Dual Opinion i.year i.province_num i.industry_num if coastal == 1, vce(cluster id)
est store coastal

*non-coastal provinces
reg GB MFC FirmAge TOP1 Indep Board Dual Opinion i.year i.province_num i.industry_num if coastal == 0, vce(cluster id)
est store non_coastal

outreg2 [coastal non_coastal] using coastal_robust.doc, replace tstat bdec(4) tdec(4) keep(GB MFC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES)


* 4. Robustness check for excluding Municipalities, excluding Beijing, Tianjin, Shanghai, Chongqing
*"北京市", "天津市", "上海市", "重庆市" correspond to "Beijing","Tianjin","Shanghai","Chongqing"  
preserve
drop if inlist(Province, "北京市", "天津市", "上海市", "重庆市")  // 排除直辖市

reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store exclude_municipalities

outreg2 [exclude_municipalities] using excluding_beijing.doc, replace tstat bdec(4) tdec(4) keep(GB MFC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)
restore

*------------------------------------------------------------------------------*
*********************************Part VII: Addressing Endogeneity***************
*Using population density as IV (instrumental variable)
*the following results are reported in "Table 5: IV Regression Results (Two-Stage Least Squares)"

*first merge the all_data.dta database with the population density database
*"所属城市" means "Headquarter city (registered city) of the listed firm, as reported in CSMAR."
merge m:1 所属城市 year using  "pop_den_all.dta", nogen keep(1 3)

*pop_den means population density
ivreg2 GB FirmAge TOP1 Indep Board Dual Opinion i.year  (MFC=pop_den),first savefp(first)
eststo second
outreg2 [first second] using 1.doc, tstat bdec(3) tdec(3) replace

*------------------------------------------------------------------------------*
*********************************Part VIII: Heterogeneity Analysis***************
*the following results are used in "Figure 3: Heterogeneity Analysis Across Firm Characteristics"
*1. by ownership
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion if SOE==1,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store d1
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion if SOE==0,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store d2
outreg2 [d1 d2] using ownership.doc,replace tstat  bdec(4) tdec(4) keep(GB MFC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)


*2. By Industry: Focus on heavily polluting vs. non-polluting industries.
* Create the pollution_industry variable
gen pollution_industry = 0

* Assign 1 to pollution industries based on industry_code
replace pollution_industry = 1 if inlist(indcode,  "C30", "C31", "C32", "C33", "D44")
replace pollution_industry = 1 if inlist(indcode, "C19", "C22", "C25", "C26", "C27", "C28")
replace pollution_industry = 1 if inlist(indcode, "B06", "B07", "B08", "B09", "C17")

reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion  if pollution_industry==1,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e1
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion  if pollution_industry==0,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e2
outreg2 [e1 e2] using pollution.doc,replace tstat  bdec(4) tdec(4) keep(GB MFC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)


*3. By Firm Size: Examine differences between large and small firms.
*Split sample using the median of total assets: firms above the median are classified as large, those below as small.
egen mid=median(Size)
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion if Size>mid,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e3
reghdfe GB MFC FirmAge TOP1 Indep Board Dual Opinion if Size<=mid,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e4
outreg2 [e3 e4] using sizeFirm.doc,replace tstat  bdec(4) tdec(4) keep(GB  MFC FirmAge TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)


