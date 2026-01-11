*------------------------------------------------------------------------------*
*****************************Part I:Descriptive Statistics********************* 
*Import Database
clear
use "all_data_v2.dta" ,clear
encode indcode, generate(ind)
winsor2 GB MAC GTI11 GTI22 firm_size TOP1 Indep Board Dual Opinion, replace cuts(1 99) by(year)

sum2docx GB MAC IC GTI11 GTI22 firm_size TOP1 Indep Board Dual Opinion using summary.docx, replace ///
    stats(N mean(%9.4f) sd min(%9.2f) median(%9.2f) max(%9.2f)) ///
    title("Table 2: Summary Statistics")

*correlation analysis
logout,save(相关性分析) word replace:pwcorr_a GB MAC IC GTI11 GTI22 firm_size TOP1 Indep Board Dual Opinion, star1(.01) star5(.05) star10(.1) 
*VIF test, saved in MyFile.doc
reg GB MAC IC GTI11 GTI22 firm_size TOP1 Indep Board Dual Opinion

asdoc vif, dec(3)
*------------------------------------------------------------------------------*

*****************************Part II: Baseline Regression*******************************
*Baseline regression
*add fixed effects， by year, province, industry, year*province, year*industry 
encode Province, generate(province_num)
encode City, generate(city_num)

encode Industry2, generate(industry_num)
gen industry_first_num = substr(indcode, 1, 1)
encode industry_first_num, gen(industry_number)

reg GB MAC firm_size TOP1 Indep Board Dual Opinion,r
est store a1

*year, province, industry fixed effect 
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num) vce(cluster id)
est store a2

*add year*industry fixed effect 
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#industry_num) vce(cluster id)
est store a3

reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store a4

*save results in word files
outreg2 [a1 a2 a3 a4] using baseline_regression.doc, replace tstat bdec(4) tdec(4) keep(GB MAC firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)
*------------------------------------------------------------------------------*
******************************Part III: Causal Mediation*************************
***mediation effect
*MAC's direct impact on GB 
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r1

*MAC on GTI11（green invention patent）
reghdfe GTI11 MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r2

*GTI11（green invention patent）on GB, control for MAC
reghdfe GB GTI11 MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r3

*MAC on GTI22（green utility patent）
reghdfe GTI22 MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r4

*GTI22（green utility patent) on GB，control for MAC
reghdfe GB GTI22 MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store r5

outreg2 [r1 r2 r3 r4 r5] using mediation_effect.doc, replace tstat bdec(4) tdec(4) ///
    keep(GB MAC GTI11 GTI22 firm_size TOP1 Indep Board Dual Opinion) ///
    addtext(Year FE, YES, Province FE, YES, Industry FE, YES, Year*Province FE, YES, Year*Industry FE, YES)

*https://www.trentonmize.com/software/sgmediation2
*for GTI11 - green invention patent
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation2 GB, mv(MAC) iv(GTI11) cv(firm_size TOP1 Indep Board Dual Opinion) 
estat bootstrap, percentile bc

*Indirect (mediated) effect ab = 
di "中介（间接）效应 ab = "_b[_bs_1]  

*Direct effect c
di "直接效应 c' = "_b[_bs_2]   

*proportion of mediation effect ab/c
di "中介效应占比ab/c = "_b[_bs_1]/(_b[_bs_1]+_b[_bs_2])  

*for GTI22 - green utility patent
bootstrap r(ind_eff) r(dir_eff), reps(1000): sgmediation2 GB, mv(MAC) iv(GTI22) cv(firm_size TOP1 Indep Board Dual Opinion) 
estat bootstrap, percentile bc

*Indirect effect 
di "中介（间接）效应 ab = "_b[_bs_1]  

*Direct effect
di "直接效应 c' = "_b[_bs_2]   

*proportion of mediation effect ab/c
di "中介效应占比ab/c = "_b[_bs_1]/(_b[_bs_1]+_b[_bs_2])
*------------------------------------------------------------------------------*

********************************Part IV:Moderating Effect************************
*调节
*IC is the internal control index 
*strategy (MAC_SL), reporting reliability (MAC_RR)
*operational efficiency (MAC_OL), and compliance (MAC_CC)

center MAC IC SL RR OL CC IS
gen MAC_IC=c_MAC*c_IC
gen MAC_SL=c_MAC*c_SL
gen MAC_RR =c_MAC*c_RR 
gen MAC_OL=c_MAC*c_OL
gen MAC_CC=c_MAC*c_CC
gen MAC_IS=c_MAC*c_IS
reghdfe GB MAC MAC_IC IC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store r1
reghdfe GB MAC MAC_SL SL firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store r2
reghdfe GB MAC MAC_RR RR firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store r3
reghdfe GB MAC MAC_OL OL firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store r4
reghdfe GB MAC MAC_CC CC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store r5
reghdfe GB MAC MAC_IS IS firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store r6
outreg2 [r1 r2 r3 r4 r5 r6] using IC_regression.doc, replace tstat bdec(4) tdec(4) keep(GB MAC IC SL OL RR CC IS MAC_IC MAC_SL MAC_RR MAC_OL MAC_CC MAC_IS firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)

*------------------------------------------------------------------------------*

*********************************Part V: Robustness Check**********************
*1. Lag MAC by one, two, and three periods to verify the consistency of results 
xtset id year
reghdfe GB L.MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store b1
reghdfe GB L2.MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store b2
reghdfe GB L3.MAC firm_size TOP1 Indep Board Dual Opinion ,absorb(year province_num industry_num year#province_num  year#industry_num) vce(cluster id)
est store b3
outreg2 [b1 b2 b3 ] using robustness_1.doc,replace tstat  bdec(4) tdec(4) keep(GB MAC L.MAC L2.MAC L3.MAC  firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)

*2. Excluding Special Years: Remove 2016 (a year of increased regulatory intensity in China).
preserve
drop if year == 2016
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store b4
* export the results to word file
outreg2 [b4] using robustness_2.doc, replace tstat bdec(4) tdec(4) keep(GB MAC firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)
restore
*------------------------------------------------------------------------------*
*3.robustness check - 11 coastal provinces

gen coastal = 0
replace coastal = 1 if inlist(Province, "辽宁省", "河北省", "山东省", "江苏省", "浙江省")
replace coastal = 1 if inlist(Province, "福建省", "广东省", "广西省", "海南省", "上海市")
replace coastal = 1 if inlist(Province, "天津市")

* Alternative approach with fewer fixed effects
* 11 coastal provinces
reg GB MAC firm_size TOP1 Indep Board Dual Opinion i.year i.province_num i.industry_num if coastal == 1, vce(cluster id)
est store coastal

* 南方地区 
reg GB MAC firm_size TOP1 Indep Board Dual Opinion i.year i.province_num i.industry_num if coastal == 0, vce(cluster id)
est store non_coastal

* 导出结果到 Word 文档
outreg2 [coastal non_coastal] using coastal_robust.doc, replace tstat bdec(4) tdec(4) keep(GB MAC firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES)
*------------------------------------------------------------------------------*
* 4. Robustness check, excluding Beijing, Tianjin, Shanghai, Chongqing
*排除北京、天津、上海、重庆的企业
preserve
drop if inlist(Province, "北京市", "天津市", "上海市", "重庆市")  // 排除直辖市

* 
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion, absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store exclude_municipalities

* export results to the word file
outreg2 [exclude_municipalities] using excluding_beijing.doc, replace tstat bdec(4) tdec(4) keep(GB MAC firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)
restore
*------------------------------------------------------------------------------*
* 5.limit the sample date from year2012 to year 2019 for robustness checks

reg GB MAC firm_size TOP1 Indep Board Dual Opinion ///
    if inrange(year, 2012, 2019), r
est store a1_firmsize

reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion ///
    if inrange(year, 2012, 2019), ///
    absorb(year province_num industry_num) vce(cluster id)
est store a2_firmsize

reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion ///
    if inrange(year, 2012, 2019), ///
    absorb(year province_num industry_num year#industry_num) vce(cluster id)
est store a3_firmsize

reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion ///
    if inrange(year, 2012, 2019), ///
    absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store a4_firmsize

outreg2 [a1_firmsize a2_firmsize a3_firmsize a4_firmsize] using baseline_regression_20122019.doc, replace ///
    tstat bdec(4) tdec(4) ///
    keep(GB MAC firm_size TOP1 Indep Board Dual Opinion) ///
    addtext(Year FE, YES, Province FE, YES, Industry FE, YES, Year*Province FE, YES, Year*Industry FE, YES)
*------------------------------------------------------------------------------*

*********************************Part VI: Addressing Endogeneity***************

*CPRI is the average of extreme cold days, extreme heat days, extreme drought days, extreme precipitation days， ln_CPRI = ln(1+CPRI)

ivreg2 GB firm_size TOP1 Indep Board Dual Opinion  i.year ( MAC = ln_CPRI),first savefp(first) r
eststo second
		
outreg2 [first second] using IV_results.doc, ///
    tstat bdec(3) tdec(3) replace ///
    nor2 ///
    addstat(First-stage F-statistic, 9.99, ///
            Kleibergen-Paap F-statistic, 9.987, ///
            Anderson-Rubin p-value, 0.0213, ///
            Stock-Wright LM p-value, 0.0214) ///
    addtext(Year FE, YES, YES) ///
    ctitle("First-stage: MFC", "Second-stage: GB") ///
    addnote("Notes: Column (1) reports first-stage regression: MAC on climate risk index (ln_CPRI) and controls.", ///
            "Column (2) reports second-stage regression: Greenwashing (GB) on instrumented MAC.", ///
            "First-stage F=9.99 exceeds Stock-Yogo threshold for 15% maximal IV size (8.96).", ///
            "Anderson-Rubin (p=0.021) and Stock-Wright (p=0.021) weak-IV-robust tests confirm significant effects.", ///
            "Robust standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1")
						
*------------------------------------------------------------------------------
*------------------------------------------------------------------------------*
*********************************Part VII: Heterogeneity Analysis***************
*Heterogeneity analysis
*1. by ownership
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion if SOE==1,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store d1
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion if SOE==0,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store d2
outreg2 [d1 d2] using ownership.doc,replace tstat  bdec(4) tdec(4) keep(GB MAC firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)

*------------------------------------------------------------------------------*
*2. By Industry: Focus on heavily polluting vs. non-polluting industries.
* Create the pollution_industry variable
gen pollution_industry = 0

* Assign 1 to pollution industries based on industry_code
replace pollution_industry = 1 if inlist(indcode,  "C30", "C31", "C32", "C33", "D44")
replace pollution_industry = 1 if inlist(indcode, "C19", "C22", "C25", "C26", "C27", "C28")
replace pollution_industry = 1 if inlist(indcode, "B06", "B07", "B08", "B09", "C17")

reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion  if pollution_industry==1,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e1
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion  if pollution_industry==0,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e2
outreg2 [e1 e2] using pollution.doc,replace tstat  bdec(4) tdec(4) keep(GB MAC firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)
*------------------------------------------------------------------------------*
*3. By Firm Size: Examine differences between large and small firms.
*Firms are classified into “Large” and “Small” groups based on the sample median of total assets (Size). 
* Speciafically, firms with Size ≥ median(Size) are defined as “Large Median Total Assets”, 
* while firms with Size &lt; median(Size) are defined as “Small Median Total Assets”.

egen mid=median(Size)
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion if Size>mid,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e3
reghdfe GB MAC firm_size TOP1 Indep Board Dual Opinion if Size<=mid,absorb(year province_num industry_num year#province_num year#industry_num) vce(cluster id)
est store e4
outreg2 [e3 e4] using sizeFirm.doc,replace tstat  bdec(4) tdec(4) keep(GB  MAC firm_size TOP1 Indep Board Dual Opinion) addtext(Year, YES, Province, YES, Industry, YES, Year*Province, YES, Year*Industry, YES)

*------------------------------------------------------------------------------*
*********************************Plot the Figures*************************************
*Code for Figure 2 
coefplot ///
  (b1, label("Lag 1") keep(L.MAC) rename(L.MAC = "MAC (Lag 1)") mcolor(navy) ciopts(lcolor(navy) recast(rcap))) ///
  (b2, label("Lag 2") keep(L2.MAC) rename(L2.MAC = "MAC (Lag 2)") mcolor(maroon) ciopts(lcolor(maroon) recast(rcap))) ///
  (b3, label("Lag 3") keep(L3.MAC) rename(L3.MAC = "MAC (Lag 3)") mcolor(forest_green) ciopts(lcolor(forest_green) recast(rcap))) ///
  (b4, label("Excl. 2016") keep(MAC) rename(MAC = "MAC (Excl. 2016)") mcolor(purple) ciopts(lcolor(purple) recast(rcap))) ///
  (exclude_municipalities, label("Excl. Municipalities") keep(MAC) rename(MAC = "MAC (Excl. Municipalities)") mcolor(teal) ciopts(lcolor(teal) recast(rcap))) ///
  (coastal, label("Coastal") keep(MFC) rename(MAC = "Coastal Provinces") mcolor(dkorange) ciopts(lcolor(dkorange) recast(rcap))) ///
  (non_coastal, label("Non-coastal") keep(MAC) rename(MAC = "Non-coastal Provinces") mcolor(gs10) ciopts(lcolor(gs10) recast(rcap))) ///
  , ///
  horizontal ///
  xline(0, lcolor(gs8)) ///
  msymbol(D) ///
  msize(small) ///
  mlabel format(%9.4f) mlabposition(12) mlabgap(*1.5) mlabsize(vsmall) ///
  xlabel(-.04(.01).01, format(%9.3f) labsize(vsmall)) ///
  ylabel(, labsize(small)) ///
  grid(none) ///
  scheme(s2color) ///
  ytitle("") ///
  title("Combined Robustness Checks (95% Confidence Intervals)", size(small)) ///
  note("Non-coastal models were only statistically significant with 90% CIs.", size(vsmall)) ///
  legend(size(vsmall) position(7) ring(1) col(2)) ///
  plotregion(margin(small) fcolor(white)) ///
  graphregion(margin(small) fcolor(white))
graph export "Combined_Robustness_Corrected.png", replace width(3200) height(2000)

*Code for Figure 3 
coefplot ///
    (d1, label("State-Owned") msymbol(O) mcolor(navy) ciopts(color(navy))) ///
    (d2, label("Private Firms") msymbol(D) mcolor(maroon) ciopts(color(maroon))) ///
    (e1, label("Heavily Polluting") msymbol(T) mcolor(forest_green) ciopts(color(forest_green))) ///
    (e2, label("non-polluting") msymbol(S) mcolor(dkorange) ciopts(color(dkorange))) ///
    (e3, label("Large Median Total Assets") msymbol(+) mcolor(purple) ciopts(color(purple))) ///
    (e4, label("Small Median Total Assets") msymbol(X) mcolor(teal) ciopts(color(teal))), ///
    keep(MFC) xline(0) ///
    ytitle("") rename(MAC = "MAC") ///
    xtitle("Coefficient Estimate with 95% Confidence Interval") ///
    title("Heterogeneity Analysis") ///
    subtitle("Across Different Firm Characteristics") ///
    mlabel mlabformat(%9.4f) mlabposition(12) mlabgap(*1.5) ///
    grid(none) scheme(s2mono) graphregion(color(white)) plotregion(color(white))
graph export "MAC_heterogeneity_july2025.png", replace width(3000)



