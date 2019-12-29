use "C:\Users\The_sun\Desktop\tmp\all_banks_for_students.dta", clear

/* Data generation */

drop if month_num < 612 | month_num > 623

gen ref_rate = .
replace ref_rate = 7.75 if month <= 613
replace ref_rate = 8 if month > 613 & month <= 615
replace ref_rate = 8.25 if month > 615

gen inf_rate = .
replace inf_rate = 2.37 if month_num == 612
replace inf_rate = 0.78 if month_num == 613
replace inf_rate = 0.62 if month_num == 614
replace inf_rate = 0.43 if month_num == 615
replace inf_rate = 0.48 if month_num == 616
replace inf_rate = 0.23 if month_num == 617
replace inf_rate = -0.01 if month_num == 618
replace inf_rate = -0.24 if month_num == 619
replace inf_rate = -0.04 if month_num == 620
replace inf_rate = 0.48 if month_num == 621
replace inf_rate = 0.42 if month_num == 622
replace inf_rate = 0.44 if month_num == 623


gen credits =  (credb+ credfb+ credf+ credh)/ assets
egen total_banks_assets = sum(assets), by(month_num)
gen size = assets / total_banks_assets
gen liquidity =  money / assets

gen roa = prof / assets
gen capita = (assets - depb - depfb - depf - deph- cbcred) / assets



/* Outliers filtering */

gen isbad = liquidity > 0.5 | roa > 0.85 | size > 0.05
bysort bank (isbad) : drop if isbad[_N]
xtset bank month_num
xtdes



// Time autocorrelation test (Wooldrige test)
*findit xtserial
*ssc install xtserial
xtserial credits liquidity roa capita ref_rate inf_rate


/* Test for serial correlation in residuals in fe model only */
*ssc install pantest2
xtreg credits liquidity roa capita ref_rate inf_rate, fe
pantest2 t


// Time autocorrelation test of Baltagi-Li(1995)
* findit xttest1
qui xtreg credits liquidity roa capita ref_rate inf_rate, re
xttest1

*DOESN"T WORK

*findit xttest2
*ssc install xttest2
*xtreg credits size liquidity roa capita ref_rate inf_rate, fe
*xttest2
* doesn't work, cause we have N>T

* Panel Darbin-Watson
* findit lmadwxt
*set matsize 800
*lmadwxt credits size liquidity roa capita ref_rate inf_rate, id(bank) it(month_num)
* Also work only in case N<T

* FE regression with e~AR(1) modelled
xtset bank month_num
xtregar credits liquidity roa capita ref_rate inf_rate, fe
est store regar


// Spatial(crossectioanal) autocorrelation test (Pesaran test - both FE, RE)
** H0: corr(eit,ejt)=0 is rejected here
** findit xtcsd
* ssc install xtcsd
xtreg credits liquidity roa capita ref_rate inf_rate, fe
set matsize 800
xtcsd, pesaran abs


***************************************************

** Groupwise Heteroscedastisity test, Wald modified test
** dont require e~N(.) errors normality
* findit xttest3
* ssc install xttest3
xtreg credits liquidity roa capita ref_rate inf_rate, fe
est store fe
xttest3

* heteroskedasticity test via LR
set matsize 800
xtgls credits liquidity roa capita ref_rate inf_rate, igls panels(heteroskedastic)
est store hetero
xtgls credits liquidity roa capita ref_rate inf_rate
est store homo
local df=e(N_g)-1
lrtest hetero homo, df(`df')
***************************************************

********************************************************************
***************** CORRECTION of a/c and h/s ************************
********************************************************************

*** Wooldridge test and other tests gives us that serial correlation is present.
** Also h/s is present
** In order to deal with it, we can ither use robust SE, or modell it.


**** Use Newye-West S.E. estimators
** 1) Newye-West estimator counts for both a/c (up to some chosen lag) 
**    and h/h in time in general form
** lag = 0.75*T^(1/3), see Stock&Watson "Intro in Econ"

qui newey credits liquidity roa capita ref_rate inf_rate i.bank, lag(1) force
est store newey_ar1


qui newey credits liquidity roa capita ref_rate inf_rate i.bank, lag(2) force
est store newey_ar2

** Dryscoll-Kraay estimator: a/c in N and T + h/h in T
**Sturcture of errors assumes heteroskedastic, autocorrelated up to some lag
** and possibly correlated between the groups (objects)
* findit xtscc
* scc install xtscc
xtscc credits liquidity roa capita ref_rate inf_rate, fe lag(1)
est store fe_dris_kraay_ar1

xtscc credits liquidity roa capita ref_rate inf_rate, fe lag(2)
est store fe_dris_kraay_ar2
** In econometrics the more flexible estimators(model, method etc.) you use, the less you can get..
** For ex., If you are sure that there is no spartial a/c,
** if Driscoll& Cray SE are used then you will probably get larger s.e. 
** Usual rule: The more flexible model(or method) you have the larger s.e. will be


** Since have a large N, small T, -robust- (or -cluster-) option
** accomodates for both heteroskedasticity and autocorrelation
** Remember Wald test give us result that sigma(i) not= sigma(j),
** and Wooldridge test detects serial a/c, so robust option may be also appropriate here
xtreg credits liquidity roa capita ref_rate inf_rate, fe robust
est store fe_robust


est table fe fe_robust  newey_ar1 newey_ar2 fe_dris_kraay_ar1 fe_dris_kraay_ar2, keep(liquidity roa capita ref_rate inf_rate _cons) b(%7.4f) p se

**pool model

qui reg credits liquidity roa capita ref_rate inf_rate
est store pool

qui xtreg credits liquidity roa capita ref_rate inf_rate, fe 
est store fe

** at this moment we only reestimate standart errors of out coeffs, but
** we also can estimate model with the relaxed assumption of either h/s,
** and a/c or both.

** Serial a/c can of 1st order can be modelled via xtregar
** here we explicitly assume that e~AR(1)
xtregar credits liquidity roa capita ref_rate inf_rate, fe 
est store xtregar_fe

* also xtgls command can modell both problems
* here we set panels(hetero) option in order to count for possible h/s
xtgls credits size liquidity roa capita ref_rate inf_rate, panels(correlated) force
est store gls_h

** hetero(groupwise) + serial autocorrelation also can be modelled via xtgls
** just by adding option corr()
xtgls credits liquidity roa capita ref_rate inf_rate, panels(correlated) corr(ar1) force
est store gls_h_ar

** ar1 option estimate one serial a/c coefficient for all objects
** However, a/c coefficients may be different for different objects
** psar1 option estimes unique serial a/c coefficients for each object

** ar option: eit=a*eit-1+vit
** psar option eit=ai*eit-1+vit

** FGLS better works in case of N<T, badly work in case of small panels
xtgls credits liquidity roa capita ref_rate inf_rate, panels(correlated) corr(psar1) force
est store gls_h_psar

** However, FGLS can give non-conservative s.e. in case of small N and T
** s.e. may be smaller than they are actually must be
** Espessially this problem arises in case of T<=N

** In this case other approach of Beck&Katz (1995) called 
** "panel-corrected standard errors" is more appropriate

** by default heteroskedastic and autocorrelated errors
** in time and objects dimentions are assumed,
** according to AR(1) process
set matsize 8000
qui xtpcse credits liquidity roa capita ref_rate inf_rate i.bank, corr(psar1)
est store pcse_fe_ar

** The tables with all estimated models for comparison purposes
est table pool fe fe_robust fe_dris_kraay gls_h gls_h_ar gls_h_psar xtregar_fe pcse_fe_ar, keep(liquidity roa capita ref_rate inf_rate _cons) b(%7.4f) star(0.1 0.05 0.01)
est table pool fe fe_robust fe_dris_kraay gls_h gls_h_ar gls_h_psar xtregar_fe pcse_fe_ar, keep(liquidity roa capita ref_rate inf_rate _cons) b(%7.4f) se t




global XVARS "liquidity roa capita ref_rate inf_rate"
xtset bank month_num
gen lag1 = L1.credits


xtreg credits  lag1 $XVARS , fe
est store fe

*Arellano-Bond without optimal matrix
xtabond credits $XVARS
est store ab1

* Validuty test
estat sargan
xtabond credits $XVARS , vce(robust)
*Serial a/c test AR(1,2)
estat abond

** NOTE: In good dynamic panel model we must expect:
*1) Existance of 1st order a/c
*2) No 2nd order a/c
*3) Validity of instruments


*Arellano-Bond with optimal matrix
xtabond credits $XVARS, twostep
est store ab2
* Validuty test
estat sargan
xtabond credits $XVARS , twostep vce(robust)
*Serial a/c test AR(1,2)
estat abond


* You can estimate AB model without a constant with nocons option:
*xtabond logpay $XVARS, twostep nocons
* Adding a constant in 1st-diff equation is equvalent to assume that there's a linear trend in level equation
* Actually, It's not usually the case in applied moddeling, hence nocons option is usefull


xtdpdsys credits $XVARS
est store bb1
estat sargan
xtdpdsys credits $XVARS , vce(robust)
estat abond

xtdpdsys credits $XVARS , twostep
est store bb2
estat sargan
xtdpdsys credits $XVARS , twostep vce(robust)
estat abond

est table fe ab1 ab2 bb1 bb2, b(%7.4f) star(0.1 0.05 0.01)
est table fe ab1 ab2 bb1 bb2, b(%7.4f) se t
**************************************************************
*Arellano-Bond estimators have one- and two-step variants. 
*But though asymptotically more efficient, the two-step estimates of the 
*s.e. tend to be severely downward biased (AB 1991; BB 1998). 
*To compensate, available a finite-sample correction to the two-step cov matrix
*derived by Windmeijer (2000) via robust option.

** twostep - optimal weighted matrix
** vce(robust) - corrected S.E. - , in 1step procedure it correct h/s and a/c, 
*in 2step procedure s.e. are already corrected to h/s and a/c 
*but there is a downwoard bias(mentioned above), which eliminates by adding robust option.
 
** lags() - specify number of lagged Y in RHS

** Can include both endo and predetermined variables:
** endog(varname, lag(2,.)) - specify endogeneous variables
** endogenous(k, lag(2,.))

** Note the difference between preditermined and endogeneous variables
** E(eit*xit)!=0 => endog (current shocks in Y can influence X now)
** E(eis*xit)!=0 for s<t (only past shocks in Y can influence X now)
**************************************************************




** How to choose between AB and BB?
* 1) check in what model signs and sizes of coeff are in line *with theory and your expectations
* 2) Sargan test for validity must hold, there must be 1st order, but not 2nd order a/c.
* 3) all things constant -> choose BB, cause it use bigger volume of moment conditions (and hence, information).

***** Unit root tests *****
* You could test whether 1) your variables are stationary or not
* 2) your model residuals are stationary or not

** If RESIDUALS are stationary when you can expect cointegration and everything is okay
** If RESIDUALS are not stationary for some objects(banks) when you can have missleding results
* cause of so called "spurious regression problem".

predict res, e
xtunitroot llc res, kernel(bartlett nwest)
xtunitroot ht res, mean
xtunitroot breitung res, lags(3) robust
xtunitroot hadri res, kernel(parzen 5)
xtunitroot ips res, lags(aic 5)
xtunitroot fisher res, dfuller lags(3) drift
xtunitroot fisher res, dfuller lags(3) trend

** Dear students, I also sent u a pdf-file with UR tests descriptions and commands 
* But as far I can see the only appropriate test for stationarity in your case is IPS(Im-Pesaran-Shin test).
*The test works well with unbalanced panels in which N is big but T is small. 
*Null hypothesis is all objects have unit root hence non-stationary. 
*H1>:exist objects for which errors are non-stationary
