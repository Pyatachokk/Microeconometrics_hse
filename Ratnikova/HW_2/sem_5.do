
xtset country t
gen lmilk=ln(milk)
gen lgdp_ppc=ln(gdp_ppc)
gen lfaoprice=ln(faoprice)
gen lyoghurt=ln(yoghurt)


reg lmilk lgdp_ppc
est store pool

xtreg lmilk lgdp_ppc, re
est store re

xtreg lmilk lgdp_ppc, fe
est store fe

*************************************************************************
*********** Multicollinearity (Panel VIF) *****************************************

xtreg lmilk lgdp_ppc, fe
est store fe
corr lgdp_ppc lfaoprice lyoghurt
vif, uncentered

xtreg lmilk lgdp_ppc lyoghurt, fe
vif, uncentered

search collin
collin lmilk lgdp_ppc


*********** AUTOCORRELATION *************

// Time autocorrelation test (Wooldrige test)
*findit xtserial
ssc install xtserial
xtserial lmilk lgdp_ppc


/* Test for serial correlation in residuals in fe model only */
*ssc install pantest2
xtreg lmilk lgdp_ppc, fe
pantest2 t


// Time autocorrelation test of Baltagi-Li(1995)
* findit xttest1
qui xtreg lmilk lgdp_ppc, re
xttest1


* findit xttest2
* ssc install xttest2
xtreg lmilk lgdp_ppc, fe
xttest2
* doesn't work, cause we have N>T

* Panel Darbin-Watson
* findit lmadwxt
set matsize 800
lmadwxt lmilk lgdp_ppc, id(country) it(t)
* Also work only in case N<T

* FE regression with e~AR(1) modelled
xtset country t
xtregar lmilk lgdp_ppc, fe
est store regar


// Spatial(crossectioanal) autocorrelation test (Pesaran test - both FE, RE)
** H0: corr(eit,ejt)=0 is rejected here
** findit xtcsd
* ssc install xtcsd
xtreg lmilk lgdp_ppc, fe
set matsize 800
xtcsd, pesaran abs


***************************************************

** Groupwise Heteroscedastisity test, Wald modified test
** dont require e~N(.) errors normality
* findit xttest3
* ssc install xttest3
xtreg lmilk lgdp_ppc, fe
xttest3

* heteroskedasticity test via LR
set matsize 800
xtgls lmilk lgdp_ppc, igls panels(heteroskedastic)
est store hetero
xtgls lmilk lgdp_ppc
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

newey lmilk lgdp_ppc lyoghurt i.country, lag(2) force
est store newey


** Dryscoll-Kraay estimator: a/c in N and T + h/h in T
**Sturcture of errors assumes heteroskedastic, autocorrelated up to some lag
** and possibly correlated between the groups (objects)
* findit xtscc
* scc install xtscc
xtscc lmilk lgdp_ppc, fe lag(2)
est store fe_dris_kraay
** In econometrics the more flexible estimators(model, method etc.) you use, the less you can get..
** For ex., If you are sure that there is no spartial a/c,
** if Driscoll& Cray SE are used then you will probably get larger s.e. 
** Usual rule: The more flexible model(or method) you have the larger s.e. will be


** Since have a large N, small T, -robust- (or -cluster-) option
** accomodates for both heteroskedasticity and autocorrelation
** Remember Wald test give us result that sigma(i) not= sigma(j),
** and Wooldridge test detects serial a/c, so robust option may be also appropriate here
xtreg lmilk lgdp_ppc, fe robust
est store fe_robust


est table fe fe_robust fe_dris_kraay, keep(lgdp_ppc _cons) b(%7.4f) p


** at this moment we only reestimate standart errors of out coeffs, but
** we also can estimate model with the relaxed assumption of either h/s,
** and a/c or both.

** Serial a/c can of 1st order can be modelled via xtregar
** here we explicitly assume that e~AR(1)
xtregar lmilk lgdp_ppc, fe 
est store xtregar_fe

* also xtgls command can modell both problems
* here we set panels(hetero) option in order to count for possible h/s
xtgls lmilk lgdp_ppc, panels(hetero) force
est store gls_h

** hetero(groupwise) + serial autocorrelation also can be modelled via xtgls
** just by adding option corr()
xtgls lmilk lgdp_ppc, panels(hetero) corr(ar1) force
est store gls_h_ar

** ar1 option estimate one serial a/c coefficient for all objects
** However, a/c coefficients may be different for different objects
** psar1 option estimes unique serial a/c coefficients for each object

** ar option: eit=a*eit-1+vit
** psar option eit=ai*eit-1+vit

** FGLS better works in case of N<T, badly work in case of small panels
xtgls lmilk lgdp_ppc, panels(hetero) corr(psar1) force
est store gls_h_psar

** However, FGLS can give non-conservative s.e. in case of small N and T
** s.e. may be smaller than they are actually must be
** Espessially this problem arises in case of T<=N

** In this case other approach of Beck&Katz (1995) called 
** "panel-corrected standard errors" is more appropriate

** by default heteroskedastic and autocorrelated errors
** in time and objects dimentions are assumed,
** according to AR(1) process
qui xtpcse lmilk lgdp_ppc i.country, corr(ar1)
est store pcse_fe_ar

** The tables with all estimated models for comparison purposes
est table pool fe fe_robust fe_dris_kraay gls_h gls_h_ar gls_h_psar xtregar_fe pcse_fe_ar, keep(lgdp_ppc _cons) b(%7.4f) star(0.1 0.05 0.01)
est table pool fe fe_robust fe_dris_kraay gls_h gls_h_ar gls_h_psar xtregar_fe pcse_fe_ar, keep(lgdp_ppc _cons) b(%7.4f) se t
