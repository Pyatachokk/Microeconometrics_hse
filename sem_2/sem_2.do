*** DATA: milkfood.dta ***

/* Is it possible to make a panel? (poolability) */

replace milk=ln(milk)
replace gdp_ppc=ln(gdp_ppc)

gen con=country if country<18
replace con=150 if country==18
replace con=country-1 if country>18

  
/* by country */

** 
egen mcmilk=mean(milk), by(country)
gen dcmilk=milk-mcmilk

egen mcgdp_ppc=mean(gdp_ppc), by(country)
gen dcgdp_ppc=gdp_ppc-mcgdp_ppc
 
**************************************************
  
/* оценивание модели (0) без ограничений */
scalar rss_0=0
scalar n_0=0
forvalues i=1/30 {
 *display "country=`i'"
 quiet reg dcmilk dcgdp_ppc if con==`i'
 est store reg`i' 
 scalar rss`i'=e(rss)
 scalar n`i'=e(N)
 scalar rss_0=rss_0+rss`i'
 scalar n_0=n_0+n`i'
 }
 *** k - number of all parameters in model (alphas and betas)
scalar k_0=30*(2)
scalar df_0=(n_0-k_0)
est tab reg1 reg2 reg3 reg4 reg5 reg6 reg7, star(0.1 0.05 0.01) b(%7.4f)

/* оценивание модели с ограничением (1) */
scalar rss_1=0
scalar n_1=0
reg dcmilk dcgdp_ppc, noconst
scalar rss_1= e(rss)
scalar n_1=e(N)
** number of K=30dummies+1beta
scalar k_1=31
scalar df_1=(n_1-k_1)

/* оценивание модели с ограничением (2) */
scalar rss_2=0
scalar n_2=0
reg milk gdp_ppc
scalar rss_2 = e(rss)
scalar n_2=e(N)
scalar k_2=(2)
scalar df_2=(n_2-k_2)

/* вычисление тестовых статистик и их p-values */
/* test models 1 vs 0 */
scalar fh1=((rss_1-rss_0)/(df_1-df_0))/((rss_0)/(df_0))
scalar pval1 = Ftail(df_1-df_0,df_0,fh1)
** 

/* test models 2 vs 0 */
scalar fh2=((rss_2-rss_0)/(df_2-df_0))/((rss_0)/(df_0))
scalar pval2 = Ftail(df_2-df_0,df_0,fh2)

/* test models 2 vs 1 */
scalar fh3=((rss_2-rss_1)/(df_2-df_1))/((rss_1)/(df_1))
scalar pval3 = Ftail(df_2-df_1,df_1,fh3)

/* просмотр результатов */
display "test 1"
scalar list fh1 pval1 
display "test 2"
scalar list fh2 pval2   
display "test 3"
scalar list fh3 pval3





**************************************************
**************************************************
/* by years */
egen mtmilk=mean(milk), by(t)
gen dtmilk=milk-mtmilk

egen mtgdp_ppc=mean(gdp_ppc), by(t)
gen dtgdp_ppc=gdp_ppc-mtgdp_ppc

/* оценивание модели (0) без ограничений */
scalar rss_0=0
scalar n_0=0
forvalues y=1993/2008 {
 *display "t=`y'"
 quiet reg dtmilk dtgdp_ppc if t == `y'
 est store reg`y' 
 scalar rss`y'=e(rss)
 scalar n`y'=e(N)
 scalar rss_0=rss_0+rss`y'
 scalar n_0=n_0+n`y'
 }
scalar k_0=16*(2)
scalar df_0=(n_0-k_0)
est tab reg1 reg2 reg3 reg4 reg5 reg6 reg7, star(0.1 0.05 0.01) b(%7.4f)

/* оценивание модели с ограничением (1) */
scalar rss_1=0
scalar n_1=0
reg dtmilk dtgdp_ppc, noconst
scalar rss_1= e(rss)
scalar n_1=e(N)
scalar k_1=1+16
scalar df_1=(n_1-k_1)

/* оценивание модели с ограничением (2) */
scalar rss_2=0
scalar n_2=0
reg milk gdp_ppc
scalar rss_2 = e(rss)
scalar n_2=e(N)
scalar k_2=(2)
scalar df_2=(n_2-k_2)

/* вычисление тестовых статистик и их p-values */
/* test 1 */
scalar fh1=((rss_1-rss_0)/(df_1-df_0))/((rss_0)/(df_0))
scalar pval1 = Ftail(df_1-df_0,df_0,fh1)

/* test 2 */
scalar fh2=((rss_2-rss_0)/(df_2-df_0))/((rss_0)/(df_0))
scalar pval2 = Ftail(df_2-df_0,df_0,fh2)

/* test 3 */
scalar fh3=((rss_2-rss_1)/(df_2-df_1))/((rss_1)/(df_1))
scalar pval3 = Ftail(df_2-df_1,df_1,fh3)

/* просмотр результатов */
display "test 1"
scalar list fh1 pval1 
display "test 2"
scalar list fh2 pval2   
display "test 3"
scalar list fh3 pval3
