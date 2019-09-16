*************************************************
* Ratnikova T.A. Applyed microeconometrics  2019*
* Analysis of poolability data to panel  10.09  *
*************************************************

xtset country t
xtdes
xtline milk, overlay legend(off) title(milk prices)
xtline milk, i(name) t(t)
xtsum country t gdp_ppc milk faoprice

/* Is our panel balance for our model? */
egen cntm=count(milk), by(country)
egen cntu=count(gdp_ppc), by(country)
browse country t cntm cntu

/* Renumbering of country */
gen con=country if country<18
replace con=country-1 if country>18
replace con=31 if country==18

kdensity milk
gen lmilk=ln(milk)
gen lgdp_ppc=ln(gdp_ppc)
kdensity lmilk
kdensity lgdp_ppc

/* Generation means on t by each country */
egen milmilk=mean(lmilk), by(country) 
egen milgdp_ppc=mean(lgdp_ppc), by(country)

/* Generation deviations from means on t by each country */
gen dilmilk=lmilk-milmilk
gen dilgdp_ppc=lgdp_ppc-milgdp_ppc

/* Estimation of model (0) without restriction */
scalar rss_ur=0
scalar n_ur=0
scalar df_ur=0
forvalue i=1/30 {
qui reg dilmilk dilgdp_ppc if con==`i'
scalar z`i'=e(rss)
scalar df`i'=e(df_r)
scalar n`i'=e(N)
scalar rss_ur=rss_ur+z`i'
scalar n_ur=n_ur+n`i'
scalar df_ur=df_ur+df`i'
scalar list rss_ur n_ur df_ur 
}

scalar list rss_ur n_ur df_ur 

/* Estimation of model (1) with FE of country */
qui reg dilmilk dilgdp_ppc
scalar rss_r1 = e(rss)
scalar n_r1=e(N)
scalar df_r1=e(df_r)
scalar list rss_r1 n_r1 df_r1

scalar list rss_r1 n_r1 df_r1 
scalar df_r1_cor = df_r1 - 29
scalar list rss_r1 n_r1 df_r1_cor

/* Estimation of model (2) Pool */
qui reg lmilk lgdp_ppc 
scalar rss_r2 = e(rss)
scalar n_r2=e(N)
scalar df_r2=e(df_r)
scalar list rss_r2 n_r2 df_r2 

/* Calculation of F-statistics and  p-values */
/*********************************************************/
scalar fh1 =((rss_r1 - rss_ur)/(df_r1_cor-df_ur))/(rss_ur/df_ur)
scalar pval1 = Ftail(df_r1_cor-df_ur,df_ur,fh1)

scalar fh2 =((rss_r2 - rss_ur)/(df_r2-df_ur))/(rss_ur/df_ur)
scalar pval2 = Ftail(df_r2-df_ur,df_ur,fh2)

scalar fh3 =((rss_r2-rss_r1)/(df_r2-df_r1_cor))/(rss_r1/df_r1_cor)
scalar pval3 = Ftail(df_r2-df_r1_cor,df_r1_cor,fh3)
scalar list pval1 pval2 pval3  fh1 fh2 fh3

/* Calculation df check */
/*********************************************************/
scalar fh1 =((rss_r1 - rss_ur)/(60-31))/(rss_ur/(n_ur-60))
scalar pval1 = Ftail(60-31,n_ur-60,fh1)

scalar fh2 =((rss_r2 - rss_ur)/(60-2))/(rss_ur/(n_ur-60))
scalar pval2 = Ftail(60-2,n_ur-60,fh2)

scalar fh3 =((rss_r2-rss_r1)/(31-2))/(rss_r1/(n_r1-31))
scalar pval3 = Ftail(31-2,n_r1-31,fh3)

/* Display of results */
scalar list pval1 pval2 pval3  fh1 fh2 fh3
