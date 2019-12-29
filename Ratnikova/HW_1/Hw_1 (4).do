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


/* Plotting section */

foreach party of varlist  credits size liquidity roa capita inf_rate ref_rate{
	egen mt_`party'=mean(`party'), by(t)
	twoway (scatter `party' t) (connected mt_`party' t, sort), name(mt_`party')
}

graph combine mt_credits mt_size mt_liquidity mt_roa mt_capita mt_inf_rate
graph combine mt_ref_rate


foreach party of varlist  credits size liquidity roa capita {
	graph box `party', over(t) name(box_`party')
}

graph combine box_credits box_size box_liquidity box_roa box_capita


xtgraph credits, name(x)
xtgraph size, name(y)
xtgraph liquidity, name(z)
xtgraph roa, name(a)
xtgraph capita, name(b)


histogram credits, name(h_1) normal
histogram size, name(h_2)
histogram liquidity, name(h_3)
histogram roa, normal name(h_4)
histogram capita, name(h_5) normal 


graph combine h_1 h_2 h_3 h_4 h_5
histogram credits, by(t) normal


correlate credits size liquidity roa capita inf_rate ref_rate


/* Outliers filtering */

gen isbad = liquidity > 0.5 | roa > 0.85 | size > 0.05
bysort bank (isbad) : drop if isbad[_N]
xtset bank month_num
xtdes

/* Individual heterogenity */

scalar rss_ur=0
scalar n_ur=0
scalar df_ur=0
forvalue i=1/678 {
qui reg credits liquidity capita roa size inf_rate ref_rate if bank==`i'
scalar z`i'=e(rss)
scalar df`i'=e(df_r)
scalar n`i'=e(N)
scalar rss_ur=rss_ur+z`i'
scalar n_ur=n_ur+n`i'
scalar df_ur=df_ur+df`i'
scalar list rss_ur n_ur df_ur 
}

scalar list rss_ur n_ur df_ur 




***
qui reg credits liquidity capita roa size inf_rate ref_rate
scalar rss_r1 = e(rss)
scalar n_r1=e(N)
scalar df_r1=e(df_r)
scalar list rss_r1 n_r1 df_r1

scalar list rss_r1 n_r1 df_r1 
scalar df_r1_cor = df_r1 - 677
scalar list rss_r1 n_r1 df_r1_cor


***
qui reg credits liquidity capita roa size inf_rate ref_rate
scalar rss_r2 = e(rss)
scalar n_r2=e(N)
scalar df_r2=e(df_r)
scalar list rss_r2 n_r2 df_r2 


scalar fh1 =((rss_r1 - rss_ur)/(df_r1_cor-df_ur))/(rss_ur/df_ur)
scalar pval1 = Ftail(df_r1_cor-df_ur,df_ur,fh1)

scalar fh2 =((rss_r2 - rss_ur)/(df_r2-df_ur))/(rss_ur/df_ur)
scalar pval2 = Ftail(df_r2-df_ur,df_ur,fh2)

scalar fh3 =((rss_r2-rss_r1)/(df_r2-df_r1_cor))/(rss_r1/df_r1_cor)
scalar pval3 = Ftail(df_r2-df_r1_cor,df_r1_cor,fh3)
scalar list pval1 pval2 pval3  fh1 fh2 fh3




/* Time heterogenity */

scalar rss_ur=0
scalar n_ur=0
scalar df_ur=0
forvalue i=612/623{
qui reg credits liquidity capita roa size inf_rate ref_rate if month==`i'
scalar z`i'=e(rss)
scalar df`i'=e(df_r)
scalar n`i'=e(N)
scalar rss_ur=rss_ur+z`i'
scalar n_ur=n_ur+n`i'
scalar df_ur=df_ur+df`i'
scalar list rss_ur n_ur df_ur 
}

scalar list rss_ur n_ur df_ur 


***
qui reg credits liquidity capita roa size inf_rate ref_rate
scalar rss_r1 = e(rss)
scalar n_r1=e(N)
scalar df_r1=e(df_r)
scalar list rss_r1 n_r1 df_r1

scalar list rss_r1 n_r1 df_r1 
scalar df_r1_cor = df_r1 - 11
scalar list rss_r1 n_r1 df_r1_cor


***
qui reg credits liquidity capita roa size inf_rate ref_rate
scalar rss_r2 = e(rss)
scalar n_r2=e(N)
scalar df_r2=e(df_r)
scalar list rss_r2 n_r2 df_r2 


scalar fh1 =((rss_r1 - rss_ur)/(df_r1_cor-df_ur))/(rss_ur/df_ur)
scalar pval1 = Ftail(df_r1_cor-df_ur,df_ur,fh1)

scalar fh2 =((rss_r2 - rss_ur)/(df_r2-df_ur))/(rss_ur/df_ur)
scalar pval2 = Ftail(df_r2-df_ur,df_ur,fh2)

scalar fh3 =((rss_r2-rss_r1)/(df_r2-df_r1_cor))/(rss_r1/df_r1_cor)
scalar pval3 = Ftail(df_r2-df_r1_cor,df_r1_cor,fh3)
scalar list pval1 pval2 pval3  fh1 fh2 fh3


/* Models without time effect */
reg credits liquidity capita roa size inf_rate ref_rate
est store pool
xtreg credits liquidity capita roa inf_rate ref_rate, fe
est store fe
xtreg credits liquidity capita roa size inf_rate ref_rate, re
xttest0
est store re
est tab pool fe re, b(%7.4f) stats (N r2) se
est tab pool fe re, b(%7.4f) stats (N r2) star
hausman fe re

/* Models with time effect */
quietly tabulate t, generate(new_)
reg lmilk lgdp_ppc new_2-new_16
est store pool_t
xtreg lmilk lgdp_ppc new_2-new_16, fe
est store fe_t
xtreg lmilk lgdp_ppc new_2-new_16, re
xttest0
est store re_t
est tab pool_t fe_t re_t, b(%7.4f) stats (N r2) se
est tab pool_t fe_t re_t, b(%7.4f) stats (N r2) star
hausman fe_t re_t



/*including the time effect*/
quietly tabulate t, generate(new_)
reg credits liquidity capita roa size inf_rate ref_rate new_1 - new_12
est store pool_t

xtreg credits liquidity capita roa size inf_rate ref_rate new_1 - new_12, fe
est store fe_t

xtreg credits liquidity capita roa size inf_rate ref_rate new_1 - new_12, re
est store re_t
hausman fe_t re_t


xtreg credits liquidity capita roa inf_rate ref_rate new_1 - new_12, fe
