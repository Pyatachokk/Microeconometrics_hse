*** DATA: milkfood.dta ***

/* preliminary analysis */
sum country t

twoway scatter milk gdp_ppc

***************************************************
/* country effect, by t */
egen mtmilk=mean(milk), by(t)
twoway (scatter milk t) (connected mtmilk t, sort), name(mtmilk)
egen mtgdp_ppc=mean(gdp_ppc), by(t)
twoway (scatter gdp_ppc t)(connected mtgdp_ppc t, sort), name(mtgdp_ppc)

graph combine mtmilk mtgdp_ppc

graph box milk, over(t)
graph hbox milk, over(name) 

/* time effect, by country */
egen mcmilk=mean(milk), by(country)
twoway (scatter milk country, sort) (scatter mcmilk country, sort), name(mcmilk)
egen mcgdp_ppc=mean(gdp_ppc), by(country)
twoway (scatter gdp_ppc country, sort) (scatter mcgdp_ppc country, sort), name(mcgdp_ppc)

graph combine mcmilk mcgdp_ppc
twoway (scatter milk country, sort) (scatter mcmilk country, sort)

***********************************************************
***********************************************************

/* panel data analysis */
xtset country t
xtdes

/* graphs */
xtline milk, i(name) t(t)
xtline milk, overlay i(name) t(t)
xtline milk, overlay i(country) t(t)

xtline milk if country!=24, overlay i(name) t(t)

	xtsum country t gdp_ppc cheese yoghurt butter margarine milk faoprice

 /*
 xtgraph yvar [if exp ][in exp ] , 
                [average(avname) bar(barname) model level(significance level)
                half group(var) offset(#) minobs(#) power(#) log(#) boxcox(#) 
                savedat(filename) v7 nograph list missing listwise i(varname) t(varname) 
                graph_options]
*/

xtgraph milk, level(99)

/* functional form */
histogram milk, normal
histogram milk, by(t) normal

gen lmilk=ln(milk)

histogram lmilk, normal
histogram lmilk, by(t) normal

	***
	histogram gdp_ppc, normal
	histogram gdp_ppc, by(t) normal
	histogram gdp_ppc, by(name) normal
	histogram gdp_ppc, by(name)

	gen lgdp_ppc=ln(gdp_ppc)

	histogram lgdp_ppc, normal
	histogram lgdp_ppc, by(t) normal
	histogram lgdp_ppc, by(name) normal 
* ---> clusters?

/* dependence */
xtline lmilk, i(country) t(gdp_ppc)
*xtline lmilk, i(country) t(lgdp_ppc)

