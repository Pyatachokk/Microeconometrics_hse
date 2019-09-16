xtset country t

replace milk = ln(milk)
replace gdp_ppc = ln(gdp_ppc)

hist milk, bin(100)
kdensity milk, normal

reg milk gdp_ppc
est store pool

xtreg milk gdp_ppc, fe
est store fe
predict uhat, u
xtsum uhat


xtreg milk gdp_ppc, re
est store re
* Compare RE and POOLED
xttest0

hausman fe re

* pooled with t
xtreg milk gdp_ppc i.t, fe
testparm i.t
est store fe_t

* pooled with t
xtreg milk gdp_ppc i.t, re
est store re_t

hausman fe_t re_t
