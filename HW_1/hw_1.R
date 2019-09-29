library(rio)
library(ggplot2)
library(tidyverse)
library(gplots)
library(MASS)

data = import("all_banks_for_students_modified.dta")
data = filter(data, month_num >= 612, month_num <=623)

summary(data)
data = mutate(data, ratio = (credb + credfb + credf + credh)/assets)

# Гетерогенность
plotmeans(ratio ~ month, data = data)

# Гистограмма
ggplot(data, aes(x=ratio)) + 
  geom_histogram(bins = 50, color="darkblue", fill="lightblue", alpha = 0.5, aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=.03)+
  stat_function(fun=dnorm,args=fitdistr(data$ratio,"normal")$estimate)

# Pool и гетерогенность
lsdv_small = lm(ratio ~ usd_av + factor(month) - 1, data=data)
yhat_lsdv <- lsdv_small$fitted.values
g <- ggplot(data, aes(money, yhat_lsdv, col = as.factor(month)))
g + geom_point() +
  geom_smooth(aes(group = as.factor(month), col = as.factor(month)), method = 'lm', se = F) +
  geom_smooth(aes(col = 'Pooled OLS'),method = 'lm', se = F) +
  labs(title = 'Heterogeneous effect')



