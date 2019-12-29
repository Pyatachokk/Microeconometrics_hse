packages <- c("foreign", "ggplot2", "BaylorEdPsych", "miscTools", "pROC", "margins", 
              "boot", "lmtest", "numDeriv", "np", "glmx", "GJRM", "MASS", "brant",
              "VGAM", "EnvStats", "nnet" ,"mlogit", "AER", "MNP", "mnlogit", "pscl",
              "crch", "truncreg")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) 
{
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library("foreign")             # импорт данных
library("ggplot2")             # красивые графики
library("BaylorEdPsych")       # PseudoR2
library("miscTools")           # Медианные значния по столбцам
library("pROC")                # ROC кривая
library("margins")             # Предельные эффекты
library("boot")                # бутстрап
library("lmtest")              # дополнительные функции для тестирования гипотез
library("numDeriv")
library("np")
library("glmx")                # гетероскдестичный пробит
library("GJRM")                # очень крутой конструктор моделей, включающий двумерный пробит
library("MASS")                # порядковый пробит и логит
library("brant")               # тест бранта о parallel lines
library("oglmx")               # обобщенная порядковая модель
library("EnvStats")            # распределение гумбеля (extreme value distribution type 1)
library("nnet")                # Multinomial Logit
library("mlogit")              # Multinomial logit альтернативный пакет и iia тест
library("mnlogit")             # Multinomial logit еще одна альтернатива на которой работает iia
# из предыдущего пакета
library("AER")                 # тест на overdispersion и tobit модель
library("MNP")                 # Multinomial probit
library("pscl")                # Zero inflated model
library("Rchoice")             # Модель со случайными коэффициентами
library("VGAM")                # Модель tobit, второй метод
library("crch")                # Модель tobit, третий метод
library("truncreg")            # Регрессия с усечением
library("hpa")  

library(dplyr)
library(ggplot2)
library(gridExtra)
library(forecast)
library(texreg)
library(xtable)

library(lmtest)
library(xtable)

xtable.coeftest <- function (x, caption = NULL, label = NULL, align =     NULL, digits = NULL, 
                             display = NULL, ...) 
{
  class(x) <- "matrix"
  li<-list(coef=x)
  return(xtable:::xtable.summary.lm(li, caption = caption, label = label, 
                                    align = align, digits = digits, display = display))
}



# Отключим scientific notation
options(scipen = 999)          


# Часть 2. Обработка данных

# Загрузка
Sys.setlocale(locale="Russian")
new_data <- read.spss("r25i_os_31.sav",   # путь к файлу (замените на свой)
                      to.data.frame = TRUE,                                                            # загружаем как dataframe
                      use.value.labels = TRUE,                                                         # использовать численные значения                                                                              # вместо слов для некоторых переменных
                      max.value.labels = 30)

# Дополнительные функции

missing_to_NA <- function(my_variable,             # обрабатываемая переменная
                          to_numeric = FALSE)      # нужно ли перевести факторную переменную в численную
{
  if (is.numeric(my_variable)) #если мы имеем дело с количественной переменной
  {
    # В РМЭЗ отсутствие ответа на вопрос закодировано числами от 99999996 до
    # 99999999, поэтому перед началом анализа следует присвоить соответствующим
    # значениям пропуски
    my_variable[(my_variable == 99999996) | (my_variable == 99999997) |
                  (my_variable == 99999998) |(my_variable == 99999999)] <- NA
  } else                       #если мы столкнулись с факторной (категориальной) переменной
  {
    # Для факторных переменных при чтении данных из SAV. файла сохраняются строковые значения,
    # некоторые из которых необходимо вручную заменить на пропуски
    my_variable[(my_variable == "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ") |
                  (my_variable =="ОТКАЗ ОТ ОТВЕТА") |
                  (my_variable == "НЕТ ОТВЕТА")] <- NA
    my_variable=droplevels(my_variable)
  }
  if (to_numeric) #если нужно перевести фактоорную переменную в ччисленную
  {
    if (is.factor(my_variable))
    {
      my_variable <- as.character(my_variable)
    }
    my_variable <- as.numeric(my_variable)
  }
  return(my_variable)
}

NA_to_value<-function(my_variable, condition, value)
{
  my_variable[condition] <- value;
  return(my_variable)
}

factor_to_binary<-function(my_variable,factor_name)
{
  new_variable=rep(x = NA, times = length(my_variable)) # создаем переменную состояющую из пропусков
  new_variable[!is.na(my_variable)                      # заменяем на 0 все значения, не соответствующие пропуску
               & my_variable != "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ"
               & my_variable != "ОТКАЗ ОТ ОТВЕТА"
               & my_variable != "НЕТ ОТВЕТА"] <- 0
  new_variable[my_variable == factor_name] <- 1         # присваиваем значение 1 при определенных значениях
  # факторной переменной
  #с нужным значением фактора
  return(new_variable)
}
# перекодируем факторную переменную в количесивенную
factor_to_numeric <- function(x)
{
  return(as.numeric(levels(x))[x])
}

new_data = new_data[new_data$uh5=="МУЖСКОЙ",]
h <- data.frame('age' = missing_to_NA(new_data$u_age))


h$work = missing_to_NA(new_data$uj6.2)
h$child = as.numeric(factor_to_binary(missing_to_NA(new_data$uj72.173), "НЕТ ДЕТЕЙ МОЛОЖЕ 18") == 0)

h$lmotiv = log(missing_to_NA(new_data$uj452.2))

h$marriage <- factor_to_binary(missing_to_NA(new_data$u_marst),"Состоите в зарегистрированном браке")

h = na.omit(h)
h = h[h$work < 100,]
h = h[h$age >= 18,]

grid.arrange(gghistogram(h$work),gghistogram(h$age),gghistogram(h$child),gghistogram(h$lmotiv))

# Часть 3

# Часть 3.1

model_tobit <- AER::tobit(work ~ age + I(age^2) + child + lmotiv + I(lmotiv * child) , 
                          left = 30, 
                          data = h)
summary(model_tobit)

xtable(coeftest(model_tobit), digits = c(4,4,4,4,4))
# Часть 3.4

# достанем фрейм и реализации оценок коэффициентов
model_tobit_frame <- model.frame(model_tobit)
model_tobit_coef <- coef(model_tobit)
sigma_tobit <- model_tobit$scale


me_hid_age = model_tobit_coef["age"] + 2 * model_tobit_coef["I(age^2)"] * median(model_tobit_frame$age)

me_hid_child = model_tobit_coef["child"] + 2 * model_tobit_coef["I(lmotiv * child)"] * median(model_tobit_frame$lmotiv)

me_hid_lmotiv = model_tobit_coef["lmotiv"] + 2 * model_tobit_coef["I(lmotiv * child)"] * round(median(model_tobit_frame$child))

median_ind = c(median(model_tobit_frame$age), round(median(model_tobit_frame$child)), median(model_tobit_frame$lmotiv))
names(median_ind) = c("Возраст", "Наличие несов. детей", "Мотивация")
xtable(t(as.matrix(median_ind)))


median_table = c(me_hid_age,me_hid_child, me_hid_lmotiv)
names(median_table) = c("Возраст", "Наличие несов. детей", "Мотивация")
xtable(t(as.matrix(median_table)))

# Часть 3.6
median_vec = t(data.frame(as.vector(colMedians(model_tobit_frame))[2:7]))
colnames(median_vec) = colnames(model_tobit_frame)
y_s = predict(model_tobit, newdata = data.frame(median_vec))
me_prob_age = (me_hid_age / sigma_tobit) * dnorm(y_s, mean = 30, sd = sigma_tobit)

me_prob_child = (me_hid_child / sigma_tobit) * dnorm(y_s, mean = 30, sd = sigma_tobit)

me_prob_lmotiv = (me_hid_lmotiv / sigma_tobit) * dnorm(y_s, mean = 30, sd = sigma_tobit)


median_prob_table = c(median(me_prob_age),median(me_prob_child), median(me_prob_lmotiv))
names(median_prob_table) = c("Возраст", "Наличие несов. детей", "Мотивация")
median_prob_table 

xtable(t(as.matrix(median_prob_table)), digits = c(5,5,5,5))

# Часть 3.7

my_vec = c(1,21,441, 0, log(200000), 0)

my_y_hid = my_vec %*% model_tobit_coef
my_y = pnorm(my_y_hid, mean = 30, sd = sigma_tobit) * my_y_hid + sigma_tobit * dnorm(my_y_hid, mean = 30, sd = sigma_tobit)
my_prob = pnorm(my_y_hid, mean = 30, sd = sigma_tobit)


xtable(t(as.matrix(my_vec)))
xtable(t(as.matrix(c(my_y_hid, my_y, my_prob))))

#Часть 3.8


me_y_age = (me_hid_age / sigma_tobit)*dnorm(y_s, mean = 30, sd = sigma_tobit)*y_s + 
        pnorm(y_s, mean = 30, sd = sigma_tobit) * me_hid_age + sigma_tobit * ((30 - y_s)/sigma_tobit) * dnorm(y_s, mean = 30, sd = sigma_tobit)


# Часть 4

# Часть 4.1
model_tobit_r <- AER::tobit(work ~ age + I(age^2) + child + lmotiv , 
                          left = 30, 
                          data = h)
summary(model_tobit_r)

lnL_UR = logLik(model_tobit)
lnL_R = logLik(model_tobit_r)

lr_stat <- 2 * (lnL_UR - lnL_R)
p_value <- as.numeric(1 - pchisq(q = lr_stat, df = 1))

# H0 rejected at 5%

# Часть 4.2

model_tobit_mar = AER::tobit(work ~ age + I(age^2) + child + lmotiv+ I(lmotiv * child) , 
                            left = 30, 
                            data = h[h$marriage == 1,])


model_tobit_nomar = AER::tobit(work ~ age + I(age^2) + child + lmotiv + I(lmotiv * child) , 
                            left = 30, 
                            data = h[h$marriage == 0,])
lnL_R = logLik(model_tobit)
lnL_UR = logLik(model_tobit_mar) + logLik(model_tobit_mar)
lr_stat <- 2 * (lnL_UR - lnL_R)


p_value <- as.numeric(1 - pchisq(q = lr_stat, df = 7))


# H0 not rejected, models are similar

# Часть 4.4

model_tobit_het <- crch(work ~ age + I(age^2) + child + lmotiv + I(lmotiv * child) | age , 
                        data = h,
                        dist = "gaussian", left = 30, link.scale = "log")
summary(model_tobit_het)

lnL_R = logLik(model_tobit)
lnL_UR = logLik(model_tobit_het)
lr_stat <- 2 * (lnL_UR - lnL_R)
p_value <- as.numeric(1 - pchisq(q = lr_stat, df = 1))

xtable(coeftest(model_tobit_het), digits = c(4,4,4,4,4))


# Часть 5.1

model_truncreg <- truncreg(work ~ age + I(age^2) + child + lmotiv+ I(lmotiv * child), 
                           data = h_train[h$work > 30,], 
                           direction = "left", point = 30)
summary(model_truncreg)
xtable(coeftest(model_truncreg), digits = c(4,4,4,4,4))


model_linear = lm(work ~ age + I(age^2) + child + lmotiv+ I(lmotiv * child),
                  data = h)


xtable(coeftest(model_linear), digits = c(4,4,4,4,4))


# Часть 5.4
h_train = head(h, round(nrow(h)*0.8))
h_test =  tail(h, round(nrow(h)*0.2))


model_truncreg <- truncreg(work ~ age + I(age^2) + child + lmotiv+ I(lmotiv * child), 
                           data = h_train[h_train$work > 30,], 
                           direction = "left", point = 30)
summary(model_truncreg)
xtable(coeftest(model_truncreg), digits = c(4,4,4,4,4))

mse_trunc = mean((h_test$work - predict(model_truncreg, newdata = h_test))^2)

model_linear = lm(work ~ age + I(age^2) + child + lmotiv+ I(lmotiv * child),
                  data =  h_train[h_train$work > 30,])

mse_lin = mean((h_test$work - predict(model_linear, newdata = h_test))^2)

xtable(coeftest(model_linear), digits = c(4,4,4,4,4))

model_tobit <- AER::tobit(work ~ age + I(age^2) + child + lmotiv + I(lmotiv * child) , 
                          left = 30, 
                          data = h_train)

mse_tobit = mean((h_test$work - predict(model_tobit, newdata = h_test))^2)

xtable(t(as.matrix(c(mse_trunc, mse_lin, mse_tobit))), digits = c(1,1,1,1))
