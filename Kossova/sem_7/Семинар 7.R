# Потанин Богдан Станиславович
# Микроэконометрика
# Семинар №7 - Усеченные модели
#--------------
# РАЗДЕЛ №0. Подготовка данных
#--------------
# Подключение библиотек
# Важно: если у вас нет какой-то из этих блиблиотек, то
# её следует установить
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
library("hpa")                 # моменты усеченного нормального распределения
# Отключим scientific notation
options(scipen = 999)            
# Загрузка данных
new_data <- read.spss("E:\\Преподавание\\Микроэконометрика\\R\\Семинар 2\\r25i_os_31.sav",   # путь к файлу (замените на свой)
            to.data.frame = TRUE,                                                            # загружаем как dataframe
            use.value.labels = TRUE,                                                         # использовать численные значения 
                                                                                             # вместо слов для некоторых переменных
                                                                                             
            max.value.labels = 30)                                                           # сколько различных значений должна принимать
                                                                                             # переменная, чтобы считаться численной, а не факторной
# Инициализируем некоторые вспомонательные фукции для обработки данных
# Добавление NA в непрерывные переменные
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
# Присваиваем некоторое значение вместо пропуска при соблюдении условия
NA_to_value<-function(my_variable, condition, value)
{
  my_variable[condition] <- value;
  return(my_variable)
}
# Перекодирование категориальных переменных в бинарные
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
# Создадим переменные
# Создадим отдельную переменную для хранения обработанных данных
# и сразу вместе с ней переменную на возраст
# Пропуски в РМЭЗ получают значения 99999997, 99999998 и 99999999,
# поэтому, при создании переменных от них следует избавляться, что можно
# осуществить при помощи функции missing_to_NA
summary(new_data$u_age)
h <- data.frame('age' = missing_to_NA(new_data$u_age))
# Переменная - работает индивид или нет
summary(new_data$uj77)
h$work <- factor_to_binary(new_data$uj77,
                        "РЕСПОНДЕНТ СЕЙЧАС РАБОТАЕТ, НАХ-СЯ В ОПЛАЧИВ. ИЛИ НЕОПЛАЧ.ОТПУСКЕ, В Т.Ч. ДЕКРЕТНОМ ИЛИ ПО УХОДУ ЗА РЕБЕНКОМ ДО 3 ЛЕТ")
# Переменная - образование
summary(new_data$u_diplom)
h$educ_1 <- factor_to_binary(new_data$u_diplom, "законченное среднее образование")
h$educ_2 <- factor_to_binary(new_data$u_diplom, "законченное среднее специальное образование")
h$educ_3 <- factor_to_binary(new_data$u_diplom, "законченное высшее образование и выше")
# Переменная - наличие детей
summary(new_data$uj72.171)
h$children <- factor_to_binary(new_data$uj72.171, "Да")
# Переменная - количество детей
h$children_n <- factor_to_numeric(missing_to_NA(new_data$uj72.172))
h$children_n[h$children == 0] <- 0
# Переменная - количество несовершеннолетних детей
h$children_n_18 <- factor_to_numeric(missing_to_NA(new_data$uj72.173))
h$children_n_18[new_data$uj72.173 == "НЕТ ДЕТЕЙ МОЛОЖЕ 18"] <- 0
h$children_n_18[h$children_n == 0] <- 0
# Переменная - пол
h$male <- 0
h$male[new_data$uh5=="МУЖСКОЙ"] <- 1
# Переменные - вес, рост и BMI (индекс массы тела)
h$weight <- missing_to_NA(new_data$um1)
h$height <- missing_to_NA(new_data$um2)
h$BMI <- h$weight / (h$height / 100) ^ 2
# Переменная - состоит в оффициальном браке
h$marriage <- factor_to_binary(missing_to_NA(new_data$u_marst),"Состоите в зарегистрированном браке")
# Определим критерий попадания в выборку
selection_condition <- (h$male == 1 & h$age >= 18)   # построим модель для совершеннолетних мужчин
# На зарплату
h$wage <- missing_to_NA(new_data$uj13.2)
h$wage[h$work == 0] <- 0
#--------------
#РАЗДЕЛ №1. Анализ симулированных данных
#--------------
# Зададим количество наблюдений
n <- 10000
# Истинные значения регрессионных коэффициентов
beta_0 <- 1
beta_1 <- 2
beta_2 <- 1.5
beta_3 <- -2
beta <- c(beta_0, beta_1, beta_2, beta_3)
# Сгенерируем независимые переменные из многомерного
# нормального распределения
X <- rmvn(n,                                 # количество наблюдений
          c(1, 1, 1),                        # вектор математических ожадинй         
          matrix(c(1, 0.5, 0.5,              # ковариционная матрица
                   0.5, 1, 0.5,
                   0.5, 0.5, 1), ncol=3))
# Случайная ошибка
sigma = 2.5
epsilon <- rnorm(n, mean = 0, sd = sigma)
# Не усеченная зависимая переменная
y_star <- beta_0 + beta_1 * X[,1] + beta_2 * X[,2] + beta_3 * X[,3] + epsilon
# Точки усечения
tr_low <- -1
tr_up <- 1.5
# Усеченная зависимая переменная
y <- y_star
y[y_star <= tr_low] <- tr_low
y[y_star >= tr_up] <- tr_up
# Запишем функцию правдоподобия
lnL <- function(x, y, X, tr_low, tr_up)
{
  sigma <- x[1]
  beta <- matrix(x[-1], ncol = 1)
  
  X <- cbind(1, X)
  
  XB <- X %*% beta
  
  n <- nrow(X)
  
  L_vector <- rep(NA, n)
  
  cond_1 <- (y == tr_low)
  cond_2 <- (y == tr_up)
  cond_3 <- ((y > tr_low) & (y < tr_up))
  
  L_vector[cond_1] <- pnorm(y[cond_1] - XB[cond_1,], 0, sigma)
  L_vector[cond_2] <- 1 - pnorm(y[cond_2] - XB[cond_2,], 0, sigma)
  L_vector[cond_3] <- dnorm(y[cond_3] - XB[cond_3,], 0, sigma)
  
  return(sum(log(L_vector)))
}
# Осуществим оптимизацию
x0 <- c(1, 0, 0, 0, 0)
opt_mle <- optim(par = x0, fn = lnL, 
                 X = X,
                 y = y,
                 tr_low = tr_low,
                 tr_up = tr_up,
                 method = "Nelder-Mead", 
                 hessian = TRUE,                          # возвращаем Гессиан
                 control = list("maxit" = 1000,           # максимальное количество итераций
                                fnscale = -1,             # ставим -1 чтобы сделать задачу максимизационной
                                "reltol" = 1e-32))        # условие остановки алгоритма (termination condition)
x1 <- opt_mle$par
# Сравним истинные и предсказанные знаечния
data.frame("beta" = c(beta_0, beta_1, beta_2, beta_3), 
           "beta_hat" = x1[2:5])
# Сравним с результатами, полученными при помощи МНК и сравнивая отношения коэффициентов
# убедимся в том, что разница существенна
  # Оценим МНК по всем наблюдениям
summary(model_lm <- lm(y~X[,1] + X[,2] + X[,3]))
coef_lm <- coef(model_lm)
  # Оценим МНК по неусеченным наблюдениям
no_tr_cond <- ((y > tr_low) & (y < tr_up))
summary(model_lm_1 <- lm(y[no_tr_cond]~X[no_tr_cond,1] + X[no_tr_cond,2] + X[no_tr_cond,3]))
coef_lm_1 <- coef(model_lm_1)
  # Подведем итоги
data.frame("beta" = c(beta_0, beta_1, beta_2, beta_3),
           "beta_tobit" = x1[2:5],
           "beta_lm" = coef_lm,
           "beta_lm_no_tr" = coef_lm_1)
# ЗАДАНИЯ
# 1. Запрограммируйте логафрим правдоподобия для усеченной регрессии
# 2. Проверьте устойчивость оценок к изменению распределения случайной ошибки. Попробуйте запрограммировать
#    функцию правдоподобия для альтетнативного распределения случайной ошибки.
#--------------
#РАЗДЕЛ №2. Анализ реальных данных
#--------------
# Для начала оценим обыкновенную регрессию
model_lm <- lm(log(wage + 1) ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
               data = h[selection_condition,])
model_lm_frame <- model.frame(model_lm)
summary(model_lm)
# Теперь воспользуемся тобит моделью
model_tobit <- AER::tobit(log(wage + 1) ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
                          left = 0, 
                          data = h[selection_condition,])
summary(model_tobit)
  # достанем фрейм и реализации оценок коэффициентов
model_tobit_frame <- model.frame(model_tobit)
model_tobit_coef <- coef(model_tobit)
sigma_tobit <- model_tobit$scale
# Получим предсказанные значения
  # математического ожидания логарифма заработной платы (y*), которое может быть и отрицательным
wage_predict_tobit <- predict(model_tobit)
  # вероятности того, что индивид будет иметь нулевую заработную плату
wage_prob_predict_tobit <- pnorm(-wage_predict_tobit, 0, sigma_tobit)
  # математическое ожидание логарифма заработной платы с учетом усечения
wage_predict_tobit_tr <- (wage_predict_tobit +                                                 # безусловная часть матожидания (xb)
                         truncatedNormalMoment(k = 1,                                          # условная случайная ошибка E(e|e>(-x*b))
                                               x_lower = - wage_predict_tobit,
                                               x_upper = rep(Inf,length(wage_predict_tobit)),
                                               mean = 0, sd = sigma_tobit))[,2] *              # берем только второй столбец с первым начальным моментом
                                                                                               # так как в первом столбец расположен нулевой начальный момент (то есть сплошные 1)
                                               (1 - wage_prob_predict_tobit)                   # домножаем на вероятность p(xb+e>0)
  # Убедимся, что усеченное математическое ожидание превышает неусеченное (подумайте почему)
cbind(wage_predict_tobit, wage_predict_tobit_tr)
# Посчитаем предельные эффекты по переменной BMI
  # на математическое ожидание заработной платы (y*)
me_tobit_BMI <- model_tobit$coefficients["BMI"]
  # на математическое ожидание заработной платы с учетом усечения в нуле (y)
me_tobit_BMI_tr <- model_tobit$coefficients["BMI"] * (1 - wage_prob_predict_tobit)
me_tobit_BMI_tr_mean <- mean(me_tobit_BMI_tr)
hist(me_tobit_BMI_tr)
  # на вероятность ненулевой заработной платы
me_tobit_BMI_pr <- model_tobit$coefficients["BMI"] * dnorm(wage_predict_tobit, 0, sigma_tobit)
# Повторим расчеты для предельного эффекта высшего образования по сравнению с отсутствием хотя бы среднего образования
  # на математическое ожидание заработной платы (y*)
me_tobit_educ_3 <- model_tobit$coefficients["educ_3"]
  # на математическое ожидание заработной платы с учетом усечения в нуле (y)
me_tobit_educ_3_tr <- model_tobit$coefficients["educ_3"] * (1 - wage_prob_predict_tobit)
me_tobit_educ_3_tr_mean <- mean(me_tobit_educ_3_tr)
hist(me_tobit_educ_3_tr)
  # на вероятность ненулевой заработной платы
wage_predict_tobit_no_educ <- wage_predict_tobit -                                      # ЗП без образования
                              model_tobit_frame$educ_3 * model_tobit_coef["educ_3"] -
                              model_tobit_frame$educ_2 * model_tobit_coef["educ_2"] -
                              model_tobit_frame$educ_1 * model_tobit_coef["educ_1"]
wage_predic_tobit_educ_3 <- wage_predict_tobit +  model_tobit_coef["educ_3"]            # ЗП с высшим образованием
me_tobit_educ_3_pr <- pnorm(wage_predic_tobit_educ_3, 0, sigma_tobit) - pnorm(wage_predict_tobit_no_educ, 0, sigma_tobit)
mean(me_tobit_educ_3_pr)
hist(me_tobit_educ_3_pr)
# ЗАДАНИЯ
# 1. Посчитайте предельный эффект возраста на y, y* и p(y>0)
# 2. Напишите функцию для автоматического расчета предельных эффектов по y при помощи численного дифференцирования
#--------------
#РАЗДЕЛ №3. Гетероскедастичность
#--------------
# Подробности https://journal.r-project.org/archive/2016-1/messner-mayr-zeileis.pdf
# Модель без гетероскедастичности
model_tobit <- crch(log(wage + 1) ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
                    data = h[selection_condition,],
                    dist = "gaussian", left = 0, link.scale = "log")
# Модель с гетераскедастичной ошибкой, дисперсия которой зависит от возраста и наличия высшего образования
model_tobit_het <- crch(log(wage + 1) ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI | age + educ_3, 
                        data = h[selection_condition,],
                        dist = "gaussian", left = 0, link.scale = "log")
summary(model_tobit_het)
# Проведем LR тест и убедимся, что нулевая гипотеза отвергается на любом разумном уровне значимости, а значит
# случайная ошибка гетероскедастични и без учета данного обстоятельства полученные оценки окажутся несостоятельными 
lr_test_statistic <- 2 * as.numeric(model_tobit_het$loglik - model_tobit$loglik)
lr_test_p_value <- 1 - pchisq(lr_test_statistic, 2)
# ЗАДАНИЯ
# 1. Проверьте, зависит ли дисперсия случайной ошибки от BMI.
# 2. Посчитайте предельный эффект по переменной BMI.
#--------------
#РАЗДЕЛ №4. Альтернативное распределение случайной ошибки
#--------------
# Случайные ошибки распределены по стьюденту с количеством степеней свободы, выступающим
# в качестве параметра
model_tobit_student = crch(log(wage + 1) ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
                           data = h[selection_condition,],
                           dist = "student", df=NULL, left= 0)
summary(model_tobit_student)
# Случайные ошибки подчиняются логистическому распределению
model_tobit_logistic = crch(log(wage + 1) ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
                           data = h[selection_condition,],
                           dist = "logistic", df=NULL, left= 0)
summary(model_tobit_student)
# Сравним коэффициенты моделе
data.frame("Нормальное распределение" = model_tobit$coefficients$location,
           "Распределение стьюдента" = model_tobit_student$coefficients$location,
           "Логистическое распределение" = model_tobit_logistic$coefficients$location)
# Сравним качество  моделей по AIC
data.frame("Нормальное распределение"=AIC(model_tobit),
           "Распределение стьюдента"=AIC(model_tobit_student),
           "Логистическое распределение"=AIC(model_tobit_logistic))
# ЗАДАНИЯ
# 1. Воплоите LM тест на нормальность, используя процедуру, описанную в учебнике Вербика (раздел 7.4.4)
# 2. Самостоятельно сделайте функцию, которая будет считать оценки тобит модели при допущении о том, что
#    случайные ошибки имеют extreme value distribution Type 1. См. функции pevd и devd.
# 3. Самостоятельно сделайте функцию, которая будет считать оценки тобит модели при допущении о том, что
#    случайные ошибки имеют распределение пирсона. См. пакет PearsonDS.
#--------------
#РАЗДЕЛ №5. Регрессия с усечением
#--------------
model_truncreg <- truncreg(log(wage + 1) ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
                           data = h[selection_condition,], 
                           direction = "left", point = 0)
summary(model_truncreg)
# Получим предсказанные значения безусловного математического ожидания логарифма заработной платы
predict(model_truncreg)
#--------------
#РАЗДЕЛ №6. Сравнение предсказательной силы
#--------------
# Разобьем выборку на train и test
  # общее число наблюдений
n <- nrow(model_lm_frame)
  # индексы наблюдений для тестовой выборки
train_ind <- sample(x = 1:n, size = (2 / 3) * n )
  # разбиваем выборку
train_frame <- model_lm_frame[train_ind,]
test_frame <- model_lm_frame[-train_ind,]
  # оцениваем модели по тестовой выборке
    # МНК
model_lm_train <- lm(train_frame[,1] ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, data = train_frame)
pr_lm <- predict(object = model_lm_train, newdata = test_frame)
mse_lm <- mean((pr_lm - test_frame$`log(wage + 1)`) ^ 2)
    # Тобит
model_tobit_train <- AER::tobit(train_frame[,1] ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
                                data = train_frame, left = 0)
pr_tobit <- predict(model_tobit_train, newdata = test_frame)
mse_tobit <- mean((pr_tobit - test_frame$`log(wage + 1)`) ^ 2)
    # Усеченная регрессия
model_truncreg_train <- truncreg(train_frame[,1] ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + BMI, 
                                 data = train_frame, 
                                 direction = "left", point = 0)
pr_truncreg <- predict(model_truncreg_train, newdata = test_frame)
mse_truncreg <- mean((pr_truncreg - test_frame$`log(wage + 1)`) ^ 2)
# Сравним mse
data.frame("МНК" = mse_lm, "Тобит" = mse_tobit, "Усеченная Регрессия" = mse_truncreg)
# ЗАДАНИЯ
# 1. Сравните MSE предсказывая вместо отрицательных значений заработной платы ноль
# 2. Сравните MSE удалив из выборки 0 и предсказывая значения для тобит модели как матожидание (y без звездочки)