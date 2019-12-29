# Потанин Богдан Станиславович
# Микроэконометрика
# Семинар №6 - Моделированние частоты и вложенный выбор
#--------------
# РАЗДЕЛ №0. Подготовка данных
#--------------
# Подключение библиотек
# Важно: если у вас нет какой-то из этих блиблиотек, то
# её следует установить
packages <- c("foreign", "ggplot2", "BaylorEdPsych", "miscTools", "pROC", "margins", 
              "boot", "lmtest", "numDeriv", "np", "glmx", "GJRM", "MASS", "brant",
              "VGAM", "EnvStats", "nnet" ,"mlogit", "AER", "MNP", "mnlogit", "pscl")
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
library("AER")                 # тест на overdispersion
library("MNP")                 # Multinomial probit
library("pscl")                # Zero inflated model
library("Rchoice")             # Модель со случайными коэффициентами
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
#--------------
#РАЗДЕЛ №1. Анализ симулированных данных
#--------------
# Зададим количество наблюдений
n <- 10000
# Истинные значения регрессионных коэффициентов
beta_0 <- 0.1
beta_1 <- 0.2
beta_2 <- -0.3
beta_3 <- 0.5
beta <- c(beta_0, beta_1, beta_2, beta_3)
# Сгенерируем независимые переменные из многомерного
# нормального распределения
X <- rmvn(n,                                 # количество наблюдений
          c(1, 1, 1),                        # вектор математических ожадинй         
          matrix(c(1, 0.5, 0.5,              # ковариционная матрица
                   0.5, 1, 0.5,
                   0.5, 0.5, 1), ncol=3))
# Лямбда
lambda <- exp(beta_0 + beta_1 * X[,1] + beta_2 * X[,2] + beta_3 * X[,3])
# Зависимая переменная
y <- rpois(n, lambda)
# Запишем функцию правдоподобия
lnL <- function(x, y, X)
{
  beta <- matrix(x, ncol = 1)
  
  X <- cbind(1, X)
  
  lambda <- exp(X %*% beta)
  
  L_vec <- dpois(y, lambda)
  
  return(sum(log(L_vec)))
}
# Осуществим оптимизацию
x0 <- c(0,0,0,0)
opt_mle <- optim(par = x0, fn = lnL, 
                 X = X,
                 y = y,
                 method = "Nelder-Mead", 
                 hessian = TRUE,                          # возвращаем Гессиан
                 control = list("maxit" = 1000,           # максимальное количество итераций
                                fnscale = -1,             # ставим -1 чтобы сделать задачу максимизационной
                                "reltol" = 1e-32))        # условие остановки алгоритма (termination condition)
x1 <- opt_mle$par
# Сравним истинные и предсказанные знаечния
data.frame("beta" = c(beta_0, beta_1, beta_2, beta_3), 
           "beta_hat" = x1[1:4])
# Сравним с результатами, полученными при помощи МНК и сравнивая отношения коэффициентов
# убедимся в том, что разница несущественна
summary(model_lm <- lm(y~X[,1] + X[,2] + X[,3]))
coef_lm <- coef(model_lm)
# Сравним отношения коэффициентов к b1
data.frame("Истина" = beta / beta[2], "Пуассон" = x1 / x1[2], "МНК" = coef_lm / coef_lm[2])
# ЗАДАНИЯ
# 1. Попробуйте оценить модель предполагая альтернативную функциональную форму для лямбды, например,
#    используя квадрат вместо экспоненты. Проверьте, как изменится преимущественно полученных вами
#    оценок перед МНК и классической пуассоновской регрессией.
# 2. Придумайте собственное дискретное распределение с носителем (support) совпадающим со множеством всех целых
#    неотрицательных чисел. Оцените регрессию предполагая, что зависимая переменная подчиняется вашему распределению,
#    предварительное симулировав данные и запрограммировав функцию правдоподобия.
#--------------
#РАЗДЕЛ №2. Анализ реальных данных
#--------------
# Сделаем переменную на частоту езды на велосипеде
h$bycicle <- missing_to_NA(factor_to_numeric(new_data$um11314c))
h$bycicle[new_data$um11314a  == "Нет"] <- 0
  # Посмотрим, как часто встречаются различные частоты
summary(as.factor(h$bycicle))
  # Построим Пуассоновскую регрессию
h_poisson <- glm(bycicle ~ age + I(age ^ 2) + 
                           educ_1 + educ_2 + educ_3 + 
                           children + marriage + 
                           BMI +  
                           work, 
                           family="poisson", 
                           data = h[(h$male == 1) & (h$age >= 18), ])
summary(h_poisson)
  # Достанем фрейм модели и коэффициенты
frame_poisson <- model.frame(h_poisson)
coef_poisson <- coef(h_poisson)
  # Получим предсказанные значения и обратим внимание, что
  # они весьма неточные
pred_poisson <- predict(h_poisson)
cor(pred_poisson, frame_poisson$bycicle)
plot(pred_poisson, frame_poisson$bycicle)
  # Посчитаем среднеквадратическую ошибку предсказания
mse_poisson <- mean((pred_poisson - frame_poisson$bycicle) ^ 2)
  # Посчитаем предельный эффект на математическое ожидание числа поездок по возрасту
age_me_poisson <- (coef_poisson["age"] + 2 * coef_poisson["I(age^2)"] * frame_poisson["age"]) * pred_poisson
  # Обратите внимание что для переменных, входящих линейно, коэффициент показывает, во сколько раз
  # увеличится математическое ожидание зависимой переменной
  # Проверим есть ли overdispersion, в случае отклонение нулевой гипотезы которого модель
  # пуассоновской регрессии не применима, поскольку дисперсия зависимой переменной, вероятно, не
  # совпадает с математическим ожиданием
dispersiontest(h_poisson, trafo = 1)
# ЗАДАНИЯ
# 1. Проверьте, можем ли мы исключить переменную на высшее образование
# 2. Посчитайте предельный эффект работы на вероятность покататься хотя бы раз на велосипеде ни разу для индивида
#    с вашими характеристиками
# 3. Посчитайте предельный эффект BMI на вероятность покататься хотя бы раз на велосипеде ни разу для индивида
#    с вашими характеристиками
# 4. Напишите функцию которая, при помощи численного дифференцирования, будет считать предельные эффекты по
#    каждой переменной в модели
# 5. Оцените модель с собственным распределением на реальных данных
#--------------
#РАЗДЕЛ №3. Симуляционный анализ отрицательной биномиальной регрессии
#--------------
# Добавим параметр alpha отвечающий за overdispersion - насколько дисперсия превышает
# математическое ожидание зависимой переменной
# Зададим количество наблюдений
n <- 10000
# Истинные значения регрессионных коэффициентов
beta_0 <- 0.5
beta_1 <- 0.2
beta_2 <- -0.3
beta_3 <- 0.5
beta <- c(beta_0, beta_1, beta_2, beta_3)
alpha = 1
# Сгенерируем независимые переменные из многомерного
# нормального распределения
X <- rmvn(n,                                 # количество наблюдений
          c(1, 1, 1),                        # вектор математических ожадинй         
          matrix(c(1, 0.5, 0.5,              # ковариционная матрица
                   0.5, 1, 0.5,
                   0.5, 0.5, 1), ncol=3))
# Случайная ошибка  
epsilon <- log(rgamma(n = n, scale = 1 / alpha, shape = alpha))
# Лямбда
lambda <- exp(beta_0 + beta_1 * X[,1] + beta_2 * X[,2] + beta_3 * X[,3] + epsilon)
# Зависимая переменная
y <- rpois(n, lambda)
# Запишем функцию правдоподобия
lnL <- function(x, y, X)
{
  beta <- matrix(x[-1], ncol = 1)
  a <- x[1]
  
  X <- cbind(1, X)
  
  lambda <- exp(X %*% beta)
  
  lnL_vec <- (log(gamma(y + (1 / a))) -
             log(gamma(y + 1)) -
             log(gamma(1 / a)) + 
             (1 / a) * (log(1 / a) - log((1 / a + lambda))) + 
             y * (log((a * lambda)) - log((1 + a * lambda))))
  
  return(sum(lnL_vec))
}
# Осуществим оптимизацию
x0 <- c(0.1, 0.1, 0.1, 0.1, 0.1)
opt_mle <- optim(par = x0, fn = lnL, 
                 X = X,
                 y = y,
                 method = "Nelder-Mead", 
                 hessian = TRUE,                          # возвращаем Гессиан
                 control = list("maxit" = 1000,           # максимальное количество итераций
                                fnscale = -1,             # ставим -1 чтобы сделать задачу максимизационной
                                "reltol" = 1e-32))        # условие остановки алгоритма (termination condition)
x1 <- opt_mle$par
# Сравним истинные и предсказанные знаечния
data.frame("beta" = beta, 
           "beta_hat" = x1[-1])
# Сравним с результатами, полученными при помощи МНК и сравнивая отношения коэффициентов
# убедимся в том, что разница несущественна
summary(model_lm <- lm(y~X[,1] + X[,2] + X[,3]))
coef_lm <- coef(model_lm)
# Сравним отношения коэффициентов к b1
data.frame("Истина" = beta / beta[2], "Пуассон" = x1[-1] / x1[3], "МНК" = coef_lm / coef_lm[2])
#--------------
#РАЗДЕЛ №4. Отрицательная биномиальная регрессия
#--------------
# Оценим модель
model_nb <- glm.nb(bycicle ~ age + I(age ^ 2) + 
                             educ_1 + educ_2 + educ_3 + 
                             children + marriage + 
                             BMI +  
                             work, 
                             data = h[(h$male == 1) & (h$age >= 18), ])
summary(model_nb)
# Достанем фрейм модели и коэффициенты
frame_nb <- model.frame(model_nb)
coef_nb <- coef(model_nb)
# Получим предсказанные значения и обратим внимание, что
# они весьма неточные
pred_nb <- predict(model_nb)
cor(pred_nb, frame_nb$bycicle)
plot(pred_nb, frame_nb$bycicle)
# Посчитаем среднеквадратическую ошибку предсказания
mse_nb <- mean((pred_nb - frame_nb$bycicle) ^ 2)
# Посчитаем предельный эффект на математическое ожидание числа поездок по возрасту
age_me_nb <- (coef_nb["age"] + 2 * coef_nb["I(age^2)"] * frame_nb["age"]) * pred_nb
# ЗАДАНИЯ
# 1. Проверьте для каждой из ранее оцененных моделей, можем ли мы исключить переменную на высшее образование
# 2. Сравните оцененные модели при помощи критерия AIC
# 3. Определите, какая из моделей лучше всех предсказывает вне выборки
#--------------
#РАЗДЕЛ №5. Zero-inflated and Hurdle model
#--------------
# Подробности https://cran.r-project.org/web/packages/pscl/vignettes/countreg.pdf
# Эти типы регрессийи используются, когда в данных имеется много нулей.
# Представляют собой комбинацию бинарной (для предсказания нулей) и 
# пуассоновской/отрицательной биномиальной (для предсказания остальных значений) регрессий
# Zero-inflated model
zi_model <- zeroinfl(bycicle ~ age + I(age ^ 2) + 
                               educ_1 + educ_2 + educ_3 + 
                               children + marriage + 
                               BMI +  
                               work, 
                               data = h[(h$male == 1) & (h$age >= 18), ])
summary(zi_model)
# Предскажем
  # Реализация оценки математического ожидания числа поездок при условии, что их будет не ноль
zi_pred_count <- predict(zi_model, type = "count")
  # Вероятность определенного числа поездок
zi_pred_prob_count <- predict(zi_model, type = "prob")
  # Реализация оценки математического ожидания числа поездок
zi_pred_prob <- predict(zi_model, type = "response")
# Hurdle model
hm_model <- hurdle(bycicle ~ age + I(age ^ 2) + 
                             educ_1 + educ_2 + educ_3 + 
                             children + marriage + 
                             BMI +  
                             work, 
                             data = h[(h$male == 1) & (h$age >= 18), ])
summary(hm_model)
#--------------
#РАЗДЕЛ №6. Модели со случайными коэффициентами
#--------------
# Статья https://mran.microsoft.com/snapshot/2014-09-09/web/packages/Rchoice/vignettes/Rchoice.pdf
# Оценим Пуассоновскую регрессию предполагаю, что эффект брака на частоту езды на велосипеде
# может разниться между индивидами, то есть соответствующий коэффициент является нормально
# распределенной случайной величиной
# Считается чрезвычайно долго
model_poisson_r <- Rchoice::Rchoice(formula = bycicle ~ age + I(age ^ 2) + 
                                              educ_1 + educ_2 + educ_3 + 
                                              children + marriage + 
                                              BMI +  
                                              work, 
                                              data = h[(h$male == 1) & (h$age >= 18), ],
                                              ranp = c(marriage = "n"),
                                              family = "poisson")
summary(model_poisson_r)
# По аналогии оценим логистическую модель со случайным коэффициентом
# С целью ускорения расчетов сократим объем выборки до 300
model_logit_r <- Rchoice::Rchoice(formula = work ~ age + I(age ^ 2) + 
                                            educ_1 + educ_2 + educ_3 + 
                                            children + marriage + 
                                            BMI, 
                                            data = h[(h$male == 1) & (h$age >= 18),][1:300, ],
                                            ranp = c(marriage = "n"),
                                            family = binomial("logit"))
summary(model_logit_r)