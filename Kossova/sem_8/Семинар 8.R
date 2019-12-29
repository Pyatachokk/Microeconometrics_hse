# Потанин Богдан Станиславович
# Микроэконометрика
# Семинар №8 - Метод Хекмана
#--------------
# РАЗДЕЛ №0. Подготовка данных
#--------------
# Подключение библиотек
# Важно: если у вас нет какой-то из этих блиблиотек, то
# её следует установить
packages <- c("foreign", "ggplot2", "BaylorEdPsych", "miscTools", "pROC", "margins", 
              "boot", "lmtest", "numDeriv", "np", "glmx", "GJRM", "MASS", "brant",
              "VGAM", "EnvStats", "nnet" ,"mlogit", "AER", "MNP", "mnlogit", "pscl",
              "crch", "truncreg", "sampleSelection")
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
library("sampleSelection")     # модель Хекмана в R
# Отключим scientific notation
options(scipen = 999)            
# Загрузка данных
Sys.setlocale(locale = "Russian")
new_data <- read.spss("r25i_os_31.sav",   # путь к файлу (замените на свой)
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
# перекодируем факторную переменную в количественную
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
# Истинные значения регрессионных коэффициентов для основного уравнения
beta_0 <- 1
beta_1 <- 2
beta_2 <- -2
gamma <- c(beta_0, beta_1, beta_2)
# Истинные значения регрессионных коэффициентов для уравнения отбора
gamma_0 <- 1
gamma_1 <- 2
gamma_2 <- -2
gamma <- c(beta_0, beta_1, beta_2)
# Сгенерируем независимые переменные из многомерного
# нормального распределения
X <- rmvn(n,                                 # количество наблюдений
          c(1, 1, 1),                        # вектор математических ожадинй         
          matrix(c(1, 0.5, 0.5,              # ковариционная матрица
                   0.5, 1, 0.5,
                   0.5, 0.5, 1), ncol=3))
# Случайная ошибка
  # Ковариационная матрица случайных ошибок
rho = 0.8
sigma = 3
epsilon_cov <- matrix(c(1, rho * sigma,
                        rho * sigma, sigma^2), ncol = 2)
epsilon <- rmvn(n, c(0,0), epsilon_cov)
# Уравнение отбора
z_star <- gamma_0 + gamma_1 * X[,1] + gamma_2 * X[,3] + epsilon[,1]
z <- as.numeric(z_star >= 0)
# Основное уравнение
y_star <- beta_0 + beta_1 * X[,1] + beta_2 * X[,2] + epsilon[,2]
y <- y_star
y[z == 0] <- NA
# Для начала реализуем двухщаговую процедуру
  # ШАГ 1
    # Оценим пробит регрессию
model_probit <- glm(z ~ X[,1] + X[,3], family = binomial(link = "probit"))
z_star_hat <- predict(model_probit)
    # Рассчитаем отношения Миллса
mills_ratio <- dnorm(z_star_hat) / pnorm(z_star_hat)
  # ШАГ 2
  # Для начала посмотрим на МНК оценки без отношения Миллса и убедимся, что они смещенные
summary(lm(y ~ X[,1] + X[,2]))
  # Оценим МНК с включением отношений Миллса
model_2step <- lm(y ~ X[,1] + X[,2] + mills_ratio)
summary(model_2step)
  # Обратите внимание, что поскольку случайные ошибки гетероскедастичны и мы этого не учли,
  # то полученная классическим методом оценка ковариационной матрицы оценок регрессионных коэффициентов
  # окажется смещенной, вследствие чего мы не сможем тестировать гипотезы. Подумайте, как это
  # можно было бы исправить.
# Теперь воспользуемся методом максимального правдоподобия
# Напишем функцию для расчета математического ожидания и дисперсии условного
# двумерного нормального распределения
# Смотрите формулу в википедии https://en.wikipedia.org/wiki/Multivariate_normal_distribution
bivNormCond_moments <- function(mean, cov_mat, cond_val)
{
  mean_cond <- mean[1] + (cov_mat[1,2] / cov_mat[2,2]) * (cond_val - mean[2])
  var_cond <- cov_mat[1,1] - (cov_mat[1,2] ^ 2) / cov_mat[2,2]
  
  return(list("mean_cond" = mean_cond,
              "var_cond" = var_cond))
}
bivNormCond_moments(c(0,0), epsilon_cov, c(-1,0,1))
# Запишем функцию правдоподобия
lnL <- function(x, y, z, y_X, z_X)
{
  # Добавляем константу в матрицы независимых переменных
  y_X <- cbind(1, y_X)
  z_X <- cbind(1, z_X)
  
  # Количество независимых переменных
  y_m <- ncol(y_X)
  z_m <- ncol(z_X)
  
  # Количество наблюдений
  n <- length(y)
  
  # Записываем значения для оцениваемых параметров
  sigma <- x[1]
  rho <- x[2]
  beta <- matrix(x[3:(y_m + 2)], ncol = 1)
  gamma <- matrix(x[-(1:(y_m + 2))], ncol = 1)
  
  # Ковариационная матрица
  epsilon_cov <- matrix(c(1, rho * sigma,
                          rho * sigma, sigma^2), ncol = 2)
  
  # Считаем детерминированные части уравнений
  y_det <- y_X %*% beta
  z_det <- z_X %*% gamma
  
  # Условия отбора
  selected <- !is.na(y)
  
  # Остатки
  y_res <- y[selected] - y_det[selected]
  
  # Параметры условного распределеиня случайных ошибок основного уравнения
  y_epsilon_cond <- bivNormCond_moments(c(0,0), epsilon_cov, cond_val = y_res)
  
  # Рассчитываем вклады в функцию правдоподобия
  
  L_vector <- rep(NA, n)
  
  L_vector[selected] <- (1 - pnorm(-z_det[selected], 
                                   mean = y_epsilon_cond$mean_cond, 
                                   sd = sqrt(y_epsilon_cond$var_cond))) *
                         dnorm(y_res, mean = 0, sd = sigma)
  
  L_vector[!selected] <- pnorm(-z_det[!selected])
  
  # Возвращаем итоговый результат
  
  return(sum(log(L_vector)))
}
# Осуществим оптимизацию взяв в качестве начальных точек 
# значения, полученные в ходе реализации двухшаговой процедуры
x0 <- c(sigma(model_2step), 0, model_2step$coefficients[-4], model_probit$coefficients)
opt_mle <- optim(par = x0, fn = lnL, 
                 y_X = X[,c(1,2)],
                 z_X = X[,c(1,3)],
                 y = y,
                 z = z,
                 method = "Nelder-Mead", 
                 hessian = TRUE,                          # возвращаем Гессиан
                 control = list("maxit" = 1000,           # максимальное количество итераций
                                fnscale = -1,             # ставим -1 чтобы сделать задачу максимизационной
                                "reltol" = 1e-32))        # условие остановки алгоритма (termination condition)
x1 <- opt_mle$par
# Сравним истинные и предсказанные знаечния
data.frame("beta" = c(beta_0, beta_1, beta_2), 
           "beta_hat" = x1[3:5])
data.frame("rho" = rho, 
           "rho_hat" = x1[2])
# ЗАДАНИЯ
# 1. Посмотрите, как изменится точность оценок если не будут соблюдены exclusion restrictions.
#    Например, сделайте так, чтобы и основное уравнение и уравнение отбора включали лишь X1 и X2
#    в качестве независимых переменных.
# 2. Посмотрите, как изменится точность оценок при нарушении допущения о совместном нормальном
#    распределении случайных ошибок
#--------------
# РАЗДЕЛ №2. Применение метода Хекмана к реальным данным
#--------------
# Создадим переменную на стаж
h$seniority <- missing_to_NA(new_data$uj161.3y)
model_heckman <- selection(work ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + 
                                  children_n_18 + I(children_n_18 ^ 2) + marriage,
                           I(log(wage + 1)) ~ seniority + I(seniority ^ 2) + educ_1 + educ_2 + educ_3,
                             data = h[(h$male == 1) & (h$age >= 18),])
  # Посмотрим на оценки ММП Хекмана
summary(model_heckman)
# Обратите внимание, что если параметр rho значим, то вероятно имеет место смещение отбора, вследствие чего МНК
# оценки окажутся смещенными
  # Посмотрим на оценки двухшагового Хекмана
summary(model_heckman$twoStep)
# Получим предсказанные значения
  # Условного на занятость и безработицу математического ожидания логарифма зарплаты
predict(model_heckman, type = "conditional", part = "outcome", newdata = h)
  # Безусловного математического ожидания логарифма зарплаты
predict(model_heckman, type = "unconditional", part = "outcome", newdata = h)
# Рассчитаем предельные эффекты на условное математическое ожидание
# при помощи численного метода
me_heckman <- function(model, variable_name, data = NULL, epsilon = 0.0000001)
{
  pr_1 <- predict(model, type = "conditional", part = "outcome", newdata = data)
  data[, variable_name] <- data[, variable_name] + epsilon
  pr_2 <- predict(model, type = "conditional", part = "outcome", newdata = data)
  return((pr_2 - pr_1) / epsilon)
}
# Обратите внимание, что условные предельные эффекты будут разниться лишь для независимых переменных,
# входящих в оба уравнения.
me_seniority = me_heckman(model_heckman, "seniority", data = h)
me_educ_3 = me_heckman(model_heckman, "educ_3", data = h)
# ЗАДАНИЯ
# 1. Проверьте возможность исключить из уравнения заработной платы переменную на образование
# 2. Проверьте, можно ли оценивать общую модель для мужчин и для женщин
#--------------
# РАЗДЕЛ №3.Полупараметрический метод Ньюи
#--------------
# Позволяет ослабить допущение о совместном нормальном распределении случайных ошибок
# Сначала следует оценить модель бинарной регрессии при поиощи непараметрического метода, например,
# предложенного Klein and Spady (код внизу под комментариями) или при помощи нейросети (если выборка очень большая). 
# Однако, поскольку расчеты очень долгие, то
# вместо этого воспользуемся пробит регрессией
# model_KS <- npindex(work ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + children_n_18 + I(children_n_18 ^ 2), data=h_train,
#                     method = "kleinspady", 
#                     gradients = FALSE)
# summary(model_KS)
model_probit <- glm(work ~ age + I(age ^ 2) + educ_1 + educ_2 + educ_3 + 
                           children_n_18 + I(children_n_18 ^ 2) + marriage, 
                   data = h[(h$male == 1) & (h$age >= 18),], 
                   family = binomial(link = "probit"))
h$work_hat <- predict(model_probit, newdata = h)
# Рассчитаем отношения миллса, хотя можно использовать любую иную сглаживающую функцию
h$mills_ratio <- dnorm(h$work_hat) / pnorm(h$work_hat)
# Напишем функции для подсчета MSE при leave-one-out кросс валидации
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
#Выберем степень полинома минимизируя MSE
  # 1 степень
model_Newey_1 <- lm(I(log(wage + 1)) ~ seniority + I(seniority ^ 2) + educ_1 + educ_2 + educ_3 +
                    mills_ratio, 
                    data = h[(h$male == 1) & (h$age >= 18) & (h$work == 1),])
loocv(model_Newey_1)
  # 2 степень
model_Newey_2 <- lm(I(log(wage + 1)) ~ seniority + I(seniority ^ 2) + educ_1 + educ_2 + educ_3 +
                    mills_ratio + I(mills_ratio ^ 2), 
                    data = h[(h$male == 1) & (h$age >= 18) & (h$work == 1),])
loocv(model_Newey_2)
  # 3 степень
model_Newey_3 <- lm(I(log(wage + 1)) ~ seniority + I(seniority ^ 2) + educ_1 + educ_2 + educ_3 +
                    mills_ratio + I(mills_ratio ^ 2) + I(mills_ratio ^ 3), 
                    data = h[(h$male == 1) & (h$age >= 18) & (h$work == 1),])
loocv(model_Newey_3)
# Лучшая степень вторая, поэтому выберем её
model_Newey <- model_Newey_2
summary(model_Newey)
# Сравним результаты метода Хекмана и Ньюи
data.frame("Newey" = model_Newey$coefficients[1:6], 
           "MLE-Heckman" = model_heckman$estimate[10:15], 
           "twostep-Heckman" = model_heckman$twoStep$coefficients[10:15])
# Поскольку оценки метода Ньюи асимптотические нормальные, то для тестирования гипотез
# достаточно бутстрапировать ковариационную матрицу
# Повторим расчеты используя бутстрап
  # Напишем функцию для бутстрапа
Newey_boot <- function(formula_selection,    
                       formula_outcome,
                       data,                 # данные
                       indices)              # вспомогательный аргумент для функции boot
{
  # Строго говоря каждую итерацию необходимо осуществлять кросс валидацию
  # по степени полинома, но с целью экономии времени опустим этот шаг
  d <- data[indices,]                           
  
  model_probit <- glm(formula_selection, 
                      data = d, 
                      family = binomial(link = "probit"))
  d$work_hat <- predict(model_probit, newdata = d)
  d$mills_ratio <- dnorm(d$work_hat) / pnorm(d$work_hat)
  model_Newey <- lm(formula_outcome, 
                    data = d[d$work == 1,])
  
  
  return(model_Newey$coefficients)   
}
  # Получим оценки
model_Newey_boot <- boot(data = h[(h$male == 1) & (h$age >= 18),], 
                         R = 100, 
                         statistic = Newey_boot, 
                         formula_selection = formula(model_probit), 
                         formula_outcome = formula(model_Newey))
  # Оценим ковариационную матрицу
Newey_cov_mat <- cov(model_Newey_boot$t)
colnames(Newey_cov_mat) <- names(model_Newey$coefficients)
rownames(Newey_cov_mat) <- names(model_Newey$coefficients)
  # Получим стандартные ошибки
Newey_std = sqrt(diag(Newey_cov_mat))
  # Посчитаем p-value
coef_std_ratio_Newey <- model_Newey$coefficients / Newey_std
p_values_Newey <- 2 * pmin(pnorm(coef_std_ratio_Newey), 1 - pnorm(coef_std_ratio_Newey))
  # Построим таблицу
data.frame("Coefficients" = model_Newey$coefficients, 
           "Std.err" = Newey_std, 
           "p-value" = p_values_Newey)
# Также, можем воспользоваться бутстрапированными перцентильные доверительными интервалами
boot.ci(model_Newey_boot, conf = c(0.9, 0.95, 0.99), type = "perc", index = 6)
# ЗАДАНИЯ
# 1. Сравните МНК, Модель Хекмана и Модель Ньюи по точности предсказания на тестовой выборке
# 2. Проверьте устойчивости оценок метода Ньюи к изменению сглаживающей функции (выберите что-то иное вместо отношения Миллса)
#    Можете использовать любую дифференцируемую строго возрастающую или убывающую функцию
# 3. Проверьте устойчивость оценок метода Ньюи к ищменению бинарной регрессии, применяемой на первом шаге