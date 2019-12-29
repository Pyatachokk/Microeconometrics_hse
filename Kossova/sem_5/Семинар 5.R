# Потанин Богдан Станиславович
# Микроэконометрика
# Семинар №5 - Модели множественного выбора
#--------------
# РАЗДЕЛ №0. Подготовка данных
#--------------
# Подключение библиотек
# Важно: если у вас нет какой-то из этих блиблиотек, то
# её следует установить
packages <- c("foreign", "ggplot2", "BaylorEdPsych", "miscTools", "pROC", "margins", 
              "boot", "lmtest", "numDeriv", "np", "glmx", "GJRM", "MASS", "brant",
              "VGAM", "EnvStats", "nnet" ,"mlogit")
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
library("MNP")                 # Multinomial probit
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
# Вектор истинных значений регрессионных коэффициентов 
# для различных категорий
beta_1 <- matrix(c(0, 0, 0, 0), ncol = 1)
beta_2 <- matrix(c(-0.5, 0.5, 1.5, -1.5 ), ncol = 1)
beta_3 <- matrix(c(-1, 1, -2, 2), ncol = 1)
# Сгенерируем независимые переменные из многомерного
# нормального распределения
X <- rmvn(n,                                 # количество наблюдений
          c(3, 2, 1),                        # вектор математических ожадинй         
          matrix(c(1, 0.5, 0.5,              # ковариционная матрица
                   0.5, 1, 0.5,
                   0.5, 0.5, 1), ncol=3))
# Незваисимые случайные ошибки из распределения Гумбеля
epsilon <- cbind(revd(n), revd(n), revd(n))
# Латентные переменные
z_star_1 <- epsilon[, 1]
z_star_2 <- beta_2[1] + X %*% beta_2[-1] + epsilon[, 2]
z_star_3 <- beta_3[1] + X %*% beta_3[-1] + epsilon[, 3]
# Наблюдаемые значения
z <- rep(NA, n)
z[(z_star_1 > z_star_2) & (z_star_1 > z_star_3)] <- 1
z[(z_star_2 > z_star_1) & (z_star_2 > z_star_3)] <- 2
z[(z_star_3 > z_star_1) & (z_star_3 > z_star_2)] <- 3
# Запишем функцию правдоподобия
lnL <- function(x, z, X)
{
  # Достанем количество групп (предполагается больше двух)
  z_n <- max(z)
  
  # Инициализируем число наблюдений и независимых переменных
  # предварительно добавив константу к X
  X <- cbind(1, X)     # добавим константу
  n <- nrow(X)         # число наблюдений
  m <- ncol(X)         # число независимых переменных включая константу
  
  # Создадим матрицу под хранение коэффициентов
  beta <- matrix(NA, ncol = m, nrow = z_n)
  beta[1, ] <- rep(0, m)
  beta_start <- 1
  beta_end <- m
  for(i in 1:(z_n - 1))
  {
    beta[i + 1,] <- x[beta_start:beta_end]
    beta_start <- beta_end + 1
    beta_end <- beta_start + m - 1
  }
  
  lnL_vec <- rep(0, n)
  
  # Посчитаем числитель и знаменатель вероятности выбора каждой из альтернатив
  # для всех индивидов в выборке
  prob_nominator <- exp(X %*% t(beta))
    # Считаем числитель как суммы по строкам
  prob_denominator <- prob_nominator %*% matrix(1, nrow = z_n, ncol = 1)
  # Рассчитаем вероятность выбора той альтернативы, которую, в конечном итоге,
  # предпочел индивид
  for(i in 1:z_n)
  {
    z_cond <- (z == i)
    lnL_vec[z_cond] <- prob_nominator[z_cond, i] / prob_denominator[z_cond]
  }
  
  return(sum(log(lnL_vec)))
}
# Осуществим оптимизацию
x0 <- c(beta_2 * 0.75, beta_3 * 1.25)
opt_mle <- optim(par = x0, fn = lnL, 
                 X = X,
                 z = z,
                 method = "Nelder-Mead", 
                 hessian = TRUE,                          # возвращаем Гессиан
                 control = list("maxit" = 1000,           # максимальное количество итераций
                                fnscale = -1,             # ставим -1 чтобы сделать задачу максимизационной
                                "reltol" = 1e-32))        # условие остановки алгоритма (termination condition)
x1 <- opt_mle$par
# Сравним истинные и предсказанные знаечния
data.frame("beta_2" = beta_2, 
           "beta_2_hat" = x1[1:4], 
           "beta_3" = beta_2, 
           "beta_3_hat" = x1[5:8])
# Сравним с результатами, полученными при помощи использования встроенной функции
model_mult_simulation=multinom(z~X[,1] + X[,2] + X[,3])
summary(model_mult_simulation)
# ЗАДАНИЯ
# 1. Повторите симуляции для пяти возможных альтернатив 
# 2. Оцените модель при допущении о том, что случайные ошибки имеют нормальное распределение
#--------------
#РАЗДЕЛ №2. Анализ реальных данных
#--------------
# Сделаем переменную на зарплату
h$wage <- missing_to_NA(new_data$uj13.2)
h$wage[h$work == 0] <- 0
# Создадим переменную на курение
h$ciga <- NA
  # Сигаретыр с фильтром
h$ciga[new_data$um74 == "Сигареты с фильтром"] <- 1
  # Сигареты без фильтра
h$ciga[new_data$um74 != "Сигареты с фильтром" & !is.na(new_data$um74)] <- 2
  # Не курит
h$ciga[new_data$um71 == "Нет"] <- 3
  # Перекодируем переменную в факторную
h$ciga <- as.factor(h$ciga)
# Посмотрим на распределение в зависимости от пола
table(h$ciga[h$age >= 18], h$male[h$age >= 18])
# Оценим модель для совершеннолетних мужчин
model_mult <- multinom(ciga ~ age + I(age ^ 2) + 
                                      BMI + I(BMI ^ 2) + 
                                      educ_1 + educ_2 + educ_3 +
                                      marriage + children +
                                      I(log(wage + 1)) + work,
                                      data = h[(h$male == 1) & (h$age >= 18),], 
                                      model = TRUE)
model_mult_summary <- summary(model_mult)
# Посчитаем p-value для каждой категории, кроме, очевидно, базовой
z_val_2 <- coef(model_mult)[1,] / model_mult_summary$standard.errors[1,]
z_val_3 <- coef(model_mult)[2,] / model_mult_summary$standard.errors[2,]
p_values_2 <- 2 * pmin(pnorm(z_val_2), 1 - pnorm(z_val_2))
p_values_3 <- 2 * pmin(pnorm(z_val_3), 1 - pnorm(z_val_3))
# Сделаем выдачу с p-value
round(data.frame("Коэффициенты-2" = coef(model_mult)[1,],
                 "Стандартные ошибки-2" = model_mult_summary$standard.errors[1,],
                 "p-value-2" = p_values_2,
                 "Коэффициенты-3" = coef(model_mult)[2,],
                 "Стандартные ошибки-3" = model_mult_summary$standard.errors[2,],
                 "p-value-3" = p_values_3), 5)
# Реализации оценок вероятностей
model_mult_probabilities <- predict(model_mult, 
                                    newdata = h[(h$male == 1) & (h$age >= 18),], 
                                    type="probs")
summary(model_mult_probabilities)
  # Интересно отметить, что вероятности курения сигарет с фильтром и без
  # возможно, связаны положительно, а вероятность курения каких-либо сигарет с
  # с вероятностю того, что индивид не курит - отрицательно
plot(model_mult_probabilities[, 1], model_mult_probabilities[, 2])
plot(model_mult_probabilities[, 1], model_mult_probabilities[, 3])
plot(model_mult_probabilities[, 2], model_mult_probabilities[, 3])
# Предсказанные категории
model_mult_predictions <- predict(model_mult)
summary(as.factor(model_mult_predictions))
# Сопоставим предсказанные и стинные значения
pred_table <- table("Предсказание" = model_mult_predictions, 
                    "Истинное значение" = model_mult$model$ciga)
# Вычислим долю верных предсказаний
  # Прогноз по модели
pred_model_true <- sum(diag(pred_table)) / sum(pred_table)
  # Навивный прогноз
table(model_mult$model$ciga)
pred_naive_true <- sum(model_mult$model$ciga == 3) / length(model_mult$model$ciga)
  # Сопоставим прогнозы
data.frame("Модель" = pred_model_true,
           "Наивный" = pred_naive_true)
# Посчитаем предельные эффекты
  # Посчитаем предельные эффекты при помощи численных методов
me_multinomial <- function(model, variable_name, newdata = NULL, epsilon = 0.0000001)
{
  if(is.null(newdata))
  {
    newdata <- model$model
  }
  pr_1 <- predict(object = model, newdata = newdata, type = "prob")
  newdata[, variable_name] <- newdata[, variable_name] + epsilon
  pr_2 <- predict(object = model, newdata = newdata, type = "prob")
  return((pr_2 - pr_1) / epsilon)
}
me_age <- me_multinomial(model_mult, "age", h[(h$male == 1) & (h$age >= 18),])
# Посчитаем средний предельный эффект
me_age_mean <- colMeans(na.omit(me_age))
  # Проверим, можно ли исключить из модели переменную на образование
model_mult_R <- multinom(ciga ~ age + I(age ^ 2) + 
                                BMI + I(BMI ^ 2) + 
                                marriage + children +
                                I(log(wage + 1)) + work,
                                data = h[(h$male == 1) & (h$age >= 18) & !is.na(h$educ_1 + h$educ_2 + h$educ_3),], 
                                model = TRUE)
  # На любом разумном уровне значимости нулевая гипотеза о допустимости исключния
  # переменных на образование отклоняется
lrtest(model_mult, model_mult_R)
# Наконец, посмотрим на изменеине отношений риска p(z=q)/p(z=1) при изменении зависимой переменной на 1
# для переменных, входящих в регрессионное уравнение линейно
exp(coef(model_mult))
# Для переменных, входящих нелинейно, способ вывода формулы для отношения рисков аналогичен способы вывода
# этой формулы для обычной логит модели
# ЗАДАНИЯ
# 1. Вычислите, во сколько раз измените отношение вероятностей принадлежности индивида к первой и третьей категориям
#    при изменении значения переменной age на единицу.
# 2. При помощи LR теста проверьте, можно ли оценивть совместную модель для мужчин
#    и женщин
#--------------
#РАЗДЕЛ №3. Проверка гипотезы о независимости от посторонних альтернатив
#--------------
# Для начала оценим модель при помощи другой функции
  # Подготовим данные
h$ciga_new <- as.character(h$ciga)
h_new <- mlogit.data(h[(h$male == 1) & (h$age >= 18),], 
                     shape = "wide", choice = "ciga_new")
  # Оценим модель по всем альтернативам
model_mlogit <- mlogit(ciga_new ~ 0 | age + I(age ^ 2) +             # формула
                                      BMI + I(BMI ^ 2) + 
                                      educ_1 + educ_2 + educ_3 +
                                      marriage + children +
                                      I(log(wage + 1)) + work, 
                                      data = h_new,                  # данные
                                      reflevel = 1)                  # базовая альтернатива
summary(model_mlogit)
  # Оценим модель лишь по первой и третьей альтернативам
model_mlogit_S <- mlogit(ciga_new ~ 0 | age + I(age ^ 2) +             
                                        BMI + I(BMI ^ 2) + 
                                        educ_1 + educ_2 + educ_3 +
                                        marriage + children +
                                        I(log(wage + 1)) + work, 
                                        data = h_new,                  
                                        alt.subset = c("1", "3"))                 
summary(model_mlogit_S)
  # Проведем IIA тест и убедимся, что не отвергается нулевая гипотеза о том, что отношение вероятностей
  # принадлежности к первой и третьей альтернативам не зависит от наличия и полезности индивида от третьей альтернативы
  mlogit::hmftest(x = model_mlogit, 
                  z = model_mlogit_S)
# ЗАДАНИЯ
# 1. Проверьте независимость отношения вероятностей выбора между второй и третьей альтернативами в зависимости
#    от третьей
#--------------
#РАЗДЕЛ №4. Мультиномиальная пробит модель (бонусный)
#--------------
# Данная модель учитывает возможность корреляции случайных ошибок уравнния
# полезности от альтернатив
# Очень подробный мануал
# https://cran.r-project.org/web/packages/mlogit/vignettes/c6.mprobit.html
# Считается невероятно долго
model_mprobit <- mlogit(ciga_new ~ 0 | marriage, 
                        data = h_new, R = 1,
                        probit = TRUE)
model_probit$omega
summary(model_probit)
# Байесовская альтернатива https://imai.fas.harvard.edu/research/files/MNPjss.pdf
# Считается весьма быстро
# Обратите внимание, что вместо p-value указаны байесовские доверительные интервалы
model_mprobit_bayes <- mnp(ciga ~ age + I(age ^ 2) + 
                                  BMI + I(BMI ^ 2) + 
                                  educ_1 + educ_2 + educ_3 +
                                  marriage + children +
                                  I(log(wage + 1)) + work,
                                  data = h[(h$male == 1) & (h$age >= 18),])
summary(model_mprobit_bayes)
# Предскажем вероятности
model_mprobit_bayes_probabilities <- predict(model_mprobit_bayes, newdata = h[(h$male == 1) & (h$age >= 18),])
model_mprobit_bayes_probabilities$p
# Сравним mlogit и mprobit вероятности попадания
  # в число курящих сигареты с филтром
cbind(na.omit(model_mprobit_bayes_probabilities$p)[, 1], na.omit(model_mult_probabilities[, 1]))
  # в число курящих сигареты без фильтра
cbind(na.omit(model_mprobit_bayes_probabilities$p)[, 2], na.omit(model_mult_probabilities[, 2]))
  # в число не курящих
cbind(na.omit(model_mprobit_bayes_probabilities$p)[, 3], na.omit(model_mult_probabilities[, 3]))
# Вместо расчета предельных эффектов следует рассчитывать разницу в постериорных вероятностях
# в зависимости от изменения независимой переменной, о чем подробней можно узнать
# из работы https://imai.fas.harvard.edu/research/files/MNPjss.pdf