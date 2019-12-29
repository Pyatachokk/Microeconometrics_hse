# Потанин Богдан Станиславович
# Микроэконометрика
# Семинар №4 - Модели порядкового выбора
#--------------
# РАЗДЕЛ №0. Подготовка данных
#--------------
# Подключение библиотек
# Важно: если у вас нет какой-то из этих блиблиотек, то
# её следует установить
packages <- c("foreign", "ggplot2", "BaylorEdPsych", "miscTools", "pROC", "margins", 
              "boot", "lmtest", "numDeriv", "np", "glmx", "GJRM", "MASS", "brant",
              "VGAM")
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
#РАЗДЕЛ №0. Анализ симулированных данных
#--------------
# Приступим к симуляциям
  # Количество наблюдений
n <- 10000
  # Матрица независимых переменных
X <- cbind(rnorm(n, 1, 2), rnorm(n, -1, 3))
  # Случайная ошибка
epsilon <- rnorm(n, 0, 1)
  # Коэффициенты
beta_1 <- 2
beta_2 <- 3
  # Создаем латентную переменную
z_star <- beta_1 * X[,1] + beta_2 * X[,2] + epsilon
  # Выберем порогвые значения
threshold_1 <- -3
threshold_2 <- 5
  # Создадим наблюдаемые значения зависимой переменной
z <- rep(NA, n)
z[z_star <= threshold_1] <- 1
z[(threshold_1 < z_star) & (z_star <= threshold_2)] <- 2
z[threshold_2 < z_star] <- 3
  # Напишем функцию правдоподобия в соответствии с представленным выше
  # процессом генерации данных
lnL <- function(x, X, z)
{
  # количество групп
  # в целях оптимизации лучше считать заранее, до вхождения
  # в оптимизируемую функцию
  z_n <- max(z)
  
  # Сохраняем количество наблюдений и независимых переменных
  n <- NROW(X)
  m <- NCOL(X)
  
  # Записываем максимизируемые параметры в отдельные векторы
  threshold <- c(-Inf, x[1:(z_n - 1)], Inf)
  beta <- matrix(x[z_n:length(x)], ncol = 1)
  
  # Будем записывать наши значения в этот вектор
  lnL_vector <- rep(NA, n)
  
  # Осуществляем для каждой группы расчет вероятности
  for(i in 1:max(z))
  {
    # в целях оптимизации до вхождения в максимизируемую функцию, конечно, лучше создать вектор
    # из threshold во избежание цикла и поиска наблюдений, удовлетворяющих условию
    # принадлежности к группе
    z_determ <- X[z == i, ] %*% beta
    lnL_vector[z == i] <- pnorm(threshold[i + 1] - z_determ) - pnorm(threshold[i] - z_determ)
  }

  return(sum(log(lnL_vector)))
}
  # Осуществим оптимизацию
x0 <- c(-2, 3, 1.3, 1.8)
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
data.frame("threshold_1" = c(threshold_1, x1[1]), 
           "threshold_2" = c(threshold_2, x1[2]),
           "beta_1" = c(beta_1, x1[3]), 
           "beta_2" = c(beta_2, x1[4]))
# ЗАДАНИЯ
# 1. Проверьте, что случится, если вы неверное специфицируете количество категорий, например,
#    объединив первую и вторую в одну
# 2. Замените распределение случайной ошибки с нормального на логистическое и посмотрите, как
#    изменятся результаты оценивания параметров модели.
# 3. Добавьте в модель возможность учета гетероскедастичной случайной ошибки по аналогии
#    с гетпробитом
#--------------
#РАЗДЕЛ №2. Порядкова пробит модель
#--------------
# Создадим дополнительные переменные
  # На удовлетворенность жизнью
h$lifesat <- NA
h$lifesat[new_data$uj65 == "Совсем не удовлетворены"] <- 1
h$lifesat[new_data$uj65 == "Не очень удовлетворены"] <- 2
h$lifesat[new_data$uj65 == "И да, и нет"] <- 3
h$lifesat[new_data$uj65 == "Скорее удовлетворены"] <- 4
h$lifesat[new_data$uj65 == "Полностью удовлетворены"] <- 5
  # На зарплату
h$wage <- missing_to_NA(new_data$uj13.2)
h$wage[h$work == 0] <- 0
  # Убедимся, что у нас нет слишком маленьких категорий
summary(as.factor(h$lifesat))
# Построим порядковую модель для совершеннолетних мужчин
# и поставим верхнюю границу возраста на 70, так как иначе
# модель плохо считается (глючит оптимизатор в polr)
model_oprobit <- polr(as.factor(lifesat) ~ age + I(age ^ 2) +
                        educ_1 + educ_2 + educ_3 + 
                        BMI + I(BMI ^ 2) +
                        children + marriage + 
                        work + 
                        I(log(wage+1)), 
                        data = h[(h$male == 1) & (h$age >= 18) & (h$age <= 70), ],
                        method = "probit")
model_oprobit_summary <- summary(model_oprobit)
  #К сожалению, выдача не содержит p-value, поэтому посчитаем их вручную
model_oprobit_summary$coefficients <- round(
                                              cbind(model_oprobit_summary$coefficients,
                                              2 * pmin(pnorm(model_oprobit_summary$coefficients[,"t value"],),
                                              1 - pnorm(model_oprobit_summary$coefficients[,"t value"]))), 5)
colnames(model_oprobit_summary$coefficients)[4]="p value"
  #Посмотрим на выдачу с включенным p-value
model_oprobit_summary
  # Обратите внимание, что p-value для threshold не интерпретируется. Не важно, насколько threshold отличается от 0.
  # При этом можно проверить, статистически ли значимо различаются два threshold между собой.
# Оценим предсказательную силу модели
  # Посмотрим, с какой вероятностью в какую категорию попадает каждое наблюдение
model_oprobit_probabilities <- model_oprobit$fitted.values
# Предскажем каждому наблюдению ту категорию, к которой он принадлежит с максимальной вероятностью
model_oprobit_predictions <- predict(model_oprobit)
# Сопоставим предсказанные и стинные значения
table("Предсказание" = model_oprobit_predictions, 
      "Истинное значение" = model_oprobit$model$`as.factor(lifesat)`)
  # Сравним с наивным прогнозом
lifesat_sum <- summary(model_oprobit$model$`as.factor(lifesat)`)
naive_pred <- max(lifesat_sum) / sum(lifesat_sum)
model_oprobit_pred <- sum(as.logical(as.factor(model_oprobit_predictions == 
                                               model_oprobit$model$`as.factor(lifesat)`))) / 
                                               sum(lifesat_sum)
data.frame("Наивный прогноз" = naive_pred, "Прогноз по модели" = model_oprobit_pred)
# Рассчитаем предельные эффекты
  # Получим значения латентной переменной
model_oprobit_latent <- model_oprobit$lp
  # Найдем предельный эффект возраста на вероятность быть скорее удовлетворенным (4-я категория).
  # Вероятность попасть в четвертую в категории скорее удовлетворенных можно рассчитать следующим образом
pnorm(model_oprobit$zeta[4] - model_oprobit_latent) - pnorm(model_oprobit$zeta[3] - model_oprobit_latent)
  #Следовательно, для этой вероятности предельный эффект возраста будет:
me_age_4 <- (model_oprobit$coefficients["age"] + 
             model_oprobit$coefficients["I(age^2)"] * 
             model_oprobit$model$age) *
             (dnorm(model_oprobit$zeta[4] - 
             model_oprobit_latent) - 
             dnorm(model_oprobit$zeta[3] - 
             model_oprobit_latent))
  # Посмотрим на его распределение
hist(me_age_4)
# Проверим возможность исключить из модели переменные на образование
model_oprobit_R <- polr(as.factor(lifesat) ~ age + I(age ^ 2) +
                        BMI + I(BMI ^ 2) +
                        children + marriage + 
                        work + 
                        I(log(wage+1)), 
                        data = h[(h$male == 1) & (h$age >= 18) & (h$age <= 70) & 
                                 !is.na(h$educ_1) & !is.na(h$educ_2) & !is.na(h$educ_3), ],
                        method = "probit")
# Воспользуемся lr тестом
lrtest(model_oprobit, model_oprobit_R)
# ЗАДАНИЯ
# 1. Повторите оценивание модели включив переменную на место жительство status, а также проверив, при помощи
#    lr теста, возможность её исключения из модели.
# 2. Объедините некоторые категории так, чтобы у вас остались лишь три. Оцените полученную модель.
#    Подумайте, можно ли как-то её сравнить с предыдущей.
########
#РАЗДЕЛ №2. Тестирование гипотезы о parallel lines assumption
########
# Создадим заранее нелинейные переменные
h$age2 = h$age ^ 2
h$BMI2 = h$BMI ^ 2
h$wage_ln = log(h$wage + 1)
# Осуществим тест Бранта о parallel lines assumption
model_oprobit <- polr(as.factor(lifesat) ~ age + age2 +
                        educ_1 + educ_2 + educ_3 + 
                        BMI + BMI2 +
                        children + marriage + 
                        work +
                        wage_ln, 
                      data = h[(h$male == 1) & (h$age >= 18) & (h$age <= 70), ],
                      method = "probit")
brant(model_oprobit)
########
#РАЗДЕЛ №3. Порядковая логит модель и отношение шансов
########
# Построим порядковую логит модель
model_ologit <- polr(as.factor(lifesat) ~ age + age2 +
                     educ_1 + educ_2 + educ_3 + 
                     BMI + BMI2 +
                     children + marriage + 
                     work +
                     wage_ln, 
                     data = h[(h$male == 1) & (h$age >= 18) & (h$age <= 70), ],
                     method = "logistic")
summary(model_ologit)
# При интерпретации отношений шансов учитывайте, что для любого номера категории k изменение 
# в отношениях шансов p(z>k)/p(z<=k) остается прежним
# Рассчитаем изменение в отношениях шансов p(z>k)/p(z<=k) при изменении независимой переменной,
# входящей в основное уравнение, на единицу
exp(model_ologit$coefficients)