library("forecast")
library("foreign")             
library("BaylorEdPsych")       
library("miscTools")           
library("pROC")                
library("margins")             
library("boot")                
library("lmtest")              
library("glmx")                
library("GJRM") 
library("dplyr")
library("ggplot2")

# Отключим scientific notation
options(scipen = 999)          


# Часть 2. Обработка данных


# Загрузка данных
Sys.setlocale(locale="Russian") 
new_data <- read.spss("r25i_os_31.sav",   # путь к файлу (замените на свой)
                      to.data.frame = TRUE,                                                            # загружаем как dataframe
                      use.value.labels = TRUE,                                                         # использовать численные значения                                                                              # вместо слов для некоторых переменных
                      max.value.labels = 30)    

# сколько различных значений должна принимать
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
h$alc = factor_to_binary(new_data$um80.0, "Да, употребляете")


# Переменная - образование
h$educ = new_data$u_diplom

#Переменная - состоит в оффициальном браке
h$marriage <- factor_to_binary(missing_to_NA(new_data$u_marst),"Состоите в зарегистрированном браке")


# Переменная - пол
h$female <- 0
h$female[new_data$uh5=="ЖЕНСКИЙ"] <- 1


#Переменная - состоит в оффициальном браке
h$marriage <- factor_to_binary(missing_to_NA(new_data$u_marst),"Состоите в зарегистрированном браке")



h$educ = missing_to_NA(h$educ)
h$educ_1 <- factor_to_binary(h$educ, "законченное высшее образование и выше")
h$educ_2 <- factor_to_binary(h$educ, "законченное среднее образование")
h$educ_3 <- as.numeric(h$educ_1 == 0 & h$educ_2 == 0)
h$educ = factor_to_binary(missing_to_NA(h$educ), "законченное высшее образование и выше")


#Переменная взаимодействия занятости и возраста
h$educ_age = h$educ * h$age


# Посмотрим на данные.
summary(h)
# Осознаем, что в целевой переменной сидят пропуски. Интуиция подсказывает, что все остальные пропуски 
# тоже вследствие этого. Проверим.
summary(h[!is.na(h$marriage),])
h = na.omit(h)

summary(h)
# Теперь всё ок



# Теперь уберём из выборки индивидов моложе 18 и мужчин
h = h[h$age >= 1 & h$female == 1,]




# Часть 2.2
# Для женатых
summary(h[h$marriage == 1,])


# Для неженатых
summary(h[h$marriage == 0,])

# Часть 2.3

# Средний и медианный индивид
summary(h)

# Часть 2.4

age_marr = ggplot() + geom_histogram(data = h[h$marriage == 1,], aes(x = age)) + xlab("Age, marriage = 1")
age_nomarr = ggplot() + geom_histogram(data = h[h$marriage == 0,], aes(x = age)) + xlab("Age, marriage = 0")
plot(age_marr)
plot(age_nomarr)

# Часть 3

# Часть 3.1

# Линейно-вероятностная модель

model_linear <- lm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                     educ + educ_age, 
                   data = h)  

summary(model_linear)

# Часть 3.2

# Скорректируем бутстрапом ковариационную матрицу

model_coef <- coef(model_linear)
# Поскольку оценка ковариационной матрицы, полученная при помощи предыдущей команды, 
# является смещенной, то оценить ковариационную матрицу лучше при помощи бутстрапа.
lm_cov <- function(formula,    # используемая для построения модели формула
                   data,       # данные
                   indices)    # вспомогательный аргумент для функции boot
{
  d <- data[indices,]              # функция boot подает на вход индексы на основании которых формируется новая выборка 
  # из старой с возвращением
  model <- lm(formula, data = d)   # по новой выборке оценивается модель
  return(coefficients(model))      # возвращаем вектор реализаций МНК оценок регрессионных коэффициентов
}
model_linear_boot <- boot(data = h, R = 200, statistic = lm_cov, formula = formula(model_linear))
# Посмотрим на полученные значения, где
# ti* (по строкам) - строка относится к i-му регрессионному коэффициенту. Например, строка t1* относится к константе.
# ogirinal - среднее значение оценок коэффициентов, полученных при помощи бутстрапа.
# std. error - реализации бутстрапированной оценки стандартной ошибки оценки регрессионного коэффициента.
model_linear_boot
# можем получить оценку ковариационной матрицы и вручную пользуясь тем, что
# свойство $t по строкам содержит оценки регрессионных коэффициентов с i-й итерации
colnames(model_linear_boot$t) <- names(coefficients(model_linear))
model_linear_cov <- cov(model_linear_boot$t)
# сравним старые и новые стандартные ошибки
data.frame("Бутстрапированные стандартные ошибки" = sqrt(diag(model_linear_cov)),
           "Старые стандартные ошибки"            = sqrt(diag(vcov(model_linear))))


# используем новую реализацию оценки ковариационной матрицы оценок регрессионных коэффициентов 
# для тестирования гипотез о регрессионных коэффициентах сохраняя допущение о нормальности, в случае
# нарушения которого полученные выводы могут оказаться ложными (так как неверно будут посчитаны p-value)
coeftest(model_linear, vcov. = model_linear_cov)
# протестируем гипотезы при помощи перцентильных бутстрапированных интервалов, опуская
# допущение о нормальности

# Часть 3.3

perc_conf_ints = matrix(nrow = 5, ncol = 3)
for (i in seq(1,5)){
  a = boot.ci(model_linear_boot,            # объект, содержащий результаты бутстрапирования
          type="perc",                  # указываем, что хотим получить перцентильный бутстрапированный интервал для оценок
          conf = c(0.95),    # указываем, каких уровней бутстрапированные доверительные интервалы нам нужны
          index = i)
  print(a$percent[4:5])
  perc_conf_ints[i,1:2] = a$percent[1,4:5]
}
perc_conf_ints[,3] = model_coef
perc_conf_ints = data.frame(perc_conf_ints)
perc_conf_ints

# Таблица для тестирования значимости
colnames(perc_conf_ints) = c("Left", "Right", "Coef")
rownames(perc_conf_ints) = c("Intercept", "Age", "Age^2", "educ", "educage")


# Часть 3.4

# Часть 3.5

# Предельные эффекты (легко интерпретируются)
# Важно: вероятность изменяется в абсолютных значениях от 0 до 1,
# а не в процентных пунктах, процентах и т.д.
model_linear_frame <- model_linear$model                # матрица использовавшихся в регрессии переменных
model_linear_frame$marriage = 1
model_linear_coef <- coef(model_linear) # реализации оценок коэффициентов


model_linear_one_frame = model_linear_frame
model_linear_one_frame$educ = 1
model_linear_one_frame$educ_age = model_linear_one_frame$age



#Для переменной возраст
me_age <- model_linear_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * model_linear_frame$age +  model_linear_coef["educ"] * model_linear_frame$educ


# Для переменной занятость

me_educ = as.matrix(model_linear_one_frame) %*% as.vector(model_linear_coef) - as.matrix(model_linear_frame)[,1:3] %*% as.vector(model_linear_coef)[1:3]


me_all_table = data.frame(age = me_age, educ = me_educ)

# Часть 3.6

# Средние предельные эффекты
me_mean = data.frame(mean(me_age), mean(me_educ))

# Предельные эффекты для среднего индивида

me_mean_ind_age = me_age <- model_linear_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * mean(model_linear_frame$age) +  model_linear_coef["educ"] * round(mean(model_linear_frame$educ)) 

mean_matrix = colMeans(as.matrix(model_linear_frame))
mean_matrix_one = colMeans(as.matrix(model_linear_one_frame))
me_mean_ind_educ = mean_matrix_one %*% as.vector(model_linear_coef) - mean_matrix[1:3]%*% as.vector(model_linear_coef)[1:3]

me_mean_ind = c(me_mean_ind_age, me_mean_ind_educ)
names(me_mean_ind) = c("Age", "educ")
# Предельные эффекты для медианного индивида


me_median_ind_age = me_age <- model_linear_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * median(model_linear_frame$age) +  model_linear_coef["educ"] * median(model_linear_frame$educ) 

median_matrix = colMedians(as.matrix(model_linear_frame))
median_matrix_one = colMedians(as.matrix(model_linear_one_frame))

me_median_ind_educ = median_matrix_one %*% as.vector(model_linear_coef) - median_matrix[1:3] %*% as.vector(model_linear_coef)[1:3]

me_median_ind = c(me_median_ind_age, me_median_ind_educ)
names(me_median_ind) = c("Age", "educ")

me_table = t(matrix(c(me_mean, me_mean_ind, me_median_ind), ncol = 3, nrow = 2))
colnames(me_table) = c("Age", "educ")
rownames(me_table)  = c("Mean ME", "ME of mean person", "ME of median person")
me_table

# Часть 3.8

acc_lin = sum(as.numeric(forecast(model_linear, newdata = model_linear_frame)$mean > 0.5) == h$marriage) / nrow(h)

acc_naive = sum(1 == h$marriage)/ nrow(h)

# Часть 3.9
# Доля прогнозов, по модулю больше 1
out = sum(forecast(model_linear, newdata = model_linear_frame)$mean > 1 | forecast(model_linear, newdata = model_linear_frame)$mean < 0) / nrow(h)

# Часть 3.10

# Прогноз для меня

my_char = c(1, 21, 441, 0, 0)

my_forec = model_linear_coef %*% my_char 

# Часть 4
# Часть 4.1

model_probit = glm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                    educ + educ_age, 
                  data = h, family = binomial(link = "probit")) 
summary(model_probit)

model_probit_frame <- model_probit$model # записываем в отдельный dataframe наблюдения,
# по которым была оценена модель
model_probit_frame$marriage = 1
model_probit_coef <- coef(model_probit)


model_probit_one_frame = model_probit_frame
model_probit_one_frame$educ = 1
model_probit_one_frame$educ_age = model_probit_one_frame$age


# Часть 4.4

latent_values_prob = predict(model_probit)

me_age_probit = (model_probit_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * model_probit_frame$age +  model_probit_coef["educ"] * model_probit_frame$educ) * dnorm(latent_values_prob)

g_3 = ggplot() + geom_histogram(data = data.frame(me = me_age_probit), aes(x=me),  bins = 40) + xlab("Marginal effect of age, probit model")

plot(g_3)

sum(me_age_probit > 0)/length(me_age_probit)
# Больше положительных

me_educ_probit = pnorm(as.matrix(model_probit_one_frame) %*% as.vector(model_probit_coef)) - pnorm(as.matrix(model_probit_frame)[,1:3] %*% as.vector(model_probit_coef)[1:3])

g_4 = ggplot() + geom_histogram(data = data.frame(me = me_educ_probit), aes(x=me),  bins = 40) + xlab("Marginal effect of educ, probit model")

plot(g_4)

sum(me_educ_probit > 0)/length(me_age_probit)
# Больше положительных


# Часть 4.6
me_mean_probit = data.frame(Age = mean(me_age_probit), educ = mean(as.vector(me_educ_probit)))
me_mean_probit


mean_matrix = colMeans(as.matrix(model_probit_frame))
mean_matrix[4] = round(mean_matrix[4])
mean_latent_values_prob = mean_matrix %*% model_probit_coef

me_mean_ind_probit_age = (model_probit_coef["age"] + 2 *                                      
                            model_probit_coef["I(age^2)"] * mean(model_probit_frame$age) +  model_probit_coef["educ"] * round(mean(model_probit_frame$educ))) * dnorm(mean_latent_values_prob)

mean_matrix_one = colMeans(as.matrix(model_probit_one_frame))
me_mean_ind_probit_educ = as.vector(pnorm(mean_matrix_one %*% as.vector(model_probit_coef)) - pnorm(mean_matrix[1:3] %*% as.vector(model_probit_coef)[1:3]))

me_mean_ind_probit = c(me_mean_ind_probit_age, me_mean_ind_probit_educ)
names(me_mean_ind_probit) = c("Age", "educ")

me_mean_ind_probit



median_latent_values_prob = colMedians(as.matrix(model_probit_frame)) %*% model_probit_coef


me_median_ind_probit_age = model_probit_coef["age"] + 2 *                                      
  model_probit_coef["I(age^2)"] * median(model_probit_frame$age) +  model_probit_coef["educ"] * median(model_probit_frame$educ) * dnorm(median_latent_values_prob)

median_matrix = colMedians(as.matrix(model_probit_frame))
median_matrix_one = colMedians(as.matrix(model_probit_one_frame))

me_median_ind_probit_educ = pnorm(median_matrix_one %*% as.vector(model_probit_coef)) - pnorm(median_matrix[1:3] %*% as.vector(model_probit_coef)[1:3])

me_median_ind_probit= c(me_median_ind_probit_age, me_median_ind_probit_educ)
names(me_median_ind_probit) = c("Age", "educ")


# Общая таблица

me_probit_table = t(matrix(c(me_mean_probit, me_mean_ind_probit, me_median_ind_probit), ncol = 3, nrow = 2))
colnames(me_probit_table) = c("Age", "educ")
rownames(me_probit_table)  = c("Mean ME", "ME of mean person", "ME of median person")
me_probit_table

# Часть 4.8

acc_probit = sum(as.numeric(predict(model_probit, newdata = model_probit_frame) > 0.5) == h$marriage) / nrow(h)

acc_naive = sum(1 == h$marriage)/ nrow(h)

# Часть 4.9

my_char = c(1, 21, 441, 0, 0)

my_forec_probit = pnorm(model_probit_coef %*% my_char)


# Часть 5
# Часть 5.1

summary(model_probit)
# Запись коэффициентов и фрейма модели
model_probit_frame <- model_probit$model # записываем в отдельный dataframe наблюдения,
# по которым была оценена модель (то есть зависимую и независимую переменные)
model_probit_coef <- coef(model_probit)  # записываем значения оценок коэффициентов модели
# Проведем тест, нулевая гипотеза которого заключается в том, что случайные ошибки
# подчиняются нормальному распределению, против альтернативны о принадлежности к более
# широкому классу распределений (пирсоновская семья).
# Рассчитаем обобщенные остатки
probit_latent <- predict(model_probit);                                           #оценка латентной переменной
F_probit_latent <- pnorm(probit_latent);                                
f_probit_latent <- dnorm(probit_latent);
generalized_residuals <- ((model_probit_frame$educ - F_probit_latent) /           #обобщенный остаток
                            (F_probit_latent * (1 - F_probit_latent))) *
  f_probit_latent;
# Считаем производные по коэффициентам
derivative_beta <- generalized_residuals *                                        # производные по бета
  cbind(1, model_probit_frame[-1])
derivative_gamma_1 <- (generalized_residuals * probit_latent ^ 2)                 # производные по коэффициенту при xb^2
derivative_gamma_2 <- (generalized_residuals * probit_latent ^ 3)                 # производные по коэффициенту при xb^3
# Проводим LM тест
ones_vector <- data.frame("my_ones" = derivative_beta[,1] / derivative_beta[,1])  # вектор единиц
ones_regression <- summary(lm(my_ones~ . + 0,                                     # регрессия на вектор единиц без константы
                              data = cbind(derivative_beta,ones_vector,
                                           derivative_gamma_1,
                                           derivative_gamma_2)));          
R2 <- ones_regression$r.squared;                                           # коэффициент детерминации регрессии
n <- length(ones_regression$residuals);                                    # число наблюдений
LM_statistics <- R2 * n;                                                   # LM статистика
LM_test_p_value <- 1 - pchisq(q = LM_statistics, df = 2);                  # p-value теста


# Часть 5.2

# LR-тест

model_hetprobit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            educ + educ_age | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")


testStatistic <- 2 * (logLik(model_hetprobit) - logLik(model_probit))
mypval <- pchisq(testStatistic, df = length(coef(model_hetprobit)) - length(coef(model_probit)),
                  lower.tail = FALSE)
mypval

# LM-тест

# Достанем выборку и коэффициенты
model_hetprobit_frame <- model.frame(model_hetprobit)
model_hetprobit_coef <- matrix(coefficients(model_hetprobit), ncol = 1)
rownames(model_hetprobit_coef) <- names(coefficients(model_hetprobit))
# Рассчитаем реализацию оценки предельного эффекта переменной BMI
# Рассчитаем значение латентной переменной из основного уравнения
model_hetprobit_latent_main <- as.matrix(model_hetprobit_frame[-1]) %*% 
  model_hetprobit_coef[2:ncol(model_hetprobit_frame)] + 
  model_hetprobit_coef[1]
# Посчитаем латентную переменную из уравнения для дисперсии случайной ошибки
model_hetprobit_std <- exp(model_hetprobit_frame$age *
                             model_hetprobit_coef[-(1:ncol(model_hetprobit_frame))])
model_hetprobit_latent_adj <- model_hetprobit_latent_main / model_hetprobit_std    
# Убедимся, что посчитанная вручную и методом вероятности занятости совпадают
cbind(model_hetprobit_latent_adj, model_hetprobit$residuals)
cbind(pnorm(model_hetprobit_latent_adj), predict(model_hetprobit))


#Теперь проведем LM тест на гетероскедастичность
#Отберем переменные, которые влияют на распределение ошибки
frame_het <- model_hetprobit_frame[c("age")]    #фрейм из переменных, влияющих на дисперсию
#случайной ошибки
names(frame_het)=paste(names(frame_het), "_het")                             #добавляем приставку к именам переменных, чтобы
#отличить их при построении регрессии
#Посчитаем обобщенные остатки
probit_latent <- predict(model_probit);                                      #оценка латентной переменной
F_probit_latent <- pnorm(probit_latent);                                
f_probit_latent <- dnorm(probit_latent);
generalized_residuals <- ((model_probit_frame$educ - F_probit_latent) /      #обобщенный остаток
                            (F_probit_latent * (1 - F_probit_latent))) *
  f_probit_latent;
derivative_beta <- generalized_residuals *                                   #производные по коэффициентам основных переменных
  cbind(1, model_probit_frame[-1])                      
derivative_alpha <- (generalized_residuals *                                 #производные по коэффициентам переменных, влияющих
                       probit_latent) * frame_het                                 #на распределение случайной ошибки
#Проводим LM тест
ones_vector <- data.frame("my_ones" = derivative_beta[,1] /                  #вектор единиц
                            derivative_beta[,1])
ones_regression <- summary(lm(my_ones~ . + 0,
                              data=cbind(ones_vector,derivative_beta,
                                         derivative_alpha)));                              #регрессия на вектор единиц
R2 <- ones_regression$r.squared;                                             #коэффициент детерминации регрессии
n <- length(ones_regression$residuals);                                      #число наблюдений
LM_statistics <- R2 * n;                                                     #LM статистика
LM_test_p_value <- 1 - pchisq(q = LM_statistics, df = dim(frame_het)[2])      #p-value теста

LM_test_p_value 

# Часть 5.3

model_hetprobit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            educ + educ_age | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")
summary(model_hetprobit)



# Часть 5.4

acc_het_probit = sum(as.numeric((predict(model_hetprobit) > 0.5)) == h$marriage) / nrow(h)
acc_het_probit > acc_probit




# Часть 5.7
# Этот тест не получилось провести, так как модели не оцениваются

model_hetprobit <- hetglm(marriage ~ age + I(age ^ 2) | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")

lnL_R = logLik(model_hetprobit)

model_hetprobit_high <- hetglm(marriage ~ age + I(age ^ 2)| age,                       # уравнение дисперсии
                          data=h[h$educ_1 == 1,], 
                          family = binomial(link="probit"),
                          link.scale = "log")
summary(model_hetprobit_high)
lnL_high = logLik(model_hetprobit_high)



model_hetprobit_mid <- hetglm(marriage ~ age + I(age ^ 2) | age,                       # уравнение дисперсии
                               data=h[h$educ_2 == 1 | h$educ_3 == 1 ,], 
                               family = binomial(link="probit"),
                               link.scale = "log")
summary(model_hetprobit_mid)
lnL_mid = logLik(model_hetprobit_mid)

model_hetprobit_low <- hetglm(marriage ~ age + I(age ^ 2)      # записываем формулу
                                | age,                       # уравнение дисперсии
                              data=h[!(h$educ_2 == 1 | h$educ_2 == 1 | h$educ_3 == 1) ,], 
                              family = binomial(link="probit"),
                              link.scale = "log")
summary(model_hetprobit_low)
lnL_low = logLik(model_hetprobit_low)



lnL_F <- lnL_high + lnL_low + lnL_mid
# Посчитаем статистку LR теста
lr_stat <- 2 * (lnL_F - lnL_R)
p_value <- as.numeric(1 - pchisq(q = lr_stat, df = 5))


# Часть 5.8


model_hetprobit_1 <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            educ + educ_age | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")
summary(model_hetprobit_1)

lnL_UR = logLik(model_hetprobit_1)

model_hetprobit_2 <- hetglm(marriage ~ age + I(age ^ 2) | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")

lnL_R = logLik(model_hetprobit_2)

lr_stat <- 2 * (lnL_UR - lnL_R)
p_value <- as.numeric(1 - pchisq(q = lr_stat, df = 2))


# Часть 6.1
# Odds ratio - я не уверен, как их правильно записать и интерпретировать.

model_logit = glm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                     educ + educ_age, 
                   data = h, family = binomial(link = "logit")) 
summary(model_logit)

model_logit_coef = coef(model_logit)
model_logit_frame = model.frame(model_logit)


odds_ratio <- exp(model_logit_coef)    

delta_value <- 0.8  

odds_ratio_age_delta_value <- exp((model_logit_coef["age"] +
                                    model_logit_coef["I(age^2)"]* 2 * model_logit_frame$age + model_logit_coef["educ"]* model_logit_frame$educ)* delta_value)

total_effect_age_max = -1 * (model_linear_coef["age"] + model_logit_coef["educ"]* model_logit_frame$educ) /        # максимизирующий эффект возраста на занятость возраст
  (2 * model_linear_coef["I(age^2)"])   



# Часть 7

# Часть 7.1

bvp <- gjrm(list(marriage ~ age + I(age ^ 2) +         # записываем формулу
                   educ + educ_age + alc,                                                        # формула второго уравнения   
                 alc ~ age + I(age ^ 2) +         # записываем формулу
                   educ + educ_age), 
            data=h,
            Model = "B",                                                # двумерный пробит                                      
            margins = c("probit", "probit"))                            # маржинальные распределения можно изменить
# Важно обратить внимание на параметр theta, который представляет собой корреляцию
# между случайными ошибками и не значим в данном случае
summary(bvp)
# Можно получить безусловные предсказания вероятностей для каждого из уравнений
predict(bvp, 1)   # уравнение на занятость
predict(bvp, 2)   # уравнение на брак


# Часть 8
# Часть 8.1

model_hetprobit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            educ + educ_age | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")

model_hetlogit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            educ + educ_age | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="logit"),
                          link.scale = "log")

roc_probit <- roc(marriage ~ predict(model_probit, type=c("response")), data = model_probit_frame)
plot(roc_probit)
AUC_probit <- roc_probit$auc


roc_logit <- roc(educ ~ predict(model_logit, type=c("response")), data = model_logit_frame)
plot(roc_logit)
AUC_logit <- roc_logit$auc



# Часть 8.2

data.frame("Модель" = c("probit", "logit", "hetprobit", "hetlogit"),                # названия моделей
           "AIC" = c(AIC(model_probit), AIC(model_logit), AIC(model_hetprobit),  AIC(model_hetlogit)),   # AIC моделей
           "BIC" = c(BIC(model_probit), BIC(model_logit), BIC(model_hetprobit), BIC(model_hetlogit))) 








