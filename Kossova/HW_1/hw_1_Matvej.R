library("foreign")             # импорт данных
library("ggplot2")             # красивые графики
library("miscTools")           # Медианные значния по столбцам
library("pROC")                # ROC кривая  
library("boot")                # бутстрап
library("lmtest")              # дополнительные функции для тестирования гипотез
library("glmx")                # гетероскдестичный пробит
library("GJRM") 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(forecast)
library(texreg)
library(xtable)



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


h <- data.frame('age' = missing_to_NA(new_data$u_age))

# Переменная - наличие работы
h$work <- factor_to_binary(new_data$uj77,
    "РЕСПОНДЕНТ СЕЙЧАС РАБОТАЕТ, НАХ-СЯ В ОПЛАЧИВ. ИЛИ НЕОПЛАЧ.ОТПУСКЕ, В Т.Ч. ДЕКРЕТНОМ ИЛИ ПО УХОДУ ЗА РЕБЕНКОМ ДО 3 ЛЕТ")


# Переменная - употребляет ли индивид алкоголь
h$alc = factor_to_binary(new_data$um80.0, "Да, употребляете")


# Переменная - пол
h$male <- 0
h$male[new_data$uh5=="МУЖСКОЙ"] <- 1



#Переменная - состоит в оффициальном браке
h$marriage <- factor_to_binary(missing_to_NA(new_data$u_marst),"Состоите в зарегистрированном браке")


#Переменная взаимодействия занятости и возраста
h$workxage = h$work * h$age


# Вспомогательные переменные образования
h$educ = missing_to_NA(new_data$u_diplom)
h$educ_high <- factor_to_binary(h$educ, "законченное высшее образование и выше")
h$educ_mid <- factor_to_binary(h$educ, "законченное среднее образование")
h$educ_other <- as.numeric(h$educ_high == 0 & h$educ_mid == 0)
h$educ = NULL

# Посмотрим на данные.
summary(h)
summary(h[!is.na(h),])
h = na.omit(h)

# Оставим в выборке только мужчин старше 18
h = h[h$age >= 18 & h$male == 1,]




# Часть 2.2
# Женатые
print(xtable((summary(h[h$marriage == 1,]))), type="latex", include.rownames = FALSE)

# Неженатые
print(xtable((summary(h[h$marriage == 0,]))), type="latex", include.rownames = FALSE)


# Часть 2.3

# Средний и медианный индивиды
ind = rbind(t(colMeans(h)), t(colMedians(h)))
rownames(ind) = c("Mean", "Median")
print(xtable(ind), type="latex")


# Часть 2.4

g_1 = ggplot() + geom_histogram(data = h[h$marriage == 1,], aes(x = age), bins = 40) + xlab("Age, marriage = 1") + ggtitle("Гистограммы возраста в зависимости от наличия брака") 
g_2 = ggplot() + geom_histogram(data = h[h$marriage == 0,], aes(x = age), bins = 40) + xlab("Age, marriage = 0")
grid.arrange(g_1, g_2)


# Часть 3

# Часть  3.1

# Линейно-вероятностная модель

model_linear <- lm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                     work + workxage, 
                   data = h)  

print(xtable(summary(model_linear), type = "latex"))
summary(model_linear)
texreg(model_linear, booktabs = TRUE, dcolumn = TRUE)
# Часть 3.2

# Скорректируем бутстрапом ковариационную матрицу

model_coef <- coef(model_linear)
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
xtable(model_linear_boot, type = "latex")
# можем получить оценку ковариационной матрицы и вручную пользуясь тем, что
# свойство $t по строкам содержит оценки регрессионных коэффициентов с i-й итерации
colnames(model_linear_boot$t) <- names(coefficients(model_linear))
model_linear_cov <- cov(model_linear_boot$t)

# сравним старые и новые стандартные ошибки
boot_data = data.frame("Бутстрапированные стандартные ошибки" = sqrt(diag(model_linear_cov)),
           "Старые стандартные ошибки"            = sqrt(diag(vcov(model_linear))))
colnames(boot_data) = c("Бутстрапированные стандартные ошибки",  "Старые стандартные ошибки"  )
xtable(boot_data, digits = c(4,4,4))

# используем новую реализацию оценки ковариационной матрицы оценок регрессионных коэффициентов 
# для тестирования гипотез о регрессионных коэффициентах сохраняя допущение о нормальности, в случае
# нарушения которого полученные выводы могут оказаться ложными (так как неверно будут посчитаны p-value)
coeftest(model_linear, vcov. = model_linear_cov)
# протестируем гипотезы при помощи перцентильных бутстрапированных интервалов, опуская
# допущение о нормальности



# Часть 3.3

perc_conf_ints = matrix(nrow = 5, ncol = 3)
for (i in (1:5)){
  mboot = boot.ci(model_linear_boot,            # объект, содержащий результаты бутстрапирования
          type="perc",                  # указываем, что хотим получить перцентильный бутстрапированный интервал для оценок
          conf = c(0.95),    # указываем, каких уровней бутстрапированные доверительные интервалы нам нужны
          index = i)
  print(mboot$percent[4:5])
  perc_conf_ints[i,1:2] = mboot$percent[1,4:5]
}
perc_conf_ints[,3] = model_coef
perc_conf_ints = data.frame(perc_conf_ints)

# Тестируем значимость
colnames(perc_conf_ints) = c("Left", "Right", "Coef", "Rejected")
rownames(perc_conf_ints) = c("Intercept", "Age", "Age^2", "Work", "Workage")
perc_conf_ints$Rejected =  0 < perc_conf_ints$Left  | 0 > perc_conf_ints$Right

xtable(perc_conf_ints, type = "latex", digits = rep(4,ncol(perc_conf_ints)+1))
# Часть 3.4

# Часть 3.5

model_linear_frame <- model_linear$model                 # матрица использовавшихся в регрессии переменных
model_linear_coef <- coef(model_linear)                  # реализации оценок коэффициентов

model_linear_one_frame = model_linear_frame
model_linear_one_frame$work = 1
model_linear_one_frame$workxage = model_linear_one_frame$age


#Для переменной возраст
me_age <- model_linear_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * model_linear_frame$age +  model_linear_coef["work"] * model_linear_frame$work

# Для переменной занятость
me_work = as.matrix(model_linear_one_frame) %*% as.vector(model_linear_coef) - as.matrix(model_linear_frame)[,1:3] %*% as.vector(model_linear_coef)[1:3]


me_all_table = data.frame(age = me_age, work = me_work)

# Все предельные эффекты
xtable(head(me_all_table))

# Часть 3.6

# Средние предельные эффекты
me_mean = data.frame(mean(me_age), mean(me_work))

# Предельные эффекты для среднего индивида

me_mean_ind_age = me_age <- model_linear_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * mean(model_linear_frame$age) +  model_linear_coef["work"] * round(mean(model_linear_frame$work)) 

mean_matrix = colMeans(as.matrix(model_linear_frame))
mean_matrix[4] = round(mean_matrix[4])

mean_matrix_one = colMeans(as.matrix(model_linear_one_frame))
me_mean_ind_work = mean_matrix_one %*% as.vector(model_linear_coef) - mean_matrix[1:3]%*% as.vector(model_linear_coef)[1:3]

me_mean_ind = c(me_mean_ind_age, me_mean_ind_work)
names(me_mean_ind) = c("Age", "Work")


# Предельные эффекты для медианного индивида

median_matrix = colMedians(as.matrix(model_linear_frame))
median_matrix_one = colMedians(as.matrix(model_linear_one_frame))

me_median_ind_age =  model_linear_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * median(model_linear_frame$age) +  model_linear_coef["work"] * median(model_linear_frame$work) 

me_median_ind_work = median_matrix_one %*% as.vector(model_linear_coef) - median_matrix[1:3] %*% as.vector(model_linear_coef)[1:3]

me_median_ind = c(me_median_ind_age, me_median_ind_work)
names(me_median_ind) = c("Age", "Work")

# Предельные эффекты при моих характеристиках

me_my_age =  model_linear_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * 21 +  model_linear_coef["work"] * 0

me_my_work = c(1,21,441,1,21) %*% as.vector(model_linear_coef) - c(1,21,441,0,0) %*% as.vector(model_linear_coef)

# Общая таблица предельных эффектов
me_table = t(matrix(c(me_mean, me_mean_ind, me_median_ind, c(me_my_age, me_my_work)), ncol = 4, nrow = 2))
colnames(me_table) = c("Age", "Work")
rownames(me_table)  = c("Mean ME", "ME of mean person", "ME of median person", "ME of Me")
me_table
xtable(me_table, type = "latex", digits = c(4,4,4))

# Часть 3.7

# Сделаем вид, что переменная возраста не взаимодействует с занятостью.
cov_mat = vcov(model_linear)
var_dydx = cov_mat[2,2] + 4 * (21 ** 2) * cov_mat[3,3] + 4 * 21  * cov_mat[2,3]
sig_dydx = sqrt(var_dydx)

# Часть 3.8

acc_lin = sum(as.numeric(forecast(model_linear, newdata = model_linear_frame)$mean > 0.5) == h$marriage) / nrow(h)

acc_naive = sum(1 == h$marriage)/ nrow(h)

acc = data.frame(Linear = acc_lin, Naive = acc_naive)
rownames(acc) = "Accuracy"

xtable(acc, type = "latex", digits = c(4,4,4))

# Часть 3.9
# Доля прогнозов, по модулю больше 1
out = sum(forecast(model_linear, newdata = model_linear_frame)$mean > 1 | forecast(model_linear, newdata = model_linear_frame)$mean < 0) / nrow(h)

# Часть 3.10

# Прогноз для меня

my_char = t(as.matrix(c(1, 21, 441, 0, 0)))
colnames(my_char) = c("Intercept", "Age", "Age^2", "Work", "Workxage")
rownames(my_char) = "Me"
xtable(my_char, type = "latex", digits = rep(0,6))

my_forec = model_linear_coef %*% t(my_char)


# Часть 4
# Часть 4.1



model_probit = glm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                    work + workxage, 
                  data = h, family = binomial(link = "probit")) 
summary(model_probit)
print(xtable(summary(model_probit), type = "latex"))


model_probit_frame <- model_probit$model 
model_probit_frame$marriage = 1
model_probit_coef <- coef(model_probit)
model_probit_one_frame = model_probit_frame
model_probit_one_frame$work = 1
model_probit_one_frame$workxage = model_probit_one_frame$age
# Часть 4.4

latent_values_prob = as.matrix(model_probit_frame) %*% model_probit_coef

me_age_probit = (model_probit_coef["age"] + 2 *                                      
  model_linear_coef["I(age^2)"] * model_probit_frame$age +  model_probit_coef["work"] * model_probit_frame$work) * dnorm(latent_values_prob)

g_3 = ggplot() + geom_histogram(data = data.frame(me = me_age_probit), aes(x=me),  bins = 40) + xlab("Предельный эффект возраста, probit") + ggtitle("Гистограммы предельных эффектов возраста и занятости, probit")

plot(g_3)

sum(me_age_probit > 0)/length(me_age_probit)
# Больше положительных эффектов

me_work_probit = pnorm(as.matrix(model_probit_one_frame) %*% as.vector(model_probit_coef)) - pnorm(as.matrix(model_probit_frame)[,1:3] %*% as.vector(model_probit_coef)[1:3])

g_4 = ggplot() + geom_histogram(data = data.frame(me = me_work_probit), aes(x=me),  bins = 40) + xlab("Предельный эффект занятости, probit")

plot(g_4)
grid.arrange(g_3,g_4)

sum(me_work_probit > 0)/length(me_age_probit)
# Больше положительных эффектов


# Часть 4.6
me_mean_probit = data.frame(Age = mean(me_age_probit), Work = mean(as.vector(me_work_probit)))
me_mean_probit


mean_matrix = colMeans(as.matrix(model_probit_frame))

mean_matrix[4] = round(mean_matrix[4])
mean_latent_values_prob = mean_matrix %*% model_probit_coef



mean_matrix_one = colMeans(as.matrix(model_probit_one_frame))

me_mean_ind_probit_age = (model_probit_coef["age"] + 2 *                                      
                            model_probit_coef["I(age^2)"] * mean(model_probit_frame$age) +  model_probit_coef["work"] * round(mean(model_probit_frame$work))) * dnorm(mean_latent_values_prob)

me_mean_ind_probit_work = pnorm(as.vector(mean_matrix_one %*% as.vector(model_probit_coef))) - pnorm(mean_matrix[1:3] %*% as.vector(model_probit_coef)[1:3])

me_mean_ind_probit = c(me_mean_ind_probit_age, me_mean_ind_probit_work)
names(me_mean_ind_probit) = c("Age", "Work")

me_mean_ind_probit



median_latent_values_prob = colMedians(as.matrix(model_probit_frame)) %*% model_probit_coef


me_median_ind_probit_age = model_probit_coef["age"] + 2 *                                      
  model_probit_coef["I(age^2)"] * median(model_probit_frame$age) +  model_probit_coef["work"] * median(model_probit_frame$work) * dnorm(median_latent_values_prob)

me_median_ind_probit_work = pnorm(colMedians(as.matrix(model_probit_one_frame)) %*% as.vector(model_probit_coef)) - pnorm(colMedians(as.matrix(model_probit_frame)[,1:3]) %*% as.vector(model_probit_coef)[1:3])

me_median_ind_probit= c(me_median_ind_probit_age, me_median_ind_probit_work)
names(me_median_ind_probit) = c("Age", "Work")


me_my_probit_age = (model_probit_coef["age"] + 2*                                      
  model_probit_coef["I(age^2)"] *21 ) * dnorm(c(1,21,441,0,0) %*%as.vector(model_probit_coef))
me_my_probit_work = pnorm(c(1,21,441,1,21) %*% as.vector(model_probit_coef)) - pnorm(c(1,21,441,0,0) %*% as.vector(model_probit_coef))


# Общая таблица

me_probit_table = t(matrix(c(me_mean_probit, me_mean_ind_probit, me_median_ind_probit, c(me_my_probit_age, me_my_probit_work)), ncol = 4, nrow = 2))
colnames(me_probit_table) = c("Age", "Work")
rownames(me_probit_table)  = c("Mean ME", "ME of mean person", "ME of median person", "ME of Me")
me_probit_table
xtable(me_probit_table, type = "latex", digits = c(4,4,4))

# Часть 4.7

cov_mat_prob = vcov(model_probit)

var_prob_dydx = (cov_mat_prob[2,2] + 4 * (21**2) * cov_mat_prob[3,3] + 0**2 * cov_mat_prob[5,5] +
  4 * 21 * cov_mat_prob[2,3] + 2 * 0 * cov_mat_prob[2,5] + 4 * 21 * 0 * cov_mat_prob[3,5]) * (dnorm(c(1,21,441,0,0) %*% model_probit_coef)**2)

std_prob_dydx = sqrt(var_prob_dydx)
# Часть 4.8

acc_probit = sum(as.numeric(predict(model_probit, newdata = model_probit_frame) > 0.5) == h$marriage) / nrow(h)

acc_naive = sum(1 == h$marriage)/ nrow(h)

acc = data.frame(Probit = acc_probit, Naive = acc_naive)
rownames(acc) = "Accuracy"
xtable(acc, type = "latex", digits = c(4,4,4))


# Часть 4.9

my_char = c(1, 21, 441, 0, 0)

my_forec_probit = dnorm(model_probit_coef %*% my_char)


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
generalized_residuals <- ((model_probit_frame$work - F_probit_latent) /           #обобщенный остаток
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
                            work + workxage | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")

het_dat = data.frame(res = (model_probit$residuals**2), age = h$age)
het_dat = aggregate(res ~ age, het_dat, sum)
het_dat = het_dat[order(het_dat$age),]
het_dat = het_dat[het_dat$age>20,]

ggplot(data = het_dat) + geom_line(aes(x=age, y = res)) + ylab("Squared residuals") + xlab("Age") + ggtitle("Квадраты остатков, агрегированные по возрасту")
plot(het_dat$res)

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
generalized_residuals <- ((model_probit_frame$work - F_probit_latent) /      #обобщенный остаток
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
                            work + workxage | age,                       # уравнение дисперсии
                          data=h, 
                         family = binomial(link="probit"),
                          link.scale = "log")
summary(model_hetprobit)
a = summary(model_hetprobit)
xtable(a$coefficients, digits = c(4,4,4,4,4))
# Часть 5.4

acc_het_probit = sum(as.numeric((predict(model_hetprobit) > 0.5)) == h$marriage) / nrow(h)
acc_het_probit > acc_probit
acc = data.frame(Hetprobit = acc_het_probit, Probit = acc_probit, Naive = acc_naive)
rownames(acc) = "Accuracy"
xtable(acc, type = "latex", digits = c(4,4,4,4))




# Часть 5.7
model_hetprobit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            work + workxage | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")



model_hetprobit_high <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            work + workxage | age,                       # уравнение дисперсии
                          data=h[h$educ_high == 1,], 
                          family = binomial(link="probit"),
                          link.scale = "log")

model_hetprobit_mid <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                                 work + workxage | age,                       # уравнение дисперсии
                               data=h[h$educ_mid == 1,], 
                               family = binomial(link="probit"),
                               link.scale = "log")

model_hetprobit_low <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                                work + workxage | age,                       # уравнение дисперсии
                              data=h[h$educ_other == 1 ,], 
                              family = binomial(link="probit"),
                              link.scale = "log")


lnL_R = logLik(model_hetprobit)
lnL_high = logLik(model_hetprobit_high)
lnL_mid = logLik(model_hetprobit_mid)
lnL_low = logLik(model_hetprobit_low)

lnL_F <- lnL_high + lnL_low + lnL_mid
# Посчитаем статистку LR теста
lr_stat <- 2 * (lnL_F - lnL_R)
p_value <- as.numeric(1 - pchisq(q = lr_stat, df = 5))


# Часть 5.8


model_probit_1 <- glm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            work + workxage ,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"))
summary(model_probit_1)

lnL_UR = logLik(model_probit_1)

model_probit_2 <- glm(marriage ~ age + I(age ^ 2) ,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"))

lnL_R = logLik(model_probit_2)

lr_stat <- 2 * (lnL_UR - lnL_R)
p_value <- as.numeric(1 - pchisq(q = lr_stat, df = 2))


# Часть 6.1


model_logit = glm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                     work + workxage, 
                   data = h, family = binomial(link = "logit")) 
summary(model_logit)

xtable(summary(model_logit), type = "latex")
model_logit_coef = coef(model_logit)
model_logit_frame = model.frame(model_logit)

# Часть 6.2
# Отношение шансов
odds_ratio <- exp(as.matrix(model_logit_frame) %*% model_logit_coef)    

delta_val = 1

fr_del_age = model_logit_frame
fr_del_age$age = fr_del_age$age +1
fr_del_age$`I(age^2)` = fr_del_age$age^2
fr_del_age$workxage = fr_del_age$age * fr_del_age$work


fr_work_one = model_logit_frame
fr_work_one$work = 1
fr_work_one$workxage = fr_del_age$age

fr_work_zero = model_logit_frame
fr_work_zero$workxage = 0
fr_work_zero$work = 0

# Изменения в отношениях Шансов.
# По возрасту
odds_age = exp(c(1,22,484, 0, 0) %*% model_logit_coef)   - exp(c(1,21,441, 0, 0) %*% model_logit_coef)   

# По наличию работы
odds_work = exp(c(1,21,441, 1, 21) %*% model_logit_coef)   - exp(c(1,21,441, 0, 0) %*% model_logit_coef)   

odds = data.frame(Age = odds_age, Work = odds_work)
row.names(odds) = "Delta ratio"

xtable(odds, digits = 4)

# Часть 7

# Часть 7.1

bvp <- gjrm(list(marriage ~ age + I(age ^ 2) +         # записываем формулу
                   work + workxage + alc,                                                        # формула второго уравнения   
                 alc ~ age + I(age ^ 2) +         # записываем формулу
                   work + workxage), 
            data=h,
            Model = "B",                                                # двумерный пробит                                      
            margins = c("probit", "probit"))                            # маржинальные распределения можно изменить
# Важно обратить внимание на параметр theta, который представляет собой корреляцию
# между случайными ошибками и не значим в данном случае
a = summary(bvp)

xtable(a$tableP1)
xtable(a$tableP2)


# Часть 8
# Часть 8.1

model_hetprobit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            work + workxage | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="probit"),
                          link.scale = "log")

model_hetlogit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            work + workxage | age,                       # уравнение дисперсии
                          data=h, 
                          family = binomial(link="logit"),
                          link.scale = "log")

roc_probit <- roc(marriage ~ predict(model_probit, type=c("response")), data = model_probit_frame)
AUC_probit <- roc_probit$auc
acc_het_probit = sum(as.numeric(predict(model_hetprobit, newdata = model_probit_frame) > 0.5) == h$marriage) / nrow(h)
roc_logit <- roc(work ~ predict(model_logit, type=c("response")), data = model_logit_frame)
AUC_logit <- roc_logit$auc
acc_het_logit = sum(as.numeric(predict(model_hetlogit, newdata = model_logit_frame) > 0.5) == h$marriage) / nrow(h)
print(acc_het_logit == acc_het_probit)
grid.arrange(ggroc(roc_probit) + ggtitle("ROC-кривая для hetprobit-модели"), ggroc(roc_logit)+ ggtitle("ROC-кривая для hetlogit-модели"), nrow = 1, ncol = 2)


ssp = data.frame(sens = roc_probit$sensitivities, spec = roc_probit$specificities)
ssl = data.frame(sens = roc_logit$sensitivities, spec = roc_logit$specificities)

ssp = ssp[ssp$spec <0.81 & ssp$spec > 0.79 ,]
ssl = ssl[ssl$spec <0.83 & ssl$spec > 0.77 ,]

# Часть 8.2

b = data.frame("Модель" = c("probit", "logit", "hetprobit"),                # названия моделей
           "AIC" = c(AIC(model_probit), AIC(model_logit), AIC(model_hetprobit)),   # AIC моделей
           "BIC" = c(BIC(model_probit), BIC(model_logit), BIC(model_hetprobit))) 

xtable(b)


# Часть 8.4

train = head(h, -500)
test = tail(h, 500)


model_hetprobit <- hetglm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                            work + workxage | age,                       # уравнение дисперсии
                          data=train, 
                          family = binomial(link="probit"),
                          link.scale = "log")

model_probit = glm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                     work + workxage, 
                   data = train, family = binomial(link = "probit")) 

model_logit = glm(marriage ~ age + I(age ^ 2) +         # записываем формулу
                    work + workxage, 
                  data = train, family = binomial(link = "logit")) 



acc_1 = sum(as.numeric(predict(model_hetprobit, newdata = test) > 0.5) == as.vector(test$marriage))

acc_2 = sum(as.numeric(predict(model_probit, newdata = test) > 0.5) == as.vector(test$marriage))

acc_3 = sum(as.numeric(predict(model_logit, newdata = test) > 0.5) == as.vector(test$marriage))

acc_4 = sum(1 == as.vector(test$marriage))

mse = data.frame(Hetprobit = acc_1, Probit = acc_2, Logit = acc_3, Naive = acc_4) / nrow(test)
row.names(mse) = "MSE"

xtable(mse)
