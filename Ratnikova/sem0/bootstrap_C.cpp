#include <RcppArmadillo.h>               //Подключаем библиотеку

using namespace Rcpp;                    //Используем пространство имен

// [[Rcpp::depends("RcppArmadillo")]]

// [[Rcpp::export]]    
NumericMatrix bootstrap_C(          
    NumericMatrix X,                
    NumericVector y,
		int bootstrap_iterations = 100)
{
  
  //NumericVector аналог обычного вектора c() в R
  //NumericMatrix аналог обычной матрицы matrix() в R
  
  //Для удобства загрузим некоторые функции из R
  
  Rcpp::Environment stats_env("package:stats"); //для начала подключаем пространство из R
  Rcpp::Function cov_R = stats_env["cov"];      //достаем функцию из этого пространства
  
  Rcpp::Environment base_env("package:base");
  Rcpp::Function diag_R = base_env["diag"];
  
  //Сохраним количество наблюдений и независимых переменных
  
  int n = X.nrow();
  int m = X.ncol();
  
  //Инициализируем некоторые векторы заранее, чтобы не тратить ресурсы на их
  //инициализацию внутри цикла
  
  NumericVector sample_ind_bootstrap = NumericVector(n);
  
  NumericMatrix X_bootstrap = NumericMatrix(n, m);
  NumericVector y_bootstrap = NumericVector(n);
  
  NumericMatrix B_hat = NumericMatrix(bootstrap_iterations, m);
  
  arma::mat X_arma = as<arma::mat>(X);
  arma::vec y_arma = as<arma::vec>(y);

  //Приступим к бутстрапированию
  
  for (int i = 0; i < bootstrap_iterations; i++)
  {
    //Создаем выборку индексов с возвращением
    //Функция runif(n, 0, n) генерирует n реализаций из равногомерного
    //распределения на отрезке от 0 до n
    //Функция floor убирает десятичную часть числа
    sample_ind_bootstrap = floor(runif(n, 0, n));
    
    //Создаем новую выборку, используя выборку из индексов,
    //параллельно радуясь быстрым циклам в C++
    for (int j = 0; j < n; j++)
    {
      
      for (int t = 0; t < m; t++)
      {
        X_bootstrap(j, t) = X(sample_ind_bootstrap[j], t);
      }
      
      y_bootstrap[j] = y[sample_ind_bootstrap[j]];
    }

    //Для ускрорения расчетов конвертируем наши матрицы в другой класс, с экземплярами
    //которого можно быстро и легко совершать матричные операции.
    //В идеале это нужно делать до цикла, но с целью упрощения кода
    //несколько пожертвуем производительностью
    X_arma = as<arma::mat>(X_bootstrap);
    y_arma = as<arma::vec>(y_bootstrap);
    
    //Рассчитаем оценки регрессионных коэффициентов используя Armadillo
    //Функция wrap конвертирует armar::mat в NumericMatrix или NumericVector
    NumericVector B_hat_new = wrap(inv(X_arma.t() * X_arma) * X_arma.t() * y_arma);
    B_hat(i, _) = B_hat_new;
  }
  
  //Оценим ковариационную матрицу оценок регрессионных коэффициентов
  NumericMatrix B_hat_cov = cov_R(B_hat);
  rownames(B_hat_cov) = colnames(X);
  colnames(B_hat_cov) = colnames(X);
  
	return(B_hat_cov);
}