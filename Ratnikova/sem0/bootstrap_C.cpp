#include <RcppArmadillo.h>               //���������� ����������

using namespace Rcpp;                    //���������� ������������ ����

// [[Rcpp::depends("RcppArmadillo")]]

// [[Rcpp::export]]    
NumericMatrix bootstrap_C(          
    NumericMatrix X,                
    NumericVector y,
		int bootstrap_iterations = 100)
{
  
  //NumericVector ������ �������� ������� c() � R
  //NumericMatrix ������ ������� ������� matrix() � R
  
  //��� �������� �������� ��������� ������� �� R
  
  Rcpp::Environment stats_env("package:stats"); //��� ������ ���������� ������������ �� R
  Rcpp::Function cov_R = stats_env["cov"];      //������� ������� �� ����� ������������
  
  Rcpp::Environment base_env("package:base");
  Rcpp::Function diag_R = base_env["diag"];
  
  //�������� ���������� ���������� � ����������� ����������
  
  int n = X.nrow();
  int m = X.ncol();
  
  //�������������� ��������� ������� �������, ����� �� ������� ������� �� ��
  //������������� ������ �����
  
  NumericVector sample_ind_bootstrap = NumericVector(n);
  
  NumericMatrix X_bootstrap = NumericMatrix(n, m);
  NumericVector y_bootstrap = NumericVector(n);
  
  NumericMatrix B_hat = NumericMatrix(bootstrap_iterations, m);
  
  arma::mat X_arma = as<arma::mat>(X);
  arma::vec y_arma = as<arma::vec>(y);

  //��������� � ����������������
  
  for (int i = 0; i < bootstrap_iterations; i++)
  {
    //������� ������� �������� � ������������
    //������� runif(n, 0, n) ���������� n ���������� �� ��������������
    //������������� �� ������� �� 0 �� n
    //������� floor ������� ���������� ����� �����
    sample_ind_bootstrap = floor(runif(n, 0, n));
    
    //������� ����� �������, ��������� ������� �� ��������,
    //����������� ������� ������� ������ � C++
    for (int j = 0; j < n; j++)
    {
      
      for (int t = 0; t < m; t++)
      {
        X_bootstrap(j, t) = X(sample_ind_bootstrap[j], t);
      }
      
      y_bootstrap[j] = y[sample_ind_bootstrap[j]];
    }

    //��� ���������� �������� ������������ ���� ������� � ������ �����, � ������������
    //�������� ����� ������ � ����� ��������� ��������� ��������.
    //� ������ ��� ����� ������ �� �����, �� � ����� ��������� ����
    //��������� ���������� �������������������
    X_arma = as<arma::mat>(X_bootstrap);
    y_arma = as<arma::vec>(y_bootstrap);
    
    //���������� ������ ������������� ������������� ��������� Armadillo
    //������� wrap ������������ armar::mat � NumericMatrix ��� NumericVector
    NumericVector B_hat_new = wrap(inv(X_arma.t() * X_arma) * X_arma.t() * y_arma);
    B_hat(i, _) = B_hat_new;
  }
  
  //������ �������������� ������� ������ ������������� �������������
  NumericMatrix B_hat_cov = cov_R(B_hat);
  rownames(B_hat_cov) = colnames(X);
  colnames(B_hat_cov) = colnames(X);
  
	return(B_hat_cov);
}