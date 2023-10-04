// saved as hierarchical_var.stan
data {
  int N; // number of observations (number of rows in the panel)
  int K; // number of dimensions of the outcome variable
  int I; // number of individuals
  int T; // The greatest number of time periods for any individual
  int<lower = 1, upper = I> individual[N]; // integer vector the same length 
  // as the panel, indicating which individual each row corresponds to
  int<lower = 1, upper = T> time[N]; // integer vector with individual-time 
  // (not absolute time). Each individual starts at 1
  matrix[N, K] Y; // the outcome matrix, each individual stacked on each 
  // other, observations ordered earliest to latest
}
parameters {
  // individual-level parameters
  corr_matrix[K] Omega_local[I]; // cholesky factor of correlation matrix of 
  // residuals each individual (across K outcomes)
  vector<lower = 0>[K] tau[I]; // scale vector for residuals
  matrix[K, K] z_beta[I];
  vector[K] z_alpha[I];
  
  // hierarchical priors (individual parameters distributed around these)
  real<lower = 0, upper = 1> rho;
  corr_matrix[K] Omega_global;
  vector[K] tau_location;
  vector<lower =0>[K] tau_scale;
  matrix[K,K] beta_hat_location;
  matrix<lower = 0>[K,K] beta_hat_scale;
  vector[K] alpha_hat_location;
  vector<lower = 0>[K] alpha_hat_scale;
}
transformed parameters {
  matrix[K, K] beta[I]; // VAR(1) coefficient matrix
  vector[K] alpha[I]; // intercept matrix
  corr_matrix[K] Omega[I];
  
  for(i in 1:I) {
    alpha[i] = alpha_hat_location + alpha_hat_scale .*z_alpha[i];
    beta[i] = beta_hat_location + beta_hat_scale .*z_beta[i];
    Omega[i] = rho*Omega_global + (1-rho)*Omega_local[i];
  }
    
}
model {
  // hyperpriors
  rho ~ beta(2, 2);
  tau_location ~ cauchy(0, 1);
  tau_scale ~ cauchy(0, 1);
  alpha_hat_location ~ normal(0, 1);
  alpha_hat_scale ~ cauchy(0, 1);
  to_vector(beta_hat_location) ~ normal(0, .5);
  to_vector(beta_hat_scale) ~ cauchy(0, .5);
  Omega_global ~ lkj_corr(1);

  
  // hierarchical priors
  for(i in 1:I) {
    // non-centered parameterization
    z_alpha[i] ~ normal(0, 1);
    to_vector(z_beta[i]) ~ normal(0, 1);
    tau[i] ~ normal(tau_location, tau_scale);
    Omega_local[i] ~ lkj_corr(10);
  }
  
  // likelihood
  for(n in 1:N) {
    if(time[n]>1) {
      Y[n] ~ multi_normal(alpha[individual[n]] + beta[individual[n]]*Y[n-1]', 
                          quad_form_diag(Omega[individual[n]], tau[individual[n]]));
    }
  }
}