// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N; // number of obs
  int<lower=0> D_pre; // number of covariates before internvention
  int<lower=0> D_post; // number of covariates after internvention
  int<lower=0> M; // number of counties
  int<lower=0> y[N]; // deaths
  matrix[N, D_pre] X_pre; // covars for pre-trend
  matrix[N, D_post] X_post;  // covars for post-trend, the only one here is days since intervention
  vector[N] offset;  // log of population
  int<lower=0> county_id[N];  // county indicator
  int<lower=0> nchs_id[N];  // nchs indicator
  matrix[N, 3] tpoly_pre; // days since threhsold
  matrix[N, 2] tpoly_post; // days since intervention
  // for temporal correlation
  int<lower=0> Tmax;
  int<lower=1, upper=Tmax * 6> time_nchs_id[N];
}


parameters {
  // spatial effect related
  corr_matrix[3] Omega_rand_eff;
  matrix<lower=-10,upper=10>[M, 3] rand_eff;
  vector<lower=0.0, upper=10.0>[3] scale_rand_eff;

  // other params
  matrix<lower=-10,upper=10>[6, 3] nchs_pre;
  matrix<lower=-10,upper=10>[D_pre, 3] beta_covars_pre;
  row_vector<lower=-10,upper=10>[3] baseline_pre;
  matrix<lower=-10,upper=10>[D_post, 2] beta_covars_post;
  row_vector<lower=-10,upper=10>[2] baseline_post;
  matrix<lower=-10,upper=10>[6, 2] nchs_post;
  real<lower=0.025, upper=200.0> overdisp;

  // for AR(1) error
  vector<lower=-10,upper=10>[Tmax * 6] time_eff;
  real<lower=0.0, upper=1.0> autocor;
  real<lower=0.0, upper=20.0> scale_time_eff;
}

transformed parameters {
  // covariance matrix cor random effects
  cov_matrix[3] Sigma_rand_eff = quad_form_diag(Omega_rand_eff, scale_rand_eff);
  // heterogeneous effects unrolled
  vector[N] rand_eff_term = rows_dot_product(
    rand_eff[county_id, 1:3], tpoly_pre
  );

  vector[N] pre_term = rows_dot_product(
    (
      + X_pre * beta_covars_pre  // interaction with covariates pre-interv
      + rep_matrix(baseline_pre, N)
      + nchs_pre[nchs_id, 1:3]
    ),
    tpoly_pre
  );
  vector[N] post_term = rows_dot_product(
    (
      X_post * beta_covars_post  // interaction with covariates post-interv
      + rep_matrix(baseline_post, N)
      + nchs_post[nchs_id, 1:2]
    ),
    tpoly_post
  );
  vector[N] time_term = time_eff[time_nchs_id];
  vector[N] log_rate_pre_interv = offset + rand_eff_term + pre_term + time_term;
  vector[N] log_rate = log_rate_pre_interv + post_term;
}

model {
  // rand effects'
  overdisp ~ exponential(1.0);
  Omega_rand_eff ~ lkj_corr(2.0);
  scale_rand_eff ~ normal(0, 10.0);
  // random effect priors (independent)
  for (i in 1:M) {
    row(rand_eff, i) ~ multi_normal(rep_vector(0.0, 3), Sigma_rand_eff);
  }
  // scale_spatial_eff ~ normal(0, 10.0);
  for (j in 1:3) {
    col(rand_eff, j) ~ normal(0.0, 100.0);
  }

  // time eff
  for (j in 1:6) {
      time_eff[(Tmax * (j - 1) + 2):(Tmax * j)] ~ normal(autocor * time_eff[(Tmax * (j - 1) + 1):(Tmax * j - 1)], scale_time_eff);
      time_eff[Tmax * (j - 1) + 1] ~ normal(0.0, scale_time_eff);
      sum(time_eff[(Tmax * (j - 1) + 1):(Tmax * j)]) ~ normal(0., 0.001);
  }
  autocor ~ beta(9, 10);
  scale_time_eff ~ normal(0, 1.0);


  // otherparams
  to_vector(beta_covars_pre) ~ normal(0, 10.0);
  to_vector(beta_covars_post) ~ normal(0, 10.0);
  nchs_pre[1,1:3] ~ normal(0, 0.001);
  nchs_post[1,1:2] ~ normal(0, 0.001);
  to_vector(nchs_pre[2:6,1:2]) ~ normal(0, 10.0);
  to_vector(nchs_post[2:6,1:2]) ~ normal(0, 10.0);
  baseline_post ~ normal(0.0, 10.0);
  baseline_pre ~ normal(0.0, 10.0);
  
  // likelihood
  y ~ neg_binomial_2(exp(log_rate) + 1e-8, overdisp);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    real rate_ = exp(fmax(fmin(log_rate[n], 12.0), -12.0));
    real overdisp_ = fmin(fmax(overdisp, 0.025), 200.0);
    log_lik[n] = neg_binomial_2_lpmf(y[n] | rate_, overdisp_);
  }
}
