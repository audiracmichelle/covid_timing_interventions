// The input data is a vector 'y' of length 'N'.
functions {
    vector clamp_zero(vector x) {
        return 0.5 * (x + fabs(x));
    }
    // vector clamp_lower(vector x, real val) {
    //     return val + clamp_zero(x - val);
    // }
    // vector clamp_upper(vector x, real val) {
    //     return - clamp_zero(val - x) - val;
    // }
    // vector clamp(vector x, real lower, real upper) {
    //     return clamp_upper(clamp_lower(x, lower), upper);
    // }
    // these indicator are hacks because rstan doesn't seem
    // to be able to handle vectorized indicators
    vector indicator_lower(vector x, real lower) {
        return clamp_zero(x - lower) ./ (clamp_zero(x - lower) + 1e-6);
    }
    vector indicator_upper(vector x, real upper) {
        return clamp_zero(upper - x) ./ (clamp_zero(upper - x) + 1e-6);
    }
    vector indicator(vector x, real lower, real upper) {
        return indicator_lower(indicator_upper(x, upper), lower);
    }
    // see kar et al 2017 below for the definition
    vector cable_transition(vector t, real tau, real gam) {
        return 0.25 * square(clamp_zero(t - tau + gam)) / (gam + 1e-3);
    }
    matrix cable_post(vector t, real tau, real gam) {
        return append_col(
            (
                cable_transition(t, tau, gam) .* indicator_upper(t, tau + gam)
                + clamp_zero(t - tau) .* indicator_lower(t, tau + gam)
            ),
            square(clamp_zero(t - tau - gam))
        );
    }
}

data {
  int<lower=0> N; // number of obs
  int<lower=0> D_pre; // number of covariates before internvention
  int<lower=0> D_post; // number of covariates after internvention
  int<lower=0> y[N]; // deaths
  vector[N] offset;  // log of population
  matrix[N, D_pre] X_pre; // covars for pre-trend
  matrix[N, D_post] X_post;  // covars for post-trend, the only one here is days since intervention
  matrix[N, 3] tpoly_pre; // days since threhsold
  //
  int<lower=0> N_states; // number of states
  int<lower=1, upper=N_states> state_id[N];  // county indicator
  // int<lower=0> tm1_index[N];
  int<lower=1, upper=6> nchs_id[N];  // nchs indicator
  int<lower=0> M; // number of counties
  int<lower=1, upper=M> county_id[N];  // county indicator
  int<lower=0> county_brks[M + 1];
  int<lower=0> county_lens[M];
}


parameters {
  matrix<lower=-10,upper=10>[6, 3] nchs_pre;
  matrix<lower=-10,upper=10>[D_pre, 3] beta_covars_pre;
  row_vector<lower=-10,upper=10>[3] baseline_pre;
  matrix<lower=-10,upper=10>[D_post, 2] beta_covars_post;
  row_vector<lower=-10,upper=10>[2] baseline_post;
  matrix<lower=-10,upper=10>[6, 2] nchs_post;
  real<lower=0.02, upper=100.0> overdisp;
  //
  corr_matrix[3] Omega_rand_eff;
  corr_matrix[3] Omega_state_eff;
  matrix<lower=-10.0,upper=10.0>[M, 3] rand_eff;
  matrix<lower=-10.0,upper=10.0>[N_states, 3] state_eff;
  vector<lower=0.001, upper=20.0>[3] scale_rand_eff;
  vector<lower=0.001, upper=20.0>[3] scale_state_eff;
  // for temporal error
  vector<lower=-10.0, upper=10.0>[N] time_term;
  // real<lower=0.0, upper=1.0> autocor_unc;
  // real<lower=0.001, upper=20.0> scale_time_term;
  // for bent
  vector[N] days_since_intrv; // days since/before intervention
  real<lower=0, upper=1> lag_scaled;
  real<lower=0.0, upper=1.0> duration_unc;
}

transformed parameters {
  cov_matrix[3] Sigma_rand_eff = quad_form_diag(Omega_rand_eff, scale_rand_eff);
  cov_matrix[3] Sigma_state_eff = quad_form_diag(Omega_state_eff, scale_state_eff);

  vector[N] rand_eff_term = rows_dot_product(
    rand_eff[county_id, :],  // random effects unfolded
    tpoly_pre
  );

  vector[N] state_eff_term = rows_dot_product(
    state_eff[state_id, :],  // random effects unfolded
    tpoly_pre
  );

  real<lower=11, upper=16> lag = 11.0 + lag_scaled * 5.0;
  real duration = 8 * duration_unc;
  matrix[N, 2] tpoly_post = cable_post(0.01 * days_since_intrv, 0.01 * lag, 0.01 * duration);
  //                                    ^ 0.01 is just for stability
  // real autocor = 0.5 + 0.5 * autocor_unc;
  //
  
  vector[N] pre_term = rows_dot_product(
    (
      + X_pre * beta_covars_pre  // interaction with covariates pre-interv
      + rep_matrix(baseline_pre, N)
      + nchs_pre[nchs_id, :]
    ),
    tpoly_pre
  );
  vector[N] post_term = rows_dot_product(
    (
      X_post * beta_covars_post  // interaction with covariates post-interv
      + rep_matrix(baseline_post, N)
      + nchs_post[nchs_id, :]
    ),
    tpoly_post
  );
  vector[N] log_rate_pre_interv = offset + state_eff_term + rand_eff_term + pre_term + time_term;
  vector[N] log_rate = log_rate_pre_interv + post_term;
}

model {
  // parameter priors
  overdisp ~ exponential(1.0);
  // rand eff stuff
  Omega_rand_eff ~ lkj_corr(2.0);
  scale_rand_eff ~ normal(0, 10.0);
  Omega_state_eff ~ lkj_corr(2.0);
  scale_state_eff ~ normal(0, 10.0);
  for (i in 1:M)
    row(rand_eff, i) ~ multi_normal(rep_vector(0.0, 3), Sigma_rand_eff);
  for (i in 1:N_states)
    row(state_eff, i) ~ multi_normal(rep_vector(0.0, 3), Sigma_state_eff);
  for (j in 1:3) {
    col(rand_eff, j) ~ normal(0, 100.0);  // tiny reg
    col(state_eff, j) ~ normal(0, 100.0);  // tiny reg
  }
  for (i in 1:M)
    row(rand_eff, i) ~ multi_normal(rep_vector(0.0, 3), Sigma_rand_eff);
  for (j in 1:3)
    col(rand_eff, j) ~ normal(0, 100.0);  // tiny reg
  
  // basic hockey tick stuff
  to_vector(beta_covars_pre) ~ normal(0, 10.0);
  to_vector(beta_covars_post) ~ normal(0, 10.0);
  nchs_pre[1,:] ~ normal(0, 0.001);
  nchs_post[1,:] ~ normal(0, 0.001);
  to_vector(nchs_pre[2:6,:]) ~ normal(0, 10.0);
  to_vector(nchs_post[2:6,:]) ~ normal(0, 10.0);
  baseline_post ~ normal(0.0, 10.0);
  baseline_pre ~ normal(0.0, 10.0);
  // cable bent priors
  lag_scaled ~ beta(1.89, 5.11);
  duration_unc ~ beta(2.0, 4.0);
  // AR(1) priors implicit is marginal scale 1.0
  for (j in 1:M) {
    sum(time_term[(county_brks[j] + 1):(county_brks[j + 1])]) ~ normal(0.0, 0.001 * county_lens[j]);
    // time_term[(county_brks[j] + 2):(county_brks[j + 1])] ~ normal(autocor * time_term[(county_brks[j] + 1):(county_brks[j + 1]) - 1], 0.1);
    target += dot_self(
      time_term[(county_brks[j] + 2):(county_brks[j + 1])] - time_term[(county_brks[j] + 1):(county_brks[j + 1]) - 1]
    ) / square(0.1);
    // time_term[county_brks[j] + 1] ~ normal(0, 0.1 / sqrt(1 - square(autocor)));
    time_term[county_brks[j] + 1] ~ normal(0, 0.1);
  }
  // scale_time_term ~ inv_gamma(1.0, 1.0);
  // autocor_unc ~ beta(5, 5);

  // likelihood
  y ~ neg_binomial_2(exp(log_rate) + 1e-8, overdisp);
}

generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    real rate_ = exp(fmax(fmin(log_rate[n], 12.0), -12.0));
    real phi_ = fmin(fmax(overdisp, 0.01), 100.0);
    log_lik[n] = neg_binomial_2_lpmf(y[n] | rate_, phi_);
  }
}
