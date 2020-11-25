data {
  int<lower=0> N;//number of observations
  vector[N] treecovp;//dependednt variable
  int<lower = 0, upper = 1> use[N];//declare the y variable as binary
  }

parameters {
  real beta0;
  real beta1;
     }

model {
  // Prior part of Bayesian inference
  beta0 ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  // Likelihood
  use ~ bernoulli_logit(beta0 + beta1 * treecovp);//model
    }  

generated quantities {

vector[N] y_hat;
for (n in 1:N)
y_hat[n] = inv_logit(beta0 + beta1 *treecovp[n]);
}