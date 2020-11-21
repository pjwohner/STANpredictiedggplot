data {
  int<lower=0> N;
  vector[N] treecovp;
  int<lower = 0, upper = 1> use[N];//declare the y variable (i)
  int<lower = 0> N_new;
  vector[N_new] x_new;
}

parameters {
  real beta0;
  real beta1;
     }

model {
  // Prior part of Bayesian inference
  beta0 ~ normal(0, 1);
  beta1 ~ normal(0, 1);
  // Likelihood
  use ~ bernoulli_logit(beta0 + beta1 * treecovp);
    }  

generated quantities {
  //generate odds=exponentiated backtransformed overall slope and intercept estimates  
  //real oddsintercept = 1/exp(beta_0);//for negative estimate
  //real oddsbeta1 = exp(beta_1);//treecovp


//initialize variable to get prob from odds
vector[N_new] prob;
vector[N_new] y_new;
vector[N_new] log_lik;
for (n in 1:N_new){
y_new[n] = bernoulli_logit_rng(beta0 + x_new[n] * beta1);
}

//log liklihood 
  for (n in 1:N_new){
  log_lik[n] = bernoulli_logit_lpmf(use[n] | y_new[n]);  
  }
//sample predicted odds values from the model for posterior predictive checks
//this delogarythmize the logits to odds ratios by using an exponentiate function
//for(n in 1:N_new){
//log_lik[n] = bernoulli_logit_rng(y_new[n]);
//} 
//get predicted probabilities for plotting values
for(n in 1:N_new){
prob[n] = log_lik[n]/(1+log_lik[n]);
} 

}