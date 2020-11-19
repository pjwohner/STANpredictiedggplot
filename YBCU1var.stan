data {
  // Define variables in data
  // Number of level-1 observations
  int<lower=0> Ni;
  vector[Ni]treecovp;//predictor variable 
  int<lower = 0, upper = 1> use[Ni];//declare the y variable (i)
}

parameters {
  real beta_0;// Population intercept
  real beta_1; //SLOPE for variable 1
  
  // Level-1 errors
  real<lower=0> sigma_e0;
  vector[Ni] u_0;
    }

transformed parameters  {
 real ogit[Ni]; 
    //the linear predictor for the odds of individual observations
  for(n in 1:Ni){
     ogit[n] = beta_0 + beta_1 * treecovp[n];}
  }   

model {
  // Prior part of Bayesian inference
  
  beta_0 ~ normal(0, 1);
  beta_1 ~ normal(0, 1);
  sigma_e0 ~ normal(0, 1);
  u_0 ~ normal(0, 1);
  
  // Likelihood 
   use ~ bernoulli_logit(beta_0 + beta_1 * treecovp);
  }  

generated quantities {

 //generate odds=exponentiated backtransformed overall slope and intercept estimates  
  real oddsintercept = 1/exp(beta_0);//for negative estimate
  real oddsbeta1 = exp(beta_1);//treecovp
 
 //initialize variable to get the odds of the logit
//real<lower=0> use_rep[Ni]; 
//initialize variable to get prob from odds
//real<lower=0,upper=1> prob[Ni]; 
//log liklihood 
  vector[Ni] log_lik;
  for (n in 1:Ni){
  log_lik[n] = bernoulli_logit_lpmf(use[n] | ogit[n]);  
  }
//sample predicted odds values from the model for posterior predictive checks
//this delogarythmize the logits to odds ratios by using an exponentiate poisson function
//for(n in 1:Ni){
//use_rep[n] = poisson_log_rng(ogit[n]);
//} 
//get predicted probabilities for plotting values
//for(n in 1:Ni){
//prob[n] = use_rep[n]/(1+use_rep[n]);

//} 

}
