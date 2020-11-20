data {
  // Define variables in data
  // Number of level-1 observations
  int<lower=0> N;
  vector[N]treecovp;//predictor variable 
  int<lower = 0, upper = 1> use[N];//declare the y variable (i)
}

parameters {
  real beta_0;// Population intercept
  real beta_1; //SLOPE for variable 1
     }

//transformed parameters  {
// real pred[N]; 
    //the linear predictor for the odds of individual observations
  //for(n in 1:N){
   //  pred[n] = beta_0 + beta_1 * treecovp[n];}
 // }   

model {
  // Prior part of Bayesian inference
  
  beta_0 ~ normal(0, 1);
  beta_1 ~ normal(0, 1);
 
  // Likelihood 
   use ~ bernoulli_logit(beta_0 + beta_1 * treecovp);
  }  

//generated quantities {

 //generate odds=exponentiated backtransformed overall slope and intercept estimates  
  //real oddsintercept = 1/exp(beta_0);//for negative estimate
  //real oddsbeta1 = exp(beta_1);//treecovp
 
 //initialize variable to get the odds of the logit
//real<lower=0> use_rep[Ni]; 
//initialize variable to get prob from odds
//real<lower=0,upper=1> prob[Ni]; 
//log liklihood 
  //vector[N] log_lik;
 // vector[N] use_rep;
  //vector[N] prob;
 // for (n in 1:N){
 // log_lik[n] = bernoulli_logit_lpmf(use[n] | pred[n]);  
 // }
//sample predicted odds values from the model for posterior predictive checks
//this delogarythmize the logits to odds ratios by using an exponentiate poisson function

//for(n in 1:N){
//use_rep[n] = bernoulli_logit_rng(pred[n]);
//} 
//get predicted probabilities for plotting values
//for(n in 1:N){
//prob[n] = use_rep[n]/(1+use_rep[n]);

//} 

//}
