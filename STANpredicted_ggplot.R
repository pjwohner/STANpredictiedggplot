##start with a one variable model to get the plot running

rm(list=ls()) 
library(rstan)
library(loo)
library(dplyr)
setwd("c:/R_PROJECTS/STANpredictedggplot")
YBCU <- read.table("Laymon_NestdataforSTAN7.txt", 
                   header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

freq_model<-glm(YBCU$use~YBCU$forbcovp,family = binomial)
scale(YBCU$treecovp)
names(YBCU)

cuckoolist1 <- list      (N   = nrow(YBCU),
                          N_new = 101,
                          x_new = seq(0,1,0.01),
                          use     = YBCU$use,
                          treecovp = YBCU$treecovp
                      )
#give this to stan
fit1 <- stan( file = 'YBCU1var.stan', 
              data = cuckoolist1, 
              chains = 3, 
              iter = 10000, 
              warmup = 1000, 
              thin = 10,
              control = list(adapt_delta = 0.99, max_treedepth = 15)
)                                    
#use this to assess convergence (change fit to appropriate model before running)
print(fit1,probs=c(0.075, 0.5, 0.925),pars = c("beta0","beta1", "y_new"))
pairs(fit1,pars = c("beta0","beta1"))
traceplot(fit1,pars = c("beta_0", "beta_1"),inc_warmup = FALSE)
#get the draws
fit1_samples = extract(fit1)
#see where predictions are in the list
str(fit1_samples)
#predictions are #3
predicted <- fit1_samples[[3]]
#for loop to get means of predicted
m_predict <- matrix(0,nrow=cuckoolist1$N_new, ncol = 2)
for (i in 1:101) {
  m_predict[i]<- mean(predicted[,i])
}


mean(predicted[,c(1:101)])










