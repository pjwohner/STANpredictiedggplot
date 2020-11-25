##start with a one variable model to get the plot running

rm(list=ls()) 
library(rstan)
library(loo)
library(dplyr)
library(tidybayes)
setwd("c:/R_PROJECTS/STANpredictedggplot")
YBCU <- read.table("Laymon_NestdataforSTAN7.txt", 
                   header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

freq_model<-glm(YBCU$use~YBCU$forbcovp,family = binomial)
scale(YBCU$treecovp)
names(YBCU)

cuckoolist1 <- list      (N   = nrow(YBCU),
                          #N_hat = 101,
                          #x_hat = seq(0,1,0.01),
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
print(fit1, probs=c(0.075, 0.5, 0.925),pars = c("beta0","beta1","y_hat"))
pairs(fit1, pars = c("beta0","beta1"))
traceplot(fit1, pars = c("beta_0", "beta_1"),inc_warmup = FALSE)

#get the draws
fit1_samples = extract(fit1)
#see where predictions are in the list

#str(fit1_samples)
#predictions are #3
predicted <- fit1_samples[[3]]
#for loop to get means of predicted
m_predict <- matrix(0,nrow=cuckoolist1$N, ncol = 4)

for (i in 1:cuckoolist1$N) {
  m_predict[i,1]<- cuckoolist1$treecovp[i]
  m_predict[i,2]<- mean(predicted[,i])
  m_predict[i,3]<- quantile(predicted[,i],probs= 0.025) 
  m_predict[i,4]<- quantile(predicted[,i],probs= 0.975) 
}
new_frame<-as.data.frame(m_predict)
colnames(new_frame) = c("treecov","meanPr","lower","upper")

ggplot(new_frame,aes(x=treecov,y=meanPr))+
  theme_bw()+
  geom_point(aes(x=YBCU$treecovp,y=YBCU$use),color = "black",size = 1,alpha = 1/2) +
  geom_jitter(aes(x=YBCU$treecovp,y=YBCU$use),width = 0, height = .02, alpha = 0.5) +
  geom_line(aes(x=treecov,y=meanPr))+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2)+
  theme(legend.text = element_text(size = 13),legend.position=c(0.2, 0.8),legend.title =element_blank())+
  labs(title = "", x = "Tree cover 5 m (%)", 
       y = "Probability of use")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0, size=12,color="black"))+
  theme(axis.text.y = element_text(angle = 0, hjust = 0, size=12,color="black"))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  ylim(0, 1)+
  xlim(0,1)+
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.4), angle = 00))

  
   
  
  








