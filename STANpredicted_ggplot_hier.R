#This is a non-centered 3 level hierarchical logistic regression. The non-centered 
#makes the model space easier to sample in the funnel and solves divergent samples

rm(list=ls())   #this clears the workspace
library(rstan)
library(loo)
library(dplyr)
library(ggplot2)
setwd("c:/Laymon_YBCU_2020/KRV_NEST_SELECTION_SURVIVAL/NestSiteSelection")
YBCU <- read.table("Laymon_NestdataforSTAN7.txt", 
                   header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

############################################################
###   fit1 top logistic 3 level hierarchical model ########
############################################################

cuckoolist1 <- list     (Ni   = nrow(YBCU),
                         Nj   = length(unique(YBCU$site)),
                         site     = as.numeric(YBCU$site),
                         use     = YBCU$use,
                         #perwill = YBCU$perwill,
                         #forbcovp = YBCU$forbcovp,
                         #qmdm = YBCU$qmdm,
                         treecovp = YBCU$treecovp
                                 )
#give this to stan
fit1 <- stan( file = 'Hier1Var.stan', 
              data = cuckoolist1, 
              chains = 3, 
              iter = 8000, 
              warmup = 800, 
              thin = 8,
              control = list(adapt_delta = 0.99, max_treedepth = 15)
)                                    
#use this to assess convergence (change fit to apropriate model before running)
print(fit1,probs=c(0.075, 0.5, 0.925),pars = c("beta_0","beta_1","beta_0j","y_hat"))
pairs(fit1,pars = c("beta_0","beta_1"))
traceplot(fit1,pars = c("beta_0", "beta_1"),inc_warmup = FALSE)

##########################################################################
####     FUNCTION to make Bayeisan logistic predicted ggplots   ##########
##########################################################################

#get the draws
fit1_samples = extract(fit1)
#see where predictions are in the list

#str(fit1_samples)
#predictions are #3
predicted <- fit1_samples[[7]]
#for loop to get means of predicted
m_predict <- matrix(0,nrow=cuckoolist1$Ni, ncol = 5)

for (i in 1:cuckoolist1$Ni) {
  m_predict[i,1]<- cuckoolist1$treecovp[i]
  m_predict[i,2]<- mean(predicted[,i])
  m_predict[i,3]<- quantile(predicted[,i],probs= 0.025) 
  m_predict[i,4]<- quantile(predicted[,i],probs= 0.975) 
  m_predict[i,5]<- cuckoolist1$site[i]
}

new_frame<-as.data.frame(m_predict)
colnames(new_frame) = c("treecov","meanPr","lower","upper","site")


ggplot(new_frame,aes(x=treecov,y=meanPr))+
  theme_bw()+
  geom_point(aes(x=treecov,y=meanPr,color = site))+
  geom_line(aes(group=site))+
  
  geom_point(aes(x=YBCU$treecovp,y=YBCU$use,color=YBCU$site),size = 1,alpha = 1/2) +
  geom_jitter(aes(x=YBCU$treecovp,y=YBCU$use),width = 0, height = .02, alpha = 0.5) +
  geom_ribbon(aes(ymin=lower, ymax=upper, group = site), alpha=0.2)+
  #theme(legend.text = element_text(size = 13),legend.position=c(0.2, 0.8),legend.title =element_blank())+
  labs(title = "", x = "Tree cover 5 m (%)", 
       y = "Probability of use")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0, size=12,color="black"))+
  theme(axis.text.y = element_text(angle = 0, hjust = 0, size=12,color="black"))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  ylim(0, 1)+
  xlim(0,1)+
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.4), angle = 00))

