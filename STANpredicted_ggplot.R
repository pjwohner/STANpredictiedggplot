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
library(tidybayes)
#get the draws
fit1_samples = extract(fit1)
#see where predictions are in the list
str(fit1_samples)
#predictions are #3
predicted <- fit1_samples[[3]]
#for loop to get means of predicted
m_predict <- matrix(0,nrow=cuckoolist1$N_new, ncol = 2)

for (i in 1:101) {
  m_predict[i,1]<- mean(predicted[,i])
}
m_predict[,2] <- seq(0,1,0.01)
new_frame<-as.data.frame(m_predict)
colnames(m_predict) = c("y","x")
ggplot(new_frame,aes(x=x,y=y))+
  theme_bw()+
  geom_point()+
  #geom_smooth(se = TRUE,level = 0.85,color = "black")+
      stat_lineribbon() 
#.width = c(.50, .80, .95)
##frequentist
ggplot(YBCU, aes(treecovp, use)) +  ##arguements are data, x, y
  theme_bw()+
  geom_point(color = "black",size = 1,alpha = 1/2) +
  geom_jitter(width = 0, height = .02, alpha = 0.5) + 
  #ylim(0, 0.15)+
  #xlim(-10,40)+
  scale_shape_manual(values=c(1, 16))+
  scale_color_manual(values=c('black','black'))+
  scale_size_manual(values=c(2,2))+
  theme(legend.text = element_text(size = 14),legend.position=c(0.2, 0.8),legend.title =element_blank())+
  geom_smooth(method = "glm",method.args = list(family = "binomial"),
              se = TRUE,level = 0.85,color = "black")+
  labs(title = "", x = "Tree cover 5 m (%)", 
       y = "Probability of use")+
  theme(axis.text.x = element_text(angle = 0, hjust = 0, size=12,color="black"))+
  theme(axis.text.y = element_text(angle = 0, hjust = 0, size=12,color="black"))+
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+
  theme(axis.title.y = element_text(size = rel(1.4), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1.4), angle = 00))









