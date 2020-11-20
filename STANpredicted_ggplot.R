##start with a one variable model to get the plot running

rm(list=ls())   #this clears the workspace
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
fit1_samples = extract(fit1)



str(fit1_samples)

predicted = fit1_samples[[3]]

 
mean(predicted[,1:101])

qplot(betas)
qplot(alpha)
traceplot(fit1)







v2 = c(234, 17, 42.5, 64, 38)
v2 <  200 

v2[v2 > 200]
v2[v2 >= 17]
v2[v2!=64]
v2[v2 ==17|v2==42.5]
v2[v2!17]
v=NULL
v[8]=254
v[2]=10
v[12]=13.4
v
M<-matrix(0,nrow=3,ncol=7)
M[1,]=c(1995,10.0,5,6,7,6,7)
M[2,]=c(2000,7.5,4,3,4,1,3)
M[3,]=c(2005,13.5,4,3,4,4,3)
M[1,]
M[,1]
max(M)
mean(M[,2])
M[M[,2]<=10,2]

M[M[,2]> mean(M[,2]),2]

##exponential growth function
#value of growth rate
lambda = 1.2
#vector for holding state variable
N = numeric(10)
#initial value of N
N[1] = 10
#loop to calculate N over time Use t to index vector
for (t in 2:10){
  #store the values of N in a vector
  N[t] = lambda * N[t-1]
}#end of loop
#if plot has only 1 arguement it assumes it is a time series
plot(N)
rm(list = ls())

#gompertz model for plant growth
mu0 = 1
k =0.3
#B is numeric with size 30
B = numeric(30)
R = numeric(30)
r = numeric(30)
#the first element of B to the value of 10
#B= biomass
B[1] = 10


for (t in 2:length(B)) {
  B[t] = B[t-1] + (mu0*B[t-1])*(1-(k/mu0*log(B[t-1]/B[1])))
  R[t] = (mu0*B[t-1])*(1-(k/mu0*log(B[t-1]/B[1])))
  r[t] = R[t]/B[t]
}
par(mfrow=c(2,2))
plot(B)
plot(R[2:length(B)])
plot(r[2:length(B)])

#######################################################################################
#3gompertz model for plant growth with matrix
mu0 = 1
k =0.3
#B is numeric with size 30
B = numeric(30)
t = numeric(30)
mymatrix<-matrix(0,nrow=30,ncol=2)
#the first element of B to the value of 10
#B= biomass
B[1] = 10
t[1] = 1
for (t in 2:length(B)) {
  B[t] = B[t-1] + (mu0*B[t-1])*(1-(k/mu0*log(B[t-1]/B[1])))
}

for(i in 1:30){
  mymatrix[i,2] <- B[i]
  mymatrix[i,1] <- i
}

plot(x = mymatrix[,1],y = mymatrix[,2])
mymatrix


a = matrix(0,nrow=5,ncol=5)
for (i in 1:5){
  for (j in 1:5) {
    a[j,i]= j*i 
  }#end of j
}#end of i
a
####################################################
##  exponential growth function with  nested loops
#####################################################
#value of growth rate
lambda = seq(1,1.6,0.1)
N = matrix(0,nrow=10,ncol=length(lambda))
N[1,]=10

#loop to calculate N over time Use t to index vector
for (j in 1:length(lambda)){
  for (t in 2:10){      
    #store the values of N in a vector
    N[t,j] = lambda[j] * N[t-1,j]
  } #end of j loop
  
} #end of t loop
#if plot has only 1 arguement it assumes it is a time series


rm(list = ls())
v=c(5,6,7,45,123,324)

pwnormalize <- function(x){
  normvector = x/(sum(x))
  return (normvector)
}
pwnormalize(v)
##  lists
name = "Poudre"
n = 100
a = c(0.3,0.2,0.5)
trans = matrix(0,nrow=3,ncol=3)
trans[1,]=c(0,1.1,1.7)
trans[2,]=c(0.6,0,0)
trans[3,]=c(0,0.9,0.9)
trans
population = list(location = name, size = n, composition = a,transition = trans)
population$transition[1,3]

population$size
population$composition
population$composition[2:3]
population$composition[population$composition >.2]
population$transition
population$transition[1,3]
ls() # what's in the working directoryu
rm(n)

##Peterson course
vect <- c(1:12)
jims.matrix = matrix(vect,nrow=4)
jims.matrixb = matrix(vect, ncol=3, byrow = TRUE)
jims.matrixb[2:3,1]
jims.matrix[c(1,3),2]
jims.matrix[,c(1,3)]<- -99
jims.matrix[c(1:2),]<-NA

species = c("dog","rabbit","cat")
 
trial.error<-matrix(vect,ncol=3,byrow=TRUE)

as.numeric(trial.error)
is.matrix(trial.error)
new.val<-trial.error[,1]+trial.error[,2]

trial.error[,3]<-c(3,4,5,6)
mydater<-as.data.frame(trial.error)
colnames(mydater) = c("pixie","dixie","horse")

HW.list <- list(matrix(c("Danny",15, "Ty",270, "Dr. Beeper",207, "Maggie",13,"Judge Smails",150,"Carl",22,"Lacey",0,"Al",322), ncol = 2, byrow = TRUE),
                c(0, 2310, 577, 0, 917, 0, 716,1898),
                c("bike", "convertible", "bmw", "none", "rolls", "none", "jaguar", "rolls"),
                runif(200,0,1))
names(HW.list) <- c("players", "stock.portfolio", "vehicle","junk")

new.df<-as.data.frame(HW.list$players)

new.df[,3]<-as.numeric(HW.list$stock.portfolio)

new.df$salary<-as.numeric(new.df$salary)
as.numeric(new.df$portfolio)
new.df[,4]<- salary+portfolio
colnames(new.df)=c("person","salary","portfolio","total.income")

new.df%>%
  mutate(tot.income=salary+portfolio)
new.df%>%
  summarize(sum(portfolio))
