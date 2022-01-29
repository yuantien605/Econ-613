# A2 Yuan Tien
getwd()
setwd("/Users/yuantien/Desktop/R/613/Data")
library(tidyverse)
library(readr)

dind = list.files(pattern="datind")
dind = dind[-c(1:5)] #get rid of 2004:2008
for (i in 1:length(dind)) assign(dind[i], read_csv(dind[i])) #find data in my file and read multiple files

Mind <- do.call("rbind", list(datind2009.csv, datind2010.csv, datind2011.csv, datind2012.csv, 
                              datind2013.csv, datind2014.csv, datind2015.csv,datind2016.csv, 
                              datind2017.csv, datind2018.csv, datind2019.csv))

#Exercise 1

#1.1
#Can I use this function???
cor(Mind$wage, Mind$age, use = "complete.obs") #I have excluded NA, the result is  -0.1772896

#1.2
Mind2 <- Mind[!is.na(Mind$age) & !is.na(Mind$wage),] #clear obs with NA wage and NA age 
Beta <- solve(t(Mind2$age) %*% Mind2$age) %*% t(Mind2$age) %*% Mind2$wage  
as.numeric(Beta) #226.4472

#1.3
#I estimate the variance of error and than use it to compute standard error
error <- Mind2$wage - as.numeric(Beta) * Mind2$age

ErrorSquare <- t(error) %*% error/(length(Mind2$wage)-1) #-1 since we have one regressor 

VarBeta <- ErrorSquare %*% solve(t(Mind2$age) %*% Mind2$age) 
VarBeta # 0.794986

#Using bootstrap 49
Results <- mat.or.vec(49, 1) #since we only have one coefficient to estimate

set.seed(99713)    

for (i in 1:49){
  sam <- sample(1:length(Mind2$wage), length(Mind2$wage), rep = TRUE) #rep = TRUE to give weight to random index
  boot <- Mind2[sam,] #randomly select observations
  Beta <- solve(t(boot$age) %*% boot$age) %*% t(boot$age) %*% boot$wage  
  Results[i] <- Beta
}

Results
mean(Results)
sd(Results)

#Using bootstrap 499

Results2 <- mat.or.vec(499, 1)

for (i in 1:499){
  sam <- sample(1:length(Mind2$wage), length(Mind2$wage), rep = TRUE) 
  boot <- Mind2[sam,] 
  Beta <- solve(t(boot$age) %*% boot$age) %*% t(boot$age) %*% boot$wage
  Results2[i] <- Beta
}

Results2
mean(Results)
sd(Results)

#Using bootstrap gives me a higher standard error, while the coefficient (estimated by mean in bootstrap) is nearly the same.

#Exercise 2
