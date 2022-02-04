# A2 Yuan Tien
getwd()
setwd("/Users/yuantien/Desktop/R/613/Data")
library(tidyverse)
library(readr)

#When I first read the question I thought it is 2009 - 2019. 
#dind = list.files(pattern="datind")
#dind = dind[-c(1:5)] #get rid of 2004:2008
#for (i in 1:length(dind)) assign(dind[i], read_csv(dind[i])) #find data in my file and read multiple files

#Mind <- do.call("rbind", list(datind2009.csv, datind2010.csv, datind2011.csv, datind2012.csv, 
#                              datind2013.csv, datind2014.csv, datind2015.csv,datind2016.csv, 
#                              datind2017.csv, datind2018.csv, datind2019.csv))

#Exercise 1
Mind <- datind2009.csv

#1.1
#Can I use this function???
cor(Mind$wage, Mind$age, use = "complete.obs") #I have excluded NA, the result is  -0.1788512

#1.2
Mind2 <- Mind[!is.na(Mind$age) & !is.na(Mind$wage),] #clear obs with NA wage and NA age 
X <- Mind2 %>%
  select(age) %>%
  mutate(intercept = 1) %>%
  as.matrix()
  
Beta <- solve(t(X) %*% X) %*% t(X) %*% Mind2$wage  
as.numeric(Beta) #the coefficient of age is -180.1765, and the intercept is 22075.1066

#1.3
#I estimate the variance of error and than use it to compute standard error
error <- Mind2$wage - (Beta[1,1]) * Mind2$age - Beta[2,1] #Beta is a matrix, the first row is age coefficient and the second is the intercept

ErrorSquare <- t(error) %*% error/(length(Mind2$wage)-1) #-1 since we have one regressor 

VarBeta <- ErrorSquare %*% solve(t(Mind2$age) %*% Mind2$age) 
VarBeta # 6.500652

#Using bootstrap 49
Results <- mat.or.vec(49, 2) #since we only have one coefficient to estimate

set.seed(99713)    

for (i in 1:49){
  sam <- sample(1:length(Mind2$wage), length(Mind2$wage), rep = TRUE) #rep = TRUE to give weight to random index
  boot <- Mind2[sam,] #randomly select observations
  X <- boot %>%
    select(age) %>%
    mutate(intercept = 1) %>%
    as.matrix()
  Beta <- solve(t(X) %*% X) %*% t(X) %*% boot$wage  
  Results[i,] <- Beta
}
Results <- as.data.frame(Results)
lapply(Results, mean) #beta age: -178.5496, beta intercept: 21975.63  Note that V1 is the age coefficient and V2 is the intercept
lapply(Results, sd) #age sd: 5.249532, intercept sd: 291.9666

#Using bootstrap 499

Results2 <- mat.or.vec(499, 2)

for (i in 1:499){
  sam <- sample(1:length(Mind2$wage), length(Mind2$wage), rep = TRUE) 
  boot <- Mind2[sam,] 
  X <- boot %>%
    select(age) %>%
    mutate(intercept = 1) %>%
    as.matrix()
  Beta <- solve(t(X) %*% X) %*% t(X) %*% boot$wage  
  Results2[i,] <- Beta
}
Results2 <- as.data.frame(Results2)
lapply(Results2, mean) # beta age: -180.5353, beta intercept: 22095.49. Note that V1 is the age coefficient and V2 is the intercept
lapply(Results2, sd) #age sd: 5.328217, intercept sd: 301.4035
#Using bootstrap gives me a higher standard error. 
#While the coefficient (estimated by mean in bootstrap) is nearly the same, 
#the result of doing 499 times is closer to matrix form solution than doing 49 times

#Exercise 2

dind = list.files(pattern="datind")
for (i in 1:length(dind)) assign(dind[i], read_csv(dind[i])) 


Mind3 <- do.call("rbind", list(datind2005.csv, datind2006.csv, datind2007.csv, datind2008.csv, datind2009.csv, 
                              datind2010.csv, datind2011.csv, datind2012.csv, datind2013.csv, datind2014.csv, datind2015.csv,
                              datind2016.csv, datind2017.csv, datind2018.csv))
#2.1

Mind4 <- Mind3 %>%
  mutate(ag = case_when(age >= 18 & age <= 25 ~ 1,
         age >= 26 & age <= 30 ~ 2,
         age >= 31 & age <= 35 ~ 3,
         age >= 36 & age <= 40 ~ 4,
         age >= 41 & age <= 45 ~ 5,
         age >= 46 & age <= 50 ~ 6,
         age >= 51 & age <= 55 ~ 7,
         age >= 56 & age <= 60 ~ 8,
         age >  60  ~ 9,
         age <  18  ~ 0 ))
  
#2.2 

tab <- Mind4 %>%
  group_by(ag, year) %>%
  summarise(mwage = mean(wage, na.rm = TRUE))

tab <- tab[1:140,] #Of the 142 obs I get, the last two obs are NA, so I drop them

library(ggplot2)

age <- c( "1" = "18 - 25", #Here I use this to turn my ag label to full
  "2" = "26 - 30",
  "3" = "31 - 35",
  "4" = "36 - 40",
  "5" = "41 - 45",
  "6" = "46 - 50",
  "7" = "51 - 55", 
  "8" = "56 - 60",
  "9" = "60+"    ,
  "0" = "<18")

ggplot(tab, aes(x = year, y = mwage, color = as.factor(ag))) +
  geom_line() +
  facet_wrap(~ ag, labeller = as_labeller(age))

#Across age groups, the wage mostly increased by year. 
#However, wage for young workers almost plateaued over the years.  

#2.3 

#clear NA
Mind5 <- Mind3[!is.na(Mind3$age) & !is.na(Mind3$wage) & !is.na(Mind3$year),]


#using lm
reg1 <- lm(wage ~ age + as.factor(year), data = Mind5)
summary(reg1)$coefficients
#the age coefficient is  -186.87927 

#using matrix
X <- Mind5%>%
  select(age, year)%>%
  mutate(intercept = 1) %>%
  as.matrix()

solve(t(X) %*% X) %*% t(X) %*% Mind5$wage #age coefficient: -186.8827

#using plm
install.packages("plm")
library(plm)

reg2 <- plm(wage ~ age, data = Mind5, model = "within", index = "year")
summary(reg2) #-186.8793

#Compare this to model without time fixed effect

compare <- lm(wage ~ age, data = Mind5) 
summary(compare)$coefficients #age coefficient is -182.4896

#After controlling for time fixed effect, the negative effect of age on wage increases.

#Exercise 3 

#3.1 
d <- datind2007.csv
df <- d %>%
  filter(empstat != "Inactive", empstat != "Retired") #the professor said on Slack retired is inactive in labor economics. 

#3.2
df$empstat[ which(df$empstat == "Employed") ] = 1 #make dummies for empstat
df$empstat[ which(df$empstat == "Unemployed") ] = 0 

age <- df$age
empstat <- as.numeric(df$empstat)


ProLike <- function(cf, age, empstat){
  XB = cf[1] + cf[2]*age
  Prob = pnorm(XB) #return the CDF of XB or F(XB)
  Prob[Prob>0.999999] = 0.999999 # These two lines ensure that the probability is less than one and greater than 0
  Prob[Prob<0.000001] = 0.000001
  p1 = log(Prob)   #represent the log prob of (y=1)
  p0 = log(1-Prob) #represent the log prob of (y=0)
  like = empstat * p1 + (1-empstat) * p0 
  #By this method, if empstat = 1, then only the first term will be computed; otherwise, the second term will be computed
  return( -sum(like) ) #return negative for us to do maximization
}


#3.3


time <- 100
results3 <- mat.or.vec(time, 3) 
#I only need to calculate the intercept and age coef, and I add one more column to collect minimizing value

for (i in 1:time) {

  searchv = runif(2, -5, 5) #random starting search value
  result  = optim(searchv, fn = ProLike, method = "BFGS", 
                  control = list(trace = 6, maxit = 3000),
                  age = age, empstat = empstat)
  results3[i,] = c(result$par, result$value) #I collect the estimated minimizing parameter and the minimizing value  
}
results3 <- format(results3, scientific = F) #By this I turn the scientific notation to full numbers
results3 <- as.data.frame(results3)

results3[ which(results3$V3 == min(results3$V3)), ] #Find rows (estimation) that have the minimum negative loglikelihood
# This time, I get the estimated intercept: 1.042158817, estimated age coefficient: 0.00697, with the minimizing value 3555.8917
# Notes that the number of the coefficient can not be interpreted directly in probit model. 
# We can only say age has a positive effect on market participation without controlling other factors.


#3.4 

wage <- df$wage
age <- df$age
empstat <- as.numeric(df$empstat)

wage<- wage[which(!is.na(wage) ) ] #clear out NA
age <- age[which (!is.na(wage) ) ]
empstat <- empstat[which(!is.na(wage)) ] 

length(wage) #check if I have the same dimension
length(age)
length(empstat) 


ProLike2 <- function(cf, age, wage, empstat){
  XB = cf[1] + cf[2]*age + cf[3]*wage
  Prob = pnorm(XB) 
  Prob[Prob>0.999999] = 0.999999 
  Prob[Prob<0.000001] = 0.000001
  p1 = log(Prob)   #represent the log prob of (y=1)
  p0 = log(1-Prob) #represent the log prob of (y=0)
  like = empstat * p1 + (1-empstat) * p0 
  #By this method, if empstat = 1, then only the first term will be computed; otherwise, the second term will be computed
  return( -sum(like) ) #return negative for us to do maximization via minimizing the negative version
}

time <- 100
results4 <- mat.or.vec(time, 4) #I need to calculate the intercept, age and wage coefficients plus the minimizing value for later use

for (i in 1:time) {
  
  searchv = runif(3, -5, 5) #random starting search value
  result  = optim(searchv, fn = ProLike2, method = "BFGS", 
                  control = list(trace = 6, maxit = 3000),
                  age = age, wage = wage, empstat = empstat)
  results4[i,] = c(result$par, result$value) #I collect the estimated minimizing parameter and the minimizing value  
}
results4 <- format(results4, scientific = F) #By this I turn the scientific notation to full numbers
results4 <- as.data.frame(results4)

results4[ which(results4$V4 == min(results4$V4)), ]  #The age coefficient is 0.006815962, and the wage coefficient is 2.475465594


# Exercise 4
M5 <- do.call("rbind", list(datind2005.csv, datind2006.csv, datind2007.csv, 
                            datind2008.csv, datind2009.csv, datind2010.csv, 
                            datind2011.csv, datind2012.csv, datind2013.csv, datind2014.csv, datind2015.csv))

M5 <- M5 %>%
  filter(empstat != "Inactive", empstat != "Retired")


#4.2
M5$empstat[ M5$empstat == "Employed" ] = 1 #make dummies for empstat
M5$empstat[ M5$empstat == "Unemployed" ] = 0 

testM5 <- M5 %>%
  mutate(dum = 1) %>%
  pivot_wider(names_from = year, values_from = dum, values_fill = 0) #make year dummies

M6 <- testM5 %>%
  select(-"2005") # I drop 2005 to avoid perfect collinearity


age <- M6$age

empstat <- as.numeric(M6$empstat)

which(is.na(age)) #no NA
which(is.na(empstat)) #no NA

y6 <- M6$`2006`
y7 <- M6$`2007`
y8 <- M6$`2008`
y9 <- M6$`2009`
y10 <- M6$`2010`
y11 <- M6$`2011`
y12 <- M6$`2012`
y13 <- M6$`2013`
y14 <- M6$`2014`
y15 <- M6$`2015`


# ========= Probit ==============

ProLike3 <- function(cf, age, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, empstat){
  XB = cf[1] + cf[2]*age + cf[3]*y6 + cf[4] *y7 + cf[5]*y8 + cf[6]*y9 + cf[7]*y10 + cf[8]*y11 + cf[9]*y12 + cf[10]*y13 + cf[11]*y14 + cf[12]*y15
  Prob = pnorm(XB) 
  Prob[Prob>0.999999] = 0.999999 
  Prob[Prob<0.000001] = 0.000001
  p1 = log(Prob)   
  p0 = log(1-Prob) 
  like = empstat * p1 + (1-empstat) * p0 
  return( -sum(like) ) 
}

time <- 100
results5 <- mat.or.vec(time, 12+1) 
#I only need to calculate the intercept and age coef, and I add one more column to collect minimizing value

for (i in 1:time) {
  
  searchv = runif(12, -5, 5) #random starting search value
  result  = optim(searchv, fn = ProLike3, method = "BFGS", 
                  control = list(trace = 6, maxit = 3000),
                  age = age, y6 = y6, y7 = y7, y8 = y8, y9 = y9, y10 = y10, 
                  y11 = y11, y12 = y12, y13 = y13, y14 = y14, y15 = y15,
                  empstat = empstat)

  results5[i,] = c(result$par, result$value) 
}
results5 <- format(results5, scientific = F) #By this I turn the scientific notation to full numbers
results5 <- as.data.frame(results5)
results5[ which(results5$V13 == min(results5$V13)), ] 

ProRes <- results5[ which(results5$V13 == min(results5$V13)), ] #I store the result
ProRes <- as.numeric( as.vector(t(ProRes)) )
ProRes <- ProRes[-length(ProRes)] #The last value is the minimizing negative loglikelihood, and I don't need it afterward 


# After controlling for year, I got  0.012329112 as the probit "age" coefficient. The intercept is  0.748818912 

searchv = runif(12) #random starting search value
result  = optim(searchv, fn = ProLike3, method = "BFGS", 
                control = list(trace = 6, REPORT = 1, maxit = 9000),
                age = age, y6 = y6, y7 = y7, y8 = y8, y9 = y9, y10 = y10, 
                y11 = y11, y12 = y12, y13 = y13, y14 = y14, y15 = y15,
                empstat = empstat, hessian = TRUE)
result$hessian

#!!!!!!!!!!!! return 0 hessian matrix
result$value

fisher = solve(result$hessian)
#se = sqrt( diag(fisher) )
#se[2] #Here I specify "2" to get the second standard error, which is my standard error for age   

# ========= Logit ==============

LogitLike <- function(cf, age, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, empstat){
  XB = cf[1] + cf[2]*age + cf[3]*y6 + cf[4] *y7 + cf[5]*y8 + cf[6]*y9 + 
    cf[7]*y10 + cf[8]*y11 + cf[9]*y12 + cf[10]*y13 + cf[11]*y14 + cf[12]*y15
  Prob = exp(XB)/ (1 +exp(XB)) 
  Prob[Prob>0.999999] = 0.999999 
  Prob[Prob<0.000001] = 0.000001
  p1 = log(Prob)   
  p0 = log(1-Prob) 
  like = empstat * p1 + (1-empstat) * p0 
  return( -sum(like) ) 
}

time <- 100
results6 <- mat.or.vec(time, 12+1) 


for (i in 1:time) {
  
  searchv = runif(12, -5, 5) #random starting search value
  result  = optim(searchv, fn = LogitLike, method = "BFGS", 
                  control = list(trace = 6, maxit = 3000),
                  age = age, y6 = y6, y7 = y7, y8 = y8, y9 = y9, y10 = y10, 
                  y11 = y11, y12 = y12, y13 = y13, y14 = y14, y15 = y15,
                  empstat = empstat)
  
  results6[i,] = c(result$par, result$value) 
}
results6 <- format(results6, scientific = F) #By this I turn the scientific notation to full numbers
results6 <- as.data.frame(results6)

results6[ which(results6$V13 == min(results6$V13)), ] 

LogitRes <- t( results6[ which(results6$V13 == min(results6$V13)), ] )
LogitRes <- as.numeric ( as.vector(LogitRes) )
LogitRes <- LogitRes[-length(LogitRes)]


#Here the coefficient for age is  0.025307485, and for the intercept is 1.120449372. 
#Same with probit, we cannot interpret the face value of coefficient. We can just say that age is positively correlated with employment.

# ========= Linear ==============

LnLike <- function(cf, age, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, empstat){
  XB = cf[1] + cf[2]*age + cf[3]*y6 + cf[4] *y7 + cf[5]*y8 + cf[6]*y9 + 
    cf[7]*y10 + cf[8]*y11 + cf[9]*y12 + cf[10]*y13 + cf[11]*y14 + cf[12]*y15
  Prob = XB
  Prob[Prob>0.999999] = 0.999999   
  Prob[Prob<0.000001] = 0.000001
  # Note that I did  force the probability to be between [0,1], but Prob = XB can be inherently bigger than 1 or smaller than 0
  p1 = log(Prob)   
  p0 = log(1-Prob) 
  like = empstat * p1 + (1-empstat) * p0 
  return( -sum(like) ) 
}

time <- 100
results7 <- mat.or.vec(time, 12+1) 


for (i in 1:time) {
  
  searchv = runif(12, -5, 5) #random starting search value
  result  = optim(searchv, fn = LnLike, method = "BFGS", 
                  control = list(trace = 6, maxit = 3000),
                  age = age, y6 = y6, y7 = y7, y8 = y8, y9 = y9, y10 = y10, 
                  y11 = y11, y12 = y12, y13 = y13, y14 = y14, y15 = y15,
                  empstat = empstat)
  
  results7[i,] = c(result$par, result$value) 
}
results7 <- format(results7, scientific = F) 
results7 <- as.data.frame(results7)

results7[ which(results7$V13 == min(results7$V13)), ] 
LnRes <- t (results7[ which(results7$V13 == min(results7$V13)), ] )
LnRes <- as.vector(as.numeric(LnRes))
LnRes <- LnRes[-length(LnRes)]


# the estimate intercept is 1.971644518, and the estimated age coefficient is 0.105708194
#This means an age older is associated with 0.105 proportionate increase in employment probability

#Unfortunately, because I always produce zero hessian matrix in optim(), I could not report the standard error and the significance level 


# =============== Exercise 5 ====================

# 5.1


Intercept <- rep(1, length(age))
Xmat <- cbind(Intercept, age, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15)
dim(Xmat)
#  ======= Probit ========

predictPro <- Xmat %*% ProRes # X matrix times beta
marginalPro <- mean( dnorm(predictPro)) * ProRes
marginalPro #note that the first is intercept, the second is coefficient of age

#  ======= Logit ========

predictLog <- Xmat %*% LogitRes
#how to uncover marginal effect of logit function?

pdf <- exp(predictLog)/(1 + exp(predictLog))
marginalLog <- mean(pdf)* LogitRes #is this still cdf? Then, what is its pdf?
marginalLog

mean(dlogis(predictLog)) * LogitRes #this is closer, but not entirely the same

# the marginal effects are different!!!!


# 5.2

set.seed(8172)    

#bootstrap 49 times

bootmarg <- mat.or.vec(49, 12)

Fullmat <- cbind(empstat, Xmat)
Fullmat <- as.data.frame.matrix(Fullmat)

#Probit boot


for (i in 1:49){
  sam <- sample(1:nrow(Fullmat), nrow(Fullmat), rep = TRUE) #rep = TRUE to give weight to random index
  boot <- Fullmat[sam,]
  
  age <- boot$age
  empstat <- as.numeric(boot$empstat)
  
  y6 <- boot$y6
  y7 <- boot$y7
  y8 <- boot$y8
  y9 <- boot$y9
  y10 <- boot$y10
  y11 <- boot$y11
  y12 <- boot$y12
  y13 <- boot$y13
  y14 <- boot$y14
  y15 <- boot$y15
  
  #here I complete the data process
  
  time <- 100
  resBpro <- mat.or.vec(time, 12+1) 
  
  for (i in 1:time) {
    
    searchv = runif(12, -5, 5) #random starting search value
    result  = optim(searchv, fn = ProLike3, method = "BFGS", 
                    control = list(trace = 6, maxit = 1000),
                    age = age, y6 = y6, y7 = y7, y8 = y8, y9 = y9, y10 = y10, 
                    y11 = y11, y12 = y12, y13 = y13, y14 = y14, y15 = y15,
                    empstat = empstat)
    
    resBpro[i,] = c(result$par, result$value) 
  }
  resBpro <- format(resBpro, scientific = F) #By this I turn the scientific notation to full numbers
  resBpro <- as.data.frame(resBpro)

  resBpro <- resBpro[ which(resBpro$V13 == min(resBpro$V13)), ] #I store the result
  resBpro <- as.numeric( as.vector(t(resBpro)) )
  resBpro <- resBpro[-length(resBpro)]
  
  Intercept <- rep(1, length(age))
  Xmat <- cbind(Intercept, age, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15)
  #  ======= Probit ========
  
  predictPro <- Xmat %*% ProRes # X matrix times beta
  marginalPro <- mean( dnorm(predictPro)) * ProRes
  marginalPro
  
  
  
  
  bootmarg[i,] <- marginal_effect_vec
}








