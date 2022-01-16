#A1 Yuan Tien

library(tidyverse)
getwd()
setwd("/Users/yuantien/Desktop/R/613/Data")
getwd()
dathh07 <- dathh2007
rm(dathh2007)

#1.1
class(dathh07$idmen)
a <- unique(dathh07$idmen) #find unique value
length(a) #10498

getwd()

#1.2
dathh05 <- read.csv("dathh2005.csv")

length(dathh05$mstatus[dathh05$mstatus =="Couple, with Kids"]) #3374
table(dathh05$mstatus) #more convenient

#1.3
datind08 <- read.csv("datind2008.csv")
b <- unique(datind08$idind) 
length(b) #it shows 10825, but isn't this individual level data with 25510 obs?

#1.4
datind16 <- read.csv("datind2016.csv")
a <- datind16 %>%
  filter(age>= 25 & age<=35) %>%
  nrow()
a #2765

#1.5
datind09 <- read.csv("datind2009.csv")
CrossTable <- table(datind09$gender, datind09$profession)
CrossTable

#1.6
datind05 <- read.csv("datind2005.csv")
datind19 <- read.csv("datind2019.csv")
plot(density(datind05$wage, na.rm = TRUE)) #plot the distribution
plot(density(datind19$wage, na.rm = TRUE))

inter_decile <- function(x) {
  quantileX = quantile(x, prob = seq(0, 1, 0.1))
  ratio = quantileX[10]/quantileX[2] #because 10th element represent the 90% and the 2nd element represent the 10%
  return(ratio)
}


gini <- function(y) { 
  n = length(y)
  a = 1/(n-1)
  b = (n+1) 
  c = - 2*((sum((n+1-1:n)*y)))
  d = sum(y)
  return(a*(b-c/d))
} #this is sample gini coefficient.See http://www3.nccu.edu.tw/~jthuang/Gini.pdf page 2

dist_report <- function(x) {
  return(c(mean = mean(x), sd = sd(x), ratio = inter_decile(x), gini = gini(x)))
}
datind05_rm <- na.omit(datind05$wage) #clear out rows with na in wage
datind19_rm <- na.omit(datind19$wage)
datind05_rm <- datind05_rm[datind05_rm != 0] #clear out wage = 0
datind19_rm <- datind19_rm[datind19_rm != 0]
dist_report(datind05_rm) #mean = 22443.029, sd = 18076.708, ratio = 8.896, gini = 2.001
dist_report(datind19_rm) #mean = 27578.839, sd = 25107.187, ratio = 13.862, gini = 2.041

#1.7
datind10 <- read.csv("datind2010.csv")
hist(datind10$age)

datind10[datind10$gender == "Male"]
Male <- datind10[datind10$gender == "Male",] #remember to put in Comma to select the rows we like
Female <- datind10[datind10$gender == "Female",]
hist(Male$age)
hist(Female$age) #the most represented male group in the samples is around 60 years old, while the most represented female group is around 50

#1.8
datind11 <- read.csv("datind2011.csv") 
dathh11  <- read.csv("dathh2011.csv") 
m11 <- datind11 %>%
  inner_join(dathh11, by = "idmen")  #merge household dataset which contains location to ind data)

nrow(m11[m11$location == "Paris",]) #3531

#Exercise 2
#2.1
dind = list.files(pattern="datind")
for (i in 1:length(dind)) assign(dind[i], read.csv(dind[i])) #read multiple files

Mind <- do.call("rbind", list(datind2004.csv, datind2005.csv, datind2006.csv, datind2007.csv, datind2008.csv, datind2009.csv, 
                      datind2010.csv, datind2011.csv, datind2012.csv, datind2013.csv, datind2014.csv, datind2015.csv,
                      datind2016.csv, datind2017.csv, datind2018.csv, datind2019.csv))

#2.2
dhh = list.files(pattern="dathh")

for (i in 1:length(dhh)) assign(dhh[i], read.csv(dhh[i])) #read multiple files

Mhh <- do.call("rbind", list(dathh2004.csv, dathh2005.csv, dathh2006.csv, dathh2007.csv, dathh2008.csv, dathh2009.csv, 
                             dathh2010.csv, dathh2011.csv, dathh2012.csv, dathh2013.csv, dathh2014.csv, dathh2015.csv,
                             dathh2016.csv, dathh2017.csv, dathh2018.csv, dathh2019.csv))
#2.3
colnames(Mind)
colnames(Mhh)
y = c(colnames(Mind), colnames(Mhh))
y[duplicated(y)==TRUE] #X, idmen, year
#find duplicated column names --> find variables that appear in both datasets)

#2.4
M <- inner_join(Mhh, Mind, by = c("idmen", "year")) 
#I use innter_join because I believe those household ids that appear in both datasets more reliable data  

#2.5
M1 <- M #create M1 in case of unexpected accident
members_more_4 = function(x) {
  M2 = M1 %>%
    filter(year == x)
  z = table(M2$idmen)
  y = as.data.frame(z)
  nrow(y[y$Freq >=4,])
}
#create a frequency table by household -> turn to df ->calculate frequency
year = 2004:2019
more_4_by_year = sapply(year, members_more_4)
sum(more_4_by_year) #37108

#2.6
more_1_unemp = function(x) {
  M2 = M1 %>%
    filter(year == x)
  z = table(M2$idmen, M2$empstat)
  y = as.data.frame(z)
  h = y %>%
    filter(Var2 == "Unemployed")
  nrow(h[h$Freq >=1,])
}
more_unemp_year = sapply(year, more_1_unemp)
sum(more_unemp_year) #17241

#2.7
