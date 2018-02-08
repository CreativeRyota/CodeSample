# Sample Code for Time Series Modeling

# Course:   Econometrics III
#           Assignment 2

# Date:     23/02/2017
 

# Authors:
#           Ryota Akimoto
#           Nishani ******
#           Foruhar ****** 


# clear environment
rm(list = ls())

# install packages
install.packages("ggplot2")
install.packages("ggplot")
install.packages("vars")
install.packages("lmtest")
install.packages("urca")
install.packages ("tseries")
install.packages ("tsDyn")

# load data
library(readr)
library(ggplot2)
library(vars)
library(lmtest)
library(readr)
library(urca)
library(tseries)
library(tsDyn)


# urca package https://cran.r-project.org/web/packages/urca/urca.pdf

US_tbills <- read.csv("U:/Econometrie III/Assignment 2/US_tbills.csv", header=TRUE)


#US_tbills <- read_csv("U:/Econometrie III/Assignment 2/US_tbills.csv", 
                     # col_types = cols(Date = col_date(format = "%Y-%M-%D")))
View(US_tbills)

## QUESTION A 

# declare variables
Date=US_tbills$Date
T10=US_tbills$tbill10
T5=US_tbills$tbill5
T1=US_tbills$tbill1

# generate dataset 1985-1994
dummy <- data.frame(Date, T10, T5, T1) # creating a dummy data.frame
Date_94 <- US_tbills$Date[1:120]
T10_94 <- dummy$T10[1:120]
T5_94 <- dummy$T5[1:120]
T1_94 <- dummy$T1[1:120]



until94<-data.frame(Date_94, T10_94, T5_94, T1_94)

library(ggplot2)
# plot data
ggplot(data=until94, aes(x = Date_94, group=1)) + 
  geom_line(aes(y = T10_94, color="T10_94")) + 
  geom_line(aes(y = T5_94, color="T5_94")) + 
  geom_line(aes(y = T1_94, color="T1_94")) + 
  ylab(label="T10_94 , T5_94 , T1_94") + 
  xlab("Date") + 
  ggtitle("Title") + 
  scale_color_manual(name = element_blank(), values=c("T10_94"="blue", "T5_94"="green", "T1_94"="red"))

##  QUESTION B

#take log differences
logT10_94 <- diff(log(until94$T10_94))
logT5_94<- diff(log(until94$T5_94))
logT1_94 <- diff(log(until94$T1_94))

Date_94d <- until94$Date[2:120]

logFrame <- data.frame(Date_94d, logT10_94, logT5_94 , logT1_94 )

print (logFrame)

ggplot(data=logFrame, aes(x = Date_94d, group=1)) + 
  geom_line(aes(y = logT10_94, color="logT10_94")) + 
  geom_line(aes(y = logT5_94, color="logT5_94")) + 
  geom_line(aes(y = logT1_94, color="logT1_94")) + 
  ylab(label="logT10_94,logT5_94 ,logT1_94") + 
  xlab("Date") + 
  ggtitle("Title") + 
  scale_color_manual(name = element_blank(), values=c("logT10_94"="blue", "logT5_94"="green", "logT1_94"="red"))


#suitable VAR model,
#Do we have to take the levels data or differences....
library(vars)

#1.suitable var using logdifferences:
logData <- data.frame(logT1_94, logT5_94, logT10_94)
dataVar94<- ts.union(logData)
VARselect(dataVar94, lag.max = 12, season = 12)

VAR94<-VAR(dataVar94, p=VARselect(dataVar94, lag.max = 12, season = 12)$selection[1])
summary(VAR94)


#2.suitable VAR level data:
Data94 <- data.frame(T1_94, T5_94, T10_94)
dataVarr94<- ts.union(Data94)
VARselect(dataVarr94, lag.max = 12, season = 12)
#AIC AND FPE GIVE 4

VAR94<-VAR(dataVarr94, p=VARselect(dataVarr94, lag.max = 10, season = 12)$selection[1])
summary(VAR94)

## QUESTION C
library(urca)
# level data
cointeg<-ca.jo(dataVarr94,season = 12, K=4-1, ecdet = "const")
summary(cointeg)

#for lag 5 

jotest=ca.jo(data.frame(T10_94,T5_94,T1_94), type="trace", K=6-1, ecdet="const", spec="longrun",season=12)
summary(jotest)



## QUESTION D
#lag(tsdyn)=lag(urca)-1)
test94 = VECM(dataVarr94, 3, r = 1, include = c("both"), beta = NULL, estim = c("ML"), LRinclude = c("none"), exogen = NULL)
summary(test94)
print(test94)

coefA(test94)
coefB(test94)
coefPI(test94)

#https://sites.ualberta.ca/~sfossati/e509/files/slides/Lec12.pdf
#http://stackoverflow.com/questions/17517515/vector-error-correction-model-in-r




## QUESTION E

## Question A again
# generate dataset 1985-1994
Date_05 <- dummy$Date[121:252]
T10_05 <- dummy$T10[121:252]
T5_05 <- dummy$T5[121:252]
T1_05 <- dummy$T1[121:252]

until05<-data.frame(Date_05, T10_05, T5_05, T1_05)

# plot data
ggplot(data=until05, aes(x = Date_05, group=1)) + 
  geom_line(aes(y = T10_05, color="T10_05")) + 
  geom_line(aes(y = T5_05, color="T5_05")) + 
  geom_line(aes(y = T1_05, color="T1_05")) + 
  ylab(label="T10_05 , T5_05 , T1_05") + 
  xlab("Date") + 
  ggtitle("Title") + 
  scale_color_manual(name = element_blank(), values=c("T10_05"="blue", "T5_05"="green", "T1_05"="red"))

## Question B again
#take log differences
logT10_05<- diff(log(until05$T10_05))
logT5_05<- diff(log(until05$T5_05))
logT1_05<- diff(log(until05$T1_05))

Date_05d <- until05$Date[2:132]

logFrame2<- data.frame(Date_05d, logT10_05, logT5_05 , logT1_05)

ggplot(data=logFrame2, aes(x = Date_05d, group=1)) + 
  geom_line(aes(y = logT10_05, color="logT10_05")) + 
  geom_line(aes(y = logT5_05, color="logT5_05")) + 
  geom_line(aes(y = logT1_05, color="logT1_05")) + 
  ylab(label="logT10_05,logT5_05 ,logT1_05") + 
  xlab("Date") + 
  ggtitle("Title") + 
  scale_color_manual(name = element_blank(), values=c("logT10_05"="blue", "logT5_05"="green", "logT1_05"="red"))

#1.suitable var using logdifferences:
logData05 <- data.frame(logT1_05, logT5_05, logT10_05)
dataVar05<- ts.union(logData05)
VARselect(dataVar05, lag.max = 12, season = 12)
VAR05<-VAR(dataVar05, p=VARselect(dataVar05, lag.max = 12, season = 12)$selection[1])
summary(VAR05)

## -> maxlag should be 12 based on AIC

#2.suitable VAR level data:
Data05 <- data.frame(T1_05, T5_05, T10_05)
dataVarr05<- ts.union(Data05)
VARselect(dataVarr05, lag.max = 12, season = 12)
#everything gives 2

## QUESTION C again
cointeg05<-ca.jo(dataVarr05,season = 12, ecdet = "const", K=max(VARselect(dataVar05, lag.max = 10, season = 4)$selection[1]-1,2))
summary(cointeg05)
## comment: K must be K>=2 to run
#  p-1 gives K=1 

## seemingly no need for cointegration test. It has no cointegration.
## logT5_05 and logT10_05 are stationary if we check the adf test below
adf.test(logT1_05)
adf.test(logT5_05)
adf.test(logT10_05)

## QUESTION D again
VECM05 <-ca.jo(dataVarr05,season = 12, ecdet = "const", K=max(VARselect(dataVar05, lag.max = 10, season = 4)$selection[1],2))
summary(VECM05)

test05 = VECM(dataVarr05, 3, r = 1, include = c("both"), beta = NULL, estim = c("ML"), LRinclude = c("none"), exogen = NULL)
summary(test05)
print(test05)

coefA(test05)
coefB(test05)
coefPI(test05)

## comment: K must be K>=2, basically VECM is not needed.