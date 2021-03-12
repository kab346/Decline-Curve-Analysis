#detrend testing

#references libraries
library(tidyverse) #the usual library
library(dslabs) #the usual library
library(readxl) #help load excel
library(dplyr) #the usual library
library(caret) #polynomial regressions
library(zoo)
library(dplot)
library(HH) #position function

#load data 
gasprod <-read_excel("C:/Users/KATIE BROSKY/Desktop/Super Senior Year Spring/R/OG_Production_Unconventional - 125-28059.xlsx")

#date
date<-gasprod$DATE
gas<-gasprod$GAS

#remove scientific notation
options(scipen=999)

#plot the natural data
plot(gas, pch = 16, col = "blue", xlim=range(0:length(gas)), ylim=range(0:max(gas)), main=sprintf('Decline Curve for Well Number %s',gasprod$`WELL ID`[1]),
     xlab="Time [Months]", ylab="Gas Quantity in [MCF]") #Plot the results

#Linear Section - over 4 months - tank until max gas point

#assume production starts at zero - create line with slope, no intercept 
#which.max helps find the x value at which the gas is at a max
x<-c(0:which.max(gas))
y=max(gas)/which.max(gas)*x
lines(x,y,lwd=2,col="purple")

#print how long it took to reach peak
sprintf('It took %s months of gas production  for Well Number %s to reach its peak', which.max(gas), gasprod$`WELL ID`[1])

#plateau Section - how to describe?

#find position at end of plateau
'i<-c(which.max(gas):length(gas))
p<-position(gas[i]<max(gas)*.95, gas[which.max(gas):length(gas)])'

#Linear Regression of Plateau
plat <- lm(gas[which.max(gas):(which.max(gas)+5)]~c(which.max(gas):(which.max(gas)+5))) #Create a linear regression with two variables
#plot the regression against the data
a<-c(which.max(gas):(which.max(gas)+5))
lines(a,plat$coefficients[2]*a + plat$coefficients[1],lwd=2,col="purple")

#print how long it was at plateau
sprintf('Well Number %s plateaued for 5 months of gas production', gasprod$`WELL ID`[1])

#Polynomial Section - over 4 years and 8mo
poly6 <- lm(gas[(which.max(gas)+5):length(gas)] ~ poly(c((which.max(gas)+5):length(gas)), 6, raw=TRUE))
#plot the regression against the data
b<-c((which.max(gas)+5):length(gas))
lines(b, poly6$coefficients[7]*b^6 + poly6$coefficients[6]*b^5 + poly6$coefficients[5]*b^4 + poly6$coefficients[4]*b^3 + poly6$coefficients[3]*b^2 + poly6$coefficients[2]*b + poly6$coefficients[1],lwd=2, col="purple")

#print how long it declined
sprintf('Well Number %s declined for %s months of gas production', gasprod$`WELL ID`[1], (length(gas) - (which.max(gas)+5)))

#legend with all calculated coeff
legend("topright", c(sprintf('LINEAR = %s *x', max(gas)/which.max(gas)), 
                     sprintf('PLATEAU = %s *x + %s', plat$coefficients[2], plat$coefficients[1])))
      
sprintf('The equation of the delcine is a polynomial with degree of six y = %s *x^6 + %s *x^5 + %s *x^4 + %s *x^3 + %s *x^2 + %s *x + %s', 
                             poly6$coefficients[7],poly6$coefficients[6], poly6$coefficients[5], poly6$coefficients[4], poly6$coefficients[3], poly6$coefficients[2], poly6$coefficients[1])

