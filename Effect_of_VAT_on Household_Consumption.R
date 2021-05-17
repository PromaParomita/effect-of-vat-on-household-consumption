#Setting working directory
setwd("~/Desktop/Spring 2021 UTD/PPPE 6321")

#Importing data
data = read.csv('data1.csv', header=TRUE)

#Installing relevant packages
install.packages('plm')
library(plm)
install.packages('dplyr')
library(dplyr)
install.packages('tidyverse')
library(tidyverse)
install.packages('ggplot2')
library(ggplot2)
install.packages('foreign')
library(foreign)
install.packages('gplots')
library(gplots)
install.packages('tseries')
library(tseries)
install.packages('lmtest')
library(lmtest)
install.packages('stargazer')
library(stargazer)
install.packages("Hmisc")
library(Hmisc)
install.packages('corrplot')
library(corrplot)
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
install.packages('panelr')
library(panelr)
install.packages('ExPanDaR')
library(ExPanDaR)
install.packages("pastecs")
library(pastecs)
install.packages("hrbrthemes")
library(hrbrthemes)

#Creating dataframe with relevant variables
df <- data.frame(data$Country, data$Year, data$Householdcon, data$VatRate, data$IncomeRate, data$GDP, data$Govexp, data$Ir, data$Inf., data$Unemp, data$Elderpop)

# creating panel data
pdata=pdata.frame(df,index = c("Country", "Year"))

#descriptive statistics
#Generating mean of VAT rate by country
vat <- pdata %>%
group_by(Country) %>% 
  summarise(vat = mean(VatRate))
print(vat, n=50)

#new dataframe containing mean of each variable by year
d <- df%>%
  group_by(data.Year)%>%
  summarise_at(vars(-data.Country), funs(mean(., na.rm=TRUE)))
plot(d$data.Year, d$data.Householdcon)

#Correlogram plot
d1 <- d[,c(2,3,4,5,6,7,8,9,10)]
chart.Correlation(d1, histogram = TRUE, pch = 19)

#plotting all the variables over the years

d <- d %>% rename(Year=data.Year, 
         Hconsumption=data.Householdcon, 
         Vat=data.VatRate, 
         Incometax= data.IncomeRate, 
         GDP=data.GDP, 
         GovExpenditure=data.Govexp, 
         Interest=data.Ir, 
         Inflation=data.Inf., 
         Unemployment=data.Unemp,
         Elderpopulation=data.Elderpop)
colnames(d)
time.series <- ts(d[, -1], start=2007, end=2019)
plot(time.series, main= "Variables (%)")

#qqplot
qqnorm((pdata$VatRate))
qqline((pdata$VatRate))
qqnorm((pdata$Householdcon))
qqline((pdata$Householdcon))
qqnorm((pdata$IncomaRate))
qqline((pdata$IncomeRate))
qqnorm((pdata$GDP))
qqline((pdata$GDP))
qqnorm((pdata$Govexp))
qqline((pdata$Govexp))
qqnorm((pdata$Ir))
qqline((pdata$Ir))
qqnorm((pdata$Inf.))
qqline((pdata$Inf.))
qqnorm((pdata$Unemp))
qqline((pdata$Unemp))
qqnorm((pdata$Elderpop))
qqline((pdata$Elderpop))

#difference in mean
plotmeans(Householdcon~ Country, main="Heterogeineity Across Countries Household consumption", data =pdata)
plotmeans(VatRate~ Country, main="Heterogeineity Across Countries Vat Rate", data =pdata)
plotmeans(Hcon~ Year, main="Heterogeineity", data =pdata)


#fixed effect
fixed <- plm(Householdcon ~ VatRate + IncomeRate + GDP + Govexp + Ir + Inf.+ Unemp + Elderpop, model="within", data=pdata)
summary(fixed)

#time fixed effect
fixed.t <- plm(Householdcon ~ VatRate + IncomeRate + GDP + Govexp + Ir + Inf.+ Unemp  + Elderpop + factor(Year), model="within", data=pdata)
summary(fixed.t)

#Fixed effect intercepts
fixef(fixed)


#random effect
random<- plm(Householdcon ~ VatRate + IncomeRate + GDP + Ir + Govexp + Inf. + Unemp  + Elderpop, data=pdata, model="random")
summary(random)

#pooled ols
pool <- plm(Householdcon ~ VatRate + IncomeRate + GDP + Ir + Govexp + Inf. + Unemp  + Elderpop, data=pdata, model="pooling")
summary(pool)

#hausman test (fixed vs Random effect)
phtest(fixed,random)

#choose fixed effects


#pooled vs fixed effect
pFtest(fixed, pool)

#Choose fixed effecs
#Fixed vs Time Fixed effects
pFtest(fixed.t, fixed)
plmtest(fixed, c("time"),type = ("bp"))

#Choose Fixed effects

#Serial correlation
pbgtest(fixed)

#There is presence of serial correlation 

#stationary check ADF test
adf.test(pdata$Elderpop, k=2)
adf.test(pdata$Unemp, k=2)
adf.test(pdata$Inf., k=2)
adf.test(pdata$Ir, k=2)
adf.test(pdata$Govexp, k=2)
adf.test(pdata$GDP, k=2)
adf.test(pdata$IncomeRate, k=2)
adf.test(pdata$VatRate, k=2)
adf.test(pdata$Householdcon, k=2)

# All series are stationary

#Heteroscedasticity check
bptest(Householdcon ~  VatRate + IncomeRate + GDP + Ir + Govexp + Inf. + Unemp +Hdebt + Elderpop + factor(Country), data = pdata, studentize=F)
# Heteroscedasticity is present



#heteroscedasticity adjusted fixed effect coefficients
coeftest(fixed, vcovHC) 

# Adjusted for both heteroscedasticity and serial correlation coefficients
coeftest(fixed,vcovHC(fixed, method = "arellano"))

