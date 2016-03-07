#BasicANOVA.R
#ANOVA script for data analysis
#Nicole E Soltis
#03/07/2016
#-------------------------------------------------
#load your data
rm(list=ls())
setwd("~/PATH/TO/DIRECTORY/")
getwd()
MyDat <- read.csv("FILENAME.csv")
names(MyDat)
view(MyDat)

#check structure of new object
str(MyDat)
#is it a data.frame?
class(MyDat)
#list column names
attach(MyDat)
names(MyDat)

#--------------------------------------------
#basic dataframe manipulation
#optional: subset columns
MyDat2 <- MyDat[,c(1:3)]

#optional: stick 2 data frames together by column
MyDat3 <- rbind(MyDat2, OTHERFILE)

#optional: remove any duplicate rows
MyDat2 <- unique(MyDat2)

#optional: remove rows depending on the value in one column
unique(MyDat3$VARIABLE)
MyDat3 <- MyDat3[MyDat3$VARIABLE!="VALUE",]

#optional: add a column that combines 2 other columns
MyDat3$NEWVAR <- paste(MyDat3$VAR1, MyDat3$VAR2, sep='') 

#optional: add a column by performing an equation on 2 existing columns
MyDat2 <- transform(MyDat2, NEWVAR=(OLDVAR1/OLDVAR2))

#optional: rename columns
names(MyDat3)
MyDat3 <- dplyr::select(MyDat, NEWNAME = OLDNAME, NEWNAME2 = OLDNAME2, matches("."))

#save your edited data frame  
write.csv(MyDat3, "NEWDATAFRAME.csv")

#and you can read it back in here
MyDat4 <- read.csv("NEWDATAFRAME.csv")
#----------------------------------------------------------
#handy packages
#load packages for plots
library(beanplot); library(ggplot2); library(RColorBrewer) 

#load packages for data manipulation
library(plyr); library(dplyr)

#--------------------------------------------------------
#CHECK ASSUMPTIONS BEFORE STATISTICS!
#check data structure
xtabs(~ INDVAR1 + INDVAR2, MyDat4)

#check assumption of normality of your dependent variable
attach(MyDat4)
#graphically...
hist(DEPVAR)
#more graphs
require(car); require(MASS)
MyDat4$DEPVAR.t <- MyDat4$DEPVAR + 1
#is it more normal or log-normal?
qqp(MyDat4$DEPVAR.t, "norm")
qqp(MyDat4$DEPVAR.t, "lnorm")

#statistically...
shapiro.test(DEPVAR)

#try transformations
transf <- (log((DEPVAR)))
hist(transf)
shapiro.test(transf)

#negative skew: small to large
transf <- (DEPVAR)^2
transf <- (DEPVAR)^3
#positive skew: small to large
transf <- (DEPVAR)^.5
transf <- log(DEPVAR)
transf <- log10(DEPVAR)
transf <- (-1/((DEPVAR)^.5))

#other options for normalization
transf <- MyDat4$DEPVAR / sum(MyDat4$DEPVAR)
transf <- MyDat4$DEPVAR / sqrt(sum(MyDat4$DEPVAR * MyDat4$DEPVAR))

#transform your dependent variable if necessary
MyDat4 <-transform(MyDat4, TRANSFORMED = (DEPVAR^2))
attach(MyDat4)

#next check assumption of homoscedasticity
#graphically...
boxplot(traf~INDVAR1*INDVAR2,
        ylab="YTITLE", main="PLOTTITLE", las=3)
#statistically...
bartlett.test(traf~INDVAR1*INDVAR2) 
leveneTest(traf~INDVAR1)
var.test(traf~INDVAR1)
#---------------------------------------------------------
#Power analysis: have you collected enough data?
library(pwr)
#one-way anova
pwr.anova.test(k=5,f=0.25,sig.level=0.05,power=0.8)
#two-way anova
#cohen's f2, related to R-squared (model fit)
#u is df of numerator
#= ((levels of factor 1)-1) * ((levels of factor 2)-1)
#v is total number of subjects across all categories
#MINUS (levels of factor 1)*(levels of factor 2)
#these values are given in anova table
pwr.f2.test(u=2, v=294, sig.level=0.05, power=0.8)
#f2 is Cohen's f-squared value, ~R-squared, so effect size low.
#by convention, f2 0.02 is small, 0.15 is medium, 0.35 is large

#---------------------------------------------------------
#now we get to actually do an ANOVA!

#PARAMETRIC TESTS
#two-way ANOVA
MY.ANOVA <- anova(lm(traf~INDVAR1*INDVAR2))
summary(MY.ANOVA)
MY.ANOVA 
interaction.plot(INDVAR1,INDVAR2,traf)

#another way to two-way ANOVA
MYMOD.aov <- aov(traf~INDVAR1*INDVAR2)
plot(MYMOD.aov)
summary(MYMOD.aov)

#one more assumption to check: are your residuals normally distributed?
MYResid <-lm(traf~INDVAR1*INDVAR2)
#run shapiro-wilk goodness of fit test on the residuals
shapiro.test(residuals(MYResid))

#Post-hoc tests: which LEVELS under your independent variables differ?
TukeyHSD(MYMOD.aov)
interaction.plot(INDVAR1,INDVAR2,traf)

#----------------------------------------------------------------
#nonparametric alternatives to ANOVA
#Kruskal-Wallis: nonparametric one-way ANOVA
kruskal.test(DEPVAR~INDVAR)

#nonparametric alternative to two-way ANOVA
#equivalent to sign test if 2 columns
friedman.test(DEPVAR~INDVAR1|INDVAR2,data=MyDat4)