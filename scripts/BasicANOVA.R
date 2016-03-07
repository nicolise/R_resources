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

#check normality of your dependent variable


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
#delete these?
require(car)
require(MASS)
ModDat$Scale.LS.t <- ModDat$Scale.LS + 1
#fairly normal
qqp(ModDat$Scale.LS.t, "norm")
#definitely not log-normal
qqp(ModDat$Scale.LS.t, "lnorm")
#data must be integers for the rest
ModDat$Scale.LS.i <- ModDat$Scale.LS*100 + 100
ModDat$Scale.LS.i <- round(ModDat$Scale.LS.i)
#negative binomial looks sort of close, but losing information in the conversion?
nbinom <- fitdistr(ModDat$Scale.LS.i, "Negative Binomial")
qqp(ModDat$Scale.LS.i, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(ModDat$Scale.LS.i, "Poisson")
qqp(ModDat$Scale.LS.i, "pois", poisson$estimate)

#histogram of data-- goal to compare to poisson / negative binomial
dat <- hist(ModDat$Scale.LS.i, breaks=20)
hist(ModDat$Scale.LS, breaks=20)
dat
mids <- c(110, 130, 150, 170, 190, 210, 230, 250, 270, 290, 310, 330, 350, 370, 390, 410, 430, 450, 470, 490, 510, 530)
counts <- c(1813,  850,  782,  649,  553,  471,  353,  242,  184,  141,   93,   61,   30,   19,   13,    6,   10,  3,    6,    3,    1,    2)
histdat <- data.frame(cbind(mids,counts))

#overlay negative binomial
#simplest normalization
ModDat$Nmod <- ModDat$Scale.LS / sum(ModDat$Scale.LS)
#alternative normalization
#temp$Nmod <- temp$N / sqrt(sum(temp$N * temp$N))
histdat$pois <- dpois(histdat$mids, lambda = mean(histdat$counts))
histdat$nbinom <- dnbinom(histdat$counts, mu = mean(histdat$counts), size = 1)
ggplot(histdat, aes(x=mids, y=counts)) +
  geom_histogram(stat="identity", binwidth = 2.5) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank()) + 
  geom_line(aes(y = pois), col = "red") + 
  geom_line(aes(y = nbinom), col = "blue")

#----------------------------------------------------------------------
#AgFlat is nested within ExpBlock
#and both are random
#Leaf is nested within Plant
#and both are random
#And we can consider Pgeno to be nested within Species
#Igeno, Species, Pgeno, AorB are fixed

library(lme4); library(car); library(lmerTest)
#Anova(Mod, type=2)
#anova(Mod)
#Variance output of summary(Mod) gives you SS for the random factors
#rand(Mod) gives Chi-sq  and P values for random factors in packages lmerTest
#SPmodD <- lmer(Scale.LS ~ Igeno + Pgeno + Igeno:Pgeno + (1|ExpBlock/AgFlat) + AorB + (1|ExpBlock) + (1|Plant), data = MDdomest)

#linear model
#nesting: B within A as A/B or A + A:B
#fixed effects: PInLflt
#random effects: PPlant, Isolate, PInPlant, PInLeaf, Pexp
#ExpBlock and AgFlat as random effects
#but maybe include a random term for "bench"??
#ExpBlock/Bench/AgFlat

#nesting terms are already included- don't need to add Species as a separate term BUT for random effects (ExpBlock alone) do need a separate term
#PlGenoNm is a term nested within Species (but not CODED as if nested within Species = it's not an implicitly nested factor)
#AgFlat IS implicitly nested within ExpBlock -- let's fix this
#non-numeric factors to use: Igeno, PlGenoNm, Species, ExpBlock, AgFlat
#expblock is only 6 terms so I'm going to include it as a fixed effect

#optional to fix: coding of AgFlat so that it is not implicitly nested
Sys.time()
#fullmod <- lmer(Scale.LS ~ Igeno + Species/PlGenoNm + Igeno:Species/PlGenoNm + Igeno:Species + ExpBlock + (1|ExpBlock/AgFlat) + (1|IndPlant) + AorB , data = ModDat)
#trying: LesionWpi.lm and LesionWOpi.lm
LesionWpi.lm <- lmer(Scale.LS ~ Igeno + Species/PlGenoNm + Igeno:Species/PlGenoNm + Igeno:Species + ExpBlock + (1|ExpBlock/AgFlat) + (1|IndPlant) + AorB , data = ModDat)
LesionWOpi.lm <- lmer(Scale.LS ~ Igeno + Species/PlGenoNm + Igeno:Species + ExpBlock + (1|ExpBlock/AgFlat) + (1|IndPlant) + AorB , data = ModDat)
Sys.time()
sink(file='output021716.txt')
Sys.time()
#summary(fullmod) # the code generating output
#Sys.time()
rand(fullmod)
Anova(fullmod, type=2)
anova(fullmod)
Sys.time()
sink()

