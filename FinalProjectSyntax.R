#Final Project Syntax
ISS <- read.csv('/Users/kathrynharrison/Desktop/ISSdata_clean.csv')
head(ISS)
View(ISS)
str(ISS)
ISS$AgeYears[ISS$AgeYears == 999] <- NA
ISS$AgeYears

#Descriptive Statistics
sum(ISS$CurrentUse)
table(ISS$CurrentUse)

mean(ISS$AgeYears)
sd(ISS$AgeYears)

mean(ISS$Honesty)
sd(ISS$Honesty)
var(ISS$Honesty)
median(ISS$Honesty)
quantile(ISS$Honesty)

mean(ISS$Homophily)
sd(ISS$Homophily)
var(ISS$Homophily)
median(ISS$Homophily)
quantile(ISS$Homophily)

mean(ISS$MisrepConcerns)
sd(ISS$MisrepConcerns)
var(ISS$MisrepConcerns)
median(ISS$MisrepConcerns)
quantile(ISS$MisrepConcerns)

mean(ISS$DatingEfficacy)
sd(ISS$DatingEfficacy)
var(ISS$DatingEfficacy)
median(ISS$DatingEfficacy)
quantile(ISS$DatingEfficacy)

mean(ISS$ISgut)
sd(ISS$ISgut)
var(ISS$ISgut)
median(ISS$ISgut)
quantile(ISS$ISgut)

mean(ISS$ISActive)
sd(ISS$ISActive)
var(ISS$ISActive)
median(ISS$ISActive)
quantile(ISS$ISActive)

mean(ISS$ISInteractive)
sd(ISS$ISInteractive)
var(ISS$ISInteractive)
median(ISS$ISInteractive)
quantile(ISS$ISInteractive)

mean(ISS$ISPassive)
sd(ISS$ISPassive)
var(ISS$ISPassive)
median(ISS$ISPassive)
quantile(ISS$ISPassive)

mean(ISS$ISExtractive)
sd(ISS$ISExtractive)
var(ISS$ISExtractive)
median(ISS$ISExtractive)
quantile(ISS$ISExtractive)

library(stargazer)
stargazer(ISS[c("ISActive", "ISPassive", "ISInteractive", "ISExtractive", "ISGut", "Homophily", "DatingEfficacy", "Honesty", "MisrepConcerns")], type = "html",
          title = "Descriptive Statistics", digits = 2, out = "ISSTable1.htm")
#H1 - Greater misrep. concerns = more ISS & gut
#Correlations - Homphily, misrep. concerns, online dating efficacy, honesty

#Correlation Matrix
cor(ISS[, 133:136])
round(cor(ISS[, 133:136]), 
      digits = 2)

library(GGally)
ggpairs(ISS[, 133:136])

#Regressions - ISS (Active, Interactive, Extractive, Passive & Other Variables 
reg_active <- lm(ISS$ISActive ~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily)
summary(reg_active) 
anova(reg_active)
plot(reg_active)

reg_interactive <- lm(ISS$ISInteractive ~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily)
summary(reg_interactive)
anova(reg_interactive)
plot(reg_interactive)

reg_extractive <- lm(ISS$ISExtractive ~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily)
summary(reg_extractive)
anova(reg_extractive)
plot(reg_extractive)

reg_passive <- lm(ISS$ISPassive ~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily)
summary(reg_passive)
anova(reg_passive)
plot(reg_passive)

reg_gut <- lm(ISS$ISGut ~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily)
summary(reg_gut)
anova(reg_gut)
plot(reg_gut)

library(stargazer)
stargazer(reg_active, reg_interactive, reg_extractive, reg_passive, reg_gut, type = "html", 
          dep.var.labels = c("Active", "Interactive", "Extractive", "Passive", "Gut"),
          covariate.labels = c("Misrep. Concerns", "Dating Efficacy", "Honesty", "Homophily"), out = "ISSRegTable.htm")

#Graphics
library(car)
scatterplot.matrix(~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily|ISS$ISActive, data = ISS,
                   main = "Active ISS")
scatterplot.matrix(~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily|ISS$ISInteractive, data = ISS,
                   main = "Interactive ISS")
scatterplot.matrix(~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily|ISS$ISExtractive, data = ISS,
                   main = "Extractive ISS")
scatterplot.matrix(~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily|ISS$ISPassive, data = ISS,
                   main = "Passive ISS")
scatterplot.matrix(~ ISS$MisrepConcerns + ISS$DatingEfficacy + ISS$Honesty + ISS$Homophily|ISS$ISGut, data = ISS,
                   main = "Gut ISS")

#Testing Normality Assumptions 
library(car)
outlierTest(reg_active)
qqPlot(reg_active, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(reg_active) 
hist(sresid, freq = FALSE, 
     main="Distribution of Active Studentized Residuals")
xactive <- seq(min(sresid),max(sresid),length=40) 
yactive <- dnorm(xactive) 
lines(xactive, yactive)

outlierTest(reg_extractive)
qqPlot(reg_extractive, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(reg_extractive) 
hist(sresid, freq = FALSE, 
     main="Distribution of Extractive Studentized Residuals")
xextractive <- seq(min(sresid),max(sresid),length=40) 
yextractive <- dnorm(xextractive) 
lines(xextractive, yextractive)

outlierTest(reg_interactive)
qqPlot(reg_interactive, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(reg_interactive) 
hist(sresid, freq = FALSE, 
     main="Distribution of Interactive Studentized Residuals")
xinteractive <- seq(min(sresid),max(sresid),length=40) 
yinteractive <- dnorm(xinteractive) 
lines(xinteractive, yinteractive)

outlierTest(reg_passive)
qqPlot(reg_passive, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(reg_passive) 
hist(sresid, freq = FALSE, 
     main="Distribution of Passive Studentized Residuals")
xpassive <- seq(min(sresid),max(sresid),length=40) 
ypassive <- dnorm(xpassive) 
lines(xpassive, ypassive)

outlierTest(reg_gut)
qqPlot(reg_gut, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(reg_gut) 
hist(sresid, freq = FALSE, 
     main="Distribution of Gut Studentized Residuals")
xgut <- seq(min(sresid),max(sresid),length=40) 
ygut <- dnorm(xgut) 
lines(xgut, ygut)

#Test of Equal Variances 
ncvTest(reg_active)
ncvTest(reg_interactive)
ncvTest(reg_extractive)
ncvTest(reg_passive)
ncvTest(reg_gut)

#Test of Independence 
durbinWatsonTest(reg_active)
durbinWatsonTest(reg_interactive)
durbinWatsonTest(reg_extractive)
durbinWatsonTest(reg_passive)
durbinWatsonTest(reg_gut)

# Global test of model assumptions
library(gvlma)
gvmodela <- gvlma(reg_active) 
summary(gvmodela)

gvmodeli <- gvlma(reg_interactive) 
summary(gvmodeli)

gvmodele <- gvlma(reg_extractive) 
summary(gvmodele)

gvmodelp<- gvlma(reg_passive) 
summary(gvmodelp)

gvmodelg <- gvlma(reg_gut) 
summary(gvmodelg)
