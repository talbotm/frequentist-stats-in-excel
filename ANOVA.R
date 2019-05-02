# analysis of variance in R
#########################################

rm(list=ls())

dir <- function(){ 
  a <- list.files(getwd()) # to get files
  b <- list.dirs(getwd()) # to get folders
  return(list(b, a)) }




#########################################
#install.packages("car")
#install.packages("reshape")
#install.packages("multcomp")
#install.packages("dunn.test")
#install.packages("pwr")
library(lmPerm)
library(car)
library(reshape)
library(multcomp)
library(dunn.test)
library(pwr)

# open the file
setwd("/Users/talbotm/Dropbox/MIGS2016")
mydata<- read.csv(file="rawSUSdata4ANOVA.csv",head=TRUE,sep=",")

# look at the 10 data points
head(mydata,10)

# remove the Users column - first column
mydata <- mydata[2:5]     


# transform the data structure to work with ANOVA function
mydata_melt <- melt(mydata)
names(mydata_melt) <- c("games","scores")
head(mydata_melt, 20)



# Perform a one-way within-group ANOVA on four dependent variables with no interactions
# 2 approaches, same result
lm_fit = lm(scores~games, data = mydata_melt)
anova(lm_fit)

aov_fit = aov(scores~games , data = mydata_melt)
summary(aov_fit)

aovp_fit = aovp(scores~games , data = mydata_melt)
summary(aovp_fit)


# we know from the design of our experiment that the assumption of independence of information is ok
#
# Test the assumption Homoscedasticity 
# Ho: the variance is the same. If p-value < 0.05 then reject Ho -> the variance is not the same accross versions... 
# ... else: no evidence suggests that the variance is different
bartlett.test(scores~games, data=mydata_melt)


# Test the assumption of of normality
# the residual data must fall withing the envelop to meet the assumption of normaly
qqPlot(lm_fit ,  simulate=TRUE, main="Q-Q Plot", labels=FALSE)

# Test which of the method is significantly better than the other
tuk <- glht(aov_fit, linfct=mcp(games="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
tfit <- TukeyHSD(aov_fit, conf.level = 0.95)
tfit


# CONCLUSION:
# reject the null hypothesis that considered is that there was no difference in treatments mean 
# A is better than B 
# A is better than C
# D is better than B
# D is better than C
# Fail to reject the null hypothesis that A and D are different.
# Fail to reject the null hypothesis that B and C are different.

# How many more participants would we need in order to get a significant difference between A and D?

# get the means and standard deviation of each column (versions)
colmeans <- apply(mydata, 2, mean)
colStdev <- apply(mydata, 2, sd)
# A and D
pwr.t.test(d=(colmeans[1]-colmeans[4])/(colStdev[1]^2 + colStdev[4]^2),power=.8,sig.level=.05,type="paired",alternative="two.sided")
# A and B
pwr.t.test(d=(colmeans[1]-colmeans[2])/(colStdev[1]^2 + colStdev[2]^2),power=.8,sig.level=.05,type="paired",alternative="two.sided")

# T-Test : same variance, withing group
########################################
t.test(mydata$version_B,mydata$version_C, var.equal=TRUE, paired=TRUE)

# look at the distribution of C and B
plot(density(mydata$version_C))
plot(density(mydata$version_B))


# Test the assumption of of normality
# Ho: the data is the nornal. If p-value < 0.05, then reject Ho -> the data is not normal.
# Here, the assumption of normality is not met.
shapiro.test(mydata$version_B)
shapiro.test(mydata$version_C)
# same finding with the qqnorm plot, B being worst than C
qqnorm(mydata$version_B)
qqline(mydata$version_B, col = 2)
qqnorm(mydata$version_C)
qqline(mydata$version_C, col = 2)

# Test how a normally distributed data looks like on a qq plot
qqnorm(rnorm(100, mean = 5, sd = 3))
qqline(rnorm(100, mean = 5, sd = 3), col = 2)


# Test equal variance
data_var_test <- melt(data.frame(mydata$version_B, mydata$version_C))
bartlett.test(value~variable, data=data_var_test)

###################################
# permutation exact tests
#install.packages("lmPerm")
#install.packages("coin")
library(lmPerm)
library(coin)
d_permtest <-data.frame(mydata$version_B, mydata$version_C)
names(d_permtest) <- c("A", "B")
d_permtest_melt <- melt(d_permtest)
oneway_test(value~variable, data=d_permtest_melt, distribution="exact")





###################################
# Non parametric tests examples
wilcox.test(mydata[,2], mydata[,3])  # 2 dependent variable tests
kruskal.test(scores~games, data=mydata_melt) # > 2 dependent vairiable tests
dunn.test(mydata) # post-hoc test (à la tuckey)



