#############################################################
### Base graphics
#############################################################

library(readxl)
titanic <- read_excel("data/raw/Titanic3.xlsx", na="NA")

#############################################################
## Basic plot
plot(titanic$age, titanic$fare)
## Exercise 1. What would you like to change to make it good enough for publication?


## Formula based specification
plot(fare ~ age, data=titanic)



## http://cran.r-project.org/doc/contrib/Baggott-refcard-v2.pdf
## labels
plot(titanic$age, titanic$fare, xlab="age", ylab="fare paid (GBP)")

## rotate labels along y-axis
plot(fare ~ age, data=titanic, las=1)

## add regression line
abline(lm(fare~age, data=titanic), col="red")

## Use log10 of fare
plot(log10(fare)~age, data=titanic)
abline(lm(log10(fare)~age, data=titanic, subset=fare>0), col="red")

## Use logarithmic scale for fare
plot(log10(fare)~age, data=titanic)
plot(fare ~ age, data=titanic, las=1, log="y")
## Warning: log(0) does not exist
table(titanic$fare==0) # 17 paid fare 0
points(fare+3~age, data=titanic, col="red", subset=fare==0)
title("Fare paid for ticket on Titanic versus age")
## Why do you see only 7 red circles?





## change plotting character
plot(fare ~ age, data=titanic, las=1, log="y")
colors()
points(fare+3~age, data=titanic, col="red", subset=fare==0, pch=25, bg="red")
title("Fare by age")

## add text and arrows
text(48, 450, "So expensive", col="red")
arrows(41,455,37,500, length=0.1)
arrows(54,455,57.5,500, length=0.1)


## Use base graphics for simple tasks
## Example: explain logarithmic scale via graph
x <- seq(1,500,by=1)
plot(x, log10(x))

plot(x, log10(x), type="l", xaxs="i")
segments(0,2,100,2,lty=2)
arrows(100,2,100,0,lty=2)

## Exercise 2. Make a histogram of age

hist(titanic$age)
hist(titanic$age,breaks=15,cex.axis=1.5,cex.lab=1.5,xlab="age")

## Exercise 3.a. Make a boxplot of fare by passenger class.
plot(fare ~ pclass, data=titanic)
boxplot(fare ~ pclass, data=titanic, cex.axis=1.5, cex.lab=1.5, las=1)
## Exercise 3.b. There are high values for fare that disturb the plot.
##             Therefore, only show the fare up to 300 British Pounds.
boxplot(fare ~ pclass, data=titanic, ylim=c(0,300))

## Exercise 4: Find information on
* descriptive statistics
* change several categorical variables into factor
* recognize nonstandard date formats

## Exercise 5:
sapply(titanic, mode)
with(titanic,tapply(fare, pclass, FUN= summary))
