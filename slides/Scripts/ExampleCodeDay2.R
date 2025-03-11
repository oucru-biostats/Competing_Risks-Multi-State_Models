log(1000)           # natural log, does not give 3
help(log)
log(1000, 10)
log(10, 1000)       # gives 1000-log of 10
log(base=10, x=1000) # x required, base optional with natural log as default
log(b=10,x=1000)     # Argument names can be abbreviated if no risk of ambiguity

help(c)
c(3,6,8)
help(paste)
paste("Oxford","University","Clinical","Research","Unit")


good.morning <- function(work){
  if(work==TRUE) cat("wake up") else
    cat("you can stay in bed")
}
good.morning()
good.morning(work=FALSE)


#############################################################
### Base graphics
#############################################################
library(readxl)
titanic <- read_excel("data/raw/Titanic3.xlsx", na="NA")

help(par)

## colour by passenger class
plot(fare ~ age, data=titanic, las=1, log="y", col=pclass)
legend(70,7,legend=c("1st","2nd","3rd"), col=1:3, pch=1)
title("Fare vs age by pclass")


## Open the PDF device
pdf("FareAgePclass.pdf")
plot(fare ~ age, data=titanic, las=1, log="y", col=pclass)
legend(70,7,legend=c("1st","2nd","3rd"), col=1:3, pch=1)
title("Fare vs age by pclass")
## Close the device again to save the file
dev.off()

## Open the PDF device
pdf("FareAgePclassX.pdf", width=21, height=21)
plot(fare ~ age, data=titanic, las=1, log="y", col=pclass)
legend(70,7,legend=c("1st","2nd","3rd"), col=1:3, pch=1)
title("Fare vs age by pclass")
## Close the device again to save the file
dev.off()

#############################################################
### Structure of R program
#############################################################

library(lubridate)
## Gives message:

## Attaching package: ‘lubridate’
## The following objects are masked from ‘package:base’:
##     date, intersect, setdiff, union

## hierarchical structure and duplicate names
mean <- function(x) "my own nonsense mean function"
## new mean object in Workspace hides the other
mean(1:10)
## removing it makes the other visible again
find("mean")
rm(mean)
mean(1:10) # original mean not overwritten!
find("mean")

#############################################################
### Classes and methods
#############################################################

titanic$pclass <- factor(titanic$pclass)
titanic$dob <- as.Date("15 April 1912", "%d %b %Y")+ (-titanic$age*365.25)


summary(titanic$age)
summary(titanic$pclass)
summary(titanic$dob)
class(titanic$pclass)
class(titanic$dob)
help(summary) # Date class not mentioned
methods(summary)
help(summary.Date)
summary.default(titanic$dob)
class(titanic)

plot(age~dob, data=titanic)
plot(age~fare, data=titanic)
plot(age~pclass, data=titanic)

#############################################################
### Lists
#############################################################

mode(titanic)
class(titanic)

#############################################################
### finding information
#############################################################

help(descriptive)
## search for descriptive statistics


#############################################################
### apply functions
#############################################################

## https://www.listendata.com/2015/05/converting-multiple-numeric-variables.html
n.vals <- apply(titanic, MARGIN=2, FUN=\(x) length(unique(x)))
## or
n.vals <- sapply(titanic, FUN=\(x) length(unique(x)))
## or
N.unique <- function(x) length(unique(x))
n.vals <- sapply(titanic, FUN=N.unique)
## or
n.vals <- lapply(titanic, FUN=\(x) length(unique(x)))
##
n.vals

## select the variables with <= 5 unique values
titanic[n.vals<=5] <- lapply(titanic[n.vals<=5], as.factor)

## median age by passenger class
with(titanic,tapply(age, pclass, median,na.rm=T))
