############################################################
### Day 1
#############################################################


library(readxl)
titanic <- read_excel("Titanic3.xlsx", na="NA")
View(titanic)



#############################################################
## help functions
help(mean)
# ? is a shorthand for help()
?mean
# or use F1 if cursor is on name of function for which we need help

## help on a package
help(package=readxl)

#############################################################
## row selections with subset function
subset(titanic, pclass %in% c("1st","2nd"))$survived
table(subset(titanic, pclass %in% c("1st","2nd"))$survived)

subset(titanic, embarked=="Southampton" & age<2)
subset(titanic, fare<10 & age>70)
subset(titanic, sibsp %in% c(1,5) & (age==10|age>70))

## subset as function argument
xtabs(~survived, data=titanic, subset=(sex=="male"))
## do not write:
xtabs(~titanic$survived, data=titanic, subset=(titanic$sex=="male"))

#############################################################
## column selections
head(subset(titanic, select= c(sex,fare)))
head(subset(titanic, select= sex:fare))
head(subset(titanic, select= -(sex:fare)))

with(titanic, table(sex, survived))

#############################################################
## Factors
table(titanic$sex)
titanic$sex <- factor(titanic$sex)
levels(titanic$sex)
as.numeric(titanic$sex)[1:100]
titanic$sex <- factor(titanic$sex, levels=c("male","female"))
as.numeric(titanic$sex)[1:100]
table(titanic$sex)

titanic$sex <- factor(titanic$sex, labels=c("M","F"))

head(titanic)
titanic$status <- factor(titanic$survived, labels=c("no","yes"))
titanic$pclass <- factor(titanic$pclass)

#############################################################
## Dates
help(as.Date)
as.Date("15 April 1912") # R cannot interpret
as.Date("15 April 1912", format="%d %B %Y")
as.Date("15 April 12", format="%d %B %y")
julian(as.Date("15 April 1912", "%d %b %Y")) # days before Jan 1st, 1970
titanic$dob[1:10] # the days are relative to the time origin in Stata, Jan 1st, 1960
as.Date(titanic$dob,origin="1960-1-1")[1:10]
## Indeed, same as:
(as.Date("15 April 1912", "%d %B %Y") - titanic$age*365.25)[1:10]
## some further possible date formats
as.Date("15041912", "%d%m%Y")
as.Date("150412", "%d%m%y")
## convert to other format via format function
format(as.Date("1912April15", "%Y%b%d"),"%A %B %d, %Y")
# combine three columns:
a <- 14
b <- "January"
d <- 2018
paste(a,b,d,collapse=" ")
as.Date(paste(a,b,d,collapse=" "),"%d %b %Y")

library(anytime)
anydate(titanic$dob[1:10])
## ten years difference because the R date origin is assumed in anydate function
library(lubridate)
anydate(titanic$dob[1:10]) -  years(10)
anydate("15041912") # doesn't work
anydate("19120415") # does work
