## R as a pocket calculator I
2+7
2*7
7/2
2^7; 2**7

## R as a pocket calculator II
sqrt(2)
sum(1:5)
log10(1000)
10^3
pi

## Assignment
x <- sqrt(2)
print(x)     # show value
x            # shortcut for print(x)
print(x, 10) # more digits
x^2
x
x <- x^2 # overwrite value
x

## Data: selections within a single column
titanic <- data.frame(
    pclass=rep(c("1st","2nd","3rd"),c(4,2,4)),
    survived=c(1,0,0,0,1,1,0,0,0,0),
    name=c("Allison, Master. Hudson Trevor","Dulles, Mr. William Crothers","McCarthy, Mr. Timothy J","Walker, Mr. William Anderson", "Duran y More, Miss. Asuncion", "Mellinger, Miss. Madeleine Viol","Abbott, Master. Eugene Joseph", "Calic, Mr. Petar","Flynn, Mr. James", "Johnston, Miss. Catherine Helen"),
    age= c(0.9167,39,54,47,27,13,13,17,NA,NA),
	fare=c(151.55,29.7,51.8625,34.0208,13.8583,19.5,20.25,8.6625,7.75,23.4))

titanic$age
titanic$age[6,10]    # gives an error
titanic$age[c(6,10)] # c is function to combine values
titanic$age[c(6,7,8,9,10)]
6:10               # short notation for c(6,7,8,9,10)
seq(6,10,by=1)     # same as 6:10
titanic$age[6:10]
seq(1,10,by=2)
titanic$age[seq(1,10,by=2)]

## Data: selection of rows and columns
titanic[2:3, c(3,1)]
titanic[6,10]
titanic
titanic[c(2,3), c("name","pclass")]


## Exercise 1
vec <- seq(11,30)
vec[7]
## Use '-' to exclude elements
vec[-15]
## Use the concatenate function 'c'
vec[c(2,5)]
## odd numbers
index <- seq(1,length(vec),by=2)
vec[index]

## Exercise 2
fare <- titanic$fare
fare
sum(fare)
length(fare) # has value 10
sum(fare)/length(fare) # first compute the sum, divide by 10
fare/length(fare)
sum(fare/length(fare)) # first divide each value by 10, then compute the sum
mean(fare)
MeanFare <- sum(fare)/length(fare)
MeanFare
mean(fare)
# Calculate the standard deviation in three (baby) steps
fare-MeanFare
(fare-MeanFare)^2
numerator <- sum((fare-MeanFare)^2)
denominator <- (length(fare)-1)
StdFare  <- sqrt(numerator/denominator)
StdFare
sd(fare)

## Many calculation functions are vectorized
titanic$age/10 # age per 10 years for each person
c(2, 15) + c(10, -3)

## Modes
seq(0,2,by=0.25)
-2 < 2
c("Thinh", "Ronald")
letters
as.character(seq(0,2,by=0.25))
as.numeric(c(FALSE,TRUE))

## Modes: logical (I)
titanic$fare
titanic$fare[c(TRUE,FALSE,TRUE,TRUE, FALSE, TRUE,FALSE,TRUE,TRUE, FALSE)]
titanic$fare>40
titanic$pclass[titanic$fare>40]
"Thinh" > "Ronald"
titanic$pclass=="1st"
sum(titanic$pclass=="1st")

## Modes: logical (II)
TRUE & FALSE
TRUE | FALSE
!TRUE
titanic$fare
titanic$fare > 10 & titanic$fare < 40
!(titanic$fare < 10 | titanic$fare > 40)
titanic$fare[titanic$fare > 10 & titanic$fare < 40]

## Missing data
titanic$age==NA # not correct
3==NA # NA is NA, whatever the comparison value
is.na(3)
is.na(titanic$age)
table(is.na(titanic$age))

## Selection by name
teacher <- c("Ronald","Thinh","Tuyen")
room <- c(306,305,305)
names(room) <- teacher
room
names(room)
room["Ronald"]
dimnames(titanic)
 titanic[c(2,3),c("name","age")]

## Importing data
library(readxl)
titanic <- read_excel("data/raw/Titanic3.xlsx")


## Exercise 3
dim(titanic)
str(titanic)
mode(titanic$survived)
mode(titanic$age)


## Exercise 4
titanic <- read_excel("data/raw/Titanic3.xlsx", na = "NA")
## a.
head(titanic, 10)
tail(titanic, 10)
## b.
titanic <- as.data.frame(titanic)
head(titanic, 10)
tail(titanic, 10)
## c.
summary(titanic)
## d.
quantile(titanic$age, probs=c(0.05,0.25,0.5,0.75,0.95), na.rm=TRUE)
quantile(titanic$fare, probs=c(0.05,0.25,0.5,0.75,0.95), na.rm=TRUE)
## the IQR (note that this is a single number)
IQR(titanic$age, na.rm=TRUE)
IQR(titanic$fare, na.rm=TRUE)
## the the standard deviation
sd(titanic$age, na.rm = TRUE)
sd(titanic$fare, na.rm = TRUE)
## e.
table(titanic$pclass)
table(titanic$sex)
table(titanic$sex, titanic$survived)
# has no added value because no missings
table(titanic$sex, titanic$survived, useNA = "always")
## f.
addmargins(table(titanic$sex, titanic$survived))
proportions(table(titanic$sex, titanic$survived))
proportions(table(titanic$sex, titanic$survived), margin = 1)


## Exercise 5
subset(titanic,age>70)[,c("name","home_dest")]
## or
subset(titanic,subset=age>70,select=c(name,home_dest))

subset(titanic, name=="Artagaveytia, Mr. Ramon")

first <- xtabs(~sex+survived, data=titanic, subset=(pclass=="1st"))
first
third <- xtabs(~sex+survived, data=titanic, subset=(pclass=="3rd"))
third
## similar results via
first <- with(subset(titanic, pclass=="1st"), table(sex,survived))
first
third <- with(subset(titanic, pclass=="3rd"), table(sex,survived))
third



## Exercise 6
titanic$sex  <- factor(titanic$sex)
titanic$pclass <- factor(titanic$pclass)
titanic$status <- factor(titanic$survived, labels=c("no","yes"))
summary(titanic)


## Exercise 7
titanic$dob <- as.Date(titanic$dob, origin = "1960/1/1")
table(format(titanic$dob, "%d"))
min(titanic$dob, na.rm = TRUE)
max(titanic$age, na.rm = TRUE)
min(titanic$dob, na.rm = TRUE) + max(titanic$age, na.rm = TRUE) * 365.25

