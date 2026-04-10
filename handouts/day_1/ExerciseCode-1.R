# cmTbm <- read.csv("Data/cmTbmDataWithErrors.csv", stringsAsFactors=TRUE)
# summary(cmTbm)

## Code is generated when you import the csv file via the menu. 
## You are strongly advised to copy this code to your R Notebook/R Script file. 
## Then you can easily continue working on your project another day by 
##    simply running the code to import the data. 
## You don't need to use the menu again. 
## On my computer, the data set is in a subfolder "Data" of the R Notebook file 
## that contains the code of the exercises. Hence my code reads:
cmTbm <- read.csv("Data/cmTbmData.csv", stringsAsFactors=TRUE)

# summary(cmTbm[,c("age","csfwcc","sex")])

summary(cmTbm[,c("age","csfwcc","sex")])

# cmTbm$sex <- factor(cmTbm$sex, labels=c("male","female"))
# ## summarize sex variable
# summary(cmTbm$sex)

cmTbm$sex <- factor(cmTbm$sex, labels=c("male","female"))
summary(cmTbm$sex)

# cmTbm$log10.csfwcc <- log10(cmTbm$csfwcc)
# summary(cmTbm$log10.csfwcc)

cmTbm$log10.csfwcc <- log10(cmTbm$csfwcc)
summary(cmTbm$log10.csfwcc)

cmTbm$log10.csfwcc <- log10(cmTbm$csfwcc+1)
summary(cmTbm$log10.csfwcc)

library(ggplot2)

# library(ggplot2)

# ggplot(cmTbm, aes(csfwcc)) + geom_histogram(binwidth=..., boundary = 0)

ggplot(cmTbm, aes(csfwcc)) + geom_histogram(binwidth=200, boundary = 0) 

# ggplot(cmTbm, aes(log10.csfwcc)) + geom_histogram(binwidth=0.25)

ggplot(cmTbm, aes(log10.csfwcc)) + geom_histogram(binwidth=0.25) 

# ggplot(cmTbm, aes(csfwcc)) + geom_boxplot()
# ggplot(cmTbm, aes(csfwcc)) + stat_ecdf()

ggplot(cmTbm, aes(csfwcc)) + geom_boxplot()
ggplot(cmTbm, aes(csfwcc)) + stat_ecdf()

# ggplot(cmTbm, aes(log10.csfwcc)) + geom_boxplot()
# ggplot(cmTbm, aes(log10.csfwcc)) + stat_ecdf()

ggplot(cmTbm, aes(log10.csfwcc)) + geom_boxplot()
ggplot(cmTbm, aes(log10.csfwcc)) + stat_ecdf()
