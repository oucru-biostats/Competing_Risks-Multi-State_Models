# knitr::opts_chunk$set(comment=NA, warning=FALSE, echo=TRUE, message=FALSE, dev = "png", dev.args = list(type = "cairo-png")) 
library(ggplot2)

cmTbm <- read.csv("https://raw.githubusercontent.com/oucru-biostats/IntroductionToBiostatistics2024/main/data/cmTbmData.csv", stringsAsFactors=TRUE)
summary(cmTbm)

cm.hivneg  <- subset(cmTbm, groupLong=="HIV neg - CM")

fit <- lm(csfwcc ~ bldwcc, data = cm.hivneg)

# Summarize the regression result
summary(fit)
confint(fit) # Confidence intervals

# Draw the scatterplot again (and add a regression line)
ggplot(cm.hivneg, aes(bldwcc, csfwcc)) + 
  geom_point() + 
  geom_smooth(method = "lm")


library(ggExtra)
# the ggExtra package allows to create the histograms (or boxplots) alongside the scatterplot via ggMarginal
# you may have to install it first
p <- ggplot(cm.hivneg, aes(bldwcc,csfwcc)) + geom_point() + geom_smooth(method="lm")
ggMarginal(p, type = "histogram", xparams=list(binwidth=2, boundary=0), 
            yparams=list(binwidth=75, boundary=0))

plot(fit)

library(ggResidpanel)
resid_panel(fit, plot="R")

library(performance)
library(see)
library(patchwork)

# checking model assumptions
check_model(fit)

# First check if there is any 0 in each wcc by seeing if the lowest value is 0
summary(cm.hivneg$bldwcc)
summary(cm.hivneg$csfwcc)

# Create new variables with log10-transformed values
cm.hivneg$log10.bldwcc <- log10(cm.hivneg$bldwcc)
cm.hivneg$log10.csfwcc <- log10(cm.hivneg$csfwcc)

# Repeat steps b) - c)
fit.log <- lm(log10.csfwcc ~ log10.bldwcc, data = cm.hivneg)
summary(fit.log)
confint(fit.log)

ggplot(cm.hivneg, aes(log10.bldwcc, log10.csfwcc)) + 
  geom_point() + 
  geom_smooth(method = "lm")

plot(fit.log)

# As a variation, we add a boxplot instead of a histogram
p <- ggplot(cm.hivneg, aes(log10.bldwcc,log10.csfwcc)) + geom_point()
ggMarginal( p, type = "boxplot")

p + geom_smooth(method="lm")
# We use the resid\_panel function from the ggResidpanel package, and now plot all residual plots
resid_panel(fit.log,"all")
# The extreme individual is the 37th observation

cm.hivneg["149",]
# The individual in row number 149 is extreme 
# Note that the subset function keeps the row numbers as in the original data set cmTbm
# Hence, that individual is not the 149th row in cm.hivneg
# We can visualize and show its values via
ggplot(cm.hivneg, aes(log10.bldwcc, log10.csfwcc)) + 
  geom_point() + 
  geom_point(data = cm.hivneg["149",], colour = "red", size = 2)

# In fact, it is the only person with a negative value for log10.bldwcc, hence we can use that to select
subset(cm.hivneg, log10.bldwcc < 0)

# Refit without that person
fit.log.del <- lm(log10.csfwcc ~ log10.bldwcc, data = cm.hivneg, subset = row.names(cm.hivneg) != "149")
# Easier is
# fit.log.del <- lm(log10.csfwcc ~ log10.bldwcc, data = cm.hivneg, subset = log10.bldwcc > 0)
# Or observing that it is individual 37
# fit.log.del <- lm(log10.csfwcc ~ log10.bldwcc, data = cm.hivneg[-37,])
summary(fit.log.del)
confint(fit.log.del)

ggplot(subset(cm.hivneg, log10.bldwcc > 0), aes(log10.bldwcc, log10.csfwcc)) + 
  geom_point() + 
  geom_smooth(method = "lm")
plot(fit.log.del)

# Now individual with row number 126 may be problematic with respect to the QQ plot and Residuals vs Fitted
fit.log.del2 <- lm(log10.csfwcc ~ log10.bldwcc, data = cm.hivneg, subset = !row.names(cm.hivneg) %in% c("126", "149"))
summary(fit.log.del2)
confint(fit.log.del2)

# the individual in row 37 is extreme
# we can visualize and show its values via
ggplot(cm.hivneg, aes(log10.bldwcc,log10.csfwcc)) + geom_point() + 
  geom_point(data=cm.hivneg[37,], colour="red", size=2)
subset(cm.hivneg, log10.bldwcc<0 )

# -> refit without observations  37
fit.log.del.alt <- lm(log10.csfwcc~log10.bldwcc, data=cm.hivneg[-37,])
summary(fit.log.del.alt)
confint(fit.log.del.alt)

ggplot(cm.hivneg[-37,], aes(log10.bldwcc,log10.csfwcc)) + geom_point() + geom_smooth(method="lm")
resid_panel(fit.log.del.alt, plots="all")
# Now individual 16 seems to be problematic
fit.log.del2.alt <- lm(log10.csfwcc~log10.bldwcc, data=cm.hivneg[-c(16,37),])
summary(fit.log.del2.alt)
confint(fit.log.del2.alt)


# we can also decide to transform only the dependent variable, csfwcc (it was the extreme white
# blood cell count that led to the outlier on the log scale)
# remember that we don't need to assume approximate normality for the covariable, here bldwcc
fit.log.csf <- lm(log10.csfwcc~bldwcc, data=cm.hivneg)
summary(fit.log.csf)
resid_panel(fit.log.csf, plots="all")

# Prediction of csfwcc for a patient with lo10bldwcc=1
predict(fit.log.del,newdata=data.frame(log10.bldwcc=1),interval="prediction")
predict(fit.log.del,newdata=data.frame(log10.bldwcc=1),interval="confidence")
# The prediction interval is much wider than the confidence interval.

# Slightly easier is to use the following code from the ggeffects package. Note that results are not
# exactly the same because ggeffects uses the normal distribution instead of the t-distribution
# You may have to install that package first
library(ggeffects)
ggpredict(fit.log.del)
ggpredict(fit.log.del, interval="prediction")
