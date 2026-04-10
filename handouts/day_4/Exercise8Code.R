library(ggplot2)
library(gtsummary)
library(ggeffects)

# Import data
cm.tbm <- read.csv("https://raw.githubusercontent.com/oucru-biostats/IntroductionToBiostatistics2024/main/data/cmTbmData.csv", stringsAsFactors = TRUE)

# a)
# create a subset of HIV-positive patients
cm.tbm.hiv <- subset(cm.tbm, (hiv == 1) )
cm.tbm.hiv$log2.csfwcc <- log2(cm.tbm.hiv$csfwcc + 1) # add +1 to cope with 0's
# or use cm.tbm.hiv <- mutate(cm.tbm.hiv, log2.csfwcc=log2(cm.tbm.hiv$csfwcc + 1))

# describe log2.csfwcc by diagnosis
ggplot(cm.tbm.hiv, aes(diagnosis, log2.csfwcc)) + geom_boxplot() + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')

# cm.tbm.hiv$diagnosis <- factor(cm.tbm.hiv$diagnosis)
fit1 <- glm(diagnosis ~ log2.csfwcc, data = cm.tbm.hiv, family = binomial)
# summarize model
summary(fit1)
confint(fit1)
# the summary and confint present results on the logit scale
# if we want the OR and its confidence interval, we need to exponentiate these numbers
# using standard R code we write
exp(c(coef(fit1)[2],confint(fit1)[2,]))
# or in a formatted table
tbl_regression(fit1, exponentiate=TRUE)

# alternative approach for outcome: create new variable tbm which 0 for CM and 1 for TBM
cm.tbm.hiv$tbm <- ifelse(cm.tbm.hiv$diagnosis == "TBM", 1, 0)
fit2 <- glm(tbm ~ log2.csfwcc, data = cm.tbm.hiv, family = binomial)

# prediction for new patients with log2.csfwcc = 6
lp <- -4.6370 + 0.7262 * 6 # a + b * x based on the regression coefficients
# or better without rounding of intermediate values
lp <- coef(fit1)[1] + coef(fit1)[2]*6
exp(lp)/(1+exp(lp))

predict(fit1, newdata=data.frame(log2.csfwcc=6), type="response")

pred <- ggpredict(fit2)
pred
plot(pred) # plots at values 0,2,4,6,8,10,12
# we can make a slightly more beautiful figure via

pred_data <- ggpredict(fit2, terms = "log2.csfwcc [all]")

ggplot(pred_data, aes(x = x, y = predicted)) +
  # Add the confidence interval ribbon
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  # Add the prediction line
  geom_line(color = "blue", size = 1) +
  # Add the raw data points (this replaces add.data = TRUE)
  geom_point(data = attr(pred_data, "rawdata"), aes(x = x, y = response), alpha = 0.5) +
  # Apply your custom scales
  scale_y_continuous(
    name = "Probability of TBM", 
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0 (CM)", "0.25", "0.5", "0.75", "1 (TBM)")
  ) +
  xlab("log2( CSF WCC [10^3/mm3]+1)") +
  theme_minimal()
# using fit1 doesn't work if we want to add the raw data

# Import data
dfstudy <- read.csv("https://raw.githubusercontent.com/oucru-biostats/IntroductionToBiostatistics2024/main/data/DF.csv",  stringsAsFactors = TRUE)
df2009 <- subset(dfstudy, year == 2009)
df2009 <- subset(df2009, select=c(age, sex, day_ill, temp, plt, hct, reshock))


# quick descriptive stats
summary(df2009)

## almost all temperatures are 37.00 degrees
ggplot(df2009,aes(temp)) + geom_histogram(binwidth=0.01)

## platelet has a somewhat skewed distribution, or at least some very high values
ggplot(df2009,aes(plt)) + geom_boxplot()

# by group using gtsummary
tbl_summary(data=df2009, by=reshock)

## we log-transform platelet and the further results are based on log-transformed plt 
## (though we will see that this does not really affect the results).
## Also, we need to use reshock as a 0-1 variable or a variable of type factor.
df2009$log2.plt <- log2(df2009$plt)

# Univariable regression
summary(glm(reshock ~ age, data = df2009, family = binomial))
summary(glm(reshock ~ sex, data = df2009, family = binomial))
summary(glm(reshock ~ day_ill, data = df2009, family = binomial))
summary(glm(reshock ~ temp, data = df2009, family = binomial))
summary(glm(reshock ~ plt, data = df2009, family = binomial)) # untransformed plt
summary(glm(reshock ~ log2.plt, data = df2009, family = binomial)) # log-transformed plt
summary(glm(reshock ~ hct, data = df2009, family = binomial))

# We can do it all at once via the tbl_uvregression function from the gtsummary package
tbl_uvregression(data=df2009, method=glm, y=reshock, method.args=list(family=binomial), exponentiate=TRUE)

# Multivariable regression
fit <- glm(reshock ~ age + sex + day_ill + temp + log2(plt) + hct, data = df2009, family = binomial)
tbl_regression(fit, exponentiate=TRUE)

fit.quad <- glm(reshock ~ poly(age, 2) + sex + poly(day_ill, 2) + temp + log2(plt) + hct, data = df2009, family = binomial)
anova(fit, fit.quad, test = "Chisq")

# Test for interaction between sex and hct
fit.ia <- glm(reshock ~ age + hct*sex + day_ill + temp + log2(plt) , data = df2009, family = binomial)
anova(fit, fit.ia, test = "Chisq")
summary(fit.ia)
