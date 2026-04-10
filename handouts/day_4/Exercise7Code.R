# Load packages
library(ggplot2)
library(ggResidpanel)
library(gtsummary)
library(patchwork)

# Import the dataset
perulung <- read.csv("https://raw.githubusercontent.com/oucru-biostats/IntroductionToBiostatistics2024/main/data/perulung.csv", stringsAsFactors=TRUE)
# Plot the outcome (fev1) against the child's age and height
p1 <- ggplot(perulung, aes(age,fev1)) + geom_point()
p2 <- ggplot(perulung, aes(height,fev1)) + geom_point()
p1 + p2

# 2 simple linear regression models of fev1 against age or height, respectively
fit1 <- lm(fev1 ~ age, data=perulung)
summary(fit1)
fit2 <- lm(fev1 ~ height, data=perulung)
summary(fit2)

# Multiple linear regression analysis of fev1 depending on both age and height (+CI), compare
fit3 <- lm(fev1~age+height, data=perulung)
summary(fit3)
signif(confint(fit3),3)

# The coefficient of age is likely smaller because of the correlation between age and height.
ggplot(perulung, aes(age,height)) + geom_point()

fit4 <- lm(fev1~I(age-7)+I((height-120)/10), data=perulung)
summary(fit4)
signif(confint(fit4),3)

# Add sex as an additional covariate to the regression model
fit5 <- lm(fev1~I(age-7)+I((height-120)/10)+sex, data=perulung)
summary(fit5)
signif(confint(fit5),3)
## To see clearer what the estimate for the sex variable refers to, create a categorical sex covariate
perulung$sex <- factor(perulung$sex, levels=c(0,1), labels=c("female","male"))

fit6 <- lm(fev1~I(age-7)+I((height-120)/10)+sex, data=perulung)
summary(fit6)
signif(confint(fit6),3)
summary(fit6)$r.squared

# Diagnostic plots for the model
resid_panel(fit6,plots="all")
## interactive plot
# resid_interact(fit6, plots=c("qq","cookd","lev")) 

fit6b <- lm(fev1~I(age-7)+I((height-120)/10)+sex, data=perulung[-275,])
summary(fit6b)
resid_panel(fit6b,plots="all")

# Import the dataset
dengue <- read.csv("https://raw.githubusercontent.com/oucru-biostats/IntroductionToBiostatistics2024/main/data/dengueViremiaData.csv", stringsAsFactors=TRUE)

# Create a boxplot of log10-viremia by type
ggplot(dengue,aes(type,log10(vl.3))) + geom_boxplot() + geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red') + ylab("log10-viremia")

t.test(log10(vl.3)~serology, data=dengue)
wilcox.test(log10(vl.3)~serology, data=dengue)

t.test(log10(vl.3)~serology, data=dengue, subset=serotype=="DENV-1")
t.test(log10(vl.3)~serology, data=dengue, subset=serotype=="DENV-2")

# Effect of dengue serotype and serology on log10-viremia after controlling for age and gender
fit7 <- lm(log10(vl.3)~serotype+serology+age+sex, data=dengue)
summary(fit7)

## We use the shorthand "*" notation for interaction
fit8 <- lm(log10(vl.3)~serotype*serology+age+sex, data=dengue)
summary(fit8)

library(gtsummary)
tbl_regression(fit8,show_single_row = c(sex, serology, serotype),label = list(serotype ~ "DENV-2", serology ~ "secondary", sex ~ "male"))

## we can obtain expected values of viremia for each of the serology-serotype combinations
new.data <- expand.grid(serotype=levels(dengue$serotype),serology=levels(dengue$serology),age=11,sex="female")
predict(fit8, newdata=new.data, interval="confidence")

# via ggeffects it is easier
library(ggeffects)
plot(ggpredict(fit8, terms=~serotype+serology))

# Diagnostic plots for the model
resid_panel(fit8,plots="all")
