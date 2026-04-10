library(ggplot2)
library(patchwork)

# Import the dataset
cmTbm <- read.csv("https://raw.githubusercontent.com/oucru-biostats/IntroductionToBiostatistics2024/main/data/cmTbmData.csv")

# Create a new dataset cmTbm.hiv that contains HIV-positive patients only
cmTbm.hiv <- subset(cmTbm,hiv==1)

ggplot(cmTbm.hiv, aes(diagnosis,age)) + geom_boxplot() + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')

ggplot(cmTbm.hiv, aes(x = age, fill = diagnosis)) + geom_density(alpha = 0.7)

## alternative: frequency polygon (not available via ggplotgui)
ggplot(cmTbm.hiv, aes(x = age)) + geom_freqpoly(aes(colour = diagnosis),binwidth = 5)

t.test(age ~ diagnosis, data = cmTbm.hiv) 

# remove the person aged 62
t.test(age ~ diagnosis, data = cmTbm.hiv, subset = age<60)

# Wilcoxon rank-sum test
wilcox.test(age~diagnosis, data=cmTbm.hiv)

# remove the person aged 62
wilcox.test(age~diagnosis, data=cmTbm.hiv, subset=age<60)

# First, draw boxplots and calculate several summary statistics
summary(cmTbm.hiv$bldwcc)

p1 <- ggplot(cmTbm.hiv, aes(diagnosis, bldwcc)) + geom_boxplot() + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')
p2 <- ggplot(cmTbm.hiv, aes(diagnosis, log10(bldwcc))) + geom_boxplot() + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')
p1 + p2

summary(cmTbm.hiv$csfwcc)

p1 <- ggplot(cmTbm.hiv, aes(diagnosis, csfwcc)) + geom_boxplot() + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')
p2 <- ggplot(cmTbm.hiv, aes(diagnosis, log10(csfwcc + 1))) + geom_boxplot() + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')
p1 + p2

# t test (csfwcc we only use log transformed data) 
t.test(bldwcc ~ diagnosis, data = cmTbm.hiv)
t.test(log10(bldwcc) ~ diagnosis, data = cmTbm.hiv)
t.test(log10(csfwcc + 1) ~ diagnosis, data = cmTbm.hiv)

# Wilcoxon test
wilcox.test(log10(bldwcc) ~ diagnosis, data = cmTbm.hiv)
wilcox.test(bldwcc ~ diagnosis, data = cmTbm.hiv) 

wilcox.test(log10(csfwcc + 1) ~ diagnosis, data = cmTbm.hiv)
wilcox.test(csfwcc ~ diagnosis, data = cmTbm.hiv)

# Import dataset
bmData <- read.csv("https://raw.githubusercontent.com/oucru-biostats/IntroductionToBiostatistics2024/main/data/bmData.csv")

summary(bmData$wc.csf)
summary(bmData$wc.csf.fup)

# draw histogram
p1 <- ggplot(bmData, aes(wc.csf)) + geom_histogram()
p2 <- ggplot(bmData, aes(wc.csf.fup)) + geom_histogram()
p1 + p2

# perform a log-transformation
bmData$log.wc <- log10(bmData$wc.csf)
bmData$log.wc.fup <- log10(bmData$wc.csf.fup)

# draw histogram of the log-transformed variables
p1 <- ggplot(bmData, aes(log.wc)) + geom_histogram()
p2 <- ggplot(bmData, aes(log.wc.fup)) + geom_histogram()
p1 + p2

# compare
t.test(subset(bmData, group=="dexamethasone")$log.wc, subset(bmData, group=="dexamethasone")$log.wc.fup, paired = TRUE)
# alternative formulation
with( subset(bmData, group=="dexamethasone"), t.test(log.wc, log.wc.fup, paired = TRUE))
# Equivalent alternative: One sample t.test to test whether difference is 0
t.test(subset(bmData, group=="dexamethasone")$log.wc - subset(bmData, group=="dexamethasone")$log.wc.fup)
# this can also be formulated as
t.test(log.wc.fup - log.wc ~ 1, data = bmData, subset = group=="dexamethasone")

# t-test using the formula structure, with the difference as outcome.
t.test(log.wc.fup - log.wc ~ group, data = bmData)

# Or Wilcoxon test
wilcox.test(log.wc.fup-log.wc ~ group, data = bmData)
