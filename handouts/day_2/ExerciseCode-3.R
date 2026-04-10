dbinom(97, size=100, prob=0.9)

x <- 71:100
y <- dbinom(x,100,0.9)
plot(x, y, lwd=2, type="h")
lines(x[90-70], y[90-70], lwd=2, type="h", col="red")

pbinom(85, size=100, prob=0.9) + (1-pbinom(94, size=100, prob=0.9) )

# version 1: using the binomial distribution
1-pbinom(15, size=50, prob=0.2)
# equivalently:
sum(dbinom(16:50, size=50, prob=0.2)) 

# version 2: using binom.test
binom.test(16, 50, p = 0.2, alternative = "greater") 

# version 3: using prop.test
prop.test(16, 50, p = 0.2, alternative = "greater") 

1-pbinom(15, size=50, prob=0.2)+pbinom(4,size=50,prob=0.2)
## similarly
binom.test(16, 50, p = 0.2, alternative = "two.sided") 

 1-pbinom(15, size=50, prob=0.2)
# p-value > 0.025
 1-pbinom(16, size=50, prob=0.2)
 # p-value < 0.025
 # hence we reject the null hypothesis if X>16

# calculate probability of X>=17 if the alternative is true (p=0.4)
1-pbinom(16,size=50,prob=0.4) 

library(ggplot2)
binom.prob <- data.frame(x=c(0:50-0.1,0:50+0.1),     prob=c(dbinom(0:50,size=50,prob=0.2),dbinom(0:50,size=50,prob=0.4)),
    hypothesis=rep(c("p=0.2 [null]","p=0.4 [alternative]"),c(51,51)))
head(binom.prob)  # plot first 6  rows of data.frame
tail(binom.prob)  # plot last six rows of data.frame
# we shift the x-values by 0.1 
#in order to prevent the bars in the figure below for p=0.2 and p=0.4 to overlap
ggplot(binom.prob, aes(x,prob,color=hypothesis)) + 
  geom_segment(aes(xend = x, yend = 0)) +
  geom_vline(xintercept=16.5) + theme(legend.position="top")

ns <- read.csv("Data/dengueNS1.csv", as.is=FALSE)
summary(ns)

addmargins(table(ns$ns1,ns$dengue))

# prevalence:
731/853
prop.test(731,853) 

# sensitivity
499/731
prop.test(499,731) 
# specificity
122/122
prop.test(122,122) 

# PPV
499/499
prop.test(499,499) 
# NPV
122/354
prop.test(122,354) 

library(gtsummary)
ns.truepos <- subset(ns,dengue=="yes") # select only subjects with dengue
tbl_summary(data=ns,by=serotype,include=c(serotype,ns1))
## a plain table can be obtained via 
table(ns$ns1,ns$serotype)

bmData <- read.csv("Data/bmData.csv", as.is=FALSE)
summary(bmData)

xtabs(~group+death.6mo, data=bmData)

chisq.test(matrix(c(9,22,134,135), nrow=2))
chisq.test(matrix(c(9,22,134,135), nrow=2), correct=FALSE)

prop.test(c(22,9), c(157,143))
prop.test(c(22,9), c(157,143), correct=FALSE)

add_p(tbl_summary(data=bmData, by=group, include=c(group,death.6mo), 
      label=list(death.6mo~"died within 6 months")))
