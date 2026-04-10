# power.prop.test(p1=..., p2=..., power=...)

power.prop.test(p1=0.2, p2=0.4, power=0.8) 
power.prop.test(p1=0.2, p2=0.4, power=0.9) 

# power.prop.test(p1=..., p2=..., n=109)

power.prop.test(p1=0.2, p2=0.3, n=109)

library(ggplot2)
p2 <- seq(0.2,0.5,by=0.01)
power.prop.test(p1=0.2, p2=p2, n=109)$power
PowerByP2 <- data.frame(p2=p2, power=power.prop.test(p1=0.2, p2=p2, n=109)$power)
ggplot(PowerByP2, aes(p2,power)) + geom_line(linewidth=1.2) +
  geom_hline(yintercept=0.025, linetype="dotted",color="red")

# power.t.test(delta=..., sd=..., n=109)

power.t.test(delta=1, sd=2.5,n=109)

## START PRACTICAL II
pctTrial  <- read.csv("Data/pctTrial.csv")

xtabs(~AByn+group, data=pctTrial)
prop.test(c(194,163), c(221,221))
with(pctTrial, chisq.test(AByn,group))
# or
chisq.test(matrix(c(194,163,27,58), nrow=2))
# we can also use the gtsummary package for a more beautiful output
library(gtsummary)
add_p(tbl_summary(pctTrial, by=group, include=c(AByn,group)))

library(ggplot2)
ggplot(pctTrial, aes(group,LOS)) + geom_violin() + 
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')
ggplot(pctTrial, aes(LOS,group=group, color=group)) + stat_ecdf()
## numerical summary
tbl_summary(pctTrial, by=group, include=c(LOS,group))
# a bit skewed, but not too much. Hence the t-test should be ok given the sample size
wilcox.test(LOS~group,data=pctTrial)
t.test(LOS~group,data=pctTrial)

ggplot(pctTrial, aes(pct)) + geom_histogram()
ggplot(pctTrial, aes(proADM)) + geom_histogram()
ggplot(pctTrial, aes(pct,proADM)) + geom_point() 

summary(pctTrial$pct) 
summary(pctTrial$proADM) # no zero values, hence we can use log values
ggplot(pctTrial, aes(1,log10(pct))) + geom_violin() +
    geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')
ggplot(pctTrial, aes(log10(pct))) + geom_histogram() 
ggplot(pctTrial, aes(1,log10(proADM))) + geom_violin() +
    geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red')
ggplot(pctTrial, aes(log10(proADM))) + geom_histogram()
ggplot(pctTrial, aes(log10(pct),log10(proADM))) + geom_point()


## we compute both Pearson and Spearman correlation (note that Spearman is insensitive to monotone transformations)
cor.test(pctTrial$pct, pctTrial$proADM)
cor.test(log10(pctTrial$pct), log10(pctTrial$proADM))
cor.test(pctTrial$pct, pctTrial$proADM, method="spearman")

N <- 109 # sample size per group
signif <- 0.05 # significance level
prob1 <- 0.2 # success probability under $H_0$
prob2 <- 0.4 # success probability in intervention group under assumed value of $H_1$

dist.binom <- data.frame(x=0:N, p1=dbinom(x=0:N, size=N ,prob=prob1))
dist.binom
## we first save the plot in an object plot.binom because we will use it again later
library(ggplot2)
plot.binom <- ggplot(dist.binom) + 
  geom_linerange(aes(x,p1,ymin=0,ymax=p1),linewidth=1)
plot.binom
## we repeat the figure, but restrict x to the range of values with probability larger than 0.00001
plot.binom + xlim(range(which(dist.binom$p1>0.00001)))

ggplot(dist.binom) +  geom_linerange(aes(x/N,p1,ymin=0,ymax=p1),linewidth=1) 

data.p1 <- rbinom(N,1,prob1)
data.p1 # individual data
sum(data.p1) # number of "successes"
sum(data.p1)/N # proportion of "successes"

M <- 100
data.p1 <- rbinom(M,N,prob1)
data.p1
## we summarize these values in a histogram and add it to the plot
hist.p1 <- data.frame(N=sort(unique(data.p1)), freq=as.numeric(table(data.p1))/M)
hist.p1
plot.binom + xlim(range(which(dist.binom$p1>0.00001))) + 
  geom_linerange(data=hist.p1, aes(N-0.2,freq,ymin=0,ymax=freq),
                 size=1,col="orange")

M <- 1000000
data.p1 <- rbinom(M,N,prob1)
hist.p1.M <- data.frame(N=sort(unique(data.p1)), freq=as.numeric(table(data.p1))/M)
plot.binom + xlim(range(which(dist.binom$p1>0.00001))) + 
  geom_linerange(data=hist.p1, aes(N-0.2,freq,ymin=0,ymax=freq),
                 size=1,col="orange") + 
  geom_linerange(data=hist.p1.M, aes(N+0.2,freq,ymin=0,ymax=freq),
                 size=1,col="red")

M <- 1000000
data.sim <- data.frame(H0.Control = rbinom(M,N,prob1), 
                       H0.Intervention = rbinom(M,N,prob1), 
                       H1.Control = rbinom(M,N,prob1), 
                       H1.Intervention = rbinom(M,N,prob2))
summary(data.sim)  

ggplot(data.sim) + geom_histogram(aes(H0.Intervention-H0.Control,after_stat(density)), 
                                  binwidth=0.2)
Q <- quantile(data.sim$H0.Intervention-data.sim$H0.Control, 
              prob=(1-signif/2))
Q
## check whether the value of Q itself should lead to rejection already:
sum(data.sim$H0.Intervention-data.sim$H0.Control>=12)/M # P(D >= 12) under null hypothesis
sum(data.sim$H0.Intervention-data.sim$H0.Control>=13)/M # P(D >= 13) under null hypothesis

sum(data.sim$H1.Intervention-data.sim$H1.Control>=13)/M # P(D >= 13) if p=0.4
## We can also create a nice visualisation
ggplot(data.sim) + geom_histogram(aes(H0.Intervention-H0.Control,y=..density..), 
                                  binwidth=0.2) +
  geom_vline(aes(xintercept=13-0.5),color="red") + 
  geom_histogram(aes(H1.Intervention-H1.Control+0.2,y=..density..), 
                 binwidth=0.2, fill="green") + 
  xlab("difference in number of tumour responses") + ylab("probability")

sim.power <- function(Nstart, Nstop, prob1=0.2, prob2=0.4, M=1000000, signif=0.05){
    power <- data.frame(N=Nstart:Nstop, CriticalValue=NA, Power=NA)
    for(Ni in 1:(Nstop+1-Nstart)){
        data.sim <- data.frame(H0.Control = rbinom(M,Nstart+Ni-1,prob1),
                               H0.Intervention = rbinom(M,Nstart+Ni-1,prob1),
                               H1.Control = rbinom(M,Nstart+Ni-1,prob1),
                               H1.Intervention = rbinom(M,Nstart+Ni-1,prob2))
        Q <- quantile(data.sim$H0.Intervention-data.sim$H0.Control, prob=(1-signif/2))
        power[Ni, "CriticalValue"] <- Q+1
        power[Ni,"Power"] <- 1-cumsum(xtabs(~I(H1.Intervention-H1.Control), data=data.sim)/M)[as.character(Q)]
    }
    power
}
sim.power(Nstart=109,Nstop=109)
power.prop.test(p1=0.2, p2=0.4, n=109)

BinPower <- sim.power(50,200,M=100000)
ggplot(BinPower, aes(N,Power)) + geom_point(size=1)

ggplot(BinPower, aes(N,Power)) + geom_point(size=2) + xlim(90,115) + ylim(0.85,0.95)
