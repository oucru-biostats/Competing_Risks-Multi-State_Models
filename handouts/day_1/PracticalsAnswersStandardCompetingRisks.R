## -----------------------------------------------------------------------------------------
#| label: load_packages_data_II
library(mstate)
library(ggsurvfit)
library(survminer)
data(ebmt1)


## -----------------------------------------------------------------------------------------
#| label: preparation
ebmt1 <- within(ebmt1,{
  time <- pmin(srv,rel)/365.25
  stat <-  ifelse(rel<srv,relstat,srvstat*2) #relapse=1, death=2
  type <- factor(stat, 
               labels=c("Event-free","Relapse","Death"))
})
head(ebmt1)


## -----------------------------------------------------------------------------------------
#| label: compute_Aalen-Johansen
cuminc <- survfit(Surv(time,type)~1, ebmt1)
summ <- summary(cuminc, times=c(1,5))
summ
summ$lower
summ$upper


## -----------------------------------------------------------------------------------------
#| label: plot_Aalen-Johansen_overlaid
par(las=1) # labels along y-axis rotated in rest of R session
plot(cuminc, conf.int=TRUE, col = c(1,2))
legend("bottomright", legend = c("Relapse", "Death"), lwd = 1, col = c(1,2))


## -----------------------------------------------------------------------------------------
#| label: ggsurvfit_Aalen-Johansen_overlaid
ggcuminc(cuminc, outcome=c("Relapse","Death")) + 
    add_confidence_interval() + theme(legend.position = "top")


## -----------------------------------------------------------------------------------------
#| label: survminer_Aalen-Johansen_overlaid
library(cmprsk)
cuminc2 <- with(ebmt1, cuminc(time,stat))
ggcompetingrisks(cuminc2, conf.int=TRUE, multiple_panels=FALSE, ggtheme = theme_light())


## -----------------------------------------------------------------------------------------
#| label: plot_AJ_Stacked
KM.overall <- survfit(Surv(srv/365.25,srvstat)~1, data=ebmt1)
plot(KM.overall, lwd=2, lty=1, xaxs="i", conf.int=FALSE,
     fun="event", xlab="time since transplant")
lines(cuminc[2],  lwd=2, lty=2, conf.int=FALSE)
  text(c(6,6),c(0.1,0.4),c("relapse","death"))


## -----------------------------------------------------------------------------------------
#| label: survminer_AJ_Stacked
ggcompetingrisks(cuminc,  ggtheme = theme_light())


## -----------------------------------------------------------------------------------------
#| label: plot_AJ_alternate
plot(cuminc[3], fun=\(x) 1-x, xlab="time since transplant", ylim=c(0,1))
lines(cuminc[2])
text(c(6,6),c(0.1,0.9),c("relapse","death"))


## -----------------------------------------------------------------------------------------
#| label: Cox_relapse_death
fit.rel <- coxph(Surv(time,stat==1)~score, data=ebmt1)
fit.death <- coxph(Surv(time,stat==2)~score, data=ebmt1)
fit.rel
fit.death

