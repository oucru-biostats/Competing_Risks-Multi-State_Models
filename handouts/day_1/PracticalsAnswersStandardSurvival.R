## -----------------------------------------------------------------------------------------
#| label: load_packages_data
library(mstate)
library(ggsurvfit)
library(survminer)
data(ebmt1)


## -----------------------------------------------------------------------------------------
#| label: data_helpfile
?ebmt1


## -----------------------------------------------------------------------------------------
#| label: data_summary
summary(ebmt1)


## -----------------------------------------------------------------------------------------
#| label: number_events
table(ebmt1$srvstat)
table(ebmt1$relstat)


## -----------------------------------------------------------------------------------------
#| label: compute_Kaplan-Meier_basicsummary
KM.overall <- survfit(Surv(srv,srvstat)~1, data=ebmt1)
KM.overall # gives median value


## -----------------------------------------------------------------------------------------
#| label: summary_Kaplan-Meier
summary(KM.overall, times=c(1,5)*365.25)


## -----------------------------------------------------------------------------------------
#| label: plot_Kaplan-Meier
plot(KM.overall, xlab="Time since transplant (yrs)", fun="event", xscale=365.25, las=1)


## -----------------------------------------------------------------------------------------
#| label: plot_Kaplan-Meier_survfit
ggsurvfit(KM.overall, type="risk") 


## -----------------------------------------------------------------------------------------
#| label: plot_Kaplan-Meier_survfit_adapt
ggsurvfit(KM.overall, type="risk") + 
  add_confidence_interval() + 
  add_risktable() + 
  scale_x_continuous("Time since transplant (yrs)", labels=1:8, breaks=365.25*(1:8))  


## -----------------------------------------------------------------------------------------
#| label: plot_Kaplan-Meier_survminer
ggsurvplot(KM.overall, fun="event")


## -----------------------------------------------------------------------------------------
#| label: plot_Kaplan-Meier_survminer_adapt
ggsurvplot(KM.overall, fun="event", palette="black", risk.table=TRUE, censor=FALSE,
           xscale=365.25, break.x.by=365.25, xlab="Time since transplant (yrs)",
           axes.offset=FALSE, legend="none", ggtheme=theme_light()) 


## -----------------------------------------------------------------------------------------
#| label: plotKM_score
plot(survfit(Surv(srv,srvstat) ~ score, data = ebmt1),
     xlab="Time since transplant (yrs)", fun="event", xscale=365.25, las=1)


## -----------------------------------------------------------------------------------------
#| label: plotKM_score_ggsurvfit
ggsurvfit(survfit2(Surv(srv,srvstat) ~ score, data = ebmt1), type="risk") + 
    add_confidence_interval() +
    scale_x_continuous("Time since transplant (yrs)", labels=1:8, breaks=365.25*(1:8)) 


## -----------------------------------------------------------------------------------------
#| label: plotKM_score_survminer
ggsurvplot(survfit(Surv(srv,srvstat) ~ score, data = ebmt1), fun="event",
           censor=FALSE, xscale=365.25, break.x.by=365.25,
           conf.int=TRUE, xlab="Time since transplant (yrs)",
           axes.offset=FALSE, ggtheme=theme_light()) 


## -----------------------------------------------------------------------------------------
#| label: Cox_relsurv
PH.relsurv <- coxph(Surv(srv,srvstat) ~ score, data = ebmt1)
PH.relsurv
summary(PH.relsurv)

