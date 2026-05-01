## -----------------------------------------------------------------------------------------
#| label: prepare_data_III
library(mstate)
library(ggsurvfit)
library(survminer)
data(ebmt1)
ebmt1 <- within(ebmt1,{
  time <- pmin(srv,rel)/365.25
  stat <-  ifelse(rel<srv,relstat,srvstat*2) #relapse=1, death=2
  type <- factor(stat, 
               labels=c("Event-free","Relapse","Death"))
})


## -----------------------------------------------------------------------------------------
#| label: compute_weights_crprep
Webmt <- crprep(Tstop="time", status="stat", data=ebmt1, trans=1,
                cens=0, id="patid", keep=c("score","age","type"))
head(Webmt,10)
nrow(subset(Webmt))


## -----------------------------------------------------------------------------------------
#| label: compute_weights_finegray
Webmt.Relapse <- finegray(Surv(time,type)~., data=ebmt1, etype="Relapse")
head(Webmt.Relapse, 10)
nrow(Webmt.Relapse)


## -----------------------------------------------------------------------------------------
#| label: plot_AJandPL_relapse
cuminc <- survfit(Surv(time,type)~1, ebmt1) # AJ estimate
cuminc.crprep.rel <- survfit(Surv(Tstart,Tstop,status==1)~1, data=Webmt,
                             weight=weight.cens)
cuminc.finegray.rel <- survfit(Surv(fgstart,fgstop,fgstatus)~1, data=Webmt.Relapse,
                               weight=fgwt)
plot(cuminc[2],lwd=2)
lines(cuminc.crprep.rel, lwd=2, col="red", fun="event")
lines(cuminc.finegray.rel, col="red", fun="event")


## -----------------------------------------------------------------------------------------
#| label: crprep_score
Webmt.score <- crprep(Tstop="time", status="type", data=ebmt1,
                      trans=c("Relapse","Death"), cens="Event-free", id="patid",
                      keep=c("score","age","type","stat"), strata="score")
Webmt <- crprep(Tstop="time", status="type", data=ebmt1,
                      trans=c("Relapse","Death"), cens="Event-free", id="patid",
                      keep=c("score","age","type","stat"))


## -----------------------------------------------------------------------------------------
#| label: finegray_score
Webmt.Relapse.score <- finegray(Surv(time,type)~.+strata(score), data=ebmt1,
                                etype="Relapse")
Webmt.Death.score <- finegray(Surv(time,type)~.+strata(score), data=ebmt1,
                              etype="Death")
Webmt.Death <- finegray(Surv(time,type)~., data=ebmt1, etype="Death")


## -----------------------------------------------------------------------------------------
#| label: cuminc_score
############
cuminc.relapse.crprep <- survfit(Surv(Tstart,Tstop,status=="Relapse")~score,
  data=subset(Webmt.score,failcode=="Relapse"), weights=weight.cens)
cuminc.death.crprep <- survfit(Surv(Tstart,Tstop,status=="Death")~score,
               data=subset(Webmt.score,failcode=="Death"), weights=weight.cens)
############
cuminc.relapse.fg <- survfit(Surv(fgstart,fgstop,fgstatus)~score,
                             data=Webmt.Relapse.score, weight=fgwt)
cuminc.death.fg <- survfit(Surv(fgstart,fgstop,fgstatus)~score,
                           data=Webmt.Death.score, weight=fgwt)
par(mfrow=c(1,2))
plot(cuminc.relapse.fg, lwd=3,
  col=c("black","red","green"), fun="event", ylim=c(0,0.5))
lines(survfit(Surv(fgstart,fgstop,fgstatus)~score, data=Webmt.Relapse,
              weights=fgwt), lwd=1, col=c("black","red","green"), fun="event")
title("Relapse")
plot(cuminc.death.fg, lwd=3,
  col=c("black","red","green"), fun="event", ylim=c(0,0.5))
lines(survfit(Surv(fgstart,fgstop,fgstatus)~score, data=Webmt.Death,
              weights=fgwt), lwd=1, col=c("black","red","green"), fun="event")
title("Death")
legend("bottomright", levels(ebmt1$score), col=c("black","red","green"), lwd=3)


## -----------------------------------------------------------------------------------------
#| label: timecens_score
ggsurvfit(survfit2(Surv(time,stat==0)~score, data=ebmt1))


## -----------------------------------------------------------------------------------------
#| label: log-ranks
coxph(Surv(Tstart,Tstop,status=="Relapse")~score, data=
 subset(Webmt.score,failcode=="Relapse"), weights=weight.cens)$score
coxph(Surv(Tstart,Tstop,status=="Relapse")~score,
        data=subset(Webmt.score,failcode=="Relapse"&count==1))$score
survdiff(Surv(time,stat==1)~score,data=ebmt1)


## -----------------------------------------------------------------------------------------
#| label: sdhCox_relapse_death
fitsdh.rel <- coxph(Surv(Tstart,Tstop,status=="Relapse")~score,
                       data=Webmt, subset=failcode=="Relapse", weights=weight.cens)
fitsdh.death <- coxph(Surv(Tstart,Tstop,status=="Death")~score,
                       data=Webmt, subset=failcode=="Death", weights=weight.cens)
fitsdh.rel
fitsdh.death

