## -----------------------------------------------------------------------------------------------
#| label: load_packages_data
library(mstate)
library(ggsurvfit)
data(ebmt1)


## -----------------------------------------------------------------------------------------------
#| label: data_helpfile
?ebmt1


## -----------------------------------------------------------------------------------------------
#| label: data_summary
summary(ebmt1)


## -----------------------------------------------------------------------------------------------
#| label: number_events
table(ebmt1$srvstat)
table(ebmt1$relstat)


## -----------------------------------------------------------------------------------------------
#| label: compute_Kaplan-Meier
KM.overall <- survfit(Surv(srv,srvstat)~1, data=ebmt1)
KM.overall # gives median value


## -----------------------------------------------------------------------------------------------
#| label: summary_Kaplan-Meier
summary(KM.overall, times=c(1,5)*365.25)


## -----------------------------------------------------------------------------------------------
#| label: plot_Kaplan-Meier
plot(KM.overall, xlab="Time since transplant (yrs)", fun="event", xscale=365.25, las=1)


## -----------------------------------------------------------------------------------------------
ggsurvfit(KM.overall, type="risk") + 
  add_confidence_interval() + 
  add_risktable() + 
  scale_x_continuous("Time since transplant (yrs)", labels=1:8, breaks=365.25*(1:8))  


## -----------------------------------------------------------------------------------------------
#| label: preparation
ebmt1 <- within(ebmt1,{
  time <- pmin(srv,rel)/365.25
  stat <-  ifelse(rel<srv,relstat,srvstat*2) #relapse=1, death=2
  type <- factor(stat, 
               labels=c("Event-free","Relapse","Death"))
})
head(ebmt1)


## -----------------------------------------------------------------------------------------------
#| label: compute_Aalen-Johansen
cuminc <- survfit(Surv(time,type)~1, ebmt1)
summ <- summary(cuminc, times=c(1,5))
summ
summ$lower
summ$upper


## -----------------------------------------------------------------------------------------------
#| label: plot_Aalen-Johansen
par(las=1) # labels along y-axis rotated in rest of R session
plot(cuminc, conf.int=TRUE, col = c(1,2))
legend("bottomright", legend = c("Relapse", "Death"), lwd = 1, col = c(1,2))


## -----------------------------------------------------------------------------------------------
#| label: ggplot_Aalen-Johansen
ggcuminc(cuminc, outcome=c("Relapse","Death")) + 
    add_confidence_interval() + theme(legend.position = "top")


## -----------------------------------------------------------------------------------------------
#| label: plot_AJ_Stacked
KM.overall <- survfit(Surv(srv/365.25,srvstat)~1, data=ebmt1)
plot(KM.overall, lwd=2, lty=1, xaxs="i", conf.int=FALSE,
     fun="event", xlab="time since transplant")
lines(cuminc[2],  lwd=2, lty=2, conf.int=FALSE)
  text(c(6,6),c(0.1,0.4),c("relapse","death"))


## -----------------------------------------------------------------------------------------------
#| label: plot_AJ_alternate
plot(cuminc[3], fun=\(x) 1-x, xlab="time since transplant", ylim=c(0,1))
lines(cuminc[2])
text(c(6,6),c(0.1,0.9),c("relapse","death"))


## -----------------------------------------------------------------------------------------------
#| label: Cox_relsurv
PH.relsurv <- coxph(Surv(time, stat>0) ~ score, data = ebmt1)
PH.relsurv
summary(PH.relsurv)


## -----------------------------------------------------------------------------------------------
#| label: Cox_relapse_death
fit.rel <- coxph(Surv(time,stat==1)~score, data=ebmt1)
fit.death <- coxph(Surv(time,stat==2)~score, data=ebmt1)
fit.rel
fit.death


## -----------------------------------------------------------------------------------------------
#| label: weights_crprep
Webmt <- crprep(Tstop="time", status="stat", data=ebmt1, trans=c(1,2),
                cens=0, id="patid", keep=c("score","age","type"))
head(Webmt,10)
nrow(subset(Webmt,failcode==1))
nrow(subset(Webmt,failcode==2))


## -----------------------------------------------------------------------------------------------
#| label: weights_finegray_relapse
Webmt.Relapse <- finegray(Surv(time,type)~., data=ebmt1, etype="Relapse")
Webmt.Death <- finegray(Surv(time,type)~., data=ebmt1, etype="Death")
head(Webmt.Relapse, 10)
nrow(Webmt.Relapse)
nrow(Webmt.Death)


## -----------------------------------------------------------------------------------------------
#| label: compare_AJPL
cuminc.crprep <- survfit(Surv(Tstart,Tstop,status==1)~1, data=Webmt, subset=failcode==1, weight=weight.cens)
cuminc.finegray <- survfit(Surv(fgstart,fgstop,fgstatus)~1, data=Webmt.Relapse, weight=fgwt)
plot(cuminc[2],lwd=2)
lines(cuminc.crprep, lwd=2, col="red", fun="event")
lines(cuminc.finegray, col="red", fun="event")


## -----------------------------------------------------------------------------------------------
#| label: check_cuminc
summary(cuminc[2],times=c(1,5))$pstate
1-summary(cuminc.crprep,times=c(1,5))$surv
1-summary(cuminc.finegray,times=c(1,5))$surv


## -----------------------------------------------------------------------------------------------
#| label: crprep_score
Webmt.score <- crprep(Tstop="time", status="type", data=ebmt1,
                      trans=c("Relapse","Death"), cens="Event-free", id="patid", strata="score")
## Webmt.score <- crprep(Tstop="time", status="stat", data=ebmt1,
##                       trans=c(1,2), cens=0, id="patid", strata="score")


## -----------------------------------------------------------------------------------------------
#| label: finegray_score
Webmt.Relapse.score <- finegray(Surv(time,type)~.+strata(score), data=ebmt1, etype="Relapse")
Webmt.Death.score <- finegray(Surv(time,type)~.+strata(score), data=ebmt1, etype="Death")


## -----------------------------------------------------------------------------------------------
#| label: cuminc_score
## cuminc.relapse <- survfit(Surv(Tstart,Tstop,status=="Relapse")~score,
##   data=subset(Webmt.score,failcode=="Relapse"), weights=weight.cens)
## cuminc.death <- survfit(Surv(Tstart,Tstop,status=="Death")~score,
##                         data=subset(Webmt.score,failcode=="Death"), weights=weight.cens)
cuminc.relapse <- survfit(Surv(fgstart,fgstop,fgstatus)~score, data=Webmt.Relapse.score, weight=fgwt)
cuminc.death <- survfit(Surv(fgstart,fgstop,fgstatus)~score, data=Webmt.Death.score, weight=fgwt)
par(mfrow=c(1,2))
plot(cuminc.relapse, lwd=3,
  col=c("black","red","green"), fun="event", ylim=c(0,0.5))
lines(survfit(Surv(fgstart,fgstop,fgstatus)~score, data=Webmt.Relapse,
              weights=fgwt), lwd=1, col=c("black","red","green"), fun="event")
title("Relapse")
plot(cuminc.death, lwd=3,
  col=c("black","red","green"), fun="event", ylim=c(0,0.5))
lines(survfit(Surv(fgstart,fgstop,fgstatus)~score, data=Webmt.Death,
              weights=fgwt), lwd=1, col=c("black","red","green"), fun="event")
title("Death")
legend("bottomright", levels(Webmt$score), col=c("black","red","green"), lwd=3)


## -----------------------------------------------------------------------------------------------
#| label: cens_score
ggsurvfit(survfit2(Surv(time,stat==0)~score, data=ebmt1))


## -----------------------------------------------------------------------------------------------
#| label: log-ranks
coxph(Surv(Tstart,Tstop,status=="Relapse")~score, data=
 subset(Webmt.score,failcode=="Relapse"), weights=weight.cens)$score
coxph(Surv(Tstart,Tstop,status=="Relapse")~score,
        data=subset(Webmt.score,failcode=="Relapse"&count==1))$score
survdiff(Surv(time,stat==1)~score,data=ebmt1)


## -----------------------------------------------------------------------------------------------
#| label: sdhCox_relapse_death
fitsdh.rel <- coxph(Surv(Tstart,Tstop,status==1)~score,
                                      data=Webmt, subset=failcode==1, weights=weight.cens)
fitsdh.death <- coxph(Surv(Tstart,Tstop,status==2)~score,
                                      data=Webmt, subset=failcode==2, weights=weight.cens)
fitsdh.rel
fitsdh.death


## -----------------------------------------------------------------------------------------------
#| label: prepare_sdhpredict



## -----------------------------------------------------------------------------------------------
#| label: predsdh
indivs <- data.frame(score=levels(ebmt1$score))
par(mfrow=c(1,2),las=1)
pred.sdh <- survfit(fitsdh.rel, newdata=indivs)
plot(survfit(Surv(Tstart,Tstop,status=="Relapse")~score,
             data=subset(Webmt.score,failcode=="Relapse"), weights=weight.cens), lwd=1,
     col=c("black","red","green"), fun="event", ylim=c(0,0.5))
lines(pred.sdh,fun="event", col=c("black","red","green"), lwd=3)
title("Relapse")
pred.sdh <- survfit(fitsdh.death, newdata=indivs)
plot(survfit(Surv(Tstart,Tstop,status=="Death")~score,
             data=subset(Webmt.score,failcode=="Death"), weights=weight.cens), lwd=1,
             col=c("black","red","green"), fun="event", ylim=c(0,0.55))
lines(pred.sdh,fun="event", col=c("black","red","green"), lwd=3)
title("Death")
legend("bottomright", levels(ebmt1$score), col=c("black","red","green"), lwd=3 )


## -----------------------------------------------------------------------------------------------
#| label: create_stacked
tmp1 <- cbind(ebmt1,failcode=1)
tmp2 <- cbind(ebmt1,failcode=2)
ebmt.stack <- rbind(tmp1,tmp2)


## -----------------------------------------------------------------------------------------------
#| label: propcsh_stacked
coxph(Surv(Tstop,status==failcode)~strata(failcode)*score, data=Webmt, subset=count==1)


## -----------------------------------------------------------------------------------------------
#| label: create_compound
Webmt$score.comb <- with(Webmt,
  factor( (as.numeric(score)-1)*(failcode==1)+
             3*(as.numeric(score)-1)*(failcode==2),
  labels=c("Low","Medium.Rel","High.Rel","Medium.Death","High.Death"))
  )


## -----------------------------------------------------------------------------------------------
#| label: Coxcsh_compound
coxph(Surv(Tstop,status==failcode)~strata(failcode)+ score.comb, data=Webmt, subset=count==1)


## -----------------------------------------------------------------------------------------------
#| label: create_compound_dummy
Webmt$Medium.Rel <- with(Webmt,
                              ifelse(score=="Medium risk"&failcode==1, 1, 0))
Webmt$High.Rel <- with(Webmt,
                            ifelse(score=="High risk"&failcode==1, 1, 0))
Webmt$Medium.Mort <- with(Webmt,
                               ifelse(score=="Medium risk"&failcode==2, 1, 0))
Webmt$High.Mort <- with(Webmt,
                             ifelse(score=="High risk"&failcode==2, 1, 0))


## -----------------------------------------------------------------------------------------------
#| label: Coxcsh_compound_dummy
fit.csh.comb <- coxph(Surv(Tstop,status==failcode)~strata(failcode)+
                          Medium.Rel+High.Rel+Medium.Mort+High.Mort, data=Webmt, subset=count==1)
fit.csh.comb


## -----------------------------------------------------------------------------------------------
#| label: Coxcsh_interaction
coxph(Surv(Tstop,status==failcode)~strata(failcode)+score+Medium.Mort+High.Mort,
                                                  data=Webmt, subset=count==1)

