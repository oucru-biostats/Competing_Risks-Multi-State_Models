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
Webmt.score <- crprep(Tstop="time", status="type", data=ebmt1,
                      trans=c("Relapse","Death"), cens="Event-free", id="patid",
                      keep=c("score","age","type","stat"), strata="score")
Webmt <- crprep(Tstop="time", status="type", data=ebmt1,
                      trans=c("Relapse","Death"), cens="Event-free", id="patid",
                      keep=c("score","age","type","stat"))


## -----------------------------------------------------------------------------------------
#| label: sdhCox_relapse_death
fitsdh.rel <- coxph(Surv(Tstart,Tstop,status==1)~score,
                                      data=Webmt, subset=failcode=="Relapse", weights=weight.cens)
fitsdh.death <- coxph(Surv(Tstart,Tstop,status==2)~score,
                                      data=Webmt, subset=failcode=="Death", weights=weight.cens)
fitsdh.rel
fitsdh.death


## -----------------------------------------------------------------------------------------
#| label: prepare_sdhpredict



## -----------------------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------------------
#| label: create_stacked
tmp1 <- cbind(ebmt1,failcode=1)
tmp2 <- cbind(ebmt1,failcode=2)
ebmt.stack <- rbind(tmp1,tmp2)


## -----------------------------------------------------------------------------------------
#| label: propcsh_stacked
coxph(Surv(Tstop,status==failcode)~strata(failcode)*score, data=Webmt, subset=count==1)


## -----------------------------------------------------------------------------------------
#| label: create_compound
Webmt$score.comb <- with(Webmt,
  factor( (as.numeric(score)-1)*(failcode=="Relapse")+
             3*(as.numeric(score)-1)*(failcode=="Death"),
  labels=c("Low","Medium.Rel","High.Rel","Medium.Death","High.Death"))
  )


## -----------------------------------------------------------------------------------------
#| label: Coxcsh_compound
coxph(Surv(Tstop,status==failcode)~strata(failcode)+ score.comb, data=Webmt, subset=count==1)


## -----------------------------------------------------------------------------------------
#| label: create_compound_dummy
Webmt$Medium.Rel <- with(Webmt,
                              ifelse(score=="Medium risk"&failcode=="Relapse", 1, 0))
Webmt$High.Rel <- with(Webmt,
                            ifelse(score=="High risk"&failcode=="Relapse", 1, 0))
Webmt$Medium.Mort <- with(Webmt,
                               ifelse(score=="Medium risk"&failcode=="Death", 1, 0))
Webmt$High.Mort <- with(Webmt,
                             ifelse(score=="High risk"&failcode=="Death", 1, 0))


## -----------------------------------------------------------------------------------------
#| label: Coxcsh_compound_dummy
fit.csh.comb <- coxph(Surv(Tstop,status==failcode)~strata(failcode)+
                          Medium.Rel+High.Rel+Medium.Mort+High.Mort, data=Webmt, subset=count==1)
fit.csh.comb


## -----------------------------------------------------------------------------------------
#| label: Coxcsh_interaction
coxph(Surv(Tstop,status==failcode)~strata(failcode)+score+Medium.Mort+High.Mort,
                                                  data=Webmt, subset=count==1)

