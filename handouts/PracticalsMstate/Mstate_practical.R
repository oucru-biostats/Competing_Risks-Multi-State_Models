### R code from vignette source 'Mstate_practical_plus.Rnw'

###################################################
### code chunk number 1: Mstate_practical_plus.Rnw:5-6 (eval = FALSE)
###################################################
## options(width=70)


###################################################
### code chunk number 2: readtable (eval = FALSE)
###################################################
## library(mstate)
## data(ebmt1)


###################################################
### code chunk number 3: head (eval = FALSE)
###################################################
## head(ebmt1)
## tail(ebmt1)


###################################################
### code chunk number 4: dim (eval = FALSE)
###################################################
## dim(ebmt1)


###################################################
### code chunk number 5: eventtables (eval = FALSE)
###################################################
## table(ebmt1$srvstat)
## table(ebmt1$relstat)
## table(ebmt1$srvstat, ebmt1$relstat)


###################################################
### code chunk number 6: survival (eval = FALSE)
###################################################
## library(survival)
## sf0 <- survfit(Surv(srv, srvstat) ~ 1, data=ebmt1)


###################################################
### code chunk number 7: printc0 (eval = FALSE)
###################################################
## sf0


###################################################
### code chunk number 8: s0 (eval = FALSE)
###################################################
## year <- 365.25
## summary(sf0, times=year*(0:7))


###################################################
### code chunk number 9: figKM0plot (eval = FALSE)
###################################################
## plot(sf0, xscale=year)


###################################################
### code chunk number 10: figKM0 (eval = FALSE)
###################################################
## plot(sf0, xscale=year)


###################################################
### code chunk number 11: crs1 (eval = FALSE)
###################################################
## ebmt1$rfs <- pmin(ebmt1$rel, ebmt1$srv)
## ebmt1$rfsstat <- 1 - (1-ebmt1$relstat) * (1-ebmt1$srvstat)
## ebmt1$rfscr <- ebmt1$relstat
## ebmt1$rfscr[ebmt1$relstat==0 & ebmt1$srvstat==1] <- 2
## ebmt1$rfscr <- factor(ebmt1$rfscr, levels=0:2,
##                       labels=c("Censored", "Relapse", "NRM"))


###################################################
### code chunk number 12: crs2 (eval = FALSE)
###################################################
## table(ebmt1$rfsstat)
## table(ebmt1$rfscr)
## table(ebmt1$srvstat, ebmt1$relstat)


###################################################
### code chunk number 13: crs3 (eval = FALSE)
###################################################
## sfcr <- survfit(Surv(rfs, rfscr) ~ 1, data=ebmt1)
## sfcr
## summary(sfcr, times=year*(0:7))


###################################################
### code chunk number 14: figcrs0plot (eval = FALSE)
###################################################
## plot(sfcr, lwd=2, col=1:2, xscale=year)
## legend("bottomright", c("Relapse", "NRM"), lwd=2, col=1:2, bty="n")


###################################################
### code chunk number 15: figcrs0 (eval = FALSE)
###################################################
## plot(sfcr, lwd=2, col=1:2, xscale=year)
## legend("bottomright", c("Relapse", "NRM"), lwd=2, col=1:2, bty="n")


###################################################
### code chunk number 16: tmat (eval = FALSE)
###################################################
## tmat <- matrix(NA,3,3)
## tmat[1,2:3] <- 1:2
## tmat[2,3] <- 3
## dimnames(tmat) <- list(c("T","R","D"), c("T","R","D"))
## tmat


###################################################
### code chunk number 17: tmat (eval = FALSE)
###################################################
## trans.illdeath(names=c("T","R","D"))


###################################################
### code chunk number 18: msprep (eval = FALSE)
###################################################
## covs <- c("score","yrel")
## msebmt <- msprep(
##     time=c(NA,"rel","srv"),
##     status=c(NA,"relstat","srvstat"),
##     data=ebmt1, trans=tmat, id="patid",
##     keep=covs)


###################################################
### code chunk number 19: msebmt (eval = FALSE)
###################################################
## head(msebmt)


###################################################
### code chunk number 20: events (eval = FALSE)
###################################################
## events(msebmt)


###################################################
### code chunk number 21: c0 (eval = FALSE)
###################################################
## c0 <- coxph(Surv(Tstart,Tstop,status) ~ strata(trans),
##     data=msebmt)
## c0


###################################################
### code chunk number 22: figms0plot (eval = FALSE)
###################################################
## plot(survfit(c0), fun="cumhaz", col=1:3, xscale=year)


###################################################
### code chunk number 23: msf0 (eval = FALSE)
###################################################
## msf0 <- msfit(c0, trans=tmat)


###################################################
### code chunk number 24: figmsf0plot (eval = FALSE)
###################################################
## plot(msf0)


###################################################
### code chunk number 25: figmsf0 (eval = FALSE)
###################################################
## plot(msf0)


###################################################
### code chunk number 26: c1 (eval = FALSE)
###################################################
## msebmt$rel.srv <- 0
## msebmt$rel.srv[msebmt$trans==3] <- 1
## c1 <- coxph(Surv(Tstart,Tstop,status) ~ rel.srv +
##     strata(to), data=msebmt)
## c1


###################################################
### code chunk number 27: pt0 (eval = FALSE)
###################################################
## pt0 <- probtrans(msf0, predt=0)


###################################################
### code chunk number 28: trysummary0 (eval = FALSE)
###################################################
## summary(pt0)


###################################################
### code chunk number 29: pt01 (eval = FALSE)
###################################################
## pt01 <- pt0[[1]]
## head(pt01)


###################################################
### code chunk number 30: figsingle0plot (eval = FALSE)
###################################################
## plot(pt0, type="single")


###################################################
### code chunk number 31: figsingle0 (eval = FALSE)
###################################################
## plot(pt0, type="single")


###################################################
### code chunk number 32: figstacked0plot (eval = FALSE)
###################################################
## plot(pt0, ord=c(2,3,1))


###################################################
### code chunk number 33: figstacked0 (eval = FALSE)
###################################################
## plot(pt0, ord=c(2,3,1))


###################################################
### code chunk number 34: figfilled0plot (eval = FALSE)
###################################################
## plot(pt0, ord=c(2,3,1), type="filled")


###################################################
### code chunk number 35: figfilled0 (eval = FALSE)
###################################################
## plot(pt0, ord=c(2,3,1), type="filled")


###################################################
### code chunk number 36: msm4 (eval = FALSE)
###################################################
## tmat4 <- transMat(x = list(c(2, 3), c(4), c(), c()),
##                  names = c("Tx", "Rel", "NRM", "Death after Rel"))
## tmat4
## msebmt4 <- msprep(
##     time = c(NA, "rel", "srv"),
##     status = c(NA, "relstat", "srvstat"),
##     data=ebmt1, trans=tmat, id="patid",
##     keep=covs)
## events(msebmt4)
## c0 <- coxph(Surv(Tstart,Tstop,status) ~ strata(trans),
##     data=msebmt4)
## c0
## msf0 <- msfit(c0, trans=tmat4)
## # plot(msf0) # Try this yourself
## pt0 <- probtrans(msf0, predt=0)


###################################################
### code chunk number 37: figfilled4plot (eval = FALSE)
###################################################
## plot(pt0, ord=c(2, 4, 3, 1), type="filled")


###################################################
### code chunk number 38: figfilled4 (eval = FALSE)
###################################################
## plot(pt0, ord=c(2, 4, 3, 1), type="filled")


###################################################
### code chunk number 39: yreltable (eval = FALSE)
###################################################
## table(ebmt1$yrel,exclude=NULL)


###################################################
### code chunk number 40: scoretable (eval = FALSE)
###################################################
## table(ebmt1$score)


###################################################
### code chunk number 41: figKM3plot (eval = FALSE)
###################################################
## sf3 <- survfit(Surv(srv, srvstat) ~ score, data=ebmt1)
## plot(sf3, xscale=year, col=1:3)


###################################################
### code chunk number 42: figKM3 (eval = FALSE)
###################################################
## sf3 <- survfit(Surv(srv, srvstat) ~ score, data=ebmt1)
## plot(sf3, xscale=year, col=1:3)


###################################################
### code chunk number 43: OrdinarySurvivalAnalysis2 (eval = FALSE)
###################################################
## c2 <- coxph(Surv(srv, srvstat) ~ score, data = ebmt1)
## c2


###################################################
### code chunk number 44: expandcovs (eval = FALSE)
###################################################
## msebmtsav <- msebmt
## msebmt <- expand.covs(msebmt, covs)


###################################################
### code chunk number 45: headexpandcovs (eval = FALSE)
###################################################
## head(msebmt)


###################################################
### code chunk number 46: expandcovs2 (eval = FALSE)
###################################################
## msebmt <- msebmtsav
## msebmt <- expand.covs(msebmt, covs, longnames=FALSE)
## head(msebmt)


###################################################
### code chunk number 47: fixyrel (eval = FALSE)
###################################################
## msebmt$yrel1.3[is.na(msebmt$yrel1.3)] <- 0
## msebmt$yrel2.3[is.na(msebmt$yrel2.3)] <- 0
## head(msebmt)


###################################################
### code chunk number 48: cst (eval = FALSE)
###################################################
## c2 <- coxph(Surv(Tstart,Tstop,status) ~ score + strata(trans),
##     data=msebmt)
## c2


###################################################
### code chunk number 49: cs123t (eval = FALSE)
###################################################
## c3 <- coxph(Surv(Tstart,Tstop,status) ~ score1.1 + score2.1 +
##     score1.2 + score2.2 + score1.3 + score2.3 + strata(trans),
##     data=msebmt)
## c3


###################################################
### code chunk number 50: separatecox (eval = FALSE)
###################################################
## coxph(Surv(Tstart,Tstop,status) ~ score, data=msebmt,
##     subset=(trans==1))


###################################################
### code chunk number 51: anova (eval = FALSE)
###################################################
## anova(c2,c3)


###################################################
### code chunk number 52: c4 (eval = FALSE)
###################################################
## msebmt$rel.srv <- 0
## msebmt$rel.srv[msebmt$trans==3] <- 1
## c4 <- coxph(Surv(Tstart,Tstop,status) ~ score + rel.srv +
##     strata(to), data=msebmt)
## c4


###################################################
### code chunk number 53: c5 (eval = FALSE)
###################################################
## c5 <- coxph(Surv(Tstart,Tstop,status) ~ score + yrel1.3 + yrel2.3 +
##     rel.srv + strata(to), data=msebmt)
## c5


###################################################
### code chunk number 54: ndata (eval = FALSE)
###################################################
## ndata <- data.frame(trans=1:3, from=c(1,1,2),
##     to=c(2,3,3), score=1)
## ndata$score <- factor(ndata$score, levels=1:3,
##     labels=levels(msebmt$score))
## ndata$strata <- c(1,2,2)
## ndata$yrel2.3 <- ndata$yrel1.3 <- 0
## ndata$rel.srv <- 0
## ndata$rel.srv[ndata$trans==3] <- 1
## ndata


###################################################
### code chunk number 55: msfit (eval = FALSE)
###################################################
## HvH <- msfit(c5,newdata=ndata,trans=tmat)


###################################################
### code chunk number 56: trysummary (eval = FALSE)
###################################################
## summary(HvH)


###################################################
### code chunk number 57: figcumbaselinetransplot (eval = FALSE)
###################################################
## plot(HvH)


###################################################
### code chunk number 58: figcumbaselinetrans (eval = FALSE)
###################################################
## plot(HvH)


###################################################
### code chunk number 59: H01 (eval = FALSE)
###################################################
## H0 <- HvH$Haz[HvH$Haz$trans==2,]
## H1 <- HvH$Haz[HvH$Haz$trans==3,]


###################################################
### code chunk number 60: H01ratio (eval = FALSE)
###################################################
## head(H1$Haz/H0$Haz)


###################################################
### code chunk number 61: H23 (eval = FALSE)
###################################################
## H2 <- H3 <- H1
## H2$Haz <- H2$Haz*exp(c5$coef[3])
## H3$Haz <- H3$Haz*exp(c5$coef[4])


###################################################
### code chunk number 62: figcbh2plot (eval = FALSE)
###################################################
## plot(H1$time/365.25,H1$Haz,type="s",ylim=c(0,max(H1$Haz)),
##     xlab="Years since transplant",ylab="Cumulative hazard",
##     lwd=2,col="red2")
## lines(H2$time/365.25,H2$Haz,type="s",lwd=2,col="orangered")
## lines(H3$time/365.25,H3$Haz,type="s",lwd=2,col="orange")
## lines(H0$time/365.25,H0$Haz,type="s",lwd=2,col=3)
## legend("topleft",
##     c("Relapse in 1993-1996","Relapse in 1997-1999",
##         "Relapse in 2000 or later","No relapse"),
##     lwd=2,col=c("red2","orangered","orange",3),bty="n")


###################################################
### code chunk number 63: figcbh2 (eval = FALSE)
###################################################
## plot(H1$time/365.25,H1$Haz,type="s",ylim=c(0,max(H1$Haz)),
##     xlab="Years since transplant",ylab="Cumulative hazard",
##     lwd=2,col="red2")
## lines(H2$time/365.25,H2$Haz,type="s",lwd=2,col="orangered")
## lines(H3$time/365.25,H3$Haz,type="s",lwd=2,col="orange")
## lines(H0$time/365.25,H0$Haz,type="s",lwd=2,col=3)
## legend("topleft",
##     c("Relapse in 1993-1996","Relapse in 1997-1999",
##         "Relapse in 2000 or later","No relapse"),
##     lwd=2,col=c("red2","orangered","orange",3),bty="n")


###################################################
### code chunk number 64: pt1 (eval = FALSE)
###################################################
## HvH1 <- HvH
## pt1 <- probtrans(HvH1,predt=0,direction="forward")


###################################################
### code chunk number 65: figfilledplot (eval = FALSE)
###################################################
## plot(pt1, ord=c(2,3,1), type="filled")


###################################################
### code chunk number 66: figfilled (eval = FALSE)
###################################################
## plot(pt1, ord=c(2,3,1), type="filled")


###################################################
### code chunk number 67: yrel12 (eval = FALSE)
###################################################
## ndata.copy <- ndata # make a copy for later
## # first dummy variable = 1 for transition 3
## ndata$yrel1.3[ndata$trans==3] <- 1
## ndata
## HvH2 <- msfit(c5,newdata=ndata,trans=tmat)
## ndata <- ndata.copy # use the copy
## # second dummy variable = 1 for transition 3
## ndata$yrel2.3[ndata$trans==3] <- 1
## ndata
## HvH3 <- msfit(c5,newdata=ndata,trans=tmat)
## pt11 <- pt1[[1]]
## pt2 <- probtrans(HvH2,predt=0,direction="forward")
## pt21 <- pt2[[1]]
## pt3 <- probtrans(HvH3,predt=0,direction="forward")
## pt31 <- pt3[[1]]


###################################################
### code chunk number 68: figpt123plot (eval = FALSE)
###################################################
## plot(pt11$time/365.25,pt11$pstate3,type="s",ylim=c(0,1),
##     xlab="Years since transplant",ylab="Probability of death",
##     lwd=2,col="red2")
## lines(pt21$time/365.25,pt21$pstate3,type="s",lwd=2,col="orangered")
## lines(pt31$time/365.25,pt31$pstate3,type="s",lwd=2,col="orange")
## legend("topleft",
##     c("Relapse in 1993-1996","Relapse in 1997-1999",
##         "Relapse in 2000 or later"),
##     lwd=2,col=c("red2","orangered","orange"),bty="n")


###################################################
### code chunk number 69: figpt123 (eval = FALSE)
###################################################
## plot(pt11$time/365.25,pt11$pstate3,type="s",ylim=c(0,1),
##     xlab="Years since transplant",ylab="Probability of death",
##     lwd=2,col="red2")
## lines(pt21$time/365.25,pt21$pstate3,type="s",lwd=2,col="orangered")
## lines(pt31$time/365.25,pt31$pstate3,type="s",lwd=2,col="orange")
## legend("topleft",
##     c("Relapse in 1993-1996","Relapse in 1997-1999",
##         "Relapse in 2000 or later"),
##     lwd=2,col=c("red2","orangered","orange"),bty="n")


###################################################
### code chunk number 70: highrisk (eval = FALSE)
###################################################
## ndata <- ndata.copy # use copy from last time
## ndata$score <- "High risk"
## ndata
## ndata.copy <- ndata # make a copy for later
## HvH1 <- msfit(c5,newdata=ndata,trans=tmat)
## ndata <- ndata.copy # used the copy
## # first dummy variable = 1 for transition 3
## ndata$yrel1.3[ndata$trans==3] <- 1
## ndata
## HvH2 <- msfit(c5,newdata=ndata,trans=tmat)
## ndata <- ndata.copy
## # second dummy variable = 1 for transition 3
## ndata$yrel2.3[ndata$trans==3] <- 1
## ndata
## HvH3 <- msfit(c5,newdata=ndata,trans=tmat)
## pt1 <- probtrans(HvH1,predt=0,direction="forward")
## pt11 <- pt1[[1]]
## pt2 <- probtrans(HvH2,predt=0,direction="forward")
## pt21 <- pt2[[1]]
## pt3 <- probtrans(HvH3,predt=0,direction="forward")
## pt31 <- pt3[[1]]


###################################################
### code chunk number 71: figpt345plot (eval = FALSE)
###################################################
## plot(pt11$time/365.25,pt11$pstate3,type="s",ylim=c(0,1),
##     xlab="Years since transplant",ylab="Probability of death",
##     lwd=2,col="red2")
## lines(pt21$time/365.25,pt21$pstate3,type="s",lwd=2,col="orangered")
## lines(pt31$time/365.25,pt31$pstate3,type="s",lwd=2,col="orange")
## legend("topleft",
##     c("Relapse in 1993-1996","Relapse in 1997-1999",
##         "Relapse in 2000 or later"),
##     lwd=2,col=c("red2","orangered","orange"),bty="n")


###################################################
### code chunk number 72: figpt345 (eval = FALSE)
###################################################
## plot(pt11$time/365.25,pt11$pstate3,type="s",ylim=c(0,1),
##     xlab="Years since transplant",ylab="Probability of death",
##     lwd=2,col="red2")
## lines(pt21$time/365.25,pt21$pstate3,type="s",lwd=2,col="orangered")
## lines(pt31$time/365.25,pt31$pstate3,type="s",lwd=2,col="orange")
## legend("topleft",
##     c("Relapse in 1993-1996","Relapse in 1997-1999",
##         "Relapse in 2000 or later"),
##     lwd=2,col=c("red2","orangered","orange"),bty="n")


###################################################
### code chunk number 73: rel3 (eval = FALSE)
###################################################
## covs <- c("score", "yrel", "rel")
## ebmt1_old <- subset(ebmt1, age > 40)
## ebmt1_old$rel <- ebmt1_old$rel / year
## ebmt1_old$srv <- ebmt1_old$srv / year
## msebmt <- msprep(
##   time=c(NA, "rel", "srv"),
##   status=c(NA, "relstat", "srvstat"),
##   data=ebmt1_old, trans=tmat, id="patid",
##   keep=covs)
## # This will define rel.3 to be able to include in a Cox model
## msebmt <- expand.covs(msebmt, covs, longnames=FALSE)
## names(msebmt)[1] <- "id" # because of a bug in LMAJ


###################################################
### code chunk number 74: rel3_part2 (eval = FALSE)
###################################################
## # The Cox model including rel.3
## c2MT <- coxph(Surv(Tstart, Tstop, status) ~ rel.3 + strata(trans), 
##               data = msebmt)
## c2MT


###################################################
### code chunk number 75: probtransLMAJ (eval = FALSE)
###################################################
## c0 <- coxph(Surv(Tstart, Tstop, status) ~ strata(trans),
##     data = msebmt)
## msf0 <- msfit(c0, trans = tmat)
## 
## # Using probtrans()
## pt <- probtrans(msf0, predt = 1)
## # Using LMAJ()
## LMAJ1 <- LMAJ(msebmt, s = 1, from = 1)
## # Compare the results
## plot(pt[[1]]$time, pt[[1]]$pstate2, type = "s", lwd = 2, ylim = c(0, 1),
##      xlab = "Months since SCT", ylab = "Probability",
##      main = "Probability of being alive with relapse,\nconditional on being in SCT at 1 year",
##      axes = FALSE)
## axis(1, at = seq(1, 8, by = 1))
## axis(2)
## box()
## lines(LMAJ1$time, LMAJ1$pstate2, type = "s", lwd = 2, col = 2)
## legend("topright", c("AJ", "LMAJ"), lwd = 2, col = 1:2, bty = "n")


###################################################
### code chunk number 76: MT3 (eval = FALSE)
###################################################
## # Apply Markov test, grid of weekly time points over first 7.5 yrs
## grid <- seq(0, 4, by = 1/12)
## # Markov test for transition 3
## set.seed(2026) # for reproducible results
## names(msebmt)[1] <- "id"
## MT <- MarkovTest(msebmt, id = "id", transition = 3, grid = grid, B = 100)
## # Print results
## MT


###################################################
### code chunk number 77: figMTplot (eval = FALSE)
###################################################
## # Plot, along with first 10 bootstrap samples
## plot(MT, grid, what = "states", idx = 1:10,
##      xlab = "Months since SCT", ylab = "Log-rank test statistic",
##      main = "Transition Relapse -> Death")


###################################################
### code chunk number 78: figMT (eval = FALSE)
###################################################
## # Plot, along with first 10 bootstrap samples
## plot(MT, grid, what = "states", idx = 1:10,
##      xlab = "Months since SCT", ylab = "Log-rank test statistic",
##      main = "Transition Relapse -> Death")


###################################################
### code chunk number 79: figxxxplot (eval = FALSE)
###################################################
## plot(0:1,1:2)


###################################################
### code chunk number 80: figxxx (eval = FALSE)
###################################################
## plot(0:1,1:2)


