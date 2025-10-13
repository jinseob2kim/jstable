library(survival)
lung
lung$sex <- factor(lung$sex)
fit1 <- coxph(Surv(time, status) ~ sex + age + cluster(inst), data = lung, model = TRUE)
fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), data = lung, model = TRUE)
cox2.display(fit1)
cox2.display(fit2)



data <- mgus2
head(data)

data$etime <- with(data, ifelse(pstat==0, futime, ptime))
data$event <- with(data, ifelse(pstat==0, 2*death, 1))
data$event2 <- data$event
data$event <- factor(data$event, 0:2, labels=c("censor", "pcm", "death"))
data$event2 <- factor(data$event2, 0:2, labels=c("A", "B", "C"))

pdata <- finegray(Surv(etime, event) ~ ., data=data)
head(pdata)

fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age+sex,
               weight=fgwt, data=pdata, model = T)
summary(fgfit)
fgfit$



MSMfit <- coxph(Surv(etime, event) ~ sex+ age, data = data, id = id, model = T)
summary(MSMfit)

MSMfit$states


cox2.display(fit1)
cox2.display(fit2)
cox2.display(MSMfit)
cox2.display(MSMfit, event_msm = "pcm")
cox2.display(MSMfit, event_msm = "censor")
cox2.display(MSMfit, event_msm = "(s0)")
cox2.display(MSMfit, event_msm = c("censor", "pcm", "death"))
cox2.display(MSMfit, event_msm = c("pcm"))
cox2.display(MSMfit, event_msm = c("death"))

cox2.display(fgfit)

MSMfit2 <- coxph(Surv(etime, event2) ~ sex+ age, data = data, id = id, model = T)


cox2.display(MSMfit, event_msm = "pcm")
cox2.display(MSMfit2, event_msm = "A")
MSMfit2$states


cox.obj.withmodel = MSMfit
event_msm = c("censor", "pcm", "death")
#event_msm = "(s0)"
pcut.univariate = NULL
data_for_univariate = NULL
dec=3
