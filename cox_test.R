## interaction term은 빼고 진행한다.
## 


library(survival)
lung
lung$sex <- factor(lung$sex)

## cluster, frailty, normal check
fit1 <- coxph(Surv(time, status) ~ sex + age + cluster(inst), data = lung, model = TRUE)
fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), data = lung, model = TRUE)
fit3 <- coxph(Surv(time, status) ~ sex + age , data = lung, model = TRUE)

## interaction term - 안된다
fit33 <- coxph(Surv(time, status) ~ sex*age , data = lung, model = TRUE)
summary(fit33)



cox2.display(fit1)
cox2.display(fit2)
cox2.display(fit3)
cox2.display(fit33)
jstable::cox2.display(fit33)


## msm 
data <- mgus2
head(data)

data$etime <- with(data, ifelse(pstat==0, futime, ptime))
data$event <- with(data, ifelse(pstat==0, 2*death, 1))
data$event2 <- data$event
data$event <- factor(data$event, 0:2, labels=c("censor", "pcm", "death"))
data$event2 <- factor(data$event2, 0:2, labels=c("A", "B", "C"))
data$event3 <- rep_len(0:3, nrow(data))
data$yn <- factor(data$event3 %% 2, 0:1, labels=c("No", "Yes"))

data$event3 <- factor(data$event3, 0:3, labels=c("A", "B", "C", "D"))


fit4 <- coxph(Surv(etime, event) ~ sex + age + cluster(event3), id = id, data = data, model = TRUE) #3 ~ 4
fit5 <- coxph(Surv(etime, event2) ~ sex + age + cluster(yn),id = id, data = data, model = TRUE) # 3 ~ 2
fit6 <- coxph(Surv(etime, yn) ~ sex + age + cluster(event3), id = id, data = data, model = TRUE) # 2~4

cox2.display(fit4)
cox2.display(fit5)
cox2.display(fit6)

cox2.display(fit5, event_msm = "A")
cox2.display(fit5, event_msm = "B")
cox2.display(fit5, event_msm = "C")
cox2.display(fit5, event_msm = c("B","C"))
cox2.display(fit5, event_msm = c("B","D"))

cox2.display(fit6)
# factor level이 2인 경우에는 오류가 뜨는데, msm >=3 인건가?
cox2.display(fit6, event_msm = "No")
cox2.display(fit6, event_msm = "Yes")


## interaction term - 안된다 fit7 ~ fit13
fit7 <- coxph(Surv(time, status) ~ sex*age + cluster(inst), data = lung, model = TRUE)
fit8 <- coxph(Surv(time, status) ~ ph.ecog*age + frailty(inst), data = lung, model = TRUE)
fit9 <- coxph(Surv(time, status) ~ sex*age , data = lung, model = TRUE)

cox2.display(fit7)
cox2.display(fit8)
cox2.display(fit9)

fit10 <- coxph(Surv(etime, event) ~ sex*age + event3, id = id, data = data, model = TRUE)
fit11 <- coxph(Surv(etime, event2) ~ sex*age + cluster(event3),id = id, data = data, model = TRUE)
fit12 <- coxph(Surv(etime, yn) ~ sex*age + cluster(event3), id = id, data = data, model = TRUE)
fit13 <- coxph(Surv(etime, event) ~ sex*age + cluster(event3), id = id, data = data, model = TRUE)

cox2.display(fit10)
cox2.display(fit11)
cox2.display(fit12)
cox2.display(fit13)

# multi-state models do not currently support frailty terms

# fit7 <- coxph(Surv(etime, event) ~ sex + age + frailty(event3), id = id, data = data, model = TRUE)
# fit8 <- coxph(Surv(etime, event2) ~ sex + age + frailty(event3),id = id, data = data, model = TRUE)
# fit9 <- coxph(Surv(etime, yn) ~ sex + age + frailty(event3), id = id, data = data, model = TRUE)
# 
# cox2.display(fit7)
# cox2.display(fit8)
# cox2.display(fit9)




## finegray check

pdata <- finegray(Surv(etime, event) ~ ., data=data)
#head(pdata)

fgfit <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age+sex,
               weight=fgwt, data=pdata, model = T)
summary(fgfit)
cox2.display(fgfit)


fgfit2 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age+sex+ cluster(id),
               weight=fgwt, data=pdata, model = T)
cox2.display(fgfit2)

## frailty는 안된다.
# fgfit3 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age+sex + frailty(id),
#                weight=fgwt, data=pdata, model = T)
# 
# cox2.display(fgfit3)



## interaction 안된다.
# fgfit4 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ age*sex+ cluster(id),
#                 weight=fgwt, data=pdata, model = T)
# cox2.display(fgfit4)



# factor level 3
MSMfit <- coxph(Surv(etime, event) ~ sex+ age, data = data, id = id, model = T)
# factor level 
MSMfit2 <- coxph(Surv(etime, event2) ~ sex+ age, data = data, id = id, model = T)
# factor level 4
MSMfit3 <- coxph(Surv(etime, event3) ~ sex+ age, data = data, id = id, model = T)
# factpr level 2
MSMfit0 <- coxph(Surv(etime, yn) ~ sex+ age, data = data, id = id, model = T)

cox2.display(MSMfit0)
cox2.display(MSMfit0, event_msm = "Yes")
cox2.display(MSMfit0, event_msm = "No")
cox2.display(MSMfit0, event_msm = c("No", "Yes"))

summary(MSMfit)

#jstable::mk.lev(data)

MSMfit$states


cox2.display(fit1)
cox2.display(fit2)
cox2.display(MSMfit)
cox2.display(MSMfit, event_msm = "pcm")
cox2.display(MSMfit, event_msm = "censor")
cox2.display(MSMfit, event_msm = "(s0)")
cox2.display(MSMfit, event_msm = c("censor", "pcm", "death"))
cox2.display(MSMfit, event_msm = c("pcm", "death"))
cox2.display(MSMfit, event_msm = c("pcm"))
cox2.display(MSMfit, event_msm = c("A"))

cox2.display(fgfit)

MSMfit2 <- coxph(Surv(etime, event2) ~ sex+ age, data = data, id = id, model = T)
MSMfit3 <- coxph(Surv(etime, event3) ~ sex+ age, data = data, id = id, model = T)

cox2.display(MSMfit, event_msm = "pcm")
cox2.display(MSMfit2, event_msm = "A")
cox2.display(MSMfit2, event_msm = "B")
MSMfit2$states


cox2.display(MSMfit3, event_msm = "A")
cox2.display(MSMfit3, event_msm = "B")
cox2.display(MSMfit3, event_msm = "C")
cox2.display(MSMfit3, event_msm = "D")
cox2.display(MSMfit3)
cox2.display(MSMfit3, event_msm = c("B","C"))
cox2.display(MSMfit3, event_msm = c("B","C","D"))
MSMfit3$states


# 


## 직접 테스트
cox.obj.withmodel = MSMfit
event_msm = c("censor", "pcm", "death")
#event_msm = "(s0)"
event_msm = c("pcm", "death")
event_msm = "pcm"
pcut.univariate = NULL
data_for_univariate = NULL
dec=3

## 이제 label 확인
cox.obj.withmodel = fit1
event_msm = NULL
# #event_msm = "(s0)"
# event_msm = c("pcm", "death")
# event_msm = "pcm"
pcut.univariate = NULL
data_for_univariate = NULL
dec=3

################################
cox.obj.withmodel = MSMfit3
event_msm = c("B","C")
pcut.univariate = NULL
data_for_univariate = NULL
dec=3

cox2.display(MSMfit3, event_msm = c("B"))
cox2.display(MSMfit3, event_msm = c("B","C"))
cox2.display(MSMfit3, event_msm = c("B","C","D"))