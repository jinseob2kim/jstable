# jstable

[![Build Status](https://travis-ci.org/jinseob2kim/jstable.svg?branch=master)](https://travis-ci.org/jinseob2kim/jstable)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/jstable)](http://cran.r-project.org/package=jstable)
[![GitHub issues](https://img.shields.io/github/issues/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/issues)
[![GitHub forks](https://img.shields.io/github/forks/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/network)
[![GitHub stars](https://img.shields.io/github/stars/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/stargazers)
[![GitHub license](https://img.shields.io/github/license/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/blob/master/LICENSE)
[![GitHub last commit](https://img.shields.io/github/last-commit/google/skia.svg)](https://github.com/jinseob2kim/jstable)
[![GitHub contributors](https://img.shields.io/github/contributors/jinseob2kim/jstable.svg?maxAge=2592000)](https://github.com/jinseob2kim/jstable/graphs/contributors)


Research tables from GEE, GLMM, cox mixed effect model results

## Install

```r
devtools::install_github('jinseob2kim/jstable')
```

## GEE Table: from `geeglm` object from **geepack** package

```r
library(jstable)
library(geepack)  ## for dietox data
data(dietox)
dietox$Cu <- as.factor(dietox$Cu)
dietox$ddn = as.numeric(rnorm(nrow(dietox)) > 0)
gee01 <- geeglm (Weight ~ Time + Cu , id = Pig, data = dietox, family = gaussian, corstr = "ex")
geeglm.display(gee01)

gee02 <- geeglm (ddn ~ Time + Cu , id = Pig, data = dietox, family = binomial, corstr = "ex")
geeglm.display(gee02)
```

## Mixed model Table: from `lmerMod` or `glmerMod` object from **lme4** package

```r
l1 = lmer(Weight ~ Time + Cu + (1|Pig) + (1|Evit), data = dietox) 
lmer.display(l1, ci.ranef = T)

l2 = glmer(ddn ~ Weight + Time + (1|Pig), data= dietox, family= "binomial")
lmer.display(l2)
```


## Cox model with `frailty` or `cluster` options

```r
library(survival)
fit1 <- coxph(Surv(time, status) ~ ph.ecog + age + cluster(inst), lung)
fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frail(inst), lung)
cox2.display(fit1)
cox2.display(fit2)
```

## Cox mixed effect model Table: from `coxme`  object from **coxme** package

```r
library(coxme)
fit <- coxme(Surv(time, status) ~ ph.ecog + age + (1|inst), lung)
coxme.display(fit) 
```
