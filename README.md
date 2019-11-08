jstable
================

[![Build Status](https://travis-ci.org/jinseob2kim/jstable.svg?branch=master)](https://travis-ci.org/jinseob2kim/jstable) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jinseob2kim/jstable?branch=master&svg=true)](https://ci.appveyor.com/project/jinseob2kim/jstable) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/jstable)](https://cran.r-project.org/package=jstable) [![CRAN\_Download\_Badge](https://cranlogs.r-pkg.org/badges/jstable)](https://CRAN.R-project.org/package=jstable) [![codecov](https://codecov.io/github/jinseob2kim/jstable/branch/master/graphs/badge.svg)](https://codecov.io/github/jinseob2kim/jstable) [![GitHub issues](https://img.shields.io/github/issues/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/issues) [![GitHub stars](https://img.shields.io/github/stars/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/stargazers) [![GitHub license](https://img.shields.io/github/license/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/blob/master/LICENSE)

Regression Tables from 'GLM', 'GEE', 'GLMM', 'Cox' and 'survey' Results for Publication.

Install
-------

``` r
install.packages("jstable")


## From github: latest version
remotes::install_github('jinseob2kim/jstable')
library(jstable)
```

GLM Table
---------

``` r
## Gaussian
glm_gaussian <- glm(mpg~cyl + disp, data = mtcars)
glmshow.display(glm_gaussian, decimal = 2)
```

    ## $first.line
    ## [1] "Linear regression predicting mpg\n"
    ## 
    ## $table
    ##      crude coeff.(95%CI)   crude P value adj. coeff.(95%CI)   
    ## cyl  "-2.88 (-3.51,-2.24)" "< 0.001"     "-1.59 (-2.98,-0.19)"
    ## disp "-0.04 (-0.05,-0.03)" "< 0.001"     "-0.02 (-0.04,0)"    
    ##      adj. P value
    ## cyl  "0.034"     
    ## disp "0.054"     
    ## 
    ## $last.lines
    ## [1] "No. of observations = 32\nR-squared = 0.7596\nAIC value = 167.1456\n\n"
    ## 
    ## attr(,"class")
    ## [1] "display" "list"

``` r
## Binomial
glm_binomial <- glm(vs~cyl + disp, data = mtcars, family = binomial)
glmshow.display(glm_binomial, decimal = 2)
```

    ## $first.line
    ## [1] "Logistic regression predicting vs\n"
    ## 
    ## $table
    ##      crude OR.(95%CI)   crude P value adj. OR.(95%CI)    adj. P value
    ## cyl  "0.2 (0.08,0.56)"  "0.002"       "0.15 (0.02,1.02)" "0.053"     
    ## disp "0.98 (0.97,0.99)" "0.002"       "1 (0.98,1.03)"    "0.715"     
    ## 
    ## $last.lines
    ## [1] "No. of observations = 32\nAIC value = 23.8304\n\n"
    ## 
    ## attr(,"class")
    ## [1] "display" "list"

GEE Table: from `geeglm` object from **geepack** package
--------------------------------------------------------

``` r
library(geepack)  ## for dietox data
data(dietox)
dietox$Cu <- as.factor(dietox$Cu)
dietox$ddn = as.numeric(rnorm(nrow(dietox)) > 0)
gee01 <- geeglm (Weight ~ Time + Cu , id = Pig, data = dietox, family = gaussian, corstr = "ex")
geeglm.display(gee01)
```

    ## $caption
    ## [1] "GEE(gaussian) predicting Weight by Time, Cu - Group Pig"
    ## 
    ## $table
    ##            crude coeff(95%CI)   crude P value adj. coeff(95%CI)  
    ## Time       "6.94 (6.79,7.1)"    "< 0.001"     "6.94 (6.79,7.1)"  
    ## Cu: ref.=1 NA                   NA            NA                 
    ##    2       "-0.59 (-3.73,2.54)" "0.711"       "-0.84 (-3.9,2.23)"
    ##    3       "1.9 (-1.87,5.66)"   "0.324"       "1.77 (-1.9,5.45)" 
    ##            adj. P value
    ## Time       "< 0.001"   
    ## Cu: ref.=1 NA          
    ##    2       "0.593"     
    ##    3       "0.345"     
    ## 
    ## $metric
    ##                                  crude coeff(95%CI) crude P value
    ##                                  NA                 NA           
    ## Estimated correlation parameters "0.775"            NA           
    ## No. of clusters                  "72"               NA           
    ## No. of observations              "861"              NA           
    ##                                  adj. coeff(95%CI) adj. P value
    ##                                  NA                NA          
    ## Estimated correlation parameters NA                NA          
    ## No. of clusters                  NA                NA          
    ## No. of observations              NA                NA

``` r
gee02 <- geeglm (ddn ~ Time + Cu , id = Pig, data = dietox, family = binomial, corstr = "ex")
geeglm.display(gee02)
```

    ## $caption
    ## [1] "GEE(binomial) predicting ddn by Time, Cu - Group Pig"
    ## 
    ## $table
    ##            crude OR(95%CI)    crude P value adj. OR(95%CI)    
    ## Time       "1.01 (0.97,1.06)" "0.559"       "1.01 (0.97,1.06)"
    ## Cu: ref.=1 NA                 NA            NA                
    ##    2       "0.79 (0.58,1.08)" "0.135"       "0.79 (0.58,1.07)"
    ##    3       "0.89 (0.67,1.18)" "0.421"       "0.89 (0.67,1.18)"
    ##            adj. P value
    ## Time       "0.554"     
    ## Cu: ref.=1 NA          
    ##    2       "0.134"     
    ##    3       "0.42"      
    ## 
    ## $metric
    ##                                  crude OR(95%CI) crude P value
    ##                                  NA              NA           
    ## Estimated correlation parameters "-0.019"        NA           
    ## No. of clusters                  "72"            NA           
    ## No. of observations              "861"           NA           
    ##                                  adj. OR(95%CI) adj. P value
    ##                                  NA             NA          
    ## Estimated correlation parameters NA             NA          
    ## No. of clusters                  NA             NA          
    ## No. of observations              NA             NA

Mixed model Table: `lmerMod` or `glmerMod` object from **lme4** package
-----------------------------------------------------------------------

``` r
library(lme4)
l1 <- lmer(Weight ~ Time + Cu + (1|Pig), data = dietox) 
lmer.display(l1, ci.ranef = T)
```

    ## $table
    ##                      crude coeff(95%CI) crude P value adj. coeff(95%CI)
    ## Time                   6.94 (6.88,7.01)     0.0000000  6.94 (6.88,7.01)
    ## Cu: ref.=1                         <NA>            NA              <NA>
    ##    2                 -0.58 (-4.67,3.51)     0.7811327 -0.84 (-4.47,2.8)
    ##    3                   1.9 (-2.23,6.04)     0.3670740  1.77 (-1.9,5.45)
    ## Random effects                     <NA>            NA              <NA>
    ## Pig                 40.34 (28.08,54.95)            NA              <NA>
    ## Residual             11.37 (10.3,12.55)            NA              <NA>
    ## Metrics                            <NA>            NA              <NA>
    ## No. of groups (Pig)                  72            NA              <NA>
    ## No. of observations                 861            NA              <NA>
    ## Log-likelihood                  -2400.8            NA              <NA>
    ## AIC value                        4801.6            NA              <NA>
    ##                     adj. P value
    ## Time                   0.0000000
    ## Cu: ref.=1                    NA
    ##    2                   0.6527264
    ##    3                   0.3442310
    ## Random effects                NA
    ## Pig                           NA
    ## Residual                      NA
    ## Metrics                       NA
    ## No. of groups (Pig)           NA
    ## No. of observations           NA
    ## Log-likelihood                NA
    ## AIC value                     NA
    ## 
    ## $caption
    ## [1] "Linear mixed model fit by REML : Weight ~ Time + Cu + (1 | Pig)"

``` r
l2 <- glmer(ddn ~ Weight + Time + (1|Pig), data= dietox, family= "binomial")
lmer.display(l2)
```

    ## $table
    ##                      crude OR(95%CI) crude P value adj. OR(95%CI)
    ## Weight                    1 (1,1.01)     0.5059625  1 (0.98,1.02)
    ## Time                1.01 (0.97,1.05)     0.5173971  1 (0.88,1.15)
    ## Random effects                  <NA>            NA           <NA>
    ## Pig                                0            NA           <NA>
    ## Metrics                         <NA>            NA           <NA>
    ## No. of groups (Pig)               72            NA           <NA>
    ## No. of observations              861            NA           <NA>
    ## Log-likelihood                -595.5            NA           <NA>
    ## AIC value                    1199.01            NA           <NA>
    ##                     adj. P value
    ## Weight                 0.8751956
    ## Time                   0.9718584
    ## Random effects                NA
    ## Pig                           NA
    ## Metrics                       NA
    ## No. of groups (Pig)           NA
    ## No. of observations           NA
    ## Log-likelihood                NA
    ## AIC value                     NA
    ## 
    ## $caption
    ## [1] "Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) : ddn ~ Weight + Time + (1 | Pig)"

Cox model with `frailty` or `cluster` options
---------------------------------------------

``` r
library(survival)
fit1 <- coxph(Surv(time, status) ~ ph.ecog + age, cluster = inst, lung, model = T)  ## model = T: to extract original data
fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), lung, model = T)
cox2.display(fit1)
```

    ## $table
    ##         crude HR(95%CI)    crude P value adj. HR(95%CI)  adj. P value
    ## ph.ecog "1.61 (1.25,2.08)" "< 0.001"     "1.56 (1.22,2)" "< 0.001"   
    ## age     "1.02 (1,1.03)"    "0.008"       "1.01 (1,1.02)" "0.085"     
    ## 
    ## $ranef
    ##         [,1] [,2] [,3] [,4]
    ## cluster   NA   NA   NA   NA
    ## inst      NA   NA   NA   NA
    ## 
    ## $metric
    ##                     [,1] [,2] [,3] [,4]
    ## <NA>                  NA   NA   NA   NA
    ## No. of observations  226   NA   NA   NA
    ## No. of events        163   NA   NA   NA
    ## 
    ## $caption
    ## [1] "Marginal Cox model on time ('time') to event ('status') - Group inst"

``` r
cox2.display(fit2)
```

    ## $table
    ##         crude HR(95%CI)    crude P value adj. HR(95%CI)     adj. P value
    ## ph.ecog "1.64 (1.31,2.05)" "< 0.001"     "1.58 (1.26,1.99)" "< 0.001"   
    ## age     "1.02 (1,1.04)"    "0.043"       "1.01 (0.99,1.03)" "0.225"     
    ## 
    ## $ranef
    ##         [,1] [,2] [,3] [,4]
    ## frailty   NA   NA   NA   NA
    ## inst      NA   NA   NA   NA
    ## 
    ## $metric
    ##                     [,1] [,2] [,3] [,4]
    ## <NA>                  NA   NA   NA   NA
    ## No. of observations  226   NA   NA   NA
    ## No. of events        163   NA   NA   NA
    ## 
    ## $caption
    ## [1] "Frailty Cox model on time ('time') to event ('status') - Group inst"

Cox mixed effect model Table: `coxme` object from **coxme** package
-------------------------------------------------------------------

``` r
library(coxme)
fit <- coxme(Surv(time, status) ~ ph.ecog + age + (1|inst), lung)
coxme.display(fit) 
```

    ## $table
    ##         crude HR(95%CI)    crude P value adj. HR(95%CI)     adj. P value
    ## ph.ecog "1.66 (1.32,2.09)" "< 0.001"     "1.61 (1.27,2.03)" "< 0.001"   
    ## age     "1.02 (1,1.04)"    "0.043"       "1.01 (0.99,1.03)" "0.227"     
    ## 
    ## $ranef
    ##                 [,1] [,2] [,3] [,4]
    ## Random effect     NA   NA   NA   NA
    ## inst(Intercept) 0.02   NA   NA   NA
    ## 
    ## $metric
    ##                     [,1] [,2] [,3] [,4]
    ## <NA>                  NA   NA   NA   NA
    ## No. of groups(inst)   18   NA   NA   NA
    ## No. of observations  226   NA   NA   NA
    ## No. of events        163   NA   NA   NA
    ## 
    ## $caption
    ## [1] "Mixed effects Cox model on time ('time') to event ('status') - Group inst"

GLM for survey data : `svyglm` object from **survey** package
-------------------------------------------------------------

``` r
library(survey)
data(api)
apistrat$tt = c(rep(1, 20), rep(0, nrow(apistrat) -20))
apistrat$tt2 = factor(c(rep(0, 40), rep(1, nrow(apistrat) -40)))

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
ds <- svyglm(api00~ell+meals+mobility + tt2, design=dstrat)
ds2 <- svyglm(tt~ell+meals+mobility + tt2, design=dstrat, family = quasibinomial())
svyregress.display(ds)
```

    ## $first.line
    ## [1] "Linear regression predicting api00- weighted data\n"
    ## 
    ## $table
    ##             crude coeff.(95%CI)    crude P value adj. coeff.(95%CI)   
    ## ell         "-3.73 (-4.35,-3.11)"  "< 0.001"     "-0.48 (-1.25,0.29)" 
    ## meals       "-3.38 (-3.71,-3.05)"  "< 0.001"     "-3.14 (-3.69,-2.59)"
    ## mobility    "-1.43 (-3.3,0.44)"    "0.137"       "0.22 (-0.55,0.99)"  
    ## tt2: 1 vs 0 "10.98 (-34.16,56.12)" "0.634"       "6.13 (-17.89,30.15)"
    ##             adj. P value
    ## ell         "0.222"     
    ## meals       "< 0.001"   
    ## mobility    "0.573"     
    ## tt2: 1 vs 0 "0.618"     
    ## 
    ## $last.lines
    ## [1] "No. of observations = 200\nAIC value = 2309.8282\n\n"
    ## 
    ## attr(,"class")
    ## [1] "display" "list"

``` r
svyregress.display(ds2)
```

    ## $first.line
    ## [1] "Logistic regression predicting tt- weighted data\n"
    ## 
    ## $table
    ##             crude OR.(95%CI)   crude P value adj. OR.(95%CI)   
    ## ell         "1.02 (1,1.05)"    "0.047"       "1.11 (1.03,1.21)"
    ## meals       "1.01 (0.99,1.03)" "0.255"       "0.95 (0.91,1)"   
    ## mobility    "1.01 (0.98,1.03)" "0.506"       "1.1 (0.98,1.23)" 
    ## tt2: 1 vs 0 "0 (0,0)"          "< 0.001"     "0 (0,0)"         
    ##             adj. P value
    ## ell         "0.009"     
    ## meals       "0.068"     
    ## mobility    "0.114"     
    ## tt2: 1 vs 0 "< 0.001"   
    ## 
    ## $last.lines
    ## [1] "No. of observations = 200\n\n"
    ## 
    ## attr(,"class")
    ## [1] "display" "list"

Cox model for survey data :`svycoxph` object from **survey** package
--------------------------------------------------------------------

``` r
data(pbc, package="survival")
pbc$sex = factor(pbc$sex)
pbc$stage = factor(pbc$stage)
pbc$randomized<-with(pbc, !is.na(trt) & trt>0)
biasmodel<-glm(randomized~age*edema,data=pbc,family=binomial)
pbc$randprob<-fitted(biasmodel)

if (is.null(pbc$albumin)) pbc$albumin<-pbc$alb ##pre2.9.0

dpbc <- svydesign(id=~1, prob=~randprob, strata=~edema, data=subset(pbc,randomized))

model <- svycoxph(Surv(time,status>0)~ sex + protime + albumin + stage,design=dpbc)
svycox.display(model)
```

    ## Stratified Independent Sampling design (with replacement)
    ## svydesign(id = ~1, prob = ~randprob, strata = ~edema, data = subset(pbc, 
    ##     randomized))
    ## Stratified Independent Sampling design (with replacement)
    ## svydesign(id = ~1, prob = ~randprob, strata = ~edema, data = subset(pbc, 
    ##     randomized))
    ## Stratified Independent Sampling design (with replacement)
    ## svydesign(id = ~1, prob = ~randprob, strata = ~edema, data = subset(pbc, 
    ##     randomized))
    ## Stratified Independent Sampling design (with replacement)
    ## svydesign(id = ~1, prob = ~randprob, strata = ~edema, data = subset(pbc, 
    ##     randomized))
    ## Stratified Independent Sampling design (with replacement)
    ## svydesign(id = ~1, prob = ~randprob, strata = ~edema, data = subset(pbc, 
    ##     randomized))

    ## $table
    ##               crude HR(95%CI)      crude P value adj. HR(95%CI)       
    ## sex: f vs m   "0.62 (0.4,0.97)"    "0.038"       "0.55 (0.33,0.9)"    
    ## protime       "1.37 (1.09,1.72)"   "0.006"       "1.52 (1.2,1.91)"    
    ## albumin       "0.2 (0.14,0.29)"    "< 0.001"     "0.31 (0.2,0.47)"    
    ## stage: ref.=1 NA                   NA            NA                   
    ##    2          "5.67 (0.77,41.78)"  "0.089"       "10.94 (1.01,118.55)"
    ##    3          "9.78 (1.37,69.94)"  "0.023"       "17.03 (1.69,171.6)" 
    ##    4          "22.89 (3.2,163.48)" "0.002"       "22.56 (2.25,226.42)"
    ##               adj. P value
    ## sex: f vs m   "0.017"     
    ## protime       "< 0.001"   
    ## albumin       "< 0.001"   
    ## stage: ref.=1 NA          
    ##    2          "0.049"     
    ##    3          "0.016"     
    ##    4          "0.008"     
    ## 
    ## $metric
    ##                        [,1] [,2] [,3] [,4]
    ## <NA>                     NA   NA   NA   NA
    ## No. of observations  312.00   NA   NA   NA
    ## No. of events        144.00   NA   NA   NA
    ## AIC                 1483.12   NA   NA   NA
    ## 
    ## $caption
    ## [1] "Survey cox model on time ('time') to event ('status > 0')"

Sub-group analysis for Cox/svycox model
---------------------------------------

``` r
library(survival); library(dplyr)
lung %>% 
mutate(status = as.integer(status == 1),
       sex = factor(sex),
       kk = factor(as.integer(pat.karno >= 70)),
       kk1 = factor(as.integer(pat.karno >= 60))) -> lung

#TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data=lung, line = TRUE)

## Survey data
library(survey)
data.design <- svydesign(id = ~1, data = lung)
#TableSubgroupMultiCox(Surv(time, status) ~ sex, var_subgroups = c("kk", "kk1"), data = data.design, line = FALSE)
```
