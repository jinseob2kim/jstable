jstable
================

[![Build
Status](https://travis-ci.org/jinseob2kim/jstable.svg?branch=master)](https://travis-ci.org/jinseob2kim/jstable)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jinseob2kim/jstable?branch=master&svg=true)](https://ci.appveyor.com/project/jinseob2kim/jstable)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/jstable)](https://cran.r-project.org/package=jstable)
[![codecov](https://codecov.io/github/jinseob2kim/jstable/branch/master/graphs/badge.svg)](https://codecov.io/github/jinseob2kim/jstable)
[![GitHub
issues](https://img.shields.io/github/issues/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/issues)
[![GitHub
forks](https://img.shields.io/github/forks/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/network)
[![GitHub
stars](https://img.shields.io/github/stars/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/stargazers)
[![GitHub
license](https://img.shields.io/github/license/jinseob2kim/jstable.svg)](https://github.com/jinseob2kim/jstable/blob/master/LICENSE)
[![GitHub last
commit](https://img.shields.io/github/last-commit/google/skia.svg)](https://github.com/jinseob2kim/jstable)
[![GitHub
contributors](https://img.shields.io/github/contributors/jinseob2kim/jstable.svg?maxAge=2592000)](https://github.com/jinseob2kim/jstable/graphs/contributors)

Regression Results Tables from 'GLM', 'GEE', 'GLMM', 'Cox' and 'survey' Results.

## Install

``` r
remotes::install_github('jinseob2kim/jstable')
library(jstable)
```

## GLM Table

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

## GEE Table: from `geeglm` object from **geepack** package

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
    ## Time       "0.99 (0.95,1.03)" "0.68"        "0.99 (0.95,1.03)"
    ## Cu: ref.=1 NA                 NA            NA                
    ##    2       "1 (0.76,1.34)"    "0.973"       "1.01 (0.76,1.34)"
    ##    3       "1 (0.77,1.3)"     "0.988"       "1 (0.77,1.3)"    
    ##            adj. P value
    ## Time       "0.68"      
    ## Cu: ref.=1 NA          
    ##    2       "0.971"     
    ##    3       "0.987"     
    ## 
    ## $metric
    ##                                  crude OR(95%CI) crude P value
    ##                                  NA              NA           
    ## Estimated correlation parameters "-0.027"        NA           
    ## No. of clusters                  "72"            NA           
    ## No. of observations              "861"           NA           
    ##                                  adj. OR(95%CI) adj. P value
    ##                                  NA             NA          
    ## Estimated correlation parameters NA             NA          
    ## No. of clusters                  NA             NA          
    ## No. of observations              NA             NA

## Mixed model Table: `lmerMod` or `glmerMod` object from **lme4** package

``` r
library(lme4)
l1 = lmer(Weight ~ Time + Cu + (1|Pig) + (1|Evit), data = dietox) 
lmer.display(l1, ci.ranef = T)
```

    ## $table
    ##                      crude coeff(95%CI) crude P value adj. coeff(95%CI)
    ## Time                   6.94 (6.88,7.01)     0.0000000  6.94 (6.88,7.01)
    ## Cu: ref.=1                         <NA>            NA              <NA>
    ##    2                 -0.57 (-4.66,3.52)     0.7837049 -0.81 (-4.42,2.8)
    ##    3                   1.9 (-2.23,6.04)     0.3666825 1.78 (-1.87,5.42)
    ## Random effects                     <NA>            NA              <NA>
    ## Pig                  39.7 (27.82,54.93)            NA              <NA>
    ## Evit                     0.91 (0,13.45)            NA              <NA>
    ## Residual             11.37 (10.3,12.55)            NA              <NA>
    ## Metrics                            <NA>            NA              <NA>
    ## No. of groups (Pig)                  72            NA              <NA>
    ## No. of groups (Evit)                  3            NA              <NA>
    ## No. of observations                 861            NA              <NA>
    ## Log-likelihood                 -2400.69            NA              <NA>
    ## AIC value                       4801.38            NA              <NA>
    ##                      adj. P value
    ## Time                    0.0000000
    ## Cu: ref.=1                     NA
    ##    2                    0.6599124
    ##    3                    0.3393017
    ## Random effects                 NA
    ## Pig                            NA
    ## Evit                           NA
    ## Residual                       NA
    ## Metrics                        NA
    ## No. of groups (Pig)            NA
    ## No. of groups (Evit)           NA
    ## No. of observations            NA
    ## Log-likelihood                 NA
    ## AIC value                      NA
    ## 
    ## $caption
    ## [1] "Linear mixed model fit by REML : Weight ~ Time + Cu + (1 | Pig) + (1 | Evit)"

``` r
l2 = glmer(ddn ~ Weight + Time + (1|Pig), data= dietox, family= "binomial")
lmer.display(l2)
```

    ## $table
    ##                      crude OR(95%CI) crude P value   adj. OR(95%CI)
    ## Weight                    1 (1,1.01)     0.8611499    1.02 (1,1.04)
    ## Time                0.99 (0.95,1.03)     0.6718412 0.87 (0.76,0.99)
    ## Random effects                  <NA>            NA             <NA>
    ## Pig                                0            NA             <NA>
    ## Metrics                         <NA>            NA             <NA>
    ## No. of groups (Pig)               72            NA             <NA>
    ## No. of observations              861            NA             <NA>
    ## Log-likelihood               -593.77            NA             <NA>
    ## AIC value                    1195.54            NA             <NA>
    ##                     adj. P value
    ## Weight                0.04359124
    ## Time                  0.03996422
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

## Cox model with `frailty` or `cluster` options

``` r
library(survival)
fit1 <- coxph(Surv(time, status) ~ ph.ecog + age + cluster(inst), lung, model = T)  ## model = T: to extract original data
fit2 <- coxph(Surv(time, status) ~ ph.ecog + age + frailty(inst), lung, model = T)
cox2.display(fit1)
```

    ## $table
    ##         crude HR(95%CI)    crude P value adj. HR(95%CI)  adj. P value
    ## ph.ecog "1.61 (1.25,2.08)" "< 0.001"     "1.56 (1.22,2)" "< 0.001"   
    ## age     "1.02 (1.01,1.03)" "0.007"       "1.01 (1,1.02)" "0.085"     
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
    ## age     "1.02 (1,1.04)"    "0.041"       "1.01 (0.99,1.03)" "0.225"     
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

## Cox mixed effect model Table: `coxme` object from **coxme** package

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

## GLM for survey data : `svyglm` object from **survey** package

``` r
library(survey)
data(api)
apistrat$tt = c(rep(1, 20), rep(0, nrow(apistrat) -20))
apistrat$tt2 = factor(c(rep(0, 40), rep(1, nrow(apistrat) -40)))

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
ds <- svyglm(api00~ell+meals+cname+mobility + tt2, design=dstrat)
ds2 <- svyglm(tt~ell+meals+cname+mobility + tt2, design=dstrat, family = quasibinomial())
svyregress.display(ds)
```

    ## $first.line
    ## [1] "Linear regression predicting api00- weighted data\n"
    ## 
    ## $table
    ##                     crude coeff.(95%CI)        crude P value
    ## ell                 "-3.73 (-4.35,-3.11)"      "< 0.001"    
    ## meals               "-3.38 (-3.71,-3.05)"      "< 0.001"    
    ## cname: ref.=Alameda NA                         NA           
    ##    Amador           "47.84 (-52.72,148.4)"     "0.353"      
    ##    Butte            "-68.16 (-168.72,32.4)"    "0.186"      
    ##    Colusa           "-143.16 (-243.72,-42.6)"  "0.006"      
    ##    Contra Costa     "83.73 (-38.42,205.89)"    "0.181"      
    ##    El Dorado        "28.65 (-82.39,139.68)"    "0.614"      
    ##    Fresno           "-141.53 (-263.93,-19.12)" "0.025"      
    ##    Humboldt         "16.84 (-83.72,117.4)"     "0.743"      
    ##    Inyo             "-15.72 (-119.64,88.19)"   "0.767"      
    ##    Kern             "-16.93 (-162.52,128.67)"  "0.82"       
    ##    Kings            "-166.16 (-266.72,-65.6)"  "0.001"      
    ##    Los Angeles      "-61.65 (-170.91,47.61)"   "0.27"       
    ##    Marin            "136.85 (14.32,259.38)"    "0.03"       
    ##    Mariposa         "10.84 (-89.72,111.4)"     "0.833"      
    ##    Mendocino        "-63.14 (-163.71,37.43)"   "0.22"       
    ##    Merced           "-124.3 (-226.55,-22.05)"  "0.018"      
    ##    Monterey         "-32.33 (-181.51,116.86)"  "0.672"      
    ##    Napa             "-35.16 (-135.72,65.4)"    "0.494"      
    ##    Orange           "15.8 (-107.96,139.56)"    "0.803"      
    ##    Placer           "111.2 (-24.09,246.48)"    "0.109"      
    ##    Riverside        "-104.26 (-208.46,-0.06)"  "0.052"      
    ##    Sacramento       "-3.86 (-146.36,138.64)"   "0.958"      
    ##    San Bernardino   "-127.61 (-246.83,-8.38)"  "0.038"      
    ##    San Diego        "8.96 (-110.28,128.2)"     "0.883"      
    ##    San Francisco    "-163.11 (-272.65,-53.56)" "0.004"      
    ##    San Joaquin      "-2.07 (-106.6,102.45)"    "0.969"      
    ##    San Mateo        "79.45 (-98.09,257)"       "0.382"      
    ##    Santa Barbara    "47.84 (-52.72,148.4)"     "0.353"      
    ##    Santa Clara      "-31.04 (-165.5,103.42)"   "0.652"      
    ##    Santa Cruz       "57.37 (-43.49,158.24)"    "0.267"      
    ##    Shasta           "-32.66 (-151.88,86.56)"   "0.592"      
    ##    Siskiyou         "84.84 (-15.72,185.4)"     "0.1"        
    ##    Solano           "48.84 (-51.72,149.4)"     "0.343"      
    ##    Sonoma           "52.84 (-90.54,196.21)"    "0.471"      
    ##    Stanislaus       "16.84 (-83.72,117.4)"     "0.743"      
    ##    Tehama           "69.84 (-30.72,170.4)"     "0.175"      
    ##    Tulare           "-68.08 (-189.66,53.49)"   "0.274"      
    ##    Tuolumne         "17.84 (-82.72,118.4)"     "0.729"      
    ##    Ventura          "12.01 (-106.02,130.04)"   "0.842"      
    ##    Yolo             "-76.14 (-184.86,32.57)"   "0.172"      
    ## mobility            "-1.43 (-3.3,0.44)"        "0.137"      
    ## tt2: 1 vs 0         "10.98 (-34.16,56.12)"     "0.634"      
    ##                     adj. coeff.(95%CI)         adj. P value
    ## ell                 "-0.7 (-1.53,0.13)"        "0.1"       
    ## meals               "-3.29 (-3.96,-2.63)"      "< 0.001"   
    ## cname: ref.=Alameda NA                         NA          
    ##    Amador           "-53.93 (-110.17,2.32)"    "0.062"     
    ##    Butte            "-21.1 (-85.36,43.15)"     "0.521"     
    ##    Colusa           "-38.75 (-98.1,20.6)"      "0.203"     
    ##    Contra Costa     "-5.5 (-82.43,71.43)"      "0.889"     
    ##    El Dorado        "-63.5 (-122.31,-4.68)"    "0.036"     
    ##    Fresno           "40.55 (-40.49,121.58)"    "0.328"     
    ##    Humboldt         "-114.4 (-170.09,-58.72)"  "< 0.001"   
    ##    Inyo             "-32.29 (-112.96,48.37)"   "0.434"     
    ##    Kern             "3.74 (-67.51,74.98)"      "0.918"     
    ##    Kings            "-56.85 (-125.93,12.23)"   "0.109"     
    ##    Los Angeles      "19.49 (-47.64,86.61)"     "0.57"      
    ##    Marin            "6.26 (-89.25,101.77)"     "0.898"     
    ##    Mariposa         "-87.63 (-143.96,-31.3)"   "0.003"     
    ##    Mendocino        "7.71 (-106.73,122.14)"    "0.895"     
    ##    Merced           "-17.59 (-92.74,57.57)"    "0.647"     
    ##    Monterey         "-23.17 (-102.88,56.55)"   "0.57"      
    ##    Napa             "-53.44 (-112.67,5.79)"    "0.079"     
    ##    Orange           "58.31 (-6,122.61)"        "0.078"     
    ##    Placer           "4.71 (-76.87,86.29)"      "0.91"      
    ##    Riverside        "-55.23 (-125.6,15.14)"    "0.126"     
    ##    Sacramento       "-28.47 (-110.05,53.11)"   "0.495"     
    ##    San Bernardino   "-50.28 (-123.21,22.66)"   "0.179"     
    ##    San Diego        "20.35 (-41.69,82.4)"      "0.521"     
    ##    San Francisco    "-125.39 (-233.85,-16.93)" "0.025"     
    ##    San Joaquin      "-50.45 (-115.19,14.29)"   "0.129"     
    ##    San Mateo        "-26.21 (-144.27,91.85)"   "0.664"     
    ##    Santa Barbara    "-18.6 (-73.55,36.36)"     "0.508"     
    ##    Santa Clara      "-9.75 (-85.89,66.39)"     "0.802"     
    ##    Santa Cruz       "-20.95 (-79.87,37.98)"    "0.487"     
    ##    Shasta           "72.01 (-31.59,175.6)"     "0.175"     
    ##    Siskiyou         "37.72 (-22.34,97.78)"     "0.22"      
    ##    Solano           "34.26 (-31.39,99.91)"     "0.308"     
    ##    Sonoma           "11.83 (-65.01,88.66)"     "0.763"     
    ##    Stanislaus       "-105.52 (-161.41,-49.63)" "< 0.001"   
    ##    Tehama           "99.7 (34.38,165.02)"      "0.003"     
    ##    Tulare           "16.4 (-53.88,86.68)"      "0.648"     
    ##    Tuolumne         "-60.52 (-117.27,-3.77)"   "0.038"     
    ##    Ventura          "-2.86 (-63.78,58.07)"     "0.927"     
    ##    Yolo             "12.21 (-87.11,111.53)"    "0.81"      
    ## mobility            "0.17 (-0.62,0.96)"        "0.674"     
    ## tt2: 1 vs 0         "15.01 (-8.5,38.52)"       "0.213"     
    ## 
    ## $last.lines
    ## [1] "No. of observations = 200\nAIC value = 2333.9174\n\n"
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
    ##                     crude OR.(95%CI)    crude P value
    ## ell                 "1.02 (1,1.05)"     "0.047"      
    ## meals               "1.01 (0.99,1.03)"  "0.255"      
    ## cname: ref.=Alameda NA                  NA           
    ##    Amador           "0 (0,0)"           "< 0.001"    
    ##    Butte            "0 (0,0)"           "< 0.001"    
    ##    Colusa           "0 (0,0)"           "< 0.001"    
    ##    Contra Costa     "0 (0,0)"           "< 0.001"    
    ##    El Dorado        "0 (0,0)"           "< 0.001"    
    ##    Fresno           "1.74 (0.12,25.84)" "0.687"      
    ##    Humboldt         "0 (0,0)"           "< 0.001"    
    ##    Inyo             "0 (0,0)"           "< 0.001"    
    ##    Kern             "1.73 (0.09,33.26)" "0.717"      
    ##    Kings            "0 (0,0)"           "< 0.001"    
    ##    Los Angeles      "1.86 (0.19,18.54)" "0.598"      
    ##    Marin            "0 (0,0)"           "< 0.001"    
    ##    Mariposa         "0 (0,0)"           "< 0.001"    
    ##    Mendocino        "0 (0,0)"           "< 0.001"    
    ##    Merced           "0 (0,0)"           "< 0.001"    
    ##    Monterey         "0 (0,0)"           "< 0.001"    
    ##    Napa             "0 (0,0)"           "< 0.001"    
    ##    Orange           "1.58 (0.11,22.23)" "0.734"      
    ##    Placer           "0 (0,0)"           "< 0.001"    
    ##    Riverside        "2.95 (0.23,38.08)" "0.408"      
    ##    Sacramento       "0 (0,0)"           "< 0.001"    
    ##    San Bernardino   "0.74 (0.04,14.17)" "0.839"      
    ##    San Diego        "3.89 (0.29,51.72)" "0.306"      
    ##    San Francisco    "0 (0,0)"           "< 0.001"    
    ##    San Joaquin      "0 (0,0)"           "< 0.001"    
    ##    San Mateo        "0 (0,0)"           "< 0.001"    
    ##    Santa Barbara    "0 (0,0)"           "< 0.001"    
    ##    Santa Clara      "0 (0,0)"           "< 0.001"    
    ##    Santa Cruz       "0 (0,0)"           "< 0.001"    
    ##    Shasta           "0 (0,0)"           "< 0.001"    
    ##    Siskiyou         "0 (0,0)"           "< 0.001"    
    ##    Solano           "0 (0,0)"           "< 0.001"    
    ##    Sonoma           "0 (0,0)"           "< 0.001"    
    ##    Stanislaus       "0 (0,0)"           "< 0.001"    
    ##    Tehama           "0 (0,0)"           "< 0.001"    
    ##    Tulare           "0 (0,0)"           "< 0.001"    
    ##    Tuolumne         "0 (0,0)"           "< 0.001"    
    ##    Ventura          "7.01 (0.56,88.41)" "0.134"      
    ##    Yolo             "0 (0,0)"           "< 0.001"    
    ## mobility            "1.01 (0.98,1.03)"  "0.506"      
    ## tt2: 1 vs 0         "0 (0,0)"           "< 0.001"    
    ##                     adj. OR.(95%CI)                                                      
    ## ell                 "8642653.71 (3481743.02,21453468.19)"                                
    ## meals               "0.04 (0.03,0.05)"                                                   
    ## cname: ref.=Alameda NA                                                                   
    ##    Amador           "Inf (Inf,Inf)"                                                      
    ##    Butte            "Inf (Inf,Inf)"                                                      
    ##    Colusa           "6.0036985834333e+173 (2.66764006059928e+164,1.35117166716344e+183)" 
    ##    Contra Costa     "0 (0,0)"                                                            
    ##    El Dorado        "0.05 (0,0.83)"                                                      
    ##    Fresno           "0 (0,0)"                                                            
    ##    Humboldt         "Inf (Inf,Inf)"                                                      
    ##    Inyo             "3.4381042403022e+27 (4.73418041960403e+25,2.49685472869508e+29)"    
    ##    Kern             "1.02353878660395e+47 (7.33690545697783e+44,1.42789307266638e+49)"   
    ##    Kings            "Inf (Inf,Inf)"                                                      
    ##    Los Angeles      "0 (0,0)"                                                            
    ##    Marin            "Inf (Inf,Inf)"                                                      
    ##    Mariposa         "Inf (Inf,Inf)"                                                      
    ##    Mendocino        "0 (0,0)"                                                            
    ##    Merced           "Inf (Inf,Inf)"                                                      
    ##    Monterey         "1.24710898702096e+117 (9.98245683569093e+110,1.55801407520014e+123)"
    ##    Napa             "Inf (Inf,Inf)"                                                      
    ##    Orange           "0 (0,0)"                                                            
    ##    Placer           "Inf (Inf,Inf)"                                                      
    ##    Riverside        "8.00352162124239e+65 (5.22981865895702e+62,1.22482943518484e+69)"   
    ##    Sacramento       "9.37864738133762e+272 (7.67834271719589e+257,1.14554702678853e+288)"
    ##    San Bernardino   "0 (0,0)"                                                            
    ##    San Diego        "0 (0,0)"                                                            
    ##    San Francisco    "3.1041426145938e+147 (3.6502350093894e+139,2.63974822085471e+155)"  
    ##    San Joaquin      "3.77939736184809e+268 (4.84287658080411e+253,2.94945456082067e+283)"
    ##    San Mateo        "Inf (Inf,Inf)"                                                      
    ##    Santa Barbara    "Inf (Inf,Inf)"                                                      
    ##    Santa Clara      "0 (0,0)"                                                            
    ##    Santa Cruz       "1.16743603082104e+38 (4.00441011948733e+35,3.40351473847958e+40)"   
    ##    Shasta           "Inf (Inf,Inf)"                                                      
    ##    Siskiyou         "Inf (Inf,Inf)"                                                      
    ##    Solano           "0 (0,0.03)"                                                         
    ##    Sonoma           "0 (0,0)"                                                            
    ##    Stanislaus       "Inf (Inf,Inf)"                                                      
    ##    Tehama           "Inf (Inf,Inf)"                                                      
    ##    Tulare           "0 (0,0)"                                                            
    ##    Tuolumne         "Inf (Inf,Inf)"                                                      
    ##    Ventura          "2.4899376809852e+63 (9.37351475383711e+59,6.61415682164691e+66)"    
    ##    Yolo             "1.8710128062593e+225 (1.70374718773602e+213,2.05469975028285e+237)" 
    ## mobility            "159609.3 (78913.87,322821.96)"                                      
    ## tt2: 1 vs 0         "0 (0,0)"                                                            
    ##                     adj. P value
    ## ell                 "< 0.001"   
    ## meals               "< 0.001"   
    ## cname: ref.=Alameda NA          
    ##    Amador           "< 0.001"   
    ##    Butte            "< 0.001"   
    ##    Colusa           "< 0.001"   
    ##    Contra Costa     "< 0.001"   
    ##    El Dorado        "0.038"     
    ##    Fresno           "< 0.001"   
    ##    Humboldt         "< 0.001"   
    ##    Inyo             "< 0.001"   
    ##    Kern             "< 0.001"   
    ##    Kings            "< 0.001"   
    ##    Los Angeles      "< 0.001"   
    ##    Marin            "< 0.001"   
    ##    Mariposa         "< 0.001"   
    ##    Mendocino        "< 0.001"   
    ##    Merced           "< 0.001"   
    ##    Monterey         "< 0.001"   
    ##    Napa             "< 0.001"   
    ##    Orange           "< 0.001"   
    ##    Placer           "< 0.001"   
    ##    Riverside        "< 0.001"   
    ##    Sacramento       "< 0.001"   
    ##    San Bernardino   "< 0.001"   
    ##    San Diego        "< 0.001"   
    ##    San Francisco    "< 0.001"   
    ##    San Joaquin      "< 0.001"   
    ##    San Mateo        "< 0.001"   
    ##    Santa Barbara    "< 0.001"   
    ##    Santa Clara      "< 0.001"   
    ##    Santa Cruz       "< 0.001"   
    ##    Shasta           "< 0.001"   
    ##    Siskiyou         "< 0.001"   
    ##    Solano           "< 0.001"   
    ##    Sonoma           "< 0.001"   
    ##    Stanislaus       "< 0.001"   
    ##    Tehama           "< 0.001"   
    ##    Tulare           "< 0.001"   
    ##    Tuolumne         "< 0.001"   
    ##    Ventura          "< 0.001"   
    ##    Yolo             "< 0.001"   
    ## mobility            "< 0.001"   
    ## tt2: 1 vs 0         "< 0.001"   
    ## 
    ## $last.lines
    ## [1] "No. of observations = 200\n\n"
    ## 
    ## attr(,"class")
    ## [1] "display" "list"

## Cox model for survey data :`svycoxph` object from **survey** package

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
