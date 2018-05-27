# jstable
Research tables from GEE, GLMM results

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