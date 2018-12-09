context("Show regression table")


test_that("Run glmshow.display", {
  expect_is(glmshow.display(glm(mpg ~ cyl, data = mtcars)), "display")
  expect_is(glmshow.display(glm(mpg ~ cyl + disp, data = mtcars)), "display")      
  expect_is(glmshow.display(glm(am ~ cyl, data = mtcars, family = "binomial")), "display")
  expect_is(glmshow.display(glm(am ~ cyl + disp, data = mtcars, family = "binomial")), "display")  
})



test_that("Run svyglm.display", {
  library(survey)
  data(api)
  apistrat$tt = c(rep(1, 20), rep(0, nrow(apistrat) -20))
  dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  ds <- svyglm(api00~ell+meals, design=dstrat)
  expect_is(svyregress.display(ds), "display")
})




test_that("Run geeglm.display", {
  library(geepack)
  data(dietox)
  expect_is(geeglm.display(geeglm (Weight ~ Time, id =Pig, data = dietox, family=gaussian, corstr="ex")), "list")
  expect_is(geeglm.display(geeglm (Weight ~ Time + Cu, id =Pig, data = dietox, family=gaussian, corstr="ex")), "list")    
})