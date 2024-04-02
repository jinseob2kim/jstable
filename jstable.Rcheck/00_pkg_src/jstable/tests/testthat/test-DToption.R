context("Show DT option")


test_that("Run opt.*", {
  expect_is(opt.data("mtcars"), "list")
  expect_is(opt.tb1("mtcars"), "list")
  expect_is(opt.tbreg("mtcars"), "list")
  expect_is(opt.roc("mtcars"), "list")
  expect_is(opt.simpledown("mtcars"), "list")
})
