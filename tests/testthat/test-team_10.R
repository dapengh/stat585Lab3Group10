context("test-team_10")

test_that("only takes correct shapefiles", {
  expect_error(team_10(ozbig, tolerance = 0))
  expect_error(team_10(ozbig, tolerance = TRUE))
  expect_error(team_10(ozbig,fileread=T))
  expect_error(team_10(file='./man/makedata.Rd'))
})

test_that("find geometry", {
  expect_error(team_10(list(ozbig[[1]],ozbig[[2]]),fileread=F))
})

test_that("output is a data frame",{
  expect_s3_class(team_10(ozbig,fileread=F), "data.frame")
})
