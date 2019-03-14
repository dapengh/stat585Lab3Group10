context("test-team_12")

test_that("only takes correct shapefiles", {
  expect_error(team_12(ozbig,fileread=T))
  expect_error(team_12(file='./man/makedata.Rd'))
})

test_that("find geometry", {
  expect_error(team_12(list(ozbig[[1]],ozbig[[2]]),fileread=F))
})
