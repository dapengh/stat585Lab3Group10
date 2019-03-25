context("test-team_1")

test_that("output is a data frame",{
  expect_s3_class(team_1(ozbig, tolerance=0.1, fileread = F), "data.frame")
})


test_that("only takes correct shapefiles", {
  expect_error(team_1(ozbig, tolerance = 0, fileread = F))
  expect_error(team_1(ozbig, tolerance = -1, fileread = F))
})
