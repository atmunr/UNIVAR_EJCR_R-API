context("ftest.R")

test_that("F-test figures look right", {

	samples <- data.matrix(read.csv('calibration_data.txt',
		header = FALSE, sep = ''))[, -1]
	devres <- 0.1032031

	a <- runFTest(samples, devres)
	tol <- 1e-3

	expect_equal(a[[1]],  .08212457, tolerance = tol)
	expect_equal(a[[2]], 1.57920800, tolerance = tol)
	expect_equal(a[[3]], 2.59888100, tolerance = tol)
	expect_equal(a[[4]],  .21400000, tolerance = tol)
	expect_equal(a[[5]], TRUE)
})

