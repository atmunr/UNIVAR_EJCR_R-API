context("runFTest")

test_that("F-test figures look right", {
	
	samples <- data.matrix(read.csv('calibration_data.txt',
		header = FALSE, sep = ''))[, -1]
	devres <- 0.1032031

	a <- runFTest(samples, devres)
	tol <- .01

	expect_equal(a[[1]], 0.08212457, tolerance = tol)
	expect_equal(a[[2]], 1.579208, tolerance = tol)
	expect_equal(a[[3]], 2.598881, tolerance = tol)
	expect_equal(a[[4]], 0.214, tolerance = tol)
	expect_equal(a[[5]], TRUE)
})

