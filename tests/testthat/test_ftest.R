context('ftest.R')

test_that('F-test figures look right', {

	samples <- data.matrix(read.csv('calibration_data.txt',
		header = FALSE, sep = ''))[, -1]
	devres <- 0.1032031

	c(
		noise,
		fexpect,
		critfvalue,
		cumuldist,
		pass
	) %<-% runFTest(samples, devres)

	tol <- 1e-3
	expect_equal(noise     ,  .08212457, tolerance = tol)
	expect_equal(fexpect   , 1.57920800, tolerance = tol)
	expect_equal(critfvalue, 2.59888100, tolerance = tol)
	expect_equal(cumuldist ,  .21400000, tolerance = tol)
	expect_equal(pass, TRUE)
})

