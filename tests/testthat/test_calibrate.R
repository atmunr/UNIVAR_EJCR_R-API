context("calibrate.R")

test_that("Line is fit and other values are computed correctly", {

	calib.data <- data.matrix(read.csv('calibration_data.txt',
		header = FALSE, sep = ''))

	analytes <- calib.data[, 1]
	signals  <- calib.data[, 2]
	for (i in 3 : ncol(calib.data)) {
		analytes <- c(analytes, calib.data[, 1])
		signals <- c(signals, calib.data[, i])
	}

	a <- fitSimpleLinearRegressionOLS(analytes, signals)
	tol <- 1e-3

	expect_equal(a[[1]],  1.317400, tolerance = tol)
	expect_equal(a[[2]],   .014243, tolerance = tol)
	expect_equal(a[[3]],   .030201, tolerance = tol)
	expect_equal(a[[4]],   .123650, tolerance = tol)
	expect_equal(a[[5]],   .043124, tolerance = tol)
	expect_equal(a[[6]],   .091455, tolerance = tol)
	expect_equal(a[[8]],   .103200, tolerance = tol)
	expect_equal(a[[9]], 12.765400, tolerance = tol)
	expect_equal(a[[10]],  .148190, tolerance = tol)
	expect_equal(a[[11]],  .296380, tolerance = tol)
	expect_equal(a[[12]],  .849010, tolerance = tol)
	expect_equal(a[[13]],  .999070, tolerance = tol)
})
