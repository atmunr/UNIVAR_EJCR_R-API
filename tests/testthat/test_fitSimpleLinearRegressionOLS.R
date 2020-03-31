context("fitSimpleLinearRegressionOLS")

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

	expect_equal(a[[1]], 1.3174, tolerance = tol)
	expect_equal(a[[2]], 0.014243, tolerance = tol)
	expect_equal(a[[3]], 0.030201, tolerance = tol)
	expect_equal(a[[4]], 0.12365, tolerance = tol)
	expect_equal(a[[5]], 0.043124, tolerance = tol)
	expect_equal(a[[6]], 0.091455, tolerance = tol)
	expect_equal(a[[8]], 0.1032, tolerance = tol)
})
