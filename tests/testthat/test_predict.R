context("fitSimpleLinearRegressionOLS")
context("predict.R")

test_that("The analyte concentrations are predicted correctly", {

	# Calibration
	calib.data <- data.matrix(read.csv('calibration_data.txt',
		header = FALSE, sep = ''))

	analytes <- calib.data[, 1]
	signals  <- calib.data[, 2]
	for (i in 3 : ncol(calib.data)) {
		analytes <- c(analytes, calib.data[, 1])
		signals <- c(signals, calib.data[, i])
	}

	# Prediction
	a <- fitSimpleLinearRegressionOLS(analytes, signals)
	slope <- a[[1]]
	intercept <- a[[4]]

	test.data <- data.matrix(read.csv('test_data.txt',
		header = FALSE, sep = ''))
	
	# Testing
	tol <- 1e-3
	expect_equal(predictAnalyteConcentrations(test.data, slope, intercept),
		c(.4349502, 1.5203981, 2.5577725, 3.5065905), tolerance = tol)
})
