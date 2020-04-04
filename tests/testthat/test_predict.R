library("zeallot")
context("fitSimpleLinearRegressionOLS")
context("predict.R")
context("utils.R")

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

test_that("The uncertainty of predicted responses is estimated correctly", {

	# Calibration
	calib.data <- data.matrix(read.csv('calibration_data.txt',
		header = FALSE, sep = ''))

	c(analytes, signals) %<-% createDataPoints(calib.data)

	a <- fitSimpleLinearRegressionOLS(analytes, signals)
	slope <- a[[1]]
	intercept <- a[[4]]
	gamma <- a[[9]]

	# Predict analyte concentrations
	test.data <- data.matrix(read.csv('test_data.txt',
		header = FALSE, sep = ''))

	predicted <- predictAnalyteConcentrations(test.data, slope, intercept)

	# Estimate uncertainty
	c(devstud, dev, reldev) %<-% estimateUncertaintyOfPredictedValues(
		predicted, test.data, analytes, signals, gamma, slope)
	devstud <- abs(devstud)

	# Testing
	tol <- 1e-3
	expect_equal(devstud, c(  .113910,  .106010,  .103610,  .106140), tolerance = tol)
	expect_equal(dev    , c(  .053712,  .049986,  .048856,  .050049), tolerance = tol)
	expect_equal(reldev , c(12.348900, 3.287700, 1.910100, 1.427300), tolerance = tol)
})
