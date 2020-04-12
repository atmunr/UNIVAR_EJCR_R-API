library('zeallot')
context('fitSimpleLinearRegressionOLS')
context('predict.R')
context('utils.R')

# Calibration
calib_data <- data.matrix(read.csv('calibration_data.txt',
	header = FALSE, sep = ''))

c(analytes, signals) %<-% createDataPoints(calib_data)

c(
	slope, ., .,
	intercept, ., ., ., .,
	gamma, ...
) %<-% fitSimpleLinearRegressionOLS(analytes, signals)

# Prediction
test.data <- data.matrix(read.csv('test_data.txt',
	header = FALSE, sep = ''))

predicted <- predictAnalyteConcentrations(test.data, slope, intercept)

# Estimating uncertainty
c(devstud, dev, reldev) %<-% estimateUncertaintyOfPredictedValues(
	predicted, test.data, analytes, signals, gamma, slope)
devstud <- abs(devstud)

# Testing
tol <- 1e-3

test_that('The analyte concentrations are predicted correctly', {
	expect_equal(predictAnalyteConcentrations(test.data, slope, intercept),
		c(.4349502, 1.5203981, 2.5577725, 3.5065905), tolerance = tol)
})

test_that('The uncertainty of predicted responses is estimated correctly', {
	expect_equal(devstud, c(  .113910,  .106010,  .103610,  .106140), tolerance = tol)
	expect_equal(dev    , c(  .053712,  .049986,  .048856,  .050049), tolerance = tol)
	expect_equal(reldev , c(12.348900, 3.287700, 1.910100, 1.427300), tolerance = tol)
})
