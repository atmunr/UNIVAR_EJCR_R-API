source("R/utils.R")

# Predict analyte concentrations by using a linear model.
predictAnalyteConcentrations <- function (replicates, slope, intercept) {
	
	signalmeans <- getMeansOfReplicates(replicates)

	predicted <- c()
	for (s in signalmeans) {
		predicted <- c(predicted, (s - intercept) / slope)
	}
	
	return (predicted)
}

# Estimates the standard deviation of the predicted values for the analytes,
# based on their signals, and the data set used for calibration.
estimateUncertaintyOfPredictedValues <- function
(predicted_analytes, replicates, calib_analytes, calib_signals, gamma, slope) {

	n_testsamples <- nrow(replicates)
	n_calib_datapoints <- length(calib_signals)

	deviation <- c()
	for (i in 1 : n_testsamples) {
		signals <- replicates[i,]
		n_replicates <- sum(!is.na(signals))

		s <- (1 / gamma) * sqrt(1 / n_replicates
			+ (1 / n_calib_datapoints)
			+ ((mean(signals) - mean(calib_signals)) ^ 2)
			/ ((slope ^ 2) * sum((calib_analytes - mean(calib_analytes)) ^ 2))
		)
		deviation <- c(deviation, s)
	}

	relative_deviation <- deviation * 100 / abs(predicted_analytes)

	t <- qt(.025, df = n_calib_datapoints - 2)
	return (list(deviation * t, deviation, relative_deviation))
}
