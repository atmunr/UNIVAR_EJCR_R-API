source("R/utils.R")

#' Predict analyte concentrations by using a linear model.
#'
#' Takes a list of replicate sets and predicts the corresponding concentrations
#' of analytes for each of them, by taking their means and feeding them into a
#' linear function with slope and intercept, provided in the arguments.
#'
#' @param replicate_sets A matrix where each row represents a list of replicate
#' signals. If there are some values missing in a sample, use NA instead.
#' @param slope Slope of the linear model.
#' @param intercept Intercept of the linear model.
#'
#' @return A vector where the i-th value is the predicted concentration of
#' analyte for the i-th row/sample.
predictAnalyteConcentrations <- function (replicate_sets, slope, intercept) {
	
	signalmeans <- getMeansOfReplicates(replicate_sets)

	predicted <- c()
	for (s in signalmeans) {
		predicted <- c(predicted, (s - intercept) / slope)
	}
	
	return (predicted)
}

# Estimates the standard deviation of the predicted values for the analytes,
# based on their signals, and the data set used for calibration.

#' Estimate standard deviation of predicted analyte concentration values.
#'
#' Requires the predicted values for the analytes, the list of replicate sets
#' from which they were produced, the samples that were used in calibration to
#' produce the linear model that was applied in prediction, and the gamma
#' coefficient and line slope.
#'
#' @param A vector where the i-th value is the predicted concentration of
#' analyte for the i-th testing sample.
#'
#' @param replicate_sets A matrix where the i-th row holds the signal values
#' that correspond to the i-th testing sample.
#'
#' @param calib_analytes A vector with all calibration analyte concentrations,
#' in the order in which the samples appear, and each one repeated for as many
#' replicate signals it has.
#'
#' @param calib_signals A matrix where the j-th row holds the signal values
#' that correspond to the j-th calibration sample.
#'
#' @param gamma The gamma coefficient.
#' @param slope Slope of the linear model.
estimateUncertaintyOfPredictedValues <- function
(predicted_analytes, replicate_sets, calib_analytes, calib_signals, gamma, slope) {

	n_testsamples <- nrow(replicate_sets)
	n_calib_datapoints <- length(calib_signals)

	deviation <- c()
	for (i in 1 : n_testsamples) {
		signals <- replicate_sets[i,]
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
