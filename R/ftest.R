
#' Run an F-test on a list of replicate sets.
#'
#' Runs an F-test on a list of replicate sets. using the standard deviation
#' of residuals.
#'
#' @param replicate_sets A matrix where each row represents a list of replicate
#' signals. If there are some values missing in a sample, use NA instead.
#' @param deviation_residuals The residual standard deviation obtained
#' from calibration.
#'
#' @return Returns a list with five values.
#' \itemize {
#'   \item noise The estimated amount of noise.
#'   \item fexpect The resulting expectation value.
#'   \item critvalue The resulting critical value.
#'   \item pvalue The p-value.
#'   \item pass TRUE if the p-value is less than 0.05.
#' }
runFTest <- function (replicate_sets, deviation_residuals) {

	n_samples <- nrow(replicate_sets)
	n_datapoints <- n_samples * ncol(replicate_sets)

	# All signals, mean centered per sample
	signals.c <- c()
	for (i in 1 : n_samples) {
		replicates <- replicate_sets[i,]
		m <- mean(replicates)

		for (s in replicates) {
			signals.c <- c(signals.c, s - m)
		}
	}

	deviation <- sqrt(sum((signals.c) ^ 2)) ^ 2 / (n_datapoints - n_samples)
	noise <- sqrt(deviation)

	fexpect    <- deviation_residuals ^ 2 / deviation
	cumuldist  <- pf(fexpect, n_datapoints - 2 , n_datapoints - n_samples, lower.tail = FALSE)
	critfvalue <- qf(.95, n_datapoints - 2, n_datapoints - n_samples)
	pass       <- cumuldist > .05

	return (list(noise, fexpect, critfvalue, cumuldist, pass))
}
