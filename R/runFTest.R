
# Runs an F-test on a set of signals and the deviation of residuals.
# Each sample of signals is a row in a 2D matrix.
runFTest <- function (samples, devres) {

	if (ncol(samples) < 2) { return (list(NULL, NULL, NULL, NULL)) }

	# Total number of data points
	dp <- nrow(samples) * ncol(samples)

	i <- 1 # means[i,j] is the mean of the i-th sample.
	means <- matrix(nrow = nrow(samples), ncol = ncol(samples))

	for (signals in as.list(data.frame(t(samples)))) {
		signals    <- signals[!is.na(signals)] # Filter NAs
		means[i, ] <- c(
			mean(signals) * rep(1, length(signals)),
			rep(0, ncol(samples) - length(signals))
		)
		i <- i + 1
	}
	means <- c(means)

	signals <- samples[, 1] # Signals, replacing NAs with zeros
	for (i in 2 : ncol(samples)) { signals <- c(signals, samples[, i]) }
	signals[is.na(signals)] <- 0

	# Standard deviation of signals and estimated noise level
	dev   <- sqrt(sum((signals - means) ^ 2)) ^ 2 / (dp - nrow(samples))
	noise <- sqrt(dev)

	# Expected value, p-value and critical f value
	fexpect    <- devres ^ 2 / dev
	pvalue     <- pf(fexpect, dp -2 , dp - nrow(samples), lower.tail = FALSE)
	critfvalue <- qf(.95, dp - 2, dp - nrow(samples))
	pass       <- pvalue > .05

	return (list(noise, fexpect, critfvalue, pvalue, pass))
}
