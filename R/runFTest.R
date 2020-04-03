
# Runs an f-test on a list of replicate sets, using
# the standard deviation of residuals, and returning some
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
	pvalue     <- pf(fexpect, n_datapoints -2 , n_datapoints - n_samples, lower.tail = FALSE)
	critfvalue <- qf(.95, n_datapoints - 2, n_datapoints - n_samples)
	pass       <- pvalue > .05

	return (list(noise, fexpect, critfvalue, pvalue, pass))
}
