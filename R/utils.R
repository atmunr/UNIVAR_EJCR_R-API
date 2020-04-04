
# Takes a set of replicates and returns a vector with the same length as the
# total number of sets of replicates. For each set of replicates, the output
# vector will have the mean of all the signals in that set.
#
# Example:
# >> getMeansOfReplicates(matrix(1 : 8, ncol = 2))
# >> [1] 3 4 5 6
getMeansOfReplicates <- function (replicates) {
	means <- c()
	for (i in 1 : nrow(replicates)) {
		means <- c(means, mean(replicates[i,]))
	}
	return (means)
}

# Takes a set of samples and returns two vectors: one for the analytes
# and one for the signals. Each analyte will appear as many times as
# its corresponding sample contains replicate signals.
#
# Example:
# >> createDataPoints(matrix(1 : 9), ncol = 3)
# >> [[1]]
# >> [1] 1 1 2 2 3 3
# >>
# >> [[2]]
# >> [1] 4 7 5 8 6 9
createDataPoints <- function (samples) {
	analytes <- c()
	signals  <- c()
	for (i in 1 : nrow(samples)) {
		for (j in 2 : ncol(samples)) {
			analytes <- c(analytes, samples[i,1])
			signals  <- c(signals,  samples[i,j])
		}
	}
	return (list(analytes, signals))
}
