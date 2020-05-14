
#' Get means of sets of replicates.
#'
#' Takes a set of replicates and returns the means of their signals.
#'
#' @param replicates A matrix of numbers where each row represents a set of
#' replicates. 
#'
#' @return A vector with length equal to the number of replicate sets. The i-th
#' value in the vector is the mean of the signals in the i-th set (the i-th row
#' of the matrix).
#'
#' @examples
#' getMeansOfReplicates(matrix(1 : 8, ncol = 2))
#' >> [1] 3 4 5 6
#'
getMeansOfReplicates <- function (replicates) {
	means <- c()
	for (i in 1 : nrow(replicates)) {
		means <- c(means, mean(replicates[i,]))
	}
	return (means)
}

#' Example:
#' Create data points from a set of samples.
#'
#' Takes a set of samples and returns a series of data points as two vectors.
#'
#' @param samples A matrix of numbers where each row represents a sample.
#' Each sample contains the analyte concentration as its first value, followed
#' by all of the corresponding replicate signals. If a replicate is missing, it
#' can be replaced with NA.
#'
#' @return Returns two vectors: one for the analytes and one for the signals.
#' Each analyte will appear as many times as its corresponding sample contains
#' replicate signals.
#'
#' @examples
#' createDataPoints(matrix(1 : 9), ncol = 3)
#' >> [[1]]
#' >> [1] 1 1 2 2 3 3
#' >>
#' >> [[2]]
#' >> [1] 4 7 5 8 6 9
#'
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
