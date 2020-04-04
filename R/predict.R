
# Takes a set of replicates and returns a vector with the same length as the
# total number of sets of replicates. For each set of replicates, the output
# vector will have the mean of all the signals in that set.
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

# Predict analyte concentrations by using a linear model.
predictAnalyteConcentrations <- function (replicates, slope, intercept) {
	
	signalmeans <- getMeansOfReplicates(replicates)

	predicted <- c()
	for (s in signalmeans) {
		predicted <- c(predicted, (s - intercept) / slope)
	}
	
	return (predicted)
}
