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
