# Fits a simple linear regression line by ordinary least squares on some x and
# y vectors. Returns the fitted line and some figures of merit.
fitSimpleLinearRegressionOLS <- function (x, y) {

	corcoef <- cor(x,y)

	xc <- x - mean(x)
	yc <- y  - mean(y)

	A <- sum(xc * yc) / sum(xc ^ 2) # Line slope
	B <- mean(y) - (A * mean(x))    # Line intercept

	# Residues and deviation of residuals
	E <- (A * x) + B - y
	s_E <- sqrt(sum(E ^ 2)) / sqrt(length(x) - 2)

	# Estimated standard deviation in slope and intercept
	s_A <- s_E / sqrt(sum(xc ^ 2))
	s_B <- s_E * sqrt((1 / length(x)) + ((mean(x) ^ 2) / sum(xc ^ 2)))

	t <- qt(.975, df = length(x) - 2)

	# Figures of merit
	gamma   <- abs(A) / s_E
	s0 <- (1 / gamma) * sqrt(1 + (1 / length(x))
		+ ((mean(x) ^ 2) / sum((x - mean(x)) ^ 2))
	)

	# Detection, decision and quantitation limits
	decisionlim <- qt(0.95, df = length(x) - 2) * s0
	detectlim   <- 2 * decisionlim
	quantlim    <- 10 * s0

	return (list( A, s_A, t*s_A, B, s_B, t*s_B, E, s_E,
		gamma, decisionlim, detectlim, quantlim, corcoef))
}

