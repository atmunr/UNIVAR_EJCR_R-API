# Fits a simple linear regression line by ordinary least squares on some x and
# y vectors. Returns the fitted line and some figures of merit.
fitSimpleLinearRegressionOLS <- function (x, y) {

	corcoef <- cor(x, y)

	xc <- x - mean(x)
	yc <- y  - mean(y)

	slope <- sum(xc * yc) / sum(xc ^ 2)
	intercept <- mean(y) - (slope * mean(x))

	# Residues and deviation of residuals
	residues <- (slope * x) + intercept - y
	s.residues <- sqrt(sum(residues ^ 2)) / sqrt(length(x) - 2)

	# Estimated standard deviation in slope and intercept
	s.slope <- s.residues / sqrt(sum(xc ^ 2))
	s.intercept <- s.residues * sqrt((1 / length(x))
		+ ((mean(x) ^ 2) / sum(xc ^ 2)))

	# Gamma coefficient, detection, decision and quantitation limits
	gamma   <- abs(slope) / s.residues
	s0 <- (1 / gamma) * sqrt(1 + (1 / length(x))
		+ ((mean(x) ^ 2) / sum((x - mean(x)) ^ 2))
	)
	decisionlim <- qt(0.95, df = length(x) - 2) * s0
	detectlim   <- 2 * decisionlim
	quantlim    <- 10 * s0

	t <- qt(.975, df = length(x) - 2)
	return (list(
		slope, s.slope, s.slope * t,
		intercept, s.intercept, s.intercept * t,
		residues, s.residues,
		gamma,
		decisionlim, detectlim, quantlim, corcoef
	))
}
