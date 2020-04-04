
# Fits a simple linear regression line by ordinary least squares on some x and
# y vectors. Returns the fitted line and some figures of merit.
fitSimpleLinearRegressionOLS <- function (x, y) {

	corcoef <- cor(x, y)

	xc <- x - mean(x)
	yc <- y  - mean(y)

	slope <- sum(xc * yc) / sum(xc ^ 2)
	intercept <- mean(y) - (slope * mean(x))

	residuals <- (slope * x) + intercept - y
	residuals_deviation <- sqrt(sum(residuals ^ 2)) / sqrt(length(x) - 2)

	slope_deviation <- residuals_deviation / sqrt(sum(xc ^ 2))
	intercept_deviation <- residuals_deviation * sqrt((1 / length(x))
		+ ((mean(x) ^ 2) / sum(xc ^ 2)))

	gamma   <- abs(slope) / residuals_deviation
	s0 <- (1 / gamma) * sqrt(1 + (1 / length(x))
		+ ((mean(x) ^ 2) / sum((x - mean(x)) ^ 2))
	)
	decisionlim <- qt(0.95, df = length(x) - 2) * s0
	detectlim   <- 2 * decisionlim
	quantlim    <- 10 * s0

	t <- qt(.975, df = length(x) - 2)
	return (list(
		slope, slope_deviation, slope_deviation * t,
		intercept, intercept_deviation, intercept_deviation * t,
		residuals, residuals_deviation,
		gamma,
		decisionlim, detectlim, quantlim, corcoef
	))
}
