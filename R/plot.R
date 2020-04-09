library(ggplot2)

#' Make a scatter plot with two vectors.
#'
#' Returns a scatter plot of two vectors. This function also accepts multiple
#' optional arguments to decorate the plot by adding a title, labels on the
#' axes, and a slope and intercept to draw some line inside the graph with.
#' 
#' @param x Vector of values that will be plotted on the horizontal axis.
#' @param y Vector of values that will be plotted on the vertical axis.
#' @param title A string - a name for the plot (optional).
#' @param xlabel A string - a label for the horizontal axis (optional).
#' @param ylabel A string - a label for the vertical axis (optional).
#' @param slope Slope of the line to be drawn over the plot, if desired.
#' @param intercept Intercept of the line to be drawn over the plot,
#' if desired.
plotVectors <- function (x, y, title, xlabel, ylabel, slope, intercept) {
	d <- data.frame(x, y)

	p <- ggplot(data = d)
	p <- p + geom_point(mapping = aes(x = x, y = y))

	if (!missing(title)) {
		p <- p + ggtitle(title)
		p <- p + theme(plot.title = element_text(hjust = .5))
	}

	if (!missing(xlabel)) p <- p + labs(x = xlabel)
	if (!missing(ylabel)) p <- p + labs(y = ylabel)

	if (!missing(slope) && !missing(intercept)) {
		p <- p + geom_abline(mapping = aes(slope = slope, intercept = intercept))
	}

	p
}
