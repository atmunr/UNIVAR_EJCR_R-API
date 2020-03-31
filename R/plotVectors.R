library(ggplot2)

# Returns a scatter plot of two vectors, x and y. Can also add labels, a title
# and a line :^)
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
