context("fitSimpleLinearRegressionOLS")

test_that("Line is fit and other values are computed correctly", {

	analytes <- c(0, 1, 2, 3,  4,  5)
	signals  <- c(2, 4, 6, 7, 10, 12)

	a <- fitSimpleLinearRegressionOLS(analytes, signals)
	tol <- 1e-7

	expect_equal(a[[1]], 1.9714286, tolerance = tol)
	expect_equal(a[[2]], 0.10816968, tolerance = tol)
	expect_equal(a[[3]], -0.30032719, tolerance = tol)
	expect_equal(a[[4]], 1.9047619, tolerance = tol)
	expect_equal(a[[5]], 0.32749998, tolerance = tol)
	expect_equal(a[[6]], -0.90928571, tolerance = tol)
	expect_equal(a[[7]], tolerance = tol,
		c(-0.095238095, -0.123809524, -0.152380952, 0.819047619, -0.209523810, -0.238095238))
	expect_equal(a[[8]], 0.45250625, tolerance = tol)
})
