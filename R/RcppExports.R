# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Two-Dimensional Weighted Kernel Density Estimation And Mapping the Result To Original Dimension
#' @param x The 2-D coordinate matrix
#' @param w The weighted sparse matrix, the number columns the same than the number rows than x.
#' @param l The limits of the rectangle covered by the grid as c(xl, xu, yl, yu)
#' @param h The vector of bandwidths for x and y directions, defaults to normal reference bandwidth
#' (see bandwidth.nrd), A scalar value will be taken to apply to both directions (see ks::hpi).
#' @param adjust numeric value to adjust to bandwidth, default is 1.
#' @param n number of grid points in the two directions, default is 400.
CalWkdeCpp <- function(x, w, l, h, adjust = 1.0, n = 400L) {
    .Call('_ggsc_CalWkdeCpp', PACKAGE = 'ggsc', x, w, l, h, adjust, n)
}

