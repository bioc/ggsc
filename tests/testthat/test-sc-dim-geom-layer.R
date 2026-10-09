## Coverage for the `sc_dim_geom_*` layer helpers.  A hand-built plot is used
## instead of a real `sc_dim()` figure so the tests stay fast and do not need
## the scater/scuttle toolchain.

dim_layer_plot <- function(n = 30) {
    set.seed(1)
    d <- data.frame(
        .BarcodeID = sprintf("cell%03d", seq_len(2 * n)),
        UMAP1 = c(rnorm(n), rnorm(n) + 4),
        UMAP2 = rnorm(2 * n),
        cluster = rep(c("a", "b"), each = n),
        stringsAsFactors = FALSE
    )
    ggplot2::ggplot(d, ggplot2::aes(x = UMAP1, y = UMAP2, colour = cluster)) +
        ggplot2::geom_point()
}

test_that(".dim_xy_vars() returns the two reduction coordinates", {
    p <- dim_layer_plot()
    expect_identical(.dim_xy_vars(p), c("UMAP1", "UMAP2"))
})

test_that(".dim_xy_vars() falls back to the positional columns", {
    ## a layout without the barcode column keeps the old positional behaviour
    d <- data.frame(id = 1:4, A = 1:4, B = 4:1)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = A, y = B))
    expect_identical(.dim_xy_vars(p), c("A", "B"))
})

test_that(".group_center() returns the group centre, not a sampled average", {
    set.seed(1)
    d <- data.frame(x = rnorm(30), y = rnorm(30))
    ctr <- .group_center(d, vars = c("x", "y"))
    expect_equal(nrow(ctr), 1L)
    expect_equal(unname(unlist(ctr)), unname(MASS::cov.trob(d)$center))
})

test_that(".group_center() reports too few points instead of failing", {
    d <- data.frame(x = 1:2, y = 1:2)
    expect_message(ctr <- .group_center(d, vars = c("x", "y")),
                   "Too few points")
    expect_true(all(is.na(unlist(ctr))))
})

test_that("sc_dim_geom_ellipse() adds one ellipse per group", {
    p <- dim_layer_plot()
    built <- ggplot2::ggplot_build(p + sc_dim_geom_ellipse())

    ell <- built$data[[2]]
    expect_equal(length(unique(ell$group)), 2L)
    expect_true(all(c("x", "y", "group") %in% names(ell)))
})

test_that("sc_dim_geom_ellipse() accepts ggforce::geom_mark_hull", {
    skip_if_not_installed("ggforce")
    p <- dim_layer_plot()
    expect_silent(ggplot2::ggplot_build(
        p + sc_dim_geom_ellipse(geom = ggforce::geom_mark_hull)
    ))
})

test_that("sc_dim_geom_label() labels one centre per group", {
    p <- dim_layer_plot()
    built <- ggplot2::ggplot_build(p + sc_dim_geom_label())

    lab <- built$data[[2]]
    expect_setequal(lab$label, c("a", "b"))
})

test_that("sc_dim_geom_ellipse() explains a numeric grouping column", {
    d <- data.frame(.BarcodeID = "c1", UMAP1 = 1:4, UMAP2 = 4:1, v = 1:4)
    p <- ggplot2::ggplot(d, ggplot2::aes(x = UMAP1, y = UMAP2, colour = v)) +
        ggplot2::geom_point()
    expect_error(p + sc_dim_geom_ellipse(), "group")
})

test_that("sc_dim_geom_label() explains that `data` must not be supplied", {
    p <- dim_layer_plot()
    expect_error(p + sc_dim_geom_label(data = data.frame(a = 1:3)),
                 "must not be supplied")
})
