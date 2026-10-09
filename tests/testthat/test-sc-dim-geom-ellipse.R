test_that("sc_dim_geom_ellipse() only forwards `level` to geoms that accept it", {
    ## stat_ellipse() has a `level` argument, so it is forwarded
    e1 <- sc_dim_geom_ellipse()
    expect_true("level" %in% names(e1))

    ## ggforce::geom_mark_hull() has no `level` argument.  Passing one made
    ## ggplot2 emit "Ignoring unknown parameters: `level`", so it is dropped.
    skip_if_not_installed("ggforce")
    e2 <- sc_dim_geom_ellipse(geom = ggforce::geom_mark_hull)
    expect_false("level" %in% names(e2))

    ## the documented alternative still carries the geom and mapping
    expect_identical(e2$geom, ggforce::geom_mark_hull)
    expect_null(e2$mapping)
})

test_that("sc_dim_geom_ellipse() still forwards extra arguments", {
    e <- sc_dim_geom_ellipse(foo = 1)
    expect_true("foo" %in% names(e))
    expect_identical(e$foo, 1)
})
