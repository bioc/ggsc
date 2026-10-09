.buildWkde <- function(w, coords, n = 400, joint = FALSE, joint.fun = prod){
   rlang::check_installed(c('ks', 'Matrix'), 'for the 2D weighted kernel density estimation.')
   if (inherits(w, 'matrix')){
     w <- Matrix::Matrix(w, sparse = TRUE)
   }
   lims <- c(range(coords[,1]), range(coords[,2]))
   h <- c(ks::hpi(coords[,1]), ks::hpi(coords[,2]))
   res <- CalWkdeCpp(x=as.matrix(coords[,seq(2),drop=FALSE]), w=w, l=lims, h = h, n = n)
   colnames(res) <- rownames(w)
   rownames(res) <- colnames(w)
   if (joint && !is.null(joint.fun)){
     oldcnm <- colnames(res)
     clnm <- paste(colnames(res), collapse="+")
     joint.res <- apply(res, 1, joint.fun) 
     res <- cbind(res, joint.res)
     colnames(res) <- c(oldcnm, clnm)
   }
   return(res)
}

.split.by.feature <- function(p, ncol, joint = FALSE){
   rlang::check_installed('aplot', 'for split ggplot object by features.')
   p <- p$data |> dplyr::group_split(.data$features) |>
           lapply(function(i){
              p$data <- i
              p <- .add_class(p, "ggsc")
              return(p)
            })

   indx <- length(p)
   
   if (joint){
      p[[indx]] <- p[[indx]] + ggplot2::labs(colour = 'joint_density')
   }
   
   p <- aplot::plot_list(gglist = p, ncol = ncol)
   return(p)
}

#' @importFrom ggfun get_aes_var
.cal_pie_radius <- function(data, mapping){
   x <- ggfun::get_aes_var(mapping, 'x') 
   y <- ggfun::get_aes_var(mapping, 'y')
   r = (max(data[[x]], na.rm=TRUE) - min(data[[x]], na.rm=TRUE)) * (max(data[[y]], na.rm=TRUE) - min(data[[y]], na.rm=TRUE))
   r = sqrt(r / nrow(data) / pi) * .85
   return(r)
}

.cal_ratio <- function(data, mapping){
   x <- ggfun::get_aes_var(mapping, 'x')
   y <- ggfun::get_aes_var(mapping, 'y')
   1*max(data[[x]], na.rm=TRUE)/max(data[[y]], na.rm=TRUE)
}

.set_default_cols <- function(n){
    col2 <- c("#1f78b4", "#ffff33", "#c2a5cf", "#ff7f00", "#810f7c",
              "#a6cee3", "#006d2c", "#4d4d4d", "#8c510a", "#d73027",
              "#78c679", "#7f0000", "#41b6c4", "#e7298a", "#54278f")
    grDevices::colorRampPalette(col2)(n)
}

## The two reduction coordinates plotted by a `sc_dim()` figure.
##
## `extract_data()` returns the barcode column first and the reduction
## coordinates after it, so look the coordinates up by name rather than by
## position; fall back to the old positional columns for any other layout.
.dim_xy_vars <- function(plot) {
  nm <- names(plot$data)
  if (".BarcodeID" %in% nm) {
    xy <- setdiff(nm, ".BarcodeID")
  } else {
    ## keep the previous positional behaviour for other layouts
    xy <- nm[seq_len(min(3L, length(nm)))]
    xy <- xy[-1L]
  }
  if (length(xy) < 2L) {
    cli::cli_abort("`plot` must contain at least two reduction dimensions.")
  }
  xy[seq_len(2L)]
}

## Centre of each group, used by `sc_dim_geom_label()` to position labels.
##
## This used to build a 51-point ellipse outline and then average the sampled
## boundary points.  `chol()` returns an upper-triangular factor, so a uniform
## grid of angles maps to a non-uniformly sampled ellipse: the average of the
## boundary points is offset from the true centre, and the offset scales with
## the ellipse radius -- i.e. labels drifted with `level`, even though no
## ellipse is drawn here.  The centre is available directly.
.group_center <- function(data, vars){
  if (nrow(data) - 1 < 3) {
    cli::cli_inform("Too few points to calculate an ellipse")
    res <- as.data.frame(matrix(NA_real_, nrow = 1, ncol = length(vars)))
    colnames(res) <- vars
    return(res)
  }
  v <- MASS::cov.trob(data[, vars])
  res <- as.data.frame(matrix(v$center, nrow = 1))
  colnames(res) <- vars
  return(res)
}


#' @importFrom ggplot2 ggplot_build
.check_colour <- function(x, y){
  gb <- ggplot_build(x)
  flag1 <- gb$plot$scales$has_scale('colour')
  if (flag1){
     flag1 <- inherits(gb$plot$scales$get_scales("colour"), "ScaleContinuous")
  }
  flag2 <- any(c("color", "colour") %in% names(y$mapping)) || any(c("color", "colour") %in% names(y))
  flag1 && !flag2
}


