# input layer -------------------------------------------------------------

#' Construct input layer for NET# definition.
#'
#' @param shape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}
#' @param name Name of the layer
#'
#'
#' @family layer definition functions
#' @export
#' @example inst/examples/example_netSharpLayer.R
nnlayer_input <- function(shape, name = "nnlayer_input"){
  z <- sprintf("input %s %s;", name, nnlayer_shape(shape))
  as.netSharpLayer(z, name = name, shape = shape)
}



# maximum pooling ---------------------------------------------------------

#' Create maximum pooling layer.
#'
#' @inheritParams nnlayer_input
#' @inheritParams  nnlayer_conv
#' @export
#' @family layer definition functions
nnlayer_pool <- function(layer, kernelshape, inputshape,
                         name, inputname,
                         stride, lowerpad,
                         mapcount,
                         activation = NA
                         ){
  mc <- as.list(match.call()[-1])
  if(is.null(mc$activation)) mc$activation <- NA
  if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
    inputshape <- attr(layer, "shape")
    inputname  <- attr(layer, "name")
  } else {
    layer <- ""
  }
  if(!is.null(mc$layer) && mc$layer == ".") mc$layer <- layer
  mc$type <- "max pool"
  do.call(nnlayer_conv, mc)

}

# response norm -----------------------------------------------------------

#' Create response normalization layer.
#'
#' @inheritParams  nnlayer_conv
#' @inheritParams nnlayer_input
#' @param alpha Alpha
#' @param beta  Beta
#' @param offset Offset
#' @param AvgOverFullKernel Logical. Take average over full kernel.
#' @export
#' 
#' @family layer definition functions
nnlayer_norm <- function(layer, kernelshape, inputshape,
                         name, inputname,
                         stride, lowerpad,
                         alpha = 0.0001,
                         beta = 0.75,
                         offset = 1,
                         AvgOverFullKernel = TRUE,
                         activation = NA
                         ){
  mc <- as.list(match.call()[-1])
  if(is.null(mc$activation)) mc$activation <- NA
  if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
    inputshape <- attr(layer, "shape")
    inputname  <- attr(layer, "name")
  } else {
    layer <- NULL
  }
  if(missing(kernelshape) || is.null(kernelshape)) mc$kernelshape <- rep(1, length(inputshape))
  if(!is.null(mc$layer) && mc$layer == ".") mc$layer <- layer
  mc$type   <- "response norm"
  mc$alpha  <- alpha
  mc$beta   <- beta
  mc$offset <- offset
  mc$AvgOverFullKernel <- if(AvgOverFullKernel) "true" else "false"

  do.call(nnlayer_conv, mc)

}


# fully connected ---------------------------------------------------------

#' Create fully connected layer.
#'
#' @inheritParams nnlayer_input
#' @inheritParams nnlayer_conv
#' 
#' @param nodes Number of hidden nodes in layer.
#'
#' @export
#' @family layer definition functions
#' @example inst/examples/example_netSharpLayer.R
nnlayer_full <- function(layer, nodes,
                         name, inputname,
                         activation = c("sigmoid", "rlinear", "linear")
){
  activation <- match.arg(activation)
  if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
    inputname  <- attr(layer, "name")
  } else {
    layer <- ""
  }

  z <- sprintf("hidden %s [%s] %s from %s all;",
               name,
               nodes,
               activation,
               inputname
  )
  z <- sprintf("%s\n\n%s", as.character(layer), z)
  as.netSharpLayer(z, name = name)
}


# nnlayer_output ------------------------------------------------------------


# output Class [6] from hid2 all;


#' Define output layer.
#'
#' @inheritParams nnlayer_input
#' @inheritParams nnlayer_conv
#' @inheritParams nnlayer_full
#' @family layer definition functions
#' @example inst/examples/example_netSharpLayer.R
#' @export
nnlayer_output <- function(layer, nodes,
                           name, inputname,
                           activation = c("sigmoid", "rlinear", "linear", "softmax")
){
  activation <- match.arg(activation)
  if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
    inputname  <- attr(layer, "name")
  } else {
    layer <- ""
  }

  z <- sprintf("output %s [%s] %s from %s all;",
               name,
               nodes,
               activation,
               inputname
  )
  z <- sprintf("%s\n\n%s", as.character(layer), z)
  as.netSharpLayer(z, name = name)
}

nnlayer_compute_input_size <- function(formula, sampledata){
  z <- model.matrix(object = formula, data = head(sampledata))
  ncol(z)
}

nnlayer_compute_input_size(Species ~ ., iris)
nnlayer_compute_input_size(Sepal.Length ~ ., iris)
nnlayer_compute_input_size(mpg ~ ., mtcars)
nnlayer_compute_input_size(mpg ~ . + am:vs, mtcars)
