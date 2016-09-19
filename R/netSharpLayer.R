# netDefinition <- "
# const { T = true; F = false; }
# 
# 
# hidden rnorm1 [48, 11, 11] from conv1 response norm {
# InputShape  = [48, 24, 24];
# KernelShape = [1,   4,  4];
# Stride      = [1,   2,  2];
# LowerPad    = [0, 0, 0];
# Alpha       = 0.0001;
# Beta        = 0.75;
# }
# 
# hidden pool1 [48, 9, 9] from rnorm1 max pool {
# InputShape  = [48, 11, 11];
# KernelShape = [1, 3, 3];
# Stride      = [1, 1, 1];
# }
# 
# hidden hid1 [256] rlinear from pool1 all;
# hidden hid2 [256] rlinear from hid1 all;
# output Class [6] from hid2 all;
# "



# netSharpLayer methods ---------------------------------------------------

#' Create or test for objects of type netSharpLayer.
#' 
#' @export
#' @rdname netSharpLayer
is.netSharpLayer <- function(x){
  inherits(x, "netSharpLayer")
}

#' @rdname netSharpLayer
#' @export
as.netSharpLayer <- function(x, name, shape){
  class(x) <- c("netSharpLayer", "character")
  if(!missing(name) && !is.null(name)) attr(x, "name") <- name
  if(!missing(shape) && !is.null(shape)) attr(x, "shape") <- shape
  x
}

#' @export
as.character.netSharpLayer <- function(x, ...){
  paste(capture.output(cat(x)), collapse = "\n")
}

#' @export
print.netSharpLayer <- function(x, ...){
  attributes(x) <- NULL
  cat(x)
}

# input pixels [3, 50, 50];

layer_shape <- function(shape){
  sprintf("[%s]", paste(shape, collapse = ", "))
}

layer_boolean <- function(shape){
  sh <- as.logical(shape)
  idx <- sh
  sh[idx]  <- "true"
  sh[!idx] <- "false"
  
  sprintf("[%s]", paste(sh, collapse = ", "))
}


# input layer -------------------------------------------------------------

#' Construct input layer for NET# definition.
#' 
#' @param shape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}
#' @param name Name of the layer
#' @family layer definition functions
#' @export
#' @example inst/examples/example_netSharpLayer.R
layer_input <- function(shape, name = "layer_input"){
  z <- sprintf("input %s %s;", name, layer_shape(shape))
  as.netSharpLayer(z, name = name, shape = shape)
}


# convolution -------------------------------------------------------------

#' Construct a convolution layer.
#' 
#' @export
#' @inheritParams layer_input
#' 
#' @param layer A layer object, e.g. using \code{\link{layer_input}}, or NULL
#' @param kernelshape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}
#' @param inputshape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.  If \code{layer} is specified, this can be NULL.
#' @param inputname Name of the preceding layer. If \code{layer} is specified, this can be NULL.
#' @param stride Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.
#' @param padding Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.
#' @param mapcount Number of maps to create.
#' @param activation Activation function, e.g. \code{rlinear}
#' 
#' @family layer definition functions
#' @example inst/examples/example_netSharpLayer.R
layer_conv <- function(layer, kernelshape, inputshape, 
                       name, inputname, 
                       stride, padding, sharing,
                       mapcount,
                       activation = c("rlinear"),
                       ...){
  mc <- as.list(match.call())[-1]
  if(is.null(mc$type)) type <- "conv" else type <- mc$type
  if(is.na(activation)) activation <- "" else activation <- match.arg(activation)

  if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
    inputshape <- attr(layer, "shape")
    inputname  <- attr(layer, "name")
  } else {
    layer <- ""
  }
  if(any(kernelshape == 0)){
    stop("all components of kernelshape must be positive integers")
  }
  if(missing(stride)  || is.null(stride))  stride  <- rep(1, length(inputshape))
  if(missing(padding) || is.null(padding)) padding <- rep(0, length(inputshape))
  if(missing(sharing) || is.null(sharing)) sharing <- rep(0, length(inputshape))
  calc_outshape <- function(){
    z <- (inputshape + padding - kernelshape) / stride + 1
    z
  }
  outputshape <- calc_outshape()
  padding <- ceiling(ceiling(outputshape) - outputshape)
  outputshape <- calc_outshape()
  outputshape <- outputshape[outputshape > 1]
  if(!missing(mapcount) && !is.null(mapcount)) outputshape <- c(mapcount, outputshape)
  
  
  convolution <- sprintf("  InputShape  = %s;\n  KernelShape = %s;\n  Stride      = %s;\n  LowerPad    = %s;\n  Sharing     = %s;",
                         layer_shape(inputshape),
                         layer_shape(kernelshape),
                         layer_shape(stride),
                         layer_shape(padding),
                         layer_boolean(sharing)
  )
  if(!missing(mapcount) && !is.null(mapcount)) {
    convolution <- sprintf("%s\n  Mapcount = %s;", convolution, mapcount)
  }
  
  z <- sprintf("hidden %s %s %s from %s %s {\n%s\n}", 
               name, 
               layer_shape(outputshape),
               activation,
               inputname,
               type,
               convolution
  )
  z <- sprintf("%s\n\n%s", as.character(layer), z)
  as.netSharpLayer(z, name, outputshape)
}


# maximum pooling ---------------------------------------------------------

#' Create maximum pooling layer.
#' 
#' @export
#' @inheritParams  layer_conv
#' @family layer definition functions
layer_pool <- function(layer, kernelshape, inputshape, 
                         name, inputname, 
                         stride, padding, 
                         mapcount,
                         activation = NA,
                         ...){
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
  do.call(layer_conv, mc)

}

# response norm -----------------------------------------------------------

#' Create response normalization layer.
#' 
#' @export
#' @inheritParams  layer_conv
#' @family layer definition functions
layer_norm <- function(layer, kernelshape, inputshape, 
                       name, inputname, 
                       stride, padding, 
                       mapcount,
                       alpha,
                       beta,
                       activation = NA,
                       ...){
  mc <- as.list(match.call()[-1])
  if(is.null(mc$activation)) mc$activation <- NA
  if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
    inputshape <- attr(layer, "shape")
    inputname  <- attr(layer, "name")
  } else {
    layer <- NULL
  }
  if(!is.null(mc$layer) && mc$layer == ".") mc$layer <- layer
  mc$type <- "response norm"
  do.call(layer_conv, mc)
  
}


# fully connected ---------------------------------------------------------

#' Create fully connected layer.
#' 
#' @inheritParams layer_input
#' @inheritParams layer_conv
#' 
#' @param nodes Number of nodes in hidden layer
#' 
#' @export
#' @family layer definition functions
#' @example inst/examples/example_netSharpLayer.R
layer_full <- function(layer, nodes,
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


# layer_output ------------------------------------------------------------


# output Class [6] from hid2 all;


#' Define output layer.
#' 
#' @inheritParams layer_input
#' @inheritParams layer_conv
#' @inheritParams layer_full
#' @family layer definition functions
#' @example inst/examples/example_netSharpLayer.R
#' @export
layer_output <- function(layer, nodes,
                     name, inputname,
                     activation = c("sigmoid", "rlinear", "linear")
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

layer_compute_input_size <- function(formula, sampledata){
  z <- model.matrix(object = formula, data = head(sampledata))
  ncol(z)
}

layer_compute_input_size(Species ~ ., iris)
layer_compute_input_size(Sepal.Length ~ ., iris)
layer_compute_input_size(mpg ~ ., mtcars)
layer_compute_input_size(mpg ~ . + am:vs, mtcars)
