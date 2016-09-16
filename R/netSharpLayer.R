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

#' @export
is.netSharpLayer <- function(x){
  inherits(x, "netSharpLayer")
}

#' @export
as.netSharpLayer <- function(x, name, shape){
  class(x) <- c("netSharpLayer", "character")
  if(!missing(name) && !is.null(name)) attr(x, "name") <- name
  if(!missing(shape) && !is.null(shape)) attr(x, "shape") <- shape
  x
}

#' @export
as.character.netSharpLayer <- function(x){
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


#  ------------------------------------------------------------------------


# hidden conv1 [48, 24, 24] rlinear from pixels convolve {
# InputShape  = [3, 50, 50];
# KernelShape = [3,  5,  5];
# Stride      = [1,  2,  2];
# LowerPad    = [0, 1, 1];
# Sharing     = [T, T, T];
# MapCount    = 48;
# }


#' Construct input layer for NET# definition.
#' 
#' @param shape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}
#' @param name Name of the layer
layer_input <- function(shape, name = "layer_input"){
  z <- sprintf("input %s %s;", name, layer_shape(shape))
  as.netSharpLayer(z, name = name, shape = shape)
}


#' Construct a convolution layer.
#' 
#' @export
#' @inheritParams layer_input
#' @example inst/examples/example_netSharpLayer.R
layer_conv <- function(layer, kernelshape, inputshape, 
                       name, inputname, 
                       stride, padding, 
                       mapcount,
                       activation = "rlinear"){
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
  calc_outshape <- function(){
    z <- (inputshape + padding - kernelshape) / stride + 1
    z
  }
  outputshape <- calc_outshape()
  padding <- ceiling(ceiling(outputshape) - outputshape)
  outputshape <- calc_outshape()
  outputshape <- outputshape[outputshape > 1]
  if(!missing(mapcount) && !is.null(mapcount)) outputshape <- c(mapcount, outputshape)
  
  
  convolution <- sprintf("  InputShape  = %s;\n  KernelShape = %s;\n  Stride      = %s;\n  Padding     = %s;",
                         layer_shape(inputshape),
                         layer_shape(kernelshape),
                         layer_shape(stride),
                         layer_shape(padding)
  )
  if(!missing(mapcount) && !is.null(mapcount)) {
    convolution <- sprintf("%s\n  Mapcount = %s;", convolution, mapcount)
  }
  
  z <- sprintf("hidden %s %s %s from %s convolve {\n%s\n}", 
               name, 
               layer_shape(outputshape),
               activation,
               inputname,
               convolution
  )
  z <- sprintf("%s\n\n%s", as.character(layer), z)
  as.netSharpLayer(z, name, outputshape)
}


# hidden hid1 [256] rlinear from pool1 all;

#' @export
layer_full <- function(layer, nodes,
                       name, inputname,
                       activation = "rlinear"){
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


# output Class [6] from hid2 all;


#' Define output layer.
#' 
#' @export
layer_output <- function(layer, nodes,
                     name, inputname,
                     activation = "rlinear"){
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
