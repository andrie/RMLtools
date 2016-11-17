# convolution -------------------------------------------------------------



#' Construct a convolution layer.
#' 
#' @export
#' @inheritParams nnlayer_input
#' 
#' @param layer A layer object, e.g. using \code{\link{nnlayer_input}}, or NULL
#' @param kernelshape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}
#' @param inputshape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.  If \code{layer} is specified, this can be NULL.
#' @param inputname Name of the preceding layer. If \code{layer} is specified, this can be NULL.
#' @param stride Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.
#' @param lowerpad Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.
#' @param sharing Logical vector.
#' @param mapcount Number of maps to create.
#' @param activation Activation function, e.g. \code{rlinear}
#' @param ... Other arguments passed from \code{\link{nnlayer_pool}} and \code{\link{nnlayer_norm}}
#' 
#' @family layer definition functions
#' @example inst/examples/example_netSharpLayer.R
nnlayer_conv <- function(layer, kernelshape, inputshape, 
                         name, inputname, 
                         stride, lowerpad, sharing,
                         mapcount = 1,
                         activation = c("rlinear"),
                         ...){
  
  mc <- as.list(match.call())[-1]
  if(is.null(mc$type)) type <- "convolve" else type <- mc$type
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
  if(missing(lowerpad) || is.null(lowerpad)) lowerpad <- rep(0, length(inputshape))
  if(type != "response norm") if(missing(sharing) || is.null(sharing)) sharing <- rep(0, length(inputshape))
  
  calc_outshape <- function(){
    if(length(inputshape) != length(lowerpad)) warning(name, ": lowerpad != inputshape", call. = FALSE)
    if(length(inputshape) != length(kernelshape)) warning(name, ": kernelshape != inputshape", call. = FALSE)
    if(any(padding)){
      gap <- suppressWarnings((inputshape - kernelshape) / stride + 1)
      gap <- ceiling(gap) * stride - inputshape
      suppressWarnings((inputshape + gap - kernelshape) / stride + 1)
    } else {
      suppressWarnings((inputshape + lowerpad - kernelshape) / stride + 1)
    }
    
  }
  # Compute required padding
  padding <- rep(FALSE, length(lowerpad))
  outputshape <- calc_outshape()
  lowerpad <- ceiling(ceiling(outputshape) - outputshape)
  
  if(any(lowerpad >= kernelshape * 0.5 )){
    padding <- rep(TRUE, length(lowerpad))
    lowerpad <- rep(0, length(lowerpad))
  }
  
  outputshape <- calc_outshape()
  
  # Add mapcount
  outputshape[1] <- mapcount * outputshape[1]
  
  # Construct output string
  convolution <- ""
  convolution <- sprintf(  "%s  InputShape  = %s;", convolution, nnlayer_shape(inputshape))
  convolution <- sprintf("%s\n  KernelShape = %s;", convolution, nnlayer_shape(kernelshape))
  if(any(stride != 1)){
    convolution <- sprintf("%s\n  Stride      = %s;", convolution, nnlayer_shape(stride))
  }
  if(any(lowerpad != 0)){
    convolution <- sprintf("%s\n  LowerPad    = %s;", convolution, nnlayer_shape(lowerpad))
  } else if(any(padding)){
    convolution <- sprintf("%s\n  Padding     = %s;", convolution, nnlayer_boolean(padding))
  }
  
  if(!type %in% c("response norm", "max pool") && any(!sharing)){ 
    convolution <- sprintf("%s\n  Sharing     = %s;",
                           convolution,
                           nnlayer_boolean(sharing)
    )
  }
  if(!missing(mapcount) && !is.null(mapcount)){
    convolution <- sprintf("%s\n  MapCount    = %s;", convolution, mapcount  )
  }
  if(type == "response norm"){
    convolution <- sprintf("%s\n  Alpha       = %s;", convolution, mc$alpha)
    convolution <- sprintf("%s\n  Beta        = %s;", convolution, mc$beta)
    convolution <- sprintf("%s\n  Offset      = %s;", convolution, mc$offset)
    convolution <- sprintf("%s\n  AvgOverFullKernel = %s;", convolution, mc$AvgOverFullKernel)
  }
  
  
  z <- sprintf("hidden %s %s %s from %s %s {\n%s\n}", 
               name, 
               nnlayer_shape(outputshape),
               activation,
               inputname,
               type,
               convolution
  )
  z <- sprintf("%s\n\n%s", as.character(layer), z)
  as.netSharpLayer(z, name, outputshape)
}
