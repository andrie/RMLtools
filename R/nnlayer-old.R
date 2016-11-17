# # input layer -------------------------------------------------------------
# 
# #' Construct input layer for NET# definition.
# #'
# #' @param shape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}
# #' @param name Name of the layer
# #' @family layer definition functions
# #' @export
# #' @example inst/examples/example_netSharpLayer.R
# nnlayer_input <- function(shape, name = "nnlayer_input"){
#   z <- sprintf("input %s %s;", name, nnlayer_shape(shape))
#   as.netSharpLayer(z, name = name, shape = shape)
# }
# 
# 
# # convolution -------------------------------------------------------------
# 
# #' Construct a convolution layer.
# #'
# #' @export
# #' @inheritParams nnlayer_input
# #'
# #' @param layer A layer object, e.g. using \code{\link{nnlayer_input}}, or NULL
# #' @param kernelshape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}
# #' @param inputshape Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.  If \code{layer} is specified, this can be NULL.
# #' @param inputname Name of the preceding layer. If \code{layer} is specified, this can be NULL.
# #' @param stride Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.
# #' @param padding Numeric vector describing the number of inputs in each dimension, e.g. \code{c(3, 15, 15)}.
# #' @param mapcount Number of maps to create.
# #' @param activation Activation function, e.g. \code{rlinear}
# #'
# #' @family layer definition functions
# #' @example inst/examples/example_netSharpLayer.R
# nnlayer_conv <- function(layer, kernelshape, inputshape,
#                          name, inputname,
#                          stride, padding, sharing,
#                          mapcount,
#                          activation = c("rlinear"),
#                          ...){
#   mc <- as.list(match.call())[-1]
#   if(is.null(mc$type)) type <- "convolve" else type <- mc$type
#   if(is.na(activation)) activation <- "" else activation <- match.arg(activation)
# 
#   if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
#     inputshape <- attr(layer, "shape")
#     inputname  <- attr(layer, "name")
#   } else {
#     layer <- ""
#   }
#   if(any(kernelshape == 0)){
#     stop("all components of kernelshape must be positive integers")
#   }
#   if(missing(stride)  || is.null(stride))  stride  <- rep(1, length(inputshape))
#   if(missing(padding) || is.null(padding)) padding <- rep(0, length(inputshape))
#   if(missing(sharing) || is.null(sharing)) sharing <- rep(0, length(inputshape))
#   calc_outshape <- function(){
#     z <- (inputshape + padding - kernelshape) / stride + 1
#     z
#   }
#   outputshape <- calc_outshape()
#   padding <- ceiling(ceiling(outputshape) - outputshape)
#   outputshape <- calc_outshape()
#   outputshape <- outputshape[outputshape > 1]
#   if(!missing(mapcount) && !is.null(mapcount)) outputshape <- c(mapcount, outputshape)
# 
# 
#   convolution <- sprintf("  InputShape  = %s;\n  KernelShape = %s;\n  Stride      = %s;\n  LowerPad    = %s;\n  Sharing     = %s;",
#                          nnlayer_shape(inputshape),
#                          nnlayer_shape(kernelshape),
#                          nnlayer_shape(stride),
#                          nnlayer_shape(padding),
#                          nnlayer_boolean(sharing)
#   )
#   if(!missing(mapcount) && !is.null(mapcount)) {
#     convolution <- sprintf("%s\n  Mapcount    = %s;", convolution, mapcount)
#   }
# 
#   z <- sprintf("hidden %s %s %s from %s %s {\n%s\n}",
#                name,
#                nnlayer_shape(outputshape),
#                activation,
#                inputname,
#                type,
#                convolution
#   )
#   z <- sprintf("%s\n\n%s", as.character(layer), z)
#   as.netSharpLayer(z, name, outputshape)
# }
# 
# 
# # maximum pooling ---------------------------------------------------------
# 
# #' Create maximum pooling layer.
# #'
# #' @export
# #' @inheritParams  nnlayer_conv
# #' @family layer definition functions
# nnlayer_pool <- function(layer, kernelshape, inputshape,
#                          name, inputname,
#                          stride, padding,
#                          mapcount,
#                          activation = NA,
#                          ...){
#   mc <- as.list(match.call()[-1])
#   if(is.null(mc$activation)) mc$activation <- NA
#   if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
#     inputshape <- attr(layer, "shape")
#     inputname  <- attr(layer, "name")
#   } else {
#     layer <- ""
#   }
#   if(!is.null(mc$layer) && mc$layer == ".") mc$layer <- layer
#   mc$type <- "max pool"
#   do.call(nnlayer_conv, mc)
# 
# }
# 
# # response norm -----------------------------------------------------------
# 
# #' Create response normalization layer.
# #'
# #' @export
# #' @inheritParams  nnlayer_conv
# #' @family layer definition functions
# nnlayer_norm <- function(layer, kernelshape, inputshape,
#                          name, inputname,
#                          stride, padding,
#                          mapcount,
#                          alpha,
#                          beta,
#                          activation = NA,
#                          ...){
#   mc <- as.list(match.call()[-1])
#   if(is.null(mc$activation)) mc$activation <- NA
#   if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
#     inputshape <- attr(layer, "shape")
#     inputname  <- attr(layer, "name")
#   } else {
#     layer <- NULL
#   }
#   if(!is.null(mc$layer) && mc$layer == ".") mc$layer <- layer
#   mc$type <- "response norm"
#   do.call(nnlayer_conv, mc)
# 
# }
# 
# 
# # fully connected ---------------------------------------------------------
# 
# #' Create fully connected layer.
# #'
# #' @inheritParams nnlayer_input
# #' @inheritParams nnlayer_conv
# #'
# #' @param nodes Number of nodes in hidden layer
# #'
# #' @export
# #' @family layer definition functions
# #' @example inst/examples/example_netSharpLayer.R
# nnlayer_full <- function(layer, nodes,
#                          name, inputname,
#                          activation = c("sigmoid", "rlinear", "linear")
# ){
#   activation <- match.arg(activation)
#   if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
#     inputname  <- attr(layer, "name")
#   } else {
#     layer <- ""
#   }
# 
#   z <- sprintf("hidden %s [%s] %s from %s all;",
#                name,
#                nodes,
#                activation,
#                inputname
#   )
#   z <- sprintf("%s\n\n%s", as.character(layer), z)
#   as.netSharpLayer(z, name = name)
# }
# 
# 
# # nnlayer_output ------------------------------------------------------------
# 
# 
# # output Class [6] from hid2 all;
# 
# 
# #' Define output layer.
# #'
# #' @inheritParams nnlayer_input
# #' @inheritParams nnlayer_conv
# #' @inheritParams nnlayer_full
# #' @family layer definition functions
# #' @example inst/examples/example_netSharpLayer.R
# #' @export
# nnlayer_output <- function(layer, nodes,
#                            name, inputname,
#                            activation = c("sigmoid", "rlinear", "linear")
# ){
#   activation <- match.arg(activation)
#   if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
#     inputname  <- attr(layer, "name")
#   } else {
#     layer <- ""
#   }
# 
#   z <- sprintf("output %s [%s] %s from %s all;",
#                name,
#                nodes,
#                activation,
#                inputname
#   )
#   z <- sprintf("%s\n\n%s", as.character(layer), z)
#   as.netSharpLayer(z, name = name)
# }
# 
# nnlayer_compute_input_size <- function(formula, sampledata){
#   z <- model.matrix(object = formula, data = head(sampledata))
#   ncol(z)
# }
# 
# nnlayer_compute_input_size(Species ~ ., iris)
# nnlayer_compute_input_size(Sepal.Length ~ ., iris)
# nnlayer_compute_input_size(mpg ~ ., mtcars)
# nnlayer_compute_input_size(mpg ~ . + am:vs, mtcars)
