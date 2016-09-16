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

is.netSharpLayer <- function(x){
  inherits(x, "netSharpLayer")
}

as.netSharpLayer <- function(x, name, shape){
  class(x) <- c("netSharpLayer", "character")
  if(!missing(name) && !is.null(name)) attr(x, "name") <- name
  if(!missing(shape) && !is.null(shape)) attr(x, "shape") <- shape
  x
}

as.character.netSharpLayer <- function(x){
  paste(capture.output(cat(x)), collapse = "\n")
}

print.netSharpLayer <- function(x, ...){
  attributes(x) <- NULL
  cat(x)
}

# input pixels [3, 50, 50];

layer_shape <- function(shape){
  sprintf("[%s]", paste(shape, collapse = ", "))
}

layer_input <- function(shape, name = "layer_input"){
  z <- sprintf("input %s %s;", name, layer_shape(shape))
  as.netSharpLayer(z, name = name, shape = shape)
}

# hidden conv1 [48, 24, 24] rlinear from pixels convolve {
# InputShape  = [3, 50, 50];
# KernelShape = [3,  5,  5];
# Stride      = [1,  2,  2];
# LowerPad    = [0, 1, 1];
# Sharing     = [T, T, T];
# MapCount    = 48;
# }

layer_conv <- function(layer, kernelshape, inputshape, name, inputname, stride, activation = "rlinear"){
  if(!missing(layer) && !is.null(layer) && is.netSharpLayer(layer)){
    inputshape <- attr(layer, "shape")
    inputname  <- attr(layer, "name")
  } else {
    layer <- ""
  }
  if(missing(stride) || is.null(stride)) stride <- rep(1, length(inputshape))
  outputshape <- (inputshape - kernelshape) / stride + 1
  convolution <- sprintf("  InputShape  = %s;\n  KernelShape = %s;\n  Stride      = %s;",
                         layer_shape(inputshape),
                         layer_shape(kernelshape),
                         layer_shape(stride)
  )
  
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



layer_input(c(13, 13))
layer_input(c(3, 7, 7), "pixels")

layer_conv(NULL, c(2, 2), 
           inputshape = c(13, 13), 
           name = "conv1", 
           inputname = "pixels"
)
layer_conv(NULL, 
           c(2, 2), 
           inputshape = c(13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(2, 2)
)
layer_conv(NULL, 
           c(1, 2, 2), 
           inputshape = c(3, 13, 13), 
           name = "conv1", 
           inputname = "pixels", 
           stride = c(1, 2, 2)
)

library(magrittr)

layer_input(c(3, 7, 7), name = "pixels") %>% attr("name")
layer_input(c(3, 7, 7), name = "pixels") %>% as.character()


layer_input(c(3, 50, 50), name = "pixels")
  
layer_input(c(3, 50, 50), name = "pixels") %>% 
  layer_conv(
    kernelshape = c(1, 5, 5), 
    name = "conv1", 
    stride = c(1, 2, 3)
  )

layer_input(c(3, 50, 50), name = "pixels") %>% 
  layer_conv(
    kernelshape = c(1, 5, 5), 
    name = "conv1", 
    stride = c(1, 2, 3)
  ) %>% attributes()


layer_input(c(3, 50, 50), name = "pixels") %>% 
  layer_conv(
    kernelshape = c(1, 5, 5), 
    name = "conv1", 
    stride = c(1, 2, 3)
  ) %>% 
  layer_conv(
    kernelshape = c(1, 5, 5), 
    name = "conv2"
  )
