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
#' @inheritParams nnlayer_input
#' @param x Object 
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

nnlayer_shape <- function(shape){
  sprintf("[%s]", paste(shape, collapse = ", "))
}

nnlayer_boolean <- function(shape){
  sh <- as.logical(shape)
  idx <- sh
  sh[idx]  <- "true"
  sh[!idx] <- "false"
  
  sprintf("[%s]", paste(sh, collapse = ", "))
}
