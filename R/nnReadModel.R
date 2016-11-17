#' Read machine learning model from file.
#' 
#' @param file a \code{\link{connection}} or the name of the file where the R object is saved to or read from.
#' @param ... passed to \code{\link{readRDS}} and \code{\link{saveRDS}}
#' 
#' @rdname nnSerialize
#' @export
nnReadModel <- function(file, ...){
  readRDS(file, ...)
}


stripEnv <- function(x){
  x$params$DataFrameEnvironment <- new.env()
  x$params$env <- new.env()
  x
}

#' @rdname nnSerialize
#' @importFrom base64enc base64encode
#' @export
nnSerialize <- function(x){
  base64enc::base64encode(serialize(x, NULL))
}

#' @rdname nnSerialize
#' @importFrom base64enc base64decode
#' @export
nnDeserialize <- function(x){
  unserialize(base64enc::base64decode(x))
}

#' Write machine learning model to file.
#' 
#' @inheritParams nnReadModel
#' @param x Model object
#' 
#' @rdname nnSerialize
#' @export
nnSaveModel <- function(x, file = "", ...){
  x <- stripEnv(x)
  saveRDS(x, file = file, ...)
}

#' Serialize model to ASCII string.
#' 
#' @inheritParams nnReadModel
#' @rdname nnSerialize
#' @export
nnSerializeModel <- function(x){
  x <- stripEnv(x)
  nnSerialize(x)
}


#' Serialize model to base64 encoded string.
#' 
#' @inheritParams nnReadModel
#' @rdname nnSerialize
#' @export
nnDeserializeModel <- function(x){
  nnDeserialize(x)
}
