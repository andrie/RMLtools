#' Read machine learning model from file.
#' 
#' @param file a \link{connection} or the name of the file where the R object is saved to or read from.
#' @param ... passed to \link{readRDS}
#' @export
mxReadModel <- function(file, ...){
  readRDS(file, ...)
}

#' Write machine learning model to file.
#' 
#' @inheritParams mxReadModel
#' @param x Model object
#' @param ... passed to \link{saveRDS}
#' @export
mxSaveModel <- function(x, file = "", ...){
  x$params$DataFrameEnvironment <- NULL
  x$params$env <- NULL
  saveRDS(x, file = file, ...)
}