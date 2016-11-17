#' Extract iteration data from captured output of neuralNet model.
#' 
#' @param model_output Result from \code{\link{capture.output}} on \code{\link{neuralNet}}
#' @export
nnExtractIterations <- function(model_output){
  idx <- grep("^Iter:", model_output)
  model_output[idx]
}

#' Extract the percentage improvement in modeling error from neuralNet captured output.
#' 
#' @inheritParams nnExtractIterations
#' @export
nnExtractIterationImprovement <- function(model_output){
  z <- nnExtractIterations(model_output)
  as.numeric(gsub(".*?MeanErr=.*?\\((.*)%%\\).*$", "\\1", z)) / 100
  
}

#' Extract the percentage improvement in modeling error from neuralNet captured output.
#' 
#' @inheritParams nnExtractIterations
#' @export
nnExtractIterationError <- function(model_output){
  z <- nnExtractIterations(model_output)
  as.numeric(gsub(".*?MeanErr=([0-9.]*).*$", "\\1", z))
}

#' Extract the the total number of model weights from a neuralNet captured output.
#' 
#' @inheritParams nnExtractIterations
#' @export
nnExtractTotalModelWeights <- function(model_output){
  ptn <- "Initializing.*?, (\\d*) Weights.*"
  idx <- grep(ptn, model_output)
  as.numeric(sub(ptn, "\\1", model_output[idx]))
}
