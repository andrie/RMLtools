#' Reconstruct the NET# net definition from an mxNeuralNet object
#' 
#' @param modelObject A \link{mxNeuralNet} model object
#' @export
#' @return A character vector containing the net definition
#' @examples 
#' library(MicrosoftRML)
#' model <- mxNeuralNet(isCase ~ age + parity + education + spontaneous + induced,
#'                      transforms = list(isCase = case == 1),
#'                      data = infert,
#'                      numHiddenNodes = 3
#' )
#' cat(
#'   reconstructNetDefinition(model)
#' )

reconstructNetDefinition <- function(modelObject){
  capture.output({
    modelSummary <- MicrosoftRML:::mxModelSummary(modelObject)
  })
  kvp <- modelSummary[["keyValuePairs"]]
  kvp <- kvp[!is.na(kvp)]
  
  wb <- grep("Weights|Biases", names(kvp))
  
  
  do_one <- function(x, n){
    sprintf("const %s = [%s]", n, paste(x, collapse = ", "))
  }
  
  wbs <- kvp[wb]
  consts <- sapply(seq_along(wbs), 
                   function(i)do_one(wbs[[i]], names(wbs)[i])
  )
  consts <- paste(consts, collapse = "\n\n")
  
  layers <- paste(
    kvp[["Input Layers"]],
    kvp[["Layers"]],
    sep = "\n"
  )
  
  paste(consts, "\n", layers, sep = "\n")
  
}

