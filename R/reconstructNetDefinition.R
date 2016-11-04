#' Reconstruct the NET# net definition from an neuralNet object
#' 
#' @param modelObject A \link[MicrosoftML]{neuralNet} model object
#' @param filename Optional file name. If supplied, write model specification to file.
#' @export
#' @return A character vector containing the net definition
#' @example inst/examples/example_reconstructNetDefinition.R
#' @seealso readNetDefinition

reconstructNetDefinition <- function(modelObject, filename = NULL){
  capture.output({
    modelSummary <- MicrosoftML:::mlModelSummary(modelObject)
  })
  kvp <- modelSummary[["keyValuePairs"]]
  kvp <- kvp[!is.na(kvp) & (sapply(kvp, length) != 0)]
  
  wb <- grep("Weights|Biases", names(kvp))
  
  
  do_one <- function(x, n){
    sprintf("const %s = [%s];", n, paste(x, collapse = ", "))
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
  
  z <- paste(consts, "\n", layers, sep = "\n")
  Encoding(z) <- "ascii"
  if(!missing(filename) && !is.null(filename)) writeLines(z, con = filename)
  z
  
}

