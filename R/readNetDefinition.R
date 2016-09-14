#' Reads a net definition from file.
#' 
#' @param filename Reads a model specification from file
#' @export
#' @example inst/examples/example_reconstructNetDefinition.R
readNetDefinition <- function(filename){
  paste(readLines(filename), collapse = "\n")
}