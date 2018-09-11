suppressWarnings(library(stringr))
suppressWarnings(library(DMwR))
suppressWarnings(library(shiny))
suppressWarnings(library(shinythemes))
suppressWarnings(library(knitr))
suppressWarnings(library(kableExtra))
suppressWarnings(library(Matrix))
suppressWarnings(library(matrixStats))
suppressWarnings(library(DT))
suppressWarnings(library(tidyverse))
require(ggplot2)
require(lpSolve)
require(stringr)

shinyInput <- function(FUN,id,num,...) {
  inputs <- character(num)
  for (i in seq_len(num)) {
    inputs[i] <- as.character(FUN(paste0(id,i),label=NULL,value=TRUE,...))
  }
  inputs
}
