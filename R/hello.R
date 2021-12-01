# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}

# Rastafari
#' Growing degree days
#'
#' This function computes growing degree days and its sum
#' @param Tbase Base temperature (°C)
#' @param Tmin Vector of tmin (°C)
#' @param Tmax Vector of tmax (°C)
#' @keywords growing degree days , plant development
#' @export
#' @examples
#' GDD_cirad(13,20,30)
GDD_cirad <- function(Tbase,Tmin,Tmax){
  gdd <- NULL
  for(i in 1:length(Tmin)){
    ifelse(Tmin[i]<Tmax[i],
           gdd[i] <- (Tmin[i]+Tmax[i])/2-Tbase,stop(paste0("You have at least a value of Tmin >= Tmax : data line number ", i)))
  }
  cum_gdd <- cumsum(gdd)
  return(list("GDD"=gdd,"cumGDD"=cum_gdd))
}
