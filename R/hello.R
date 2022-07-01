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
           gdd[i] <- max(0,(Tmin[i]+Tmax[i])/2-Tbase),stop(paste0("You have at least a value of Tmin >= Tmax : data line number ", i)))
  }
  cum_gdd <- cumsum(gdd)
  return(list("GDD"=gdd,"cumGDD"=cum_gdd))
}
