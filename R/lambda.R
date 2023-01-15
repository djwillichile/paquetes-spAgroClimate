#' @title calor latente de vaporización
#'
#' @description Obtiene lambda a partir de la temperatura
#'
#' @param ta temperature ºC
#' @return a numeric value kPa
#' @export

# calor latente de vaporización (lambda) [ MJ kg-1]
lambda=function(ta){
  lambda=(2500.78-2.3601*ta)/1000
  return(lambda)
}
