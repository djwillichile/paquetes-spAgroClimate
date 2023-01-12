#' @title constante psicrométrica gamma
#'
#' @description Obtiene gamma a partir de la presion atmosférica
#'
#' @param x presion kPa
#' @param lambda,cp,epsilon is contants
#' @return a numeric value
#' @export
#'
# constante psicrométrica (gamma)
get.gamma=function(x,lambda=2.45,cp=1.013*1e-3,epsilon=0.622){
  gamma=cp*x/(epsilon*lambda)
  return(gamma)
}
