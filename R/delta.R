#' @title Pendiente de la curva de presion de vapor
#'
#' @description Obtiene delta a partir de la temperatura
#'
#' @param ta temperature ºC
#' @param es temperature ºC
#' @export

get.delta=function(ta,es=NULL){
  if(is.null(es)) es=get.es(ta)
  delta=4098*es/potencia(237.3+ta,2)
  return(delta)
}
