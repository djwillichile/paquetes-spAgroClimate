#' @title Real vapor pressure
#'
#' @description Obtiene la presion de vapor real a partir de la temperatura
#'
#' @param tavg temperature ºC
#' @param relh relative humidity %
#' @return a numeric value kPa
#' @export

# presion de vapor real [kPa]
get.ea=function(tavg=NULL,tmin=NULL, tmax=NULL, relh=NULL){
  if (is.null(relh) && !is.null(tmin)) return(get.es(tmin))
  if (!is.null(tavg) && is.numeric(tavg)) {
    es=get.es(tavg)
  } else if (!is.null(tmin) && !is.null(tmax) && is.numeric(c(tmin,tmax))) {
    es=mean(get.es(tmin),get.es(tmax))
  } else {
    stop("Invalid arguments")
  }
  ea = relh * es/100
  return(ea)
}
