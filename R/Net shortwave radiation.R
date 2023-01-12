#' @title Radiación neta de onda corta
#'
#' @description Obtiene la radiación neta de onda corta
#'
#' @param srad radiación solar
#' @param albedo opcional
#' @return a numeric value
#' @export


#Radiación neta de onda corta
get.Rns=function(srad,albedo=NULL){
  if(is.null(albedo)) Rns=srad*.77 else Rns=srad*(1-albedo)
  return(Rns)
}
