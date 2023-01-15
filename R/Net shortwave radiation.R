#' @title Net shortwave radiation
#' @description The function calculates the net shortwave radiation given the solar radiation (srad) and albedo. If albedo is not given, it assumes a default value of 0.77
#' @param srad numeric, solar radiation (W/m^2)
#' @param albedo numeric, albedo of the surface (dimensionless), default value is NULL
#' @return numeric, net shortwave radiation (W/m^2).
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-guidelines for computing crop water requirements-FAO irrigation and drainage paper 56. Rome, Italy: Food and Agriculture Organization of the United Nations (FAO).
#' @note The value of albedo should be between 0 and 1
#' @examples
#' Rns(srad = 32)
#' Rns(srad = 32, albedo = 0.25)
#' @export

#Radiación neta de onda corta
Rns=function(srad,albedo=NULL){
  if(is.null(albedo)) Rns=srad*.77 else Rns=srad*(1-albedo)
  return(Rns)
}
