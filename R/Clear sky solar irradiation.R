#' @title Clear sky solar irradiation (Rso)
#' @description The function calculates the clear sky solar irradiation (Rso) from extraterrestrial solar radiation (Ra) and the altitude (alt) as described in the FAO 56 Penman-Monteith equation for evapotranspiration.
#' @param Ra Extraterrestrial solar radiation [MJ m^-2 day^-1]
#' @param alt Altitude [m]
#' @return Rso Clear sky solar irradiation [MJ m^-2 day^-1]
#' @references FAO. (1998). Irrigation and Drainage Paper 56: Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and Drainage Paper 56. FAO.
#' @note The formula used in this function is Rso=Ra*(0.75+alt*2e-5)
#' @examples
#' Rso(Ra = 13.5, alt = 1000)
#' # Output: 9.9625
#' @export
# Radiación solar en un día despejado
Rso=function(Ra,alt){
  Rso=Ra*(0.75+alt*2e-5)
  return(Rso)
}
