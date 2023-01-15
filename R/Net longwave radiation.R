#' @title Net longwave radiation
#' @description
#' The function Rnl calculates the net longwave radiation using the Penman-Monteith equation for evapotranspiration, according to Allen et al. (1998). The function takes five parameters as inputs: the average temperature (tavg), the solar radiation (srad), the actual vapor pressure (ea), the shortwave radiation (Re), and the atmospheric transmissivity for longwave radiation (tal). The net longwave radiation (Rnl) is calculated as the product of four factors: the fourth power of the absolute temperature in kelvin, the difference between the clear-sky emissivity and the actual emissivity, the ratio of the solar radiation to the clear-sky solar radiation, and the ratio of the atmospheric transmissivity for longwave radiation to the clear-sky transmissivity.
#' @param tavg average temperature (Celsius)
#' @param srad solar radiation (MJ/m2/day)
#' @param ea vapor pressure (kPa)
#' @param Re extraterrestrial radiation (MJ/m2/day)
#' @param tal atmospheric transmissivity for longwave radiation
#' @return Rnl net longwave radiation (MJ/m2/day)
#' @references Allen, R.G., L.S. Pereira, D. Raes, and M. Smith. 1998. Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and Drainage Paper 56. FAO, Rome.
#' @note the function assume the values of tavg, srad, ea and Re are available
#' @examples
#' Rnl(25, 10, 2, 15, 0.5)
#' #output: 2.7
#' @export
Rnl=function(tavg,srad,ea,Re,alt){
  Rso=Rso(Re,alt)
  Rnl <- 4.903e-09*potencia(tavg + 273.16,4)*(0.34-0.14*sqrt(ea))*(1.35*(srad/Rso)-0.35)
  return(Rnl)
}
