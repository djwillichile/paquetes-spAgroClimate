#' @title Calculates saturation vapor pressure based on temperature using the Clasius-Clapeyron equation
#' @description The function "es" calculates the saturation vapor pressure (es) in kilopascals (kPa) based on the input temperature in celsius (x) using the Clasius-Clapeyron equation.
#' @param x numeric, the temperature in celsius
#' @return The function returns a number in kilopascals (kPa) that corresponds to the saturation vapor pressure based on the input temperature.
#' @references
#' - Holton, J. R., & Hakim, G. J. (2017). An introduction to dynamic meteorology. Academic press.
#' - Wallace, J. M., & Hobbs, P. V. (1977). Atmospheric science: an introductory survey. Elsevier.
#' - Clark, M. P. (2018). Introduction to hydrometeorology. Elsevier.
#' @note It is important to note that this equation is only valid for a pure substance and its applicability is reduced when considering mixtures of gases or relative humidity.
#' @examples
#' saturationVaporPressure(30)
#' # Result: 4.32903
#' es(20)
#' # Result: 2.40766
#' @export
es=function(x){
  es=0.6108*exp((17.27*x)/(237.3 + x))
  return(es)
}

