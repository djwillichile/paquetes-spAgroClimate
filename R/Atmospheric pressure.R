#' @title Calculates atmospheric pressure in kilopascals (kPa) based on altitude and selected method
#' @description The "atmPressure()" function allows for the calculation of atmospheric pressure in kilopascals (kPa) based on the input altitude and selected method. Two methods can be selected: the International Standard Atmosphere (ISA) model and a simplified equation based on the ideal gas law at a standard temperature of 20°C.
#' @param alt numeric, the altitude in meters above sea level.
#' @param method character, the method to be used for calculating atmospheric pressure. Default is "isa" and allows for selection between "isa" and "equation".
#' @return The function returns a number in kilopascals (kPa) that corresponds to the atmospheric pressure based on the input altitude and selected method.
#' @references
#' - International Civil Aviation Organization. (2019). "International Standard Atmosphere (ISA)". Retrieved from https://www.icao.int/publications/DOC7488/Pages/Search.aspx
#' - Halliday, D., Resnick, R., & Walker, J. (2018). Fundamentals of Physics. John Wiley & Sons.
#' - Bailyn, M. (2004). Introduction to Thermodynamics and Kinetic Theory of Gases. Cambridge University Press.
#' @note It is important to note that the simplified equation based on the ideal gas law is an approximation and does not take into account factors such as humidity and air pollution, so its accuracy may vary in real-world conditions.
#' @examples
#' atmPressure(500)
#' # Result: 95.46078
#' atmPressure(500, "equation")
#' # Result: 95.52765
#' @export

# presión atmosfética [kPa]
atmPressure <- function(alt, method = "isa") {
  if (method == "isa") {
    # Constants
    T0 <- 288.15 # Temperature at sea level in K
    L <- -0.0065 # Temperature lapse rate in K/m
    p0 <- 101325 # Pressure at sea level in Pa
    R <- 287.05 # Gas constant for air in J/kg·K
    g <- 9.80665 # Standard gravitational acceleration in m/s^2
    h_tropo <- 11000 # Altitude of troposphere in m
    if (alt <= h_tropo) {
      TP <- T0 + L * alt
      p <- p0 * (TP / T0)^(-g/(L*R))
      return(p/1000)
    } else {
      return("Altitude above troposphere")
    }
  } else if (method == "equation"){
    x <- alt # Altitude in meters
    pa = 101.3*potencia((293-0.0065*x)/293,5.26)
    return(pa)
  }else {
    return("Invalid method")
  }
}
