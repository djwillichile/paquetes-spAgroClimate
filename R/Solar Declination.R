#' Solar Declination
#'
#' @title  Solar Declination
#' @description  This function calculates the solar declination angle based on the day of the year
#' @param i numeric. The day of the year, between 1 and 365
#' @return  numeric value, the solar declination angle in radians
#' @references "Astronomy Algorithms" by Jean Meeus
#' @note  the input day of year should be an integer between 1 and 365
#' @examples
#' solarDeclination(90)  # 0.06315547
#' solarDeclination(270) # -0.04569977
#' @export
solarDeclination <- function(i) {
  dec = angleTransform(23.45) * sin(2*pi*(i + 284)/ 365)
  dec
}
