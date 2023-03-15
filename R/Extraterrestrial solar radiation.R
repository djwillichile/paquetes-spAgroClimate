#' @title Extraterrestrial Solar Radiation or Radiation TOA
#'
#' @description This function calculates the extraterrestrial solar radiation or radiation
#' at the top of the atmosphere (TOA) given the latitude and either the month, date, or day of the year.
#'
#' @param latitude Numeric value specifying the geographic coordinate of the north-south position
#'   of a point on the Earth's surface in sexagesimal degrees.
#' @param months Optional, a numeric vector representing the month(s) for which to calculate solar irradiance.
#' @param dates Optional, a vector of dates (Date objects) for which to calculate solar irradiance.
#' @param doy Optional, a numeric vector representing the day(s) of the year for which to calculate solar irradiance.
#' @return A numeric value representing extraterrestrial solar radiation in kJ/m^2.
#' @references
#' Duffie, J. A., & Beckman, W. A. (2013). Solar engineering of thermal processes. John Wiley & Sons.
#'
#' Iqbal, M. (2012). An introduction to solar radiation. Elsevier.
#'
#' Liou, K. N. (2002). An introduction to atmospheric radiation. Academic Press.
#'
#' Liu, B. Y., & Jordan, R. C. (1960). The interrelationship and characteristic distribution of direct, diffuse and total solar radiation. Solar Energy, 4(3), 1-19.
#' @note
#' This function makes some simplifications in calculating extraterrestrial solar radiation and may not be applicable in all cases. The input latitude is assumed to be in decimal degrees, and the output radiation is in kJ/m^2. For further analysis of solar radiation, consider using other functions in the spAgroClimate package. Please note that this function is valid for latitude values between -66.9 and 66.9 degrees.
#' @examples
#' SolarIrradiance(latitude = 45, doy = 172) # June 21st, using day of the year
#' SolarIrradiance(latitude = 45, months = 6) # June, using month
#' SolarIrradiance(latitude = 45, dates = as.Date("2015-06-21")) # June 21st, using date
#' @export

# Extraterrestrial solar radiation
SolarIrradiance=function(latitude,months=NULL,dates=NULL,doy=NULL){
  lat=latitude*pi/180

  #Constante solar en kJ/m2 día radiación potencial que llega a cualquier punto de la trayectoria de la tierra
  Isc = 4921
  Isc = 1368*60*60/1000

  #calcula declinación solar
  if(!is.null(doy)){
    i=doy
  }else if(!is.null(months)){
    dates=as.Date(paste(2015,months,floor(
      lubridate::days_in_month(months)/2),sep = "-"))
  }else if(is.null(dates)){
    stop("This is an error message")
  }

  i <- as.numeric(format(dates, "%j"))
  dec = (23.45 * sin((pi / 180) * (360 / 365) * (i + 284))) * (pi / 180)

  #distancia tierra sol
  E0 = 1 + 0.0334 * cos((2 * pi * i) / 365)

  #argumento del acocoseno
  x = -tan(lat) * tan(dec)

  #caculo del arcocoseno para ángulo horario
  ws = (atan(-(x) / (sqrt(1 - x ^ 2))) + 2 * atan(1))

  #radiación extraterrestre sobre el techo de la atmosfera
  RA = (24 / pi) * Isc * E0 * (ws * sin(lat) * sin(dec) + cos(lat) * cos(dec) * sin(ws))/1000

  return(RA)
}
