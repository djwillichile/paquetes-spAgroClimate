#' @title Extraterrestrial solar radiation or radiation TOA
#'
#' @description This function
#'
#' @param latitude geographic coordinate specifying the north-south position
#' of a point on the Earth's surface in sexagecimal degrees
#' @return a numeric value in kJ/m2
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
