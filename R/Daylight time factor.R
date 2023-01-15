#Angulo horario
daylightTimeFactor <- function(lat,i,dec = NULL) {
  if (is.null(dec)) dec=solarDeclination(i)
  ws <- acos(-tan(lat)*tan(solarDecl(i)))
  ws
}
