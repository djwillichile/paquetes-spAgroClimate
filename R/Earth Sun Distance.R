
EarthSunDist<-function(i){
  E0 = 1 + 0.0334 * cos((2 * pi * i) / 365)
  return(E0)
}

