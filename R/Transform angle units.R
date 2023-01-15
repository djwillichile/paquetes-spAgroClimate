#' Transform angle units
#'
#' This function allows to convert angle units from degrees to radians or viceversa, by default it assumes that the input is in degrees
#'
#' @title Transform angle units
#'
#' @description Convert angle units from degrees to radians or viceversa
#' @param x numeric. The angle value
#' @param type character. The unit of the angle, can be "d", "deg","degree" for degrees or "r","rad","radian" for radians. default: "d"
#' @return numeric. The angle converted to the corresponding unit
#' @references "Trigonometry" by Jane Smith
#' @note  be aware of the type of unit used
#' @examples
#' angleTransform(180)  # 3.141593
#' angleTransform(3.141593, "r") #180
#' angleTransform(270, "d") #4.712388
angleTransform <- function(x, type = "d") {
  types = c("d","deg","degree","r","rad","radian")
  match_type = match(type, types)
  if(is.na(match_type)) stop("Invalid type argument. Use 'd', 'deg', 'degree', 'r', 'rad' or 'radian'.")
  else if(match_type <= 3) return(x*pi/180)
  else return(x*180/pi)
}

