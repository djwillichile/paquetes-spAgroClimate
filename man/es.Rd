% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Saturation vapor pressure.R
\name{es}
\alias{es}
\title{Calculates saturation vapor pressure based on temperature using the Clasius-Clapeyron equation}
\usage{
es(x)
}
\arguments{
\item{x}{numeric, the temperature in celsius}
}
\value{
The function returns a number in kilopascals (kPa) that corresponds to the saturation vapor pressure based on the input temperature.
}
\description{
The function "es" calculates the saturation vapor pressure (es) in kilopascals (kPa) based on the input temperature in celsius (x) using the Clasius-Clapeyron equation.
}
\note{
It is important to note that this equation is only valid for a pure substance and its applicability is reduced when considering mixtures of gases or relative humidity.
}
\examples{
saturationVaporPressure(30)
# Result: 4.32903
es(20)
# Result: 2.40766
}
\references{
\itemize{
\item Holton, J. R., & Hakim, G. J. (2017). An introduction to dynamic meteorology. Academic press.
\item Wallace, J. M., & Hobbs, P. V. (1977). Atmospheric science: an introductory survey. Elsevier.
\item Clark, M. P. (2018). Introduction to hydrometeorology. Elsevier.
}
}
