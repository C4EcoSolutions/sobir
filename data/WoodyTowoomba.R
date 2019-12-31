#' Woody cover in Towoomba
#'
#' Tree abundance in Towoomba Agricultural Development Center, South Africa relative to 
#' four soil fertilisation treatments and 16 soil nutrient and element concentrations, 
#' as published by Mills et al. 2017 in PLoS ONE.
#'
#' @docType data
#' 
#' @keywords datasets
#' 
#' @format A data frame with 60 rows and 18 variables:
#' \describe{
#'   \item{Treatment}{12 treatments of a combination of superphosphate (SP0-SP2; 0, 233, 466 kg/ha/yr) and ammonium sulphate (AS0-AS3; 0, 146, 291, 583, 1166 kg/ha/yr) fertilisation applications.}
#'   \item{TreeNum}{Number of trees present following three decades of fertilisation and three subsequent decades of passive protection.}
#'   \item{EC, pH, B, C, Na, Mg, P, S, N, K, Ca, Mn, Cu, Zn, N_NO3, N_NH4}{Soil nutrient and element concentration in mg/kg, except for pH (unitless), EC (uS/cm) and C (%)}
#' }
#' 
#' @references Mills et al. (2017) Effects of anabolic and catabolic nutrients on woody plant encroachment after long-term experimental fertilization in a South African savanna. PLoS ONE 12(6), p1-24.
#' 
#' @source \url{https://doi.org/10.1371/journal.pone.0179848}
#' 
#' @examples 
#' \donttest{dat = read.csv(system.file("data", "WoodyTowoomba.csv", package = "sobir"))
#' plot(dat$pH, dat$TreeNum)}
#'
"WoodyTowoomba"