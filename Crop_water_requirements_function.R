# Housekeeping
rm(list = ls())

setwd("C:/Users/tito_/Dropbox/Thesis/R") 

.libPaths("C:/Users/tito_/Dropbox/Thesis/R/Library")

# Libraries
library(decisionSupport)
library(ggplot2)

# Input table
weather_estimates <- read.csv("Input_table_CWR.csv")

# CWR function
CWR_function <- function(){
  
  # Average temperature
  tmed <- (tmax+tmin)/2
  
  # Slope of the vapor saturation pressure curve
  delta <- 4098(0.6108*exp((17.27*tmed)/(tmed+237.3)))
  
  # Vapor saturation pressure at min temperature
  etmin <- 0.6108*exp((17.27*tmin)/(tmin+237.3))
  
  # Vapor saturation pressure at max temperature
  etmax <- 0.6108*exp((17.27*tmax)/(tmax+237.3))
  
  # Vapor saturation pressure
  es <- (etmax+etmin)/2
  
  # Real vapor pressure
  ea <- (etmin*rhmin/100+etmax*rhmax/100)/2 # has many ways to be calculated. could explore more
                                            # like eamed <- ((rhmin+rhmax)/200)*0.6108*exp((17.27*tmed)/(tmed+237.3))
  # Atmospheric pressure
  P <- 101.3*((293-(0.0065*elevation))/293)^5.26
  
  # Psicometric constant
  gamma <- 0.000665*P
  
  # Lat in radians (dlat should be in decimals and negative for South hemisphere)
  lat <- dlat*pi/180
  
  # Earth-Sun inverse relative distance (wtf)
  dr <- 1+0.033*cos(2*day*pi/365)
  
  # Solar declination
  sol_dec <- 0.409*sin((2*day*pi/365)-1.39)
  
  # Radiation angle at sunset
  ws <- acos(-tan(lat)*tan(sol_dec))
  
  # Extraterrestral radiation
  Ra <- ((24*60)/pi)*0.082*dr*(ws*sin(lat)*sin(sol_dec)+cos(lat)*cos(sol_dec)*sin(ws))
  
  # Maximum isolation length
  N <- 24*ws/pi
  
  # Regression constant. Fraction of Ra in cloudy days (check if better available)
  as <- 0.25
  
  # Regression constant. Fraction of Ra in clear days (check if better available)
  bs <- 0.5
  
  # Solar radiation (can be measured directly)
  Rs <- Ra*(as+bs*n/N)
  
  # Solar radiation in a clear day (there are other equations, but this seems to
  # be appropiate for locations at sea-level)
  Rso <- Ra*(as+bs)
  
  # Albedo for reference crop
  alpha <- 0.23
  
  # Net solar radiation
  Rsn <- Rs*(1-alpha)
  
  # Net long wave radiation
  Rnl <- 4.903e-9*(((tmax+273.16)^4+(tmin+273.16)^4)/2)*(0.34-0.14*sqrt(ea))*((1.35*Rs/Rso)-0.35)
  
  # Net radiation
  Rn <- Rsn-Rnl
  
  # Soil heat flux (check other options)
  # G <- cs*((Ti-Ti2)/t)*z
  G <- 0.3*Rn
  
  # Reference evapotranspiration 
  Et0 <- (0.408*delta(Rn-G)+gamma*(900/tmed+273)*u2(es-ea))/delta+gamma(1+0.34*u2)
  
  return(list(Et0 = Et0))
}

CWR_mc_simulation <- mcSimulation(estimate = as.estimate(weather_estimates), 
                                         model_function = CWR_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")

