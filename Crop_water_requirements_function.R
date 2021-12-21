# Housekeeping
rm(list = ls())

setwd("C:/Users/tito_/Dropbox/Thesis/R") 

.libPaths("C:/Users/tito_/Dropbox/Thesis/R/Library")

# Library 

# Input table 

# CWR function

CWR <- function(){
  
  tmed <- (tmax+tmin)/2
  
  delta <- 4098(0.6108*exp((17.27*tmed)/(tmed+237.3)))
  
  etmin <- 0.6108*exp((17.27*tmin)/(tmin+237.3))
  
  etmax <- 0.6108*exp((17.27*tmax)/(tmax+237.3))
  
  es <- (etmax+etmin)/2
  
  ea <- (etmin*rhmin/100+etmax*rhmax/100)/2 # has many ways to be calculated. could explore more
                                            # like eamed <- ((rhmin+rhmax)/200)*0.6108*exp((17.27*tmed)/(tmed+237.3))
  
  P <- 101.3*((293-(0.0065*elevation))/293)^5.26
  
  gamma <- 0.000665*P
  
  Et0 <- (0.408*delta(Rn-G)+gamma*(900/tmed+273)*u2(es-ea))/delta+gamma(1+0.34*u2)
  
}