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
CWR_function <- function()
{
  i <- 2
  De <- vector()
  De[1] <- 0
  Etc_adj_v <- vector() 
  Dr <- vector()
  Dr[1] <- 0
  Irr <- vector()
  victor <- vector()
  
  # Need to adjust fc so it's not smaller than wp. Maybe do a table thing for soil type
  fc <- wp+fcd
  
  while (i<=x+1)
  {
    # Daily changing values
    tmax <- sample(seq(weather_estimates[1,5], weather_estimates[1,7], 0.1), 1)
    tmin <- sample(seq(weather_estimates[2,5], weather_estimates[2,7], 0.1), 1)
    u2 <- sample(seq(weather_estimates[3,5], weather_estimates[3,7], 0.1), 1)
    rhmin <- sample(seq(weather_estimates[5,5], weather_estimates[5,7], 0.1), 1)
    rhmax <- sample(seq(weather_estimates[6,5], weather_estimates[6,7], 0.1), 1)
    
    ## Reference evapotranspiration Et0
    
    # Day of the year
    day <- day1+i-2
    
    # Average temperature
    tmed <- (tmax+tmin)/2
    
    # Slope of the vapor saturation pressure curve
    delta <- (4098*(0.6108*exp((17.27*tmed)/(tmed+237.3))))/(tmed+237.3)^2
    
    # Vapor saturation pressure at min temperature
    etmin <- 0.6108*exp((17.27*tmin)/(tmin+237.3))
    
    # Vapor saturation pressure at max temperature
    etmax <- 0.6108*exp((17.27*tmax)/(tmax+237.3))
    
    # Vapor saturation pressure
    es <- (etmax+etmin)/2
    
    # Real vapor pressure
    # has many ways to be calculated. could explore more
    # like eamed <- ((rhmin+rhmax)/200)*0.6108*exp((17.27*tmed)/(tmed+237.3))
    # Also etmin can be taken as es, when humidity data is shit
    ea <- (etmin*rhmax/100+etmax*rhmin/100)/2 
    
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
    # Also can use Hargreaves radiation equation 
    Rs1<- 0.19*sqrt(tmax-tmin)*Ra
    
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
    # Ignored for daily calculations??
    # G <- 0.3*Rn
    # G = 0.4*exp(-0,5 IAF)*Rn
    
    # Reference evapotranspiration 
    Et0 <- (0.408*delta*Rn+gamma*900/(tmed+273)*u2*(es-ea))/(delta+gamma*(1+0.34*u2))
    
    ## Crop coefficients Kcb and Ke
    
    # Basal crop coefficient - transpiration
    Kcb <- Kcb_tab+(0.04*(u2-2)-0.004*(rhmin-45))*(h/3)^0.3
    
    # Upper limit of Kc_max
    Kc_max <- max((1.2+(0.04*(u2-2)-0.004*(rhmin-45))*(h/3)^0.3), Kcb+0.05)
    
    # Initial depletion after rain or irrigation
    Pr <- chance_event(chance = chance_rain, 
                       value_if = Prec,
                       value_if_not = 0,
                       n = 1)
    if (Pr<Et0*0.2){Pr <- 0}
    
    ###
    # Total available water
    ADT <- 1000*(fc-wp)*Zr
    
    # Readily available water
    AFA <- p*ADT
    
    # Irrigation
    ifelse(i%%Ir_interval==0, Ir <- Dr[i-1], Ir <- 0)
    ###
    
    # Exposed and moist soil fraction
    few <- min(1-fq, fw)
    
    # Deep percolation
    DP <- 0
    if(Pr>0|Ir>0){DP <- Pr+Ir/fw-De[i]}
    
    # Evaporation reduction coefficient
    ifelse(De[i-1]>AFE, Kr <- (AET-De[i-1])/(AET-AFE), Kr <- 1)
    
    # Evaporation coefficient
    Ke <- min(Kr*(Kc_max-Kcb), few*Kc_max)
    
    # Evaporation component
    E <- Ke*Et0
    
    # Daily depletion calculation
    ifelse(Pr>0|Ir>0, De[i] <- 0, De[i] <- De[i-1]-Pr-Ir/fw+E/few+DP)
    if(De[i]<0){De[i] <- 0}
    # De[i-1] <- De-Ke*Et0
    # 0<=Dei<=AET
    
    # Maximum ammount of evaporable water
    AET <- 1000*(fc-0.5*wp)*Ze
    
    # Kc
    Kc <- Kcb+Ke
    
    # ETC
    Etc <- Et0*Kc
    
    ## Non standard conditions for evapotranspiration
    
    # Deep percolation for Ks
    DPx <- 0
    if(Pr>0|Ir>0){Dpx <- Pr+Ir-Etc-Dr[i-1]}
    
    # Root zone moisture depletion
    Dr[i] <- Dr[i-1]-Pr-Ir+Etc+DPx
    if(Dr[i]<0){Dr[i] <- 0}
    
    # Transpiration reduction factor
    ifelse(Dr[i]>AFA, Ks <- (ADT-Dr[i])/((1-p)*ADT), Ks <- 1)
    
    # Adjusted coefficients
    Kc_adj <- Ks*Kcb+Ke
    Etc_adj <- Kc_adj*Et0
    
    
    Dr[i] <- Dr[i]
    De[i] <- De[i]
    Irr[i] <- Ir
    Etc_adj_v[i] <- Etc_adj
    victor[i] <- Pr
    i <- i+1
  }
  
  Etc_adj_v[1] <- 0
  Total_Etc <- sum(Etc_adj_v)
  Irr[1] <- 0 
  Total_irrigation <- sum(Irr)
  #victor[1] <- 0
  #Total_victor <- sum(victor)
  
  return(list(Total_Etc = Total_Etc,
              Total_irrigation = Total_irrigation,
              Rain = victor))
}

# Monte Carlo run
CWR_mc_simulation <- mcSimulation(estimate = as.estimate(weather_estimates),
                                   model_function = CWR_function,
                                   numberOfModelRuns = 1000,
                                   functionSyntax = "plainNames")

# Distribution plot
plot_distributions(mcSimulation_object = CWR_mc_simulation, 
                   vars = c("Total_Etc"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

# Projection to Latent Structure Analysis 
# ETo no es altamente sensitivo a variaciones de la velocidad del viento dentro de rangos normales. 
# Sale como alto en PLS ???
pls_result_CWR <- plsr.mcSimulation(object = CWR_mc_simulation,
                                     resultName = names(CWR_mc_simulation$y)[1], ncomp = 1)

plot_pls(pls_result_CWR, input_table = weather_estimates, threshold = 0, x_axis_name = 
           "Variable Importance in Projection - Crop Water Requirements")

