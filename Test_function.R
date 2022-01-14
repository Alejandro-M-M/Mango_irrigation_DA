
# Housekeeping
rm(list = ls())

setwd("C:/Users/tito_/Dropbox/Thesis/R") 

.libPaths("C:/Users/tito_/Dropbox/Thesis/R/Library")

# Library 
library(decisionSupport)
library(ggplot2)

# Input table
income_estimates <- read.csv("Input_table_uptake.csv")

# A function to let you test code line by line 
make_variables <- function(est, n=1)
  
{ x <-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("Input_table_uptake.csv"))


################################################################################
x
if (farm_size > 0 & farm_size < 3) {invest_raincatch <- investment_cost_raincatch*farm_size
  } else x <- 5

################################################################################


labor_cost <- yield_irrigation*farm_size*500


prices_irrigation <- vv(var_mean = mango_price_irrigation, 
                        var_CV = Var_CV,
                        relative_trend = 5,
                        n = years)

prices_no_irrigation <- vv(var_mean = mango_price_no_irrigation, 
                           var_CV = Var_CV,
                           relative_trend = 5,
                           n = years)


# Profit without irrigation
profit_no_irrigation <- prices_no_irrigation*yield_no_irrigation*farm_size


# Profit with well

well_supply <- ceiling(farm_size/well_water)

profit_with_well <- prices_irrigation*yield_irrigation*farm_size*water_quality+labor_cost-maintenance_well 
profit_with_well[1] <- profit_with_well[1] - (investment_cost_well*well_supply+study_cost_well)-
  (cost_irrigation_system*farm_size)-infrastructure_cost 

# Risk of not finding underground water *****(Only for 1st year with consecutive results)
## does this makes sense though?? ??? non repeatable risk...
#profit_with_well <- chance_event(chance = chance_no_underwater, 
#                                value_if = profit_no_irrigation - study_cost_well,
#                               value_if_not = profit_well,
#                              n = years)

# Profit with raincatch
profit_with_raincatch <- (prices_irrigation*yield_irrigation*farm_size)+labor_cost-
  maintenance_raincatch
profit_with_raincatch[1] <- profit_with_raincatch[1] - investment_cost_raincatch*farm_size-
  (cost_irrigation_system*farm_size)-infrastructure_cost

################################################################################
test_function <- function(){
  yield_as_rain <- 11+((year_rain-500)/(1500-500))*(13-11)
  yield_rain_var <- vv(var_mean = yield_as_rain, 
                       var_CV = 17,
                       relative_trend = 0,
                       n = 1)
  aardvark <- rnorm(1, yield_as_rain, 0.6137)
  
  lolia <- well_water
  
  return(list(yield_as_rain = yield_as_rain,
              yield_rain_var = yield_rain_var,
              lolia = lolia))
}
  
test_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates),
                                         model_function = test_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = test_mc_simulation, 
                   vars = c("lolia"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

sd(as.vector(t(test_mc_simulation$y[1])))

sd(as.vector(t(test_mc_simulation$x[14])))

sd(as.vector(t(test_mc_simulation$y[2])))


str(test_mc_simulation)

################################################################################
irrigation_function <- function(){
  

prices_irrigation <- vv(var_mean = mango_price_irrigation, 
                        var_CV = Var_CV,
                        relative_trend = 5,
                        n = years)

prices_no_irrigation <- vv(var_mean = mango_price_no_irrigation, 
                           var_CV = Var_CV,
                           relative_trend = 5,
                           n = years)


# Profit without irrigation
profit_no_irrigation <- prices_no_irrigation*yield_no_irrigation*farm_size
  

# Profit with well

well_supply <- ceiling(farm_size/well_water)

profit_with_well <- prices_irrigation*yield_irrigation*farm_size*water_quality+labor_cost-maintenance_well 
profit_with_well[1] <- profit_with_well[1] - (investment_cost_well*well_supply+study_cost_well)-
  (cost_irrigation_system*farm_size)-infrastructure_cost 

# Risk of not finding underground water *****(Only for 1st year with consecutive results)
## does this makes sense though?? ??? non repeatable risk...
#profit_with_well <- chance_event(chance = chance_no_underwater, 
#                                value_if = profit_no_irrigation - study_cost_well,
#                               value_if_not = profit_well,
#                              n = years)

# Profit with raincatch
profit_with_raincatch <- (prices_irrigation*yield_irrigation*farm_size)+labor_cost-
  maintenance_raincatch
profit_with_raincatch[1] <- profit_with_raincatch[1] - investment_cost_raincatch*farm_size-
  (cost_irrigation_system*farm_size)-infrastructure_cost

return (list(yield_irrigation = yield_irrigation,
             prices_irrigation = prices_irrigation,
             profit_with_well = profit_with_well))

}

irrigation_function()


correlation_matrix <- "         ,                   year_rain, yield_irrigation, mango_price_irrigation, rain_gs, yield_no_irrigation, mango_price_no_irrigation
                          year_rain,                    1,           0.4,                 0.7,              0,          0,                      0
                          yield_irrigation,             0.4,         1,                   0,                0,          0,                      0
                          mango_price_irrigation,       0.7,         0,                   1,                0,          0,                      0 
                          rain_gs,                      0,           0,                   0,                1,          0.6,                    0.8
                          yield_no_irrigation,          0,           0,                   0,                0.6,        1,                      0
                          mango_price_no_irrigation,    0,           0,                   0,                0.8,        0,                      1 "


                          

irrigation_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates, 
                                         correlation_matrix=data.matrix(read.csv(text = correlation_matrix, 
                                            row.names=1,
                                            strip.white=TRUE))),
                                         model_function = irrigation_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")



plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                   vars = c("yield_irrigation"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                   vars = c("year_rain"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

print(corMat(c(irrigation_mc_simulation, )))
irrigation_function()


plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "output_1")

ggplot(data.frame(profit_no_irrigation, x=years), aes(profit_no_irrigation)) +                    
  geom_line(aes(y=cumsum(profit_no_irrigation), color="no irri")) +  
  geom_line(aes(y=cumsum(profit_with_well), color="well")) +  
  geom_line(aes(y=cumsum(profit_with_raincatch), color="raincatch"))

################################################################################

dice_function <- function(x){
n <- 0
res <- vector("numeric", x)
repeat{
n=n+1
dice_1 <- sample(1:6, 1, replace = TRUE)
dice_2 <- sample(1:6, 1, replace = TRUE)
dice_3 <- sample(1:6, 1, replace = TRUE)
dice_4 <- sample(1:6, 1, replace = TRUE)
c <- dice_2 + dice_1 + dice_3 + dice_4
res[n] <- c
if (n == x)
  break
}
return(res)
}

hist(dice_function(100))

################################################################################
income_estimates <- data.frame(variable = c("dice_1",
                                            "dice_2"),
                               lower = c(1, 1),
                               median = NA,
                               upper = c(6, 6),
                               distribution = c("posnorm",
                                                "posnorm"))


dice_function <- function(){
  n <- 0
    c <- dice_2 + dice_1 
  return(list(c=c))
}


dice_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates),
                                         model_function = dice_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = dice_mc_simulation, 
                   vars = c("c"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)




flor <- vv(var_mean = 7, 
                     var_CV = 1,
                     relative_trend = 2,
                     n = 40)

plot(flor)
sd(flor)
hist(flor, breaks = 50)

rnorm(40, 7, 1)


################################################################################

# Boxplots
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = c("NPV_well",
                                             "NPV_no_irrigation",
                                             "NPV_raincatch"),
                                    method = 'boxplot')

### Well analysis

# Value of the decision for installing a well
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_decision_well", 
                                    method = 'boxplot_density')+
  xlim(-3000000, 25000000)+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))

# Value of Information analysis

mcSimulation_table_well <- data.frame(irrigation_mc_simulation$x, irrigation_mc_simulation$y[c(1,2,4)])

evpi_well <- multi_EVPI(mc = mcSimulation_table_well, first_out_var = "NPV_no_irrigation")

plot_evpi(evpi_well, decision_vars = "NPV_decision_well")

# Compound figure 

compound_figure(mcSimulation_object = irrigation_mc_simulation, 
                input_table = income_estimates, plsrResults = pls_result_well, 
                EVPIresults = evpi_well, decision_var_name = "NPV_decision_well", 
                cashflow_var_name = "Cashflow_well", 
                base_size = 7)

### Rainwater capture analysis

# Value of the decision for installing rain capture structures
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_decision_raincatch",
                                    method = 'boxplot_density')+
  xlim(-3000000, 25000000)+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))
  
farma_saiza <- 1:15
ifelse(farma_saiza/5 == 1, "flor", "no flor")
plot(log2(farma_saiza)+1)

prueba <- sample(1:500, 5)

matriz <- matrix(1:14, nrow = 4)

arboles <- lapply(X = trees, FUN = mean)

class(arboles)
  
## return of investent function v1.0
yinter <- vector()
x <- 1
years <- 1:10


while (x < 1001)
      {
        prueba <- vector()
  prueba[1] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch1"]][[x]]
  prueba[2] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch2"]][[x]]
  prueba[3] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch3"]][[x]]
  prueba[4] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch4"]][[x]]
  prueba[5] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch5"]][[x]]
  prueba[6] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch6"]][[x]]
  prueba[7] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch7"]][[x]]
  prueba[8] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch8"]][[x]]
  prueba[9] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch9"]][[x]]
  prueba[10] <- irrigation_mc_simulation[["y"]][["Cashflow_raincatch10"]][[x]]
  yinter[x] <- summary(lm(years~prueba))$coefficients[1, 1]
  x <- x + 1
}

hist(yinter, 1000)

quantile(yinter)

################### ROI function v2.3
ROI <- function(z, n, variable, mcsimu)
{
  yinter <- vector()
  years <- 1:z
  y <- 1
  while (y <= n)
  {
    res <- vector()
    x <- 1
    while (x <= z)
    {
      res[x] <- mcsimu[["y"]][[paste(variable, x, sep="")]][[y]]
      x <- x + 1
    }
    yinter[y] <- summary(lm(res~years))$coefficients[1, 1]/-(summary(lm(res~years))$coefficients[2, 1])
    y <- y + 1
  }
  return(yinter)
}

prueba <- ROI(10, 1000, "Cashflow_precise_irrigation", irrigation_mc_simulation)

quantile(prueba, probs = c(5, 25, 50, 75, 95)/100)

hist(prueba, breaks = 1000)

v1 <- rnorm(100, 50, 5)
v2 <- rnorm(100, 80, 5)
v3 <- rnorm(100, 90, 5)
v4 <- rnorm(100, 150, 5)


mi.lista <- list(v1, v2, v3, v4, v5, v6)

mi.lista <- list(mi.lista, mi.lista)

ROI(z=6, n=100, mcsimu=mi.lista)

plot(yinter)

lolia <- 1:6
loltun <- c(43.7, 89.9, 100.7, 152, 401, 501)

summary(lm(loltun~lolia))$coefficients[1, 1]/-(summary(lm(loltun~lolia))$coefficients[2, 1])

c(irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation1"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation2"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation3"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation4"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation5"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation6"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation7"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation8"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation9"]][[734]],
  irrigation_mc_simulation[["y"]][["Cashflow_precise_irrigation10"]][[734]])

################################################################################


# Housekeeping
rm(list = ls())

# A function to let you test code line by line 
make_variables <- function(est, n=1)
  
{ x <-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("Input_table_CWR.csv"))

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
  Ksv <- vector()
  ADTv <- vector()
  Kev <- vector()
  Kcbv <- vector()
  Krv <- vector()
  fewv <- vector()
  x <- 30
 
  # Need to adjust fc so it's not smaller than wp. Maybe do a table thing for soil type
  fc <- wp+fcd
  
  while (i<=x+1)
  {
    # Daily changing values
    tmax <- chance_event(chance = 1, 
                         value_if = tmax1,
                         value_if_not = 0,
                         n = 1)
    
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
    Ksv[i] <- Ks
    ADTv[i] <- ADT
    Kev[i] <- Ke 
    Kcbv[i] <- Kcb
    Krv[i] <- Kr
    fewv[i] <- tmax
    i <- i+1
  }
  return(list(Etc_adj_v,
              Irr,
              Dr,
              Ksv,
              ADTv,
              Etc,
              Kev,
              Kcbv,
              Krv,
              fewv))
}

CWR_function()

plot(lol[[3]])

# Cambiar Etc[i] por etc_adj[i]
# borrar el Dr[i]+Dr[i-1] ... ya se suma en la funcion
# cambiar fc para que no sea menor a wp


z <- 1
flor <- vector()
while (z<x)
{
  w1 <- sample(weather_estimates[[rhmax]], 1)
  z <- z+1
  flor[z] <- w1 
}

flor


zz <- c(1:10)
res <- vector()
www <- 1
while(www<10)
{
  lolia2 <- chance_event(chance = 1, 
                       value_if = ,
                       value_if_not = 0,
                       n = 1)
  res[www] <- lolia2
  www <- www+1
}












