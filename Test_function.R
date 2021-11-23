
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
test_function <- function(){
  yield_as_rain <- 11+((year_rain-500)/(1500-500))*(13-11)
  yield_rain_var <- vv(var_mean = yield_as_rain, 
                       var_CV = 17,
                       relative_trend = 0,
                       n = 1)
  aardvark <- rnorm(1, yield_as_rain, 0.6137)
  
  return(list(yield_as_rain = yield_as_rain,
              yield_rain_var = yield_rain_var,
              aardvark = aardvark))
}
  
test_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates),
                                         model_function = test_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")

plot_distributions(mcSimulation_object = test_mc_simulation, 
                   vars = c("yield_as_rain", "yield_irrigation", "aardvark"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

sd(as.vector(t(test_mc_simulation$y[1])))

sd(as.vector(t(test_mc_simulation$x[14])))

sd(as.vector(t(test_mc_simulation$y[2])))


str(test_mc_simulation)

################################################################################
irrigation_function <- function(){
  
farm_size <- 2

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
profit_well <- prices_irrigation*yield_irrigation*farm_size-maintenance_well 
profit_well[1] <- profit_well[1] - (investment_cost_well+study_cost_well)

# Risk of not finding underground water
profit_with_well <- chance_event(chance = chance_no_underwater, 
                                 value_if = profit_no_irrigation - study_cost_well,
                                 value_if_not = profit_well,
                                 n = years)

# Profit with raincatch
profit_with_raincatch <- (prices_irrigation*yield_irrigation*farm_size)-
  maintenance_raincatch
profit_with_raincatch[1] <- profit_with_raincatch[1] - investment_cost_raincatch*farm_size
return (Cashflow_well = cumsum(profit_with_well - profit_no_irrigation))

}

irrigation_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates),
                                         model_function = irrigation_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")


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




flor <- vv(var_mean = 12, 
                     var_CV = 20,
                     relative_trend = 0,
                     n = 2)
sd(flor)
hist(flor, breaks = 50)





