
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

################### ROI function v2.0
ROI <- function(z, n, variable, mcsimu)
{
  yinter <- vector()
  years <- 1:10
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
    yinter[y] <- summary(lm(years~res))$coefficients[1, 1]
    y <- y + 1
  }
  return(yinter)
}

prueba <- ROI(10, 1000, "Cashflow_precise_irrigation", irrigation_mc_simulation)

quantile(prueba, probs = c(5, 25, 50, 75, 95)/100)

hist(prueba, breaks = 1000)

prueba


