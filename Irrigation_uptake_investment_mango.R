
# Housekeeping
rm(list = ls())

# Library 
library(decisionSupport)
library(ggplot2)

# Input table
income_estimates <- read.csv("Input_table_uptake.csv")

# Function (chance no underground + risk not selling extra)
irrigation_function <- function(){
  
  # Prices for the mango. The relative trend and the variable coefficient were 
  # calculated with data from SIACON since 2009. Relative trend was calculated
  # by getting the average of the individual relative trend between years. Var_CV
  # was obtained by calculating the standard deviation of the prices, having the 
  # mean of the values given by the function with the relative trend. 
  # File - Precio_mango_Guerrero_1980_2019.xlsx
  
  prices_irrigation <- vv(var_mean = mango_price_irrigation, 
                       var_CV = Var_CV,
                       relative_trend = trend,
                       n = years)
  
  
  prices_no_irrigation <- vv(var_mean = mango_price_no_irrigation, 
                          var_CV = Var_CV,
                          relative_trend = trend,
                          n = years)
  
  labor_cost <- yield_irrigation*farm_size*labor_cost
  
  # Profit without irrigation
  profit_no_irrigation <- prices_no_irrigation*yield_no_irrigation*farm_size
  

  # Profit with well. Here, as well as with raincatch, an investment for an irrigation
  # system was considered. Also infrastructure cost for a storehouse. The investments
  # were fixed to the first value of the profit vector. For the specific case of the
  # deep well, a preliminary study cost is added, to assess the location of the aquifer 
  # and the requirements for the excavation. More than one well may be needed to supply
  # the field
  
  well_supply <- ceiling(farm_size/well_water)
  
  profit_with_well <- prices_irrigation*yield_irrigation*farm_size*water_quality-labor_cost-maintenance_well 
  profit_with_well[1] <- profit_with_well[1] - (investment_cost_well*well_supply+study_cost_well)-
    (cost_irrigation_system*farm_size)-infrastructure_cost 
  
  # Risk of not finding underground water *****(Only for 1st year with consecutive results)
  ## does this makes sense though?? ??? non repeatable risk...
  #profit_with_well <- chance_event(chance = chance_no_underwater, 
   #                                value_if = profit_no_irrigation - study_cost_well,
    #                               value_if_not = profit_well,
     #                              n = years)
  
  # Profit with raincatch. The reduction of the farm for the raincatch area must be considered
  # An area of 30x30 m is considered, reducing the farm size by 9%. Log2 is used on farm size 
  # because expenses are reduced while escalating raincatch size (needs adjustment).  
  profit_with_raincatch <- (prices_irrigation*yield_irrigation*farm_size*(1-raincatch_area))-labor_cost-
    maintenance_raincatch
  profit_with_raincatch[1] <- profit_with_raincatch[1] - investment_cost_raincatch*(log2(farm_size)+1)-
    (cost_irrigation_system*farm_size*(1-raincatch_area))-infrastructure_cost
  
  # Profit with precise irrigation
  profit_precise_irrigation <- prices_irrigation*yield_irrigation*farm_size*yield_increase
    profit_precise_irrigation[1] <- profit_precise_irrigation[1] - cost_precise_irri
  
  # Discount rate
  NPV_no_irrigation <- discount(profit_no_irrigation, discount_rate = dis_rate, calculate_NPV = TRUE)
  NPV_well <- discount(profit_with_well, discount_rate = dis_rate, calculate_NPV = TRUE)
  NPV_raincatch <- discount(profit_with_raincatch, discount_rate = dis_rate, calculate_NPV = TRUE)
  NPV_precise_irrigation <- discount(profit_precise_irrigation, discount_rate = dis_rate, calculate_NPV = TRUE)
  
  # Overall NPV of the decision (do - don't do)
  NPV_decision_well <- NPV_well-NPV_no_irrigation
  NPV_decision_raincatch <- NPV_raincatch-NPV_no_irrigation
  
  return(list(NPV_no_irrigation =  NPV_no_irrigation,
              NPV_well =  NPV_well, 
              NPV_raincatch = NPV_raincatch,
              NPV_decision_well = NPV_well - NPV_no_irrigation,
              NPV_decision_raincatch = NPV_raincatch - NPV_no_irrigation,
              NPV_rain_vs_well = NPV_raincatch - NPV_well,
              Cashflow_well = cumsum(profit_with_well - profit_no_irrigation),
              Cashflow_raincatch = cumsum(profit_with_raincatch - profit_no_irrigation),
              Cashflow_precise_irrigation = cumsum(profit_precise_irrigation)))
}

# Correlation matrix for rain. yield and price
correlation_matrix <- "         ,                   year_rain, yield_irrigation, mango_price_irrigation, rain_gs, yield_no_irrigation, mango_price_no_irrigation
                          year_rain,                    1,           0.4,                 0.7,              0,          0,                      0
                          yield_irrigation,             0.4,         1,                   0,                0,          0,                      0
                          mango_price_irrigation,       0.7,         0,                   1,                0,          0,                      0 
                          rain_gs,                      0,           0,                   0,                1,          0.6,                    0.8
                          yield_no_irrigation,          0,           0,                   0,                0.6,        1,                      0
                          mango_price_no_irrigation,    0,           0,                   0,                0.8,        0,                      1 "

# Monte Carlo simulation using the model function
irrigation_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates, 
                                         correlation_matrix=data.matrix(read.csv(text = correlation_matrix, 
                                             row.names=1,
                                             strip.white=TRUE))),
                                         model_function = irrigation_function,
                                         numberOfModelRuns = 1000,
                                         functionSyntax = "plainNames")

# Results of a Monte Carlo simulation for estimating the comparative 
# profits with and without irrigation
plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                   vars = c("NPV_well", "NPV_raincatch", "NPV_no_irrigation"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)+
                   xlim(-3000000, 25000000)+
                   geom_vline(aes(xintercept = 0))
                   
# Cashflow simulation for a well
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_well",
              color_25_75 = "dodgerblue4", color_5_95 = "dodgerblue3", color_median = "red")+
  scale_x_continuous(labels = c("0":"10"), breaks = c("0":"10"))

# Cashflow simulation for rainwater
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_raincatch")+
  scale_x_continuous(breaks = c("1":"10"))

# Raincatch decision over well
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_rain_vs_well", 
                                    method = 'boxplot_density')+
  xlim(-8000000, 2500000)+
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = 0))

# Projection to Latent Structure Analysis for raincatch vs well
pls_result_well <- plsr.mcSimulation(object = irrigation_mc_simulation,
                                     resultName = names(irrigation_mc_simulation$y)[6], ncomp = 1)

plot_pls(pls_result_well, input_table = income_estimates, threshold = 0, x_axis_name = 
           "Variable Importance in Projection - Raincatch vs well")

# Value of Information analysis
mcSimulation_table <- data.frame(irrigation_mc_simulation$x, irrigation_mc_simulation$y[c(2,3,6)])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_well")

plot_evpi(evpi, decision_vars = "NPV_rain_vs_well")


# Cashflow of investing in precise irrigation 
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_precise_irrigation")+
  scale_x_continuous(breaks = c("1":"11"))


# Return of investment. This function takes the cumulative cashflow of a decision and calculates the time 
# it would take to recover the initial investment. The objects it receives are the number of years simulated (z),
# the number of runs of the monte carlo analysis (n), the name of the cashflow variable (variable) and the name
# of the monte carlo simulation (mcsimu). The function takes the z number of values for the n monte carlo run,
# and uses a linear  regression to find the y = 0 intercept value (years to recover investment). This is done 
# for all n values. It result is a "bell-shaped" distribution, leaning over the right side (Chi-squared??). 

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

