
# Housekeeping
rm(list = ls())

setwd("C:/Users/tito_/Dropbox/Thesis/R") 

.libPaths("C:/Users/tito_/Dropbox/Thesis/R/Library")

# Library 
library(decisionSupport)
library(ggplot2)

# Input table
income_estimates <- read.csv("Input_table_uptake.csv")

# Function
irrigation_function <- function(){
  
  # Prices for the mango. The relative trend and the variable coefficient were 
  # calculated with data from SIACON since 2009. Relative trend was calculated
  # by getting the average of the individual relative trend between years. Var_CV
  # was obtained by calculating the standard deviation of the prices, having the 
  # mean of the values given by the function with the relative trend. 
  # File → Precio_mango_Guerrero_1980_2019.xlsx
  
  prices_irrigation <- vv(var_mean = mango_price_irrigation, 
                       var_CV = Var_CV,
                       relative_trend = 6.41,
                       n = years)
  
  
  prices_no_irrigation <- vv(var_mean = mango_price_no_irrigation, 
                          var_CV = Var_CV,
                          relative_trend = 6.41,
                          n = years)
  
  #labor_cost <- for irrigation and harvesting. dependent of yield and farm size 
  
  # Profit without irrigation
  profit_no_irrigation <- prices_no_irrigation*yield_no_irrigation*farm_size
  

  # Profit with well
  profit_with_well <- prices_irrigation*yield_irrigation*farm_size-maintenance_well 
  profit_with_well[1] <- profit_with_well[1] - (investment_cost_well+study_cost_well)-
    (cost_irrigation_system*farm_size)-infrastructure_cost 
  
  # Risk of not finding underground water *****(Only for 1st year with consecutive results)
  ## does this makes sense though?? ??? non repeatable risk...
  #profit_with_well <- chance_event(chance = chance_no_underwater, 
   #                                value_if = profit_no_irrigation - study_cost_well,
    #                               value_if_not = profit_well,
     #                              n = years)
  
  # Profit with raincatch
  profit_with_raincatch <- (prices_irrigation*yield_irrigation*farm_size)-
    maintenance_raincatch
  profit_with_raincatch[1] <- profit_with_raincatch[1] - investment_cost_raincatch*farm_size-
    (cost_irrigation_system*farm_size)-infrastructure_cost
  
  # Discount rate
  NPV_no_irrigation <- discount(profit_no_irrigation, discount_rate = dis_rate, calculate_NPV = TRUE)
  NPV_well <- discount(profit_with_well, discount_rate = dis_rate, calculate_NPV = TRUE)
  NPV_raincatch <- discount(profit_with_raincatch, discount_rate = dis_rate, calculate_NPV = TRUE)
  
  # Overall NPV of the decision (do - don't do)
  NPV_decision_well <- NPV_well-NPV_no_irrigation
  NPV_decision_raincatch <- NPV_raincatch-NPV_no_irrigation
  
  return(list(NPV_no_irrigation =  NPV_no_irrigation,
              NPV_well =  NPV_well, 
              NPV_raincatch = NPV_raincatch,
              NPV_decision_well = NPV_well - NPV_no_irrigation,
              NPV_decision_raincatch = NPV_raincatch - NPV_no_irrigation,
              Cashflow_well = cumsum(profit_with_well - profit_no_irrigation),
              Cashflow_raincatch = cumsum(profit_with_raincatch - profit_no_irrigation)))
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

# Cashflow simulation 
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_well",
  color_25_75 = "dodgerblue4", color_5_95 = "dodgerblue3", color_median = "red")+
  scale_x_continuous(labels = c("0":"10"), breaks = c("0":"10"))
              

# Projection to Latent Structure Analysis 
pls_result_well <- plsr.mcSimulation(object = irrigation_mc_simulation,
                                resultName = names(irrigation_mc_simulation$y)[4], ncomp = 1)

plot_pls(pls_result_well, input_table = income_estimates, threshold = 0, x_axis_name = 
           "Variable Importance in Projection - Well investment")

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
                                      

# Cashflow simulation 
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_raincatch")+
  scale_x_continuous(breaks = c("1":"10"))

# Projection to Latent Structure Analysis 
pls_result_rain <- plsr.mcSimulation(object = irrigation_mc_simulation,
                                     resultName = names(irrigation_mc_simulation$y)[5], ncomp = 1)

plot_pls(pls_result_rain, input_table = income_estimates, threshold = 0, x_axis_name = 
           "Variable Importance in Projection - Rain capture investment")

# Value of Information analysis

mcSimulation_table_rainwater <- data.frame(irrigation_mc_simulation$x, irrigation_mc_simulation$y[c(1,3,5)])

evpi_rainwater <- multi_EVPI(mc = mcSimulation_table_rainwater, first_out_var = "NPV_no_irrigation")

plot_evpi(evpi_rainwater, decision_vars = "NPV_decision_raincatch")

# Compound figure 

compound_figure(mcSimulation_object = irrigation_mc_simulation, 
                input_table = income_estimates, plsrResults = pls_result_rain, 
                EVPIresults = evpi_rainwater, decision_var_name = "NPV_decision_raincatch", 
                cashflow_var_name = "Cashflow_raincatch", 
                base_size = 7)
