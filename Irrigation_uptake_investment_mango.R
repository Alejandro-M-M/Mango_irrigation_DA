
# A function to let you test code line by line 
make_variables <- function(est, n=1)
  
{ x <-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv("Input_table_uptake.csv"))

prices_irrigation <- vv(var_mean = mango_price_irrigation, 
                        var_CV = Var_CV,
                        relative_trend = 2,
                        n = 5)

plot(prices_irrigation)

# Housekeeping
rm(list = ls())

setwd("C:/Users/tito_/Dropbox/Thesis/R") 

.libPaths("C:/Users/tito_/Dropbox/Thesis/R/Library")

# Library 
library(decisionSupport)

# Input table
income_estimates <- read.csv("Input_table_uptake.csv")


# Function
irrigation_function <- function(){
  
  farm_size <- 4
  
  prices_irrigation <- vv(var_mean = mango_price_irrigation, 
                       var_CV = Var_CV,
                       relative_trend = 5,
                       n = 5)
  
  
  prices_no_irrigation <- vv(var_mean = mango_price_no_irrigation, 
                          var_CV = Var_CV,
                          relative_trend = 5,
                          n = 5)
  
  # Risk of dry well
  "adjusted_yield_increase <- chance_event(chance = Residue_risk, 
                                               value_if = Active_export_per_year_containers_1MCP-2,
                                               value_if_not = Active_export_per_year_containers_1MCP,
                                               n = 5)"
  
  # Profit without irrigation
  profit_no_irrigation <- prices_no_irrigation*yield_no_irrigation*farm_size
  
  # Profit with well
  profit_with_well <- (prices_irrigation*yield_irrigation*farm_size)-
    investment_cost_well-study_cost_well-(maintenance_well*5)
  
  # Profit with water waterpot
  profit_with_waterpot <- (prices_irrigation*yield_irrigation*farm_size)-
    investment_cost_waterpot*farm_size-(maintenance_waterpot*5)
  
  # Discount rate
  NPV_no_irrigation <- discount(profit_no_irrigation, discount_rate = dis_rate, calculate_NPV = TRUE)
  NPV_well <- discount(profit_with_well, discount_rate = dis_rate, calculate_NPV = TRUE)
  NPV_waterpot <- discount(profit_with_waterpot, discount_rate = dis_rate, calculate_NPV = TRUE)
  
  # Overall NPV of the decision (do - don't do)
  NPV_decision_well <- NPV_well-NPV_no_irrigation
  NPV_decision_waterpot <- NPV_waterpot-NPV_no_irrigation
  
  return(list(NPV_no_irrigation =  NPV_no_irrigation,
              NPV_well =  NPV_well, 
              NPV_waterpot = NPV_waterpot,
              NPV_decision_well = NPV_well - NPV_no_irrigation,
              NPV_decision_waterpot = NPV_waterpot - NPV_no_irrigation,
              Cashflow_well = profit_with_well - profit_no_irrigation,
              Cashflow_waterpot = profit_with_waterpot - profit_no_irrigation))
}

# Monte Carlo simulation using the model function
irrigation_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates),
                                  model_function = irrigation_function,
                                  numberOfModelRuns = 1000,
                                  functionSyntax = "plainNames")

# Results of a Monte Carlo simulation for estimating the comparative 
# profits with and without irrigation
plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                   vars = c("NPV_well", "NPV_waterpot", "NPV_no_irrigation"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

# Boxplots
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = c("NPV_well",
                                             "NPV_no_irrigation",
                                             "NPV_waterpot"),
                                    method = 'boxplot')

### Well analysis

# Value of the decision for installing a well
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_decision_well", 
                                    method = 'boxplot_density')

# Cashflow simulation 
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_well")


# Projection to Latent Structure Analysis 
pls_result_well <- plsr.mcSimulation(object = irrigation_mc_simulation,
                                resultName = names(irrigation_mc_simulation$y)[4], ncomp = 1)

plot_pls(pls_result_well, input_table = income_estimates, threshold = 0, x_axis_name = 
           "Variable Importance in Projection - Well investment")

# Value of Information analysis

mcSimulation_table <- data.frame(irrigation_mc_simulation$x, irrigation_mc_simulation$y[1:5])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_well")

plot_evpi(evpi, decision_vars = "NPV_decision_well")

# Compound figure 

compound_figure(mcSimulation_object = irrigation_mc_simulation, 
                input_table = income_estimates, plsrResults = pls_result_well, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_well", 
                cashflow_var_name = "Cashflow_well", 
                base_size = 7)

### Rainwater capture analysis

# Value of the decision for installing rain capture structures
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_decision_waterpot",
                                    method = 'boxplot_density')

# Cashflow simulation 
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_waterpot")

# Projection to Latent Structure Analysis 
pls_result_rain <- plsr.mcSimulation(object = irrigation_mc_simulation,
                                     resultName = names(irrigation_mc_simulation$y)[5], ncomp = 1)

plot_pls(pls_result_rain, input_table = income_estimates, threshold = 0, x_axis_name = 
           "Variable Importance in Projection - Rain capture investment")

# Value of Information analysis

mcSimulation_table <- data.frame(irrigation_mc_simulation$x, irrigation_mc_simulation$y[1:5])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_waterpot")

plot_evpi(evpi, decision_vars = "NPV_decision_waterpot")

# Compound figure 

compound_figure(mcSimulation_object = irrigation_mc_simulation, 
                input_table = income_estimates, plsrResults = pls_result_rain, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_waterpot", 
                cashflow_var_name = "Cashflow_waterpot", 
                base_size = 7)
