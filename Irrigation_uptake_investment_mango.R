
# Housekeeping
rm(list = ls())

setwd("C:/Users/tito_/Dropbox/Thesis/R") 

.libPaths("C:/Users/tito_/Dropbox/Thesis/R/Library")

# Library 
library(decisionSupport)

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
  
  # Risk of low rainfall
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

# Value of the decision for installing a well
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_decision_well", 
                                    method = 'boxplot_density')

# Value of the decision for installing a waterpot
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_decision_waterpot",
                                    method = 'boxplot_density')

# Cashflow simulation well
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_well")

# Cashflow simulation waterpot
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_waterpot")


#######################################
#######################################
#######################################
#######################################

# Projection to Latent Structure Analysis
pls_result <- plsr.mcSimulation(object = irrigation_mc_simulation,
                                resultName = names(irrigation_mc_simulation$y)[3], ncomp = 1)

plot_pls(pls_result, input_table = income_estimates, threshold = 0)

# Value of Information analysis

# Here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# choose this carefully and be sure to run the multi_EVPI only on the variables that the you want
mcSimulation_table <- data.frame(irrigation_mc_simulation$x, irrigation_mc_simulation$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_well")

# We use the function plot_evpi() on the results from multi_EVPI() to plot the Expected 
# Value of Perfect Information (EVPI). Here we show the results with the standard settings. 
# The length of the bars is equal to EVPI.
plot_evpi(evpi, decision_vars = "NPV_decision_well")

# Finally, we can use the compound_figure() function to provide a single figure for 
# a quick assessment. The can be used to run the full decision assessment for a simple 
# binary decision ('do' or 'do not do').

compound_figure(mcSimulation_object = irrigation_mc_simulation, 
                input_table = income_estimates, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision", 
                cashflow_var_name = "Cashflow_decision", 
                base_size = 7)

# Value of Information (VoI) analysis with the Expected Value of Perfect Information (EVPI). 
mcSimulation_table_irrigation <- data.frame(irrigation_mc_simulation$x, 
                                      irrigation_mc_simulation$y[3])

evpi_irrigation <- multi_EVPI(mc = mcSimulation_table_irrigation, 
                        first_out_var = "NPV_decision")

# Expected Value of Perfect Information (EVPI). Results with the standard settings. 
# The length of the bars is equal to EVPI.
plot_evpi(evpi_irrigation, decision_vars = "NPV_decision")

