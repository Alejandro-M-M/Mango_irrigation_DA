
# Housekeeping
rm(list = ls())

setwd("C:/Users/tito_/Dropbox/Thesis/R") 

.libPaths("C:/Users/tito_/Dropbox/Thesis/R/Library")

# Library shit
library(decisionSupport)
library(igraph)
library(DiagrammeR)


# Input table
income_estimates <- data.frame(variable = c("mango_price_ton_MXN", 
                                          "Water_needed_irrigation_mm", 
                                          "growing_season_months",
                                          "rainwaer_growing_season_mm",
                                          "Investment_cost_well_MXN",
                                          "Investment_cost_waterpot_MXN",
                                          "water_retention_waterpot_m3",
                                          "maintenance_waterpot_year_MXN",
                                          "maintenance_well_year_MXN",
                                          "yield_increase",
                                          "Var_CV",
                                          "mango_yield_tons_per_hec",
                                          "market_risk_sales"),
                             lower = c(3000, 300, 2, 570, 10000, 20000, 200, 2000, 1000, 0.1, 0.75, 5,
                                       0.1),
                             median = NA,
                             upper = c(25000, 800, 5, 725, 25000, 30000, 500, 5000, 5000, 0.5, 0.75, 12,
                                       1),
                             distribution = c("posnorm", 
                                              "posnorm", 
                                              "posnorm", 
                                              "posnorm",
                                              "posnorm",
                                              "posnorm",
                                              "posnorm", 
                                              "posnorm", 
                                              "posnorm", 
                                              "posnorm",
                                              "const",
                                              "posnorm",
                                              "posnorm"),
                             label = c("Mango sale price for farmers",
                                       "Water for irrigation",
                                       "Lenght of the growing season",
                                       "rainwater during growing season",
                                       "Investment cost - well",
                                       "Investment cost - waterpot",
                                       "Amount of water in waterpot",
                                       "Maintenance cost for the waterpot",
                                       "Maintenance cost for the well",
                                       "Increase in yield when irrigated",
                                       "Variation thing",
                                       "Mango yield tons/hectare",
                                       "Risk of not selling extra production"),
                             Description = c("Mango sale price for the farmers per tons in MXN",
                                             "Water that the orchard needs for irrigation in milimeters",
                                             "Lenght of the growing seasno in months",
                                             "rainwater present during the growing season",
                                             "Investment cost to install a well",
                                             "Investment cost to install a waterpot",
                                             "Amount of water the waterpot can keep",
                                             "Maintenance cost for the waterpot",
                                             "Maintenance cost for the well",
                                             "Increase in yield when irrigated",
                                             "Variation thing",
                                             "Mango yield expressed in tons/hectare",
                                             "Risk of not being able to sell the extra production of mangos"))

# Function
irrigation_function <- function(){
  
  farm_size <- 2
  
  Prices <- vv(var_mean = mango_price_ton_MXN, 
               var_CV = Var_CV,
               relative_trend = 5,
               n = 5)
  
  
  extra_income <- Prices*((1+yield_increase)*mango_yield_tons_per_hec*farm_size*
                            market_risk_sales)
  
  # Risk of low rainfall
  "adjusted_yield_increase <- chance_event(chance = Residue_risk, 
                                               value_if = Active_export_per_year_containers_1MCP-2,
                                               value_if_not = Active_export_per_year_containers_1MCP,
                                               n = 5)"
  
  # Profit without irrigation
  profit_no_irrigation <- Prices*mango_yield_tons_per_hec*farm_size
  
  # Profit with irrigation
  profit_with_irrigation <- profit_no_irrigation+extra_income-Investment_cost_waterpot_MXN-
    (maintenance_waterpot_year_MXN*5)
  
  # Discount rate
  NPV_no_irrigation <- discount(profit_no_irrigation, discount_rate = 10, calculate_NPV = TRUE)
  NPV_irrigation <- discount(profit_with_irrigation, discount_rate = 10, calculate_NPV = TRUE)
  
  # Overall NPV of the decision (do - don't do)
  NPV_decision <- NPV_irrigation-NPV_no_irrigation
  
  return(list(NPV_no_irrigation =  NPV_no_irrigation,
              NPV_irrigation =  NPV_irrigation, 
              NPV_decision = NPV_irrigation - NPV_no_irrigation,
              Cashflow_decision = profit_with_irrigation - profit_no_irrigation))
}


# Monte Carlo simulation using the model function
irrigation_mc_simulation <- mcSimulation(estimate = as.estimate(income_estimates),
                                  model_function = irrigation_function,
                                  numberOfModelRuns = 1000,
                                  functionSyntax = "plainNames")

# Results of a Monte Carlo simulation for estimating the comparative 
# profits with and without irrigation
plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                   vars = c("NPV_no_irrigation", "NPV_irrigation"),
                   method = 'smooth_simple_overlay', 
                   base_size = 7)

# Boxplots
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = c("NPV_irrigation",
                                             "NPV_no_irrigation"),
                                    method = 'boxplot')

# Value of the decision
decisionSupport::plot_distributions(mcSimulation_object = irrigation_mc_simulation, 
                                    vars = "NPV_decision",
                                    method = 'boxplot_density')

# Cashflowsimulation
plot_cashflow(mcSimulation_object = irrigation_mc_simulation, cashflow_var_name = "Cashflow_decision")

# Projection to Latent Structure Analysis
pls_result <- plsr.mcSimulation(object = irrigation_mc_simulation,
                                resultName = names(irrigation_mc_simulation$y)[3], ncomp = 1)

plot_pls(pls_result, input_table = income_estimates, threshold = 0)

# Value of Information analysis

# Here we subset the outputs from the mcSimulation function (y) by selecting the correct variables
# choose this carefully and be sure to run the multi_EVPI only on the variables that the you want
mcSimulation_table <- data.frame(irrigation_mc_simulation$x, irrigation_mc_simulation$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_irrigation")

# We use the function plot_evpi() on the results from multi_EVPI() to plot the Expected 
# Value of Perfect Information (EVPI). Here we show the results with the standard settings. 
# The length of the bars is equal to EVPI.
plot_evpi(evpi, decision_vars = "NPV_decision")

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
