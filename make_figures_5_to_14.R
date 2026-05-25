# Creates the plots #5 to #13 
#--------------------------------------------------------------------------------------------
# Input:
# 
# Data should be in RDS format, with the following structure
#
# filename--
#           |
#           variable 1--
#                       | 
#                       baseline -- 
#                                  |   
#                                  year (1980-2150)
#                                  low1 (16.5% quantile)
#                                  median (50% quantile)
#                                  high1 (83.4% quantile)
#                   counterfactual (the NoImpacts case) -- 
#                                  |   
#                                  year 
#                                  low1 
#                                  median 
#                                  high1 
#                   abs.difference (absolute differences between every ensemble member) -- 
#                                  |   
#                                  year (1980-2150)
#                                  low1 
#                                  median 
#                                  high1 
#                   per.difference (percent differences between every ensemble member) -- 
#                                  |   
#                                  year (1980-2150)
#                                  low1 
#                                  median 
#                                  high1 
#            variable 2 -- and so on
#
#------------ How to execute -------------
# This script can be run on a standard Rstudio terminal, visual code terminal, or a Linux terminal
# with the command - source('make_figures_5_to_14.R')
#
# Muralidhar Adakudlu, Norwegian Meteorological Institute
#--------------------------------------------------------------------------------------------------


#------ Load required libraries 

# install.packages(c(
#"tidyverse",
#"ggpubr",
#"latex2exp",
#"abind",
#"patchwork",
#"rstudioapi",
#"extrafont",
#"showtext"
#))
#

library(tidyverse)
library(ggpubr)
require(latex2exp)
library(abind)
library(patchwork)
library(rstudioapi)
# libraries to adjust plot sizes
library(extrafont)
library(showtext)


# ---------------------------------------------------------------------
# Set working directory to script location
# ---------------------------------------------------------------------
remove(list=ls())
script_path <- normalizePath("make_figures_5_to_14.R")
script_dir  <- dirname(script_path)
setwd(script_dir)
data_dir    <- file.path(script_dir,'data')
figures_dir <- file.path(script_dir, 'figures')

#---------------------------------------------------------------------
# Function to check if the input data directory exists and is not empty
#----------------------------------------------------------------------
check_directory <- function(path) {
  
  # Check if directory exists
  if (!dir.exists(path)) {
    return(FALSE)
  }
  
  # Check if directory is empty
  files <- list.files(path, all.files = TRUE, no.. = TRUE)
  
  if (length(files) == 0) {
    return(FALSE)
  }
  
  return(TRUE)
}

# ---------------------------------------------------------------------
# Read input data
# ---------------------------------------------------------------------
if (check_directory(data_dir)) {
     df.emb       <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_AllImpacts.RDS'))
     df.finance <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_FRoL.RDS'))
     df.labour <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_LaPr.RDS'))
     df.govsp <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_GovSp.RDS'))
     df.mortality <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_Mortality.RDS'))
     df.landuse <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_CrFw.RDS'))
     df.behaviour <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_ClimExHB.RDS'))
     df.energy <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_En.RDS'))
     df.concrete <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_DuCn.RDS'))
     df.slr <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_SLR.RDS'))
     cat("Input data read successfully")
} else {
       stop("I/p directory is missing or is empty.")
     }

#*********** These are the variables we need for the paper
#*
variables.list <- c(
  #---------------- Economy
  "demographics_real_gdp_per_person",
  "government_government_consumption",
  "employment_realised_productivity",
  #-----------------Demography
  "demographics_population",
  "demographics_total_deaths",
  "demographics_births",
  #------------- Energy
  "energy_demand_demand_for_energy",
  #--------- Human behaviour
  "food_demand_direct_food_demand_per_person_per_day",
  #---------- Resources
  "concrete_total_yearly_concrete_use",
  #--------- Land use and agriculture ###
  "freshwater_agricultural_water_withdrawal",
  "crop_crop_yield",
  #----------- Climate
  "energy_balance_model_surface_temperature_anomaly",
  "emissions_total_co2_emissions",
  "emissions_total_ch4_emissions",
  "emissions_total_n2o_emissions",
  "emissions_total_so2_emissions",
  "emissions_total_nox_emissions",
  "emissions_voc_emissions",
  "emissions_co_emissions",
  "emissions_hfc134a_eq_emissions",
  "sea_level_total_global_sea_level_anomaly"
 )

#***************** Read the absolute values *****************

data.absolute <- sapply(variables.list, function(k) 
  {
  map_df(.x=list("counterfactual"=df.emb[[k]]$counterfactual,
                 "emb"=df.emb[[k]]$baseline,
                 "finance"=df.finance[[k]]$baseline,
                 "labor"=df.labour[[k]]$baseline,
                 "govt"=df.govsp[[k]]$baseline,
                 "demography"=df.mortality[[k]]$baseline,
                 "landuse"=df.landuse[[k]]$baseline,
                 "behavior"=df.behaviour[[k]]$baseline,
                 "energy"=df.energy[[k]]$baseline,
                 "concrete"=df.concrete[[k]]$baseline,
                 "slr"=df.slr[[k]]$baseline),
         .f=bind_rows,
         .id="Experiment")
  }, simplify = FALSE)

#***************** Read the percent differences *****************

data.difference <- sapply(variables.list, function(k) 
  {
  map_df(.x=list("emb"=df.emb[[k]]$per.difference,
                 "finance"=df.finance[[k]]$per.difference,
                 "labor"=df.labour[[k]]$per.difference,
                 "govt"=df.govsp[[k]]$per.difference,
                 "demography"=df.mortality[[k]]$per.difference,
                 "landuse"=df.landuse[[k]]$per.difference,
                 "behavior"=df.behaviour[[k]]$per.difference,
                 "energy"=df.energy[[k]]$per.difference,
                 "concrete"=df.concrete[[k]]$per.difference,
                 "slr"=df.slr[[k]]$per.difference),
         .f=bind_rows,
         .id="Experiment")
  }, simplify = FALSE)

#*********** Estimate the total GHG emissions by adding individual sources

data.absolute$emissions_total_ghg_emissions <- map_df(.x=list("total_co2_emissions"=data.absolute[names(data.absolute) %in% c("emissions_total_co2_emissions")],
                     "total_ch4_emissions"=data.absolute[names(data.absolute) %in% c("emissions_total_ch4_emissions")],
                     "total_n2o_emissions"=data.absolute[names(data.absolute) %in% c("emissions_total_n2o_emissions")],
                     "total_so2_emissions"=data.absolute[names(data.absolute) %in% c("emissions_total_so2_emissions")],
                     "total_nox_emissions"=data.absolute[names(data.absolute) %in% c("emissions_total_nox_emissions")],
                     "total_voc_emissions"=data.absolute[names(data.absolute) %in% c("emissions_voc_emissions")],
                     "total_co_emissions"=data.absolute[names(data.absolute) %in% c("emissions_co_emissions")],
                     "total_hfc134a_emissions"=data.absolute[names(data.absolute) %in% c("emissions_hfc134a_eq_emissions")]),
                     .f=bind_rows, .id="variable") %>%
      group_by(year, Experiment) %>% 
      mutate(multiply=ifelse(variable %in% c("total_n2o_emissions","total_hfc134a_emissions"), 1e-3, 1)) %>% 
      summarise(low1=sum(low1*multiply),  
                median=sum(median*multiply), 
                high1=sum(high1*multiply)
                )

data.difference$emissions_total_ghg_emissions <- map_df(.x=list("total_co2_emissions"=data.difference[names(data.difference) %in% c("emissions_total_co2_emissions")],
                                                              "total_ch4_emissions"=data.difference[names(data.difference) %in% c("emissions_total_ch4_emissions")],
                                                              "total_n2o_emissions"=data.difference[names(data.difference) %in% c("emissions_total_n2o_emissions")],
                                                              "total_so2_emissions"=data.difference[names(data.difference) %in% c("emissions_total_so2_emissions")],
                                                              "total_nox_emissions"=data.difference[names(data.difference) %in% c("emissions_total_nox_emissions")],
                                                              "total_voc_emissions"=data.difference[names(data.difference) %in% c("emissions_voc_emissions")],
                                                              "total_co_emissions"=data.difference[names(data.difference) %in% c("emissions_co_emissions")],
                                                              "total_hfc134a_emissions"=data.difference[names(data.difference) %in% c("emissions_hfc134a_eq_emissions")]),
                                                      .f=bind_rows, .id="variable") %>%
  group_by(year, Experiment) %>% 
  mutate(multiply=ifelse(variable %in% c("total_n2o_emissions","total_hfc134a_emissions"), 1e-3, 1)) %>% 
  summarise(low1=sum(low1*multiply),  
            median=sum(median*multiply), 
            high1=sum(high1*multiply),
            )


#------------ Function to manually set the years for which we draw the error bars in plots with absolute values

data.absolute.4.errorbar <- function(data){
  as.data.frame(data) %>% 
    mutate(low1 = ifelse(Experiment %in% "finance" & year %in% seq(2020,2150,by=20), low1, 
                  ifelse(Experiment %in% "govt" & year %in% seq(2023,2150,by=20), low1,
                  ifelse(Experiment %in% "labor" & year %in% seq(2026,2150,by=20), low1,
                  ifelse(Experiment %in% "slr" & year %in% seq(2029,2150,by=20), low1, 
                  ifelse(Experiment %in% "landuse" & year %in% seq(2032,2150,by=20), low1,
                  ifelse(Experiment %in% "behavior" & year %in% seq(2035,2150,by=20), low1,
                  ifelse(Experiment %in% "energy" & year %in% seq(2038,2150,by=20), low1, 
                  ifelse(Experiment %in% "concrete" & year %in% seq(2042,2150,by=20), low1,
                  ifelse(Experiment %in% "demography" & year %in% seq(2045,2150,by=20), low1,
                  ifelse(Experiment %in% "sum of individuals" & year %in% seq(2015,2145,by=30), low1,NA)))))))))),
           high1= ifelse(Experiment %in% "finance" & year %in% seq(2020,2150,by=20), high1, 
                  ifelse(Experiment %in% "govt" & year %in% seq(2023,2150,by=20), high1,
                  ifelse(Experiment %in% "labor" & year %in% seq(2026,2150,by=20), high1,
                  ifelse(Experiment %in% "slr" & year %in% seq(2029,2150,by=20), high1, 
                  ifelse(Experiment %in% "landuse" & year %in% seq(2032,2150,by=20), high1,
                  ifelse(Experiment %in% "behavior" & year %in% seq(2035,2150,by=20), high1,
                  ifelse(Experiment %in% "energy" & year %in% seq(2038,2150,by=20), high1, 
                  ifelse(Experiment %in% "concrete" & year %in% seq(2042,2150,by=20), high1,
                  ifelse(Experiment %in% "demography" & year %in% seq(2045,2150,by=20), high1,
                  ifelse(Experiment %in% "sum of individuals" & year %in% seq(2015,2145,by=30), high1,NA)))))))))),
    )
  }

cat("Data processing complete. Continue to plotting", "\n")

#------------------------------------------------------------------------------------------------------------------
#
# -------------------                             PLOTTING                 ----------------------------------------
#
#------------------------------------------------------------------------------------------------------------------

# Get the plotting functions
source('plotting_functions.R')

showtext_opts(dpi = 300)
showtext_auto()

#*******  Econ variables *********************
data.to.plot <- data.absolute$demographics_real_gdp_per_person
gdp.per.capita.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
                                       shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                                       errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% c("finance","labor","govt", "energy","slr"),],
                                       multiplier=1,
                                       values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                       values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                       values.fill=c("red","black"),
                                       breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                       labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                                     "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (B10/R6)","ClimExHB (B8/R4)","En (B6/B7)",
                                                     "DuCn (B10)","SLR"),
                                       y.label=r'(Bill. 2021\$ $Mp^{-1}$ $Yr^{-1}$)',
                                       title.label="(a) GDP per capita")

data.to.plot <- data.difference$demographics_real_gdp_per_person
gdp.per.capita.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
                                                  multiplier=1,
                                                  values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                                  values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                                  values.fill=c("black"),
                                                  breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                                  labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                                                "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (B10/R6)","ClimExHB (B8/R4)","En (B6/B7)",
                                                                "DuCn (B10)","SLR"),
                                                  y.label=r'(%)',
                                                  title.label="(b) GDP per capita")

data.to.plot <- data.absolute$government_government_consumption
govt.plot.abs <- plot.absolute.logy(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy", "slr"),],
                                                  shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                                                  errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% c("finance","labor","govt", "energy"),],
                                                  multiplier=1,
                                                  values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                                  values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                                  values.fill=c("red","black"),
                                                  breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                                  labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                                                "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (B10/R6)","ClimExHB (B8/R4)","En (B6/B7)",
                                                                "DuCn (B10)","SLR"),
                                                  y.label=r'(Bill.\$ $Yr^{-1}$)',
                                                  title.label="(e) Government spending")

data.to.plot <- data.difference$government_government_consumption
govt.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy", "slr"),],
                                                   multiplier=1,
                                                   values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                                   values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                                   values.fill=c("black"),
                                                   breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                                   labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                                                 "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (B10/R6)","ClimExHB (B8/R4)","En (B6/B7)",
                                                                 "DuCn (B10)","SLR"),
                                                   y.label=r'(%)',
                                                   title.label="(f) Government spending")

data.to.plot <- data.absolute$employment_realised_productivity
labour.plot.abs <- plot.absolute.logy(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
                                        shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                                        errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% c("finance","labor","govt", "energy","slr"),],
                                        multiplier=1,
                                        values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                        values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                        values.fill=c("red","black"),
                                        breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                        labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                                      "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (B10/R6)","ClimExHB (B8/R4)","En (B6/B7)",
                                                      "DuCn (B10)","SLR"),
                                        y.label=r'(dmnl)',
                                        title.label="(c) Labour productivity")

data.to.plot <- data.difference$employment_realised_productivity
labour.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
                                         #shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                                         #errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% c("finance","labor","govt", "energy","slr"),],
                                         multiplier=1,
                                         values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (B10/R6)","ClimExHB (B8/R4)","En (B6/B7)",
                                                       "DuCn (B10)","SLR"),
                                         y.label=r'(%)',
                                         title.label="(d) Labour productivity")


panel.finance.sector <- ggarrange(gdp.per.capita.plot.abs, gdp.per.capita.plot.diff,
                                  labour.plot.abs, labour.plot.diff,
                                  govt.plot.abs, govt.plot.diff, 
                                  nrow=3, ncol=2,
                                  heights= c(1,1,1),widths= c(1,1,1),
                                  align= "v",legend = "bottom",
                                  common.legend = T)


#************************* Mortality **************************
data.to.plot <- data.absolute$demographics_total_deaths
cases.to.plot <- c("counterfactual","emb","finance","labor","govt","demography")
mortality.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                               shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                               errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                               multiplier=1,
                               values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                               values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                               values.fill=c("red","black"),
                               breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                               labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                             "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/B11)","ClimExHB (B8/R4)","En (B6/B7)","DuCn (B10)(B8)","SLR"),
                               y.label=r'(Million $Yr^{-1}$)',
                               title.label="(a) Mortality")

data.to.plot <- data.difference$demographics_total_deaths
mortality.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                                 multiplier=1,
                                 values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                 values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                 values.fill=c("black"),
                                 breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                 labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                               "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/B11)","ClimExHB (B8/R4)","En (B6/B7)","DuCn (B10)(B8)","SLR"),
                                 y.label=r'(%)',
                                 title.label="(b) Mortality")

panel.mortality.sector <- ggarrange(mortality.plot.abs,mortality.plot.diff,
                                    nrow=1, ncol=2,
                                    heights= c(1,1,1),widths= c(1,1,1),
                                    align= "v",legend = "bottom",
                                    common.legend = T)


#****************** Energy demand ********************
data.to.plot <- data.absolute$energy_demand_demand_for_energy
cases.to.plot <- c("counterfactual","emb","finance","labor","landuse", "energy","slr")
energy.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                            shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                            errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                            multiplier=1e-3,
                            values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                            values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                            values.fill=c("red","black"),
                            breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                            labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                          "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B8/R4)","En (B6/B7)","DuCn (B10)(B8)","SLR"),
                            y.label=r'(PWh $Yr^{-1}$)',
                            title.label="(a) Energy demand")

data.to.plot <- data.difference$energy_demand_demand_for_energy
energy.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                             multiplier=1,
                             values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                             values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                             values.fill=c("black"),
                             breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                             labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                           "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B8/R4)","En (B6/B7)","DuCn (B10)(B8)","SLR"),
                             y.label=r'(%)',
                             title.label="(b) Energy demand")

panel.energy.sector <- ggarrange(energy.plot.abs,energy.plot.diff, 
                                  nrow=1, ncol=2,
                                  heights= c(1,1,1),widths= c(1,1,1),
                                  align= "v",legend = "bottom",
                                  common.legend = T)


#*************** Food demand ************************

data.to.plot <- data.absolute$food_demand_direct_food_demand_per_person_per_day
cases.to.plot <- c("counterfactual","emb","finance","labor","govt","landuse", "behavior", "energy","slr")
food.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                            shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                            errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                            multiplier=1e-3,
                            values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                            values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                            values.fill=c("red","black"),
                            breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                            labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                          "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                            y.label=r'(MCal $Person^{-1}$ $Day^{-1}$)',
                            title.label="(a) Food demand")

data.to.plot <- data.difference$food_demand_direct_food_demand_per_person_per_day
cases.to.plot <- c("counterfactual","emb","finance","labor","govt","landuse","behavior", "energy","slr")
food.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                             multiplier=1,
                             values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                             values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                             values.fill=c("black"),
                             breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                             labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                           "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                             y.label=r'(%)',
                             title.label="(b) Food demand")


panel.food.sector <- ggarrange(food.plot.abs,food.plot.diff,
                                 nrow=1, ncol=2,
                                 heights= c(1,1,1),widths= c(1,1,1),
                                 align= "v",legend = "bottom",
                                 common.legend = T)


#************* Resources *****************

data.to.plot <- data.absolute$concrete_total_yearly_concrete_use
cases.to.plot <- c("counterfactual","emb","finance","labor","govt", "energy","concrete","slr")
concrete.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                          shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                          errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                          multiplier=1e-3,
                          values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                          values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                          values.fill=c("red","black"),
                          breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                          labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                        "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                          y.label=r'(Gt $Yr^{-1}$)',
                          title.label="(a) Annual concrete production")

data.to.plot <- data.difference$concrete_total_yearly_concrete_use
concrete.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                           multiplier=1,
                           values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                           values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                           values.fill=c("black"),
                           breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                           labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                         "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                           y.label=r'(%)',
                           title.label="(b) Annual concrete production")

panel.concrete.sector <- ggarrange(concrete.plot.abs,concrete.plot.diff,
                               nrow=1, ncol=2,
                               heights= c(1,1,1),widths= c(1,1,1),
                               align= "v",legend = "bottom",
                               common.legend = T)


#************************* Crop yield and freshwater **************************

data.to.plot <- data.absolute$crop_crop_yield
cases.to.plot <- c("counterfactual","emb","finance","govt","landuse","behavior", "labor","energy")
crop.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                               shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                               errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                               multiplier=1,
                               values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                               values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                               values.fill=c("red","black"),
                               breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                               labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                             "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                               y.label=r'(PCal $MHa^{-1}$ $Yr^{-1}$)',
                               title.label="(a) Crop Yield")

data.to.plot <- data.difference$crop_crop_yield
crop.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                                multiplier=1,
                                values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                values.fill=c("black"),
                                breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                              "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                y.label=r'(%)',
                                title.label="(b) Crop Yield")


data.to.plot <- data.absolute$freshwater_agricultural_water_withdrawal
cases.to.plot <- c("counterfactual","emb","finance","labor","landuse")
water.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                          shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                          errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                          multiplier=1,
                          values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                          values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                          values.fill=c("red","black"),
                          breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                          labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                        "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                          y.label=r'($m^{3}$ $Yr^{-1}$)',
                          title.label="(c) Water use")

data.to.plot <- data.difference$freshwater_agricultural_water_withdrawal
water.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                           multiplier=1,
                           values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                           values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                           values.fill=c("black"),
                           breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                           labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                         "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                           y.label=r'(%)',
                           title.label="(d) Water Use")


panel.landuse.sector <- ggarrange(crop.plot.abs,crop.plot.diff,
                                  water.plot.abs, water.plot.diff,
                                    nrow=2, ncol=2,
                                    heights= c(1,1,1),widths= c(1,1,1),
                                    align= "v",legend = "bottom",
                                    common.legend = T)

##******************************** Feedbacks to climate **###

data.to.plot <- data.absolute$energy_balance_model_surface_temperature_anomaly
cases.to.plot <- c("counterfactual","emb","finance","labor", "government","demography","energy","behavior","concrete","slr")
sta.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                          shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                          errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                          multiplier=1,
                          values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                          values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                          values.fill=c("red","black"),
                          breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                          labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                        "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                          y.label=r'(°C)',
                          title.label="(a) Surface Temperature Anomaly")

data.to.plot <- data.difference$energy_balance_model_surface_temperature_anomaly
sta.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                           multiplier=1,
                           values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                           values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                           values.fill=c("black"),
                           breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                           labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                         "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                           y.label=r'(%)',
                           title.label="(b) Surface Temperature Anomaly")

data.to.plot <- data.absolute$emissions_total_ghg_emissions
emissions.plot.abs <- plot.absolute(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                         shading.data = data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb"),], 
                         errorbar.data = data.absolute.4.errorbar(data.to.plot)[data.absolute.4.errorbar(data.to.plot)$Experiment %in% cases.to.plot,],
                         multiplier=1e-3,
                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                         values.color=c("red","black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                         values.fill=c("red","black"),
                         breaks.plot=c("counterfactual","emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                         labels.plot=c("NoImpacts","AllImpacts","FRoL (B1/B2/R1)",
                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                         y.label=r'(Gt $CO_{2}$ eq. $Yr^{-1}$)',
                         title.label="(c) Total GHG emissions")

data.to.plot <- data.difference$emissions_total_ghg_emissions
emissions.plot.diff <- plot.difference.no.grey.line(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
                          multiplier=1,
                          values.linetype=c("solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                          values.color=c("black","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                          values.fill=c("black"),
                          breaks.plot=c("emb","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                          labels.plot=c("AllImpacts","FRoL (B1/B2/R1)",
                                        "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                          y.label=r'(%)',
                          title.label="(d) Total GHG emissions")



panel.climate.sector <- ggarrange(sta.plot.abs,sta.plot.diff,
                                  emissions.plot.abs,emissions.plot.diff,
                                  nrow=2, ncol=2,
                                  heights= c(1,1,1),widths= c(1,1,1),
                                  align= "v",legend = "bottom",
                                  common.legend = T)


##**** Plotting nonlinearities *****#############
# For nonlinearity, we consider absolute differences

data.difference <- sapply(variables.list, function(k) {
  map_df(.x=list("emb"=df.emb[[k]]$abs.difference,
                 "finance"=df.finance[[k]]$abs.difference,
                 "labor"=df.labour[[k]]$abs.difference,
                 "govt"=df.govsp[[k]]$abs.difference,
                 "demography"=df.mortality[[k]]$abs.difference,
                 "landuse"=df.landuse[[k]]$abs.difference,
                 "behavior"=df.behaviour[[k]]$abs.difference,
                 "energy"=df.energy[[k]]$abs.difference,
                 "concrete"=df.concrete[[k]]$abs.difference,
                 "slr"=df.slr[[k]]$abs.difference),
         .f=bind_rows,
         .id="Experiment")}, simplify = FALSE)

# sum of all individual impacts for each variable
data.difference.sum.of.individuals <- sapply(variables.list, function(k) {
  dummy1 <- data.difference[[k]]
  dummy2 <- dummy1[dummy1$Experiment != "emb",] %>% group_by(year) %>% summarise(low1=sum(low1),
                                                                                 median=sum(median),
                                                                                 high1=sum(high1)
                                                                                 )
  dummy2$Experiment <- "sum of individuals"
  return(dummy2)
}, simplify=FALSE)

# combine the sum of individuals to the data frame of differences
data.difference <- as.list(do.call(bind_rows, list(data.difference,data.difference.sum.of.individuals)))


# Add total ghg emissions
data.difference$emissions_total_ghg_emissions <- map_df(.x=list("total_co2_emissions"=data.difference[names(data.difference) %in% c("emissions_total_co2_emissions")],
                                                                "total_ch4_emissions"=data.difference[names(data.difference) %in% c("emissions_total_ch4_emissions")],
                                                                "total_n2o_emissions"=data.difference[names(data.difference) %in% c("emissions_total_n2o_emissions")],
                                                                "total_so2_emissions"=data.difference[names(data.difference) %in% c("emissions_total_so2_emissions")],
                                                                "total_nox_emissions"=data.difference[names(data.difference) %in% c("emissions_total_nox_emissions")],
                                                                "total_voc_emissions"=data.difference[names(data.difference) %in% c("emissions_voc_emissions")],
                                                                "total_co_emissions"=data.difference[names(data.difference) %in% c("emissions_co_emissions")],
                                                                "total_hfc134a_emissions"=data.difference[names(data.difference) %in% c("emissions_hfc134a_eq_emissions")]),
                                                        .f=bind_rows, .id="variable") %>%
  group_by(year, Experiment) %>% 
  mutate(multiply=ifelse(variable %in% c("total_n2o_emissions","total_hfc134a_emissions"), 1e-3, 1)) %>% 
  summarise(low1=sum(low1*multiply),  
            median=sum(median*multiply), 
            high1=sum(high1*multiply)
            )


# Plots

data.to.plot <- data.difference$demographics_real_gdp_per_person
gdp.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                    multiplier=1,
                                    values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                    values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                    values.fill=c("black"),
                                    breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                    labels.plot=c("AllImpacts","sum of individual impacts",
                                                  "FRoL (B1/B2/R1)",
                                                  "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                    y.label=r'(Bill.\$ $Yr^{-1}$)',
                                    title.label="(a) Real GDP per capita")

data.to.plot <- data.difference$employment_realised_productivity
productivity.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                            multiplier=1,
                                            values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                            values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                            values.fill=c("black"),
                                            breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                            labels.plot=c("AllImpacts","sum of individual impacts",
                                                          "FRoL (B1/B2/R1)",
                                                          "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                            y.label=r'(dmnl)',
                                            title.label="(b) Labour productivity")

data.to.plot <- data.difference$government_government_consumption
govt.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                    multiplier=1,
                                    values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                    values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                    values.fill=c("black"),
                                    breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                    labels.plot=c("AllImpacts","sum of individual impacts",
                                                  "FRoL (B1/B2/R1)",
                                                  "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                    y.label=r'(Bill.\$ $Yr^{-1}$)',
                                    title.label="(c) Government expenditure")

data.to.plot <- data.difference$demographics_population
population.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                    multiplier=1,
                                    values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                    values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                    values.fill=c("black"),
                                    breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                    labels.plot=c("AllImpacts","sum of individual impacts",
                                                  "FRoL (B1/B2/R1)",
                                                  "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                    y.label=r'(Mp $Yr^{-1}$)',
                                    title.label="(d) Total population")

data.to.plot <- data.difference$demographics_total_deaths
mortality.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                           multiplier=1,
                                          values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                          values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                          values.fill=c("black"),
                                          breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                          labels.plot=c("AllImpacts","sum of individual impacts",
                                                        "FRoL (B1/B2/R1)",
                                                        "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                          y.label=r'(Mp $Yr^{-1}$)',
                                          title.label="(e) Mortality")

data.to.plot <- data.difference$energy_demand_demand_for_energy
energy.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                          multiplier=1e-3,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(PWh $Yr^{-1}$)',
                                         title.label="(f) Energy demand")

cases.to.plot <- c("sum of individuals","emb","finance","labor","govt","landuse", "behavior","energy","slr")
data.to.plot <- data.difference$food_demand_direct_food_demand_per_person_per_day
food.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                    multiplier=1e-3,
                                    values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                    values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                    values.fill=c("black"),
                                    breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                    labels.plot=c("AllImpacts","sum of individual impacts",
                                                  "FRoL (B1/B2/R1)",
                                                  "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                    y.label=r'(MCal $Person^{-1}$ $Day^{-1}$)',
                                    #y.label=r'(%)',
                                    title.label="(g) Food demand")

data.to.plot <- data.difference$concrete_total_yearly_concrete_use
concrete.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                     multiplier=1e-3,
                                    values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                    values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                    values.fill=c("black"),
                                    breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                    labels.plot=c("AllImpacts","sum of individual impacts",
                                                  "FRoL (B1/B2/R1)",
                                                  "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                    y.label=r'(Gt $Year^{-1}$)',
                                    #y.label=r'(%)',
                                    title.label="(h) Annual concrete production")


data.to.plot <- data.difference$crop_crop_yield
crop.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                           multiplier=1,
                          values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                          values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                          values.fill=c("black"),
                          breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                          labels.plot=c("AllImpacts","sum of individual impacts",
                                        "FRoL (B1/B2/R1)",
                                        "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                          y.label=r'(PCal $MHa^{-1}$ $Yr^{-1}$)',
                          title.label="(i) Crop Yield")

data.to.plot <- data.difference$freshwater_agricultural_water_withdrawal
water.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                    multiplier=1,
                                    values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                    values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                    values.fill=c("black"),
                                    breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                    labels.plot=c("AllImpacts","sum of individual impacts",
                                                  "FRoL (B1/B2/R1)",
                                                  "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                    y.label=r'($m^{3}$ $Yr^{-1}$)',
                                    title.label="(j) Agricultural water use")

data.to.plot <- data.difference$emissions_total_ghg_emissions
emissions.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                   multiplier=1e-3,
                                   values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                   values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                   values.fill=c("black"),
                                   breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                   labels.plot=c("AllImpacts","sum of individual impacts",
                                                 "FRoL (B1/B2/R1)",
                                                 "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                   y.label=r'(Gt $CO_{2}$ eq. $Yr^{-1}$)',
                                   title.label="(k) Total GHG emissions")


data.to.plot <- data.difference$energy_balance_model_surface_temperature_anomaly
sta.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                   multiplier=1,
                                   values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                   values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                   values.fill=c("black"),
                                   breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                   labels.plot=c("AllImpacts","sum of individual impacts",
                                                 "FRoL (B1/B2/R1)",
                                                 "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                   y.label=r'(°C)',
                                   title.label="(l) Surface temperature anomaly")

panel.nonlinearity <- ggarrange(gdp.plot.nonlinearity, productivity.plot.nonlinearity,govt.plot.nonlinearity,
                                population.plot.nonlinearity, mortality.plot.nonlinearity,
                                energy.plot.nonlinearity, food.plot.nonlinearity,
                                concrete.plot.nonlinearity, 
                                crop.plot.nonlinearity, water.plot.nonlinearity,
                                emissions.plot.nonlinearity,sta.plot.nonlinearity,
                                nrow=4, ncol=3,
                                    heights= c(1,1,1),widths= c(1,1,1),
                                    align= "v", legend="bottom",
                                    common.legend = T)

#-----------------------------
# Plot the nonlinearities in individual emissions sources

data.to.plot <- data.difference$emissions_total_co2_emissions
co2.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(Mt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(a) Total CO2 emissions")

data.to.plot <- data.difference$emissions_total_ch4_emissions
ch4.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(Mt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(b) Total CH4 emissions")

data.to.plot <- data.difference$emissions_total_n2o_emissions
n2o.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(kt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(c) Total N2O emissions")

data.to.plot <- data.difference$emissions_total_so2_emissions
SO2.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(Mt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(d) Total SO2 emissions")

data.to.plot <- data.difference$emissions_total_nox_emissions
nox.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(Mt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(e) Total NOx emissions")

data.to.plot <- data.difference$emissions_voc_emissions
voc.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(Mt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(f) VOC emissions")

data.to.plot <- data.difference$emissions_co_emissions
co.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(Mt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(g) Total CO emissions")

data.to.plot <- data.difference$emissions_hfc134a_eq_emissions
hfc.plot.nonlinearity <- plot.difference.with.grey.line(model.data=data.to.plot,
                                         multiplier=1,
                                         values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                         values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                         values.fill=c("black"),
                                         breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                         labels.plot=c("AllImpacts","sum of individual impacts",
                                                       "FRoL (B1/B2/R1)",
                                                       "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (R6/R7/B12/B13)","ClimExHB (B9)","En (B6/B7)","DuCn (B11)","SLR"),
                                         y.label=r'(kt $CO_{2}$ eq. $Yr^{-1}$)',
                                         title.label="(h) HFC134 eq emissions")

panel.emissions <- ggarrange(co2.plot.nonlinearity,
                             ch4.plot.nonlinearity,
                             n2o.plot.nonlinearity,
                             SO2.plot.nonlinearity,
                             nox.plot.nonlinearity,
                             voc.plot.nonlinearity,
                             co.plot.nonlinearity,
                             hfc.plot.nonlinearity,
                             nrow=2, ncol=4,
                             heights= c(1,1,1),widths= c(1,1,1),
                             align= "v",legend = "bottom",
                             common.legend = T)


#------------------------------------------------------------------
#                            Save the figures
#------------------------------------------------------------------

ggsave(filename=file.path(figures_dir, paste0("f05",".png")),
       plot=panel.finance.sector,
       height = 8, width=10)

ggsave(filename=file.path(figures_dir, paste0("f06",".png")),
       plot=panel.mortality.sector,
       height =5 , width=9.5)

ggsave(filename=file.path(figures_dir, paste0("f07",".png")),
       plot=panel.energy.sector,
       height = 4, width=10)

ggsave(filename=file.path(figures_dir, paste0("f08",".png")),
       plot=panel.food.sector,
       height = 5, width=10)

ggsave(filename=file.path(figures_dir, paste0("f09",".png")),
       plot=panel.concrete.sector,
       height =5 , width=10)

ggsave(filename=file.path(figures_dir, paste0("f10",".png")),
       plot=panel.landuse.sector,
       height =8 , width=11)

ggsave(filename=file.path(figures_dir, paste0("f11",".png")),
       plot=panel.climate.sector,
       height =8 , width=12)

ggsave(filename=file.path(figures_dir, paste0("f13",".png")),
       plot=panel.nonlinearity,
       height =14.5 , width=15)

ggsave(filename=file.path(figures_dir, paste0("f14",".png")),
       plot=panel.emissions,
       height =8 , width=16)

cat("Script executed successfully. \n
    Figures saved to:", normalizePath(figures_dir), "\n")
