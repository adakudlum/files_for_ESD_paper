# ---------------------------------------------------------------------
# Set working directory to script location
# ---------------------------------------------------------------------
remove(list=ls())
script_dir  <- 'G:/My Drive/R/r-scripts/worldTrans/make_plots_nonlinearity_paper/files_for_ESD_paper'
setwd(script_dir)
data_dir    <- file.path(script_dir, "data")
figures_dir <- file.path(script_dir)

library(ggplot2)
library(tidyverse)
library(abind)
library(latex2exp)
library(showtext)

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
plot.group <- as.data.frame(data.difference$food_demand_direct_food_demand_per_person_per_day)
plot.group <- plot.group %>% group_by(Experiment,year) %>% mutate(sd=high1-median)

#-------------------------------------------------------------------------------
# Function to format pre-computed values for ggplot2
#-------------------------------------------------------------------------------
make_custom_box <- function(med, sd_val) {
  res <- c(
    ymin   = med - 1.5 * sd_val,  # Lower whisker boundary
    lower  = med - sd_val,        # Bottom of the box (-1 SD)
    middle = med,                 # Center line (Median)
    upper  = med + sd_val,        # Top of the box (+1 SD)
    ymax   = med + 1.5 * sd_val   # Upper whisker boundary
  )
  return(res)
}

ggplot(df_summary, aes(x = group)) +
  stat_summary(
    aes(y = median), 
    fun.data = function(x) make_custom_box(df_summary$median, df_summary$sd), 
    geom = "boxplot"
  ) +
  labs(
    title = "Custom Box Plot (Median & +/- 1 SD)",
    x = "Group",
    y = "Values"
  ) +
  theme_minimal()
