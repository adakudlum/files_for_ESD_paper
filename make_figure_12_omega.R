#-----------------------------------------------------------------------------------------------
# Script to compare FRIDA's economic damage (EMB and Failure rate of loan channel) with that 
# of Nordhaus' DICE model
#
# Input - RDS files containing the outputs corresponding to FRIDA v2.1

# Output - Figure for output ratios relative to NoImpacts counterfactual
#
# Run the script with source('make_figure_12_omega.R')
#----------------------------------------------------------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyverse)
library(patchwork)

# ---------------------------------------------------------------------
# Set working directory to script location
# ---------------------------------------------------------------------
remove(list=ls())
script_path <- normalizePath("make_figure_12_omega.R")
script_dir  <- dirname(script_path)
setwd(script_dir)
data_dir    <- file.path(script_dir,'data')
figures_dir <- file.path(script_dir, 'figures')

# ---------------------------------------------------------------------
# User settings
# ---------------------------------------------------------------------

file.emb     <- file.path(data_dir,'dataForCompoundDam-ClimateFeedback_AllImpacts.RDS')
file.finance <- file.path(data_dir,'dataForCompoundDam-ClimateFeedback_FRoL.RDS')

OUTPUT_FILE <- file.path(figures_dir, "f12.png")

# Nordhaus DICE-2016R damage coefficient
A2_DICE_2016R <- 0.00236

FIGWIDTH <- 8
FIGHEIGHT <- 5.5
DPI <- 300

# ---------------------------------------------------------------------
# Helper function
# ---------------------------------------------------------------------

require_columns <- function(df, required) {
  
  missing <- setdiff(required, colnames(df))
  
  if (length(missing) > 0) {
    stop(
      paste(
        "Missing required column(s):",
        paste(missing, collapse = "\n - ")
      )
    )
  }
  
}

# ---------------------------------------------------------------------
# Function to re-shape input data for plotting
# ---------------------------------------------------------------------

construct_data <- function(input_data, variable1, variable2, which_quantile) {
  data.frame(input_data[[variable1]][[which_quantile]][input_data[[variable1]]$Experiment %in% "counterfactual"], 
             input_data[[variable1]][[which_quantile]][input_data[[variable1]]$Experiment %in% "emb"], 
             input_data[[variable1]][[which_quantile]][input_data[[variable1]]$Experiment %in% "finance"], 
             input_data[[variable2]][[which_quantile]][input_data[[variable2]]$Experiment %in% "emb"]) 
}

# ---------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------

if (!file.exists(file.emb) ) {
  stop(paste("Input file not found:", normalizePath(file.emb)))
}

if (!file.exists(file.finance)) {
    stop(paste("Input file not found:", normalizePath(file.finance)))
}

df.emb     <- readRDS(file.emb)
df.finance <- readRDS(file.finance)
variables.needed <- c("demographics_real_gdp_per_person","energy_balance_model_surface_temperature_anomaly","demographics_population")

df.all <- sapply(variables.needed, function(k) {
  map_df(.x=list("counterfactual"=df.emb[[k]]$counterfactual,
                 "emb"=df.emb[[k]]$baseline,
                 "finance"=df.finance[[k]]$baseline
                 ),
         .f=bind_rows,
         .id="Experiment")}, simplify = FALSE)

#-------------------------------------------------------------------------------
# Input data doesn't have GDP, but it has GDP per capita and Population.
# So estimate GDP = GDP per capita * Population
#-------------------------------------------------------------------------------
df.all.unlist          <- do.call(rbind, df.all) 
df.all.unlist$variable <- unlist(lapply(strsplit(rownames(df.all.unlist),"\\."), '[[', 1))

real.gdp <- df.all.unlist %>%
  
  # keep only desired variables
  filter(variable %in% c("demographics_real_gdp_per_person", "demographics_population")) %>%
  
  # group
  group_by(Experiment, year) %>%
  
  # compute the product
  summarise(
    across(c(low2, low1, median, high1, high2), prod),
    
  # new variable name  
    variable = "gdp_real_gdp_in_2021c",
    .groups = "drop"
  ) %>%
  select(Experiment, year, low2, low1, median, high1, high2, variable)

# Merge the real_gdp back to the original list as a list 
df.all$gdp_real_gdp_in_2021c <- real.gdp %>% select(-variable)

#------------------------------------------------------------------------------------
# Create columnised data for plotting
#-----------------------------------------------------------------------------------

required_cols <- c("NoImpacts_GDP", "EMB_GDP",  "FRoL_GDP",  "EMB_STA")
df.vars.combined.median <- construct_data(df.all,"gdp_real_gdp_in_2021c", "energy_balance_model_surface_temperature_anomaly", "median")
colnames(df.vars.combined.median) <- required_cols
require_columns(df.vars.combined.median, required_cols)

df.vars.combined.low1 <- construct_data(df.all,"gdp_real_gdp_in_2021c", "energy_balance_model_surface_temperature_anomaly", "low1")
colnames(df.vars.combined.low1) <- required_cols
require_columns(df.vars.combined.low1, required_cols)

df.vars.combined.high1 <- construct_data(df.all,"gdp_real_gdp_in_2021c", "energy_balance_model_surface_temperature_anomaly", "high1")
colnames(df.vars.combined.high1) <- required_cols
require_columns(df.vars.combined.high1, required_cols)

# Basic validation
if (any(df.vars.combined.median$NoImpacts_GDP_median <= 0)) stop("NoImpacts GDP contains non-positive values.")
if (any(df.vars.combined.median$EMB_GDP_median <= 0)) stop("EMB GDP contains non-positive values.")
if (any(df.vars.combined.median$FRoL_GDP_median <= 0)) stop("Loan failure GDP contains non-positive values.")

# Output ratios
omega_emb_median <- df.vars.combined.median$EMB_GDP / df.vars.combined.median$NoImpacts_GDP
omega_loan_median <- df.vars.combined.median$FRoL_GDP / df.vars.combined.median$NoImpacts_GDP
omega_emb_low1 <- df.vars.combined.low1$EMB_GDP / df.vars.combined.low1$NoImpacts_GDP
omega_loan_low1 <- df.vars.combined.low1$FRoL_GDP / df.vars.combined.low1$NoImpacts_GDP
omega_emb_high1 <- df.vars.combined.high1$EMB_GDP / df.vars.combined.high1$NoImpacts_GDP
omega_loan_high1 <- df.vars.combined.high1$FRoL_GDP / df.vars.combined.high1$NoImpacts_GDP

# Nordhaus damage curve
omega_nordhaus_median <- 1 / (1 + A2_DICE_2016R * df.vars.combined.median$EMB_STA^2)
omega_nordhaus_low1 <- 1 / (1 + A2_DICE_2016R * df.vars.combined.low1$EMB_STA^2)
omega_nordhaus_high1 <- 1 / (1 + A2_DICE_2016R * df.vars.combined.high1$EMB_STA^2)

# ---------------------------------------------------------------------
# Data frames for plotting
# ---------------------------------------------------------------------

df_time_low1 <- data.frame(
  Year = seq(1980,2150,by=1),
  EMB = omega_emb_low1,
  LoanFailure = omega_loan_low1,
  Nordhaus = omega_nordhaus_low1)

df_time_median <- data.frame(
  Year = seq(1980,2150,by=1),
  EMB = omega_emb_median,
  LoanFailure = omega_loan_median,
  Nordhaus = omega_nordhaus_median)

df_time_high1 <- data.frame(
  Year = seq(1980,2150,by=1),
  EMB = omega_emb_high1,
  LoanFailure = omega_loan_high1,
  Nordhaus = omega_nordhaus_high1)
  
df_time_low1_long <- tidyr::pivot_longer(
  df_time_low1,
  cols = -Year,
  names_to = "Scenario",
  values_to = "Omega"
)
df_time_median_long <- tidyr::pivot_longer(
  df_time_median,
  cols = -Year,
  names_to = "Scenario",
  values_to = "Omega"
)
df_time_high1_long <- tidyr::pivot_longer(
  df_time_high1,
  cols = -Year,
  names_to = "Scenario",
  values_to = "Omega"
)

df_time_final <- data.frame(df_time_low1_long$Year, df_time_low1_long$Scenario,
                            df_time_low1_long$Omega, df_time_median_long$Omega, df_time_high1_long$Omega)
colnames(df_time_final) <- c("Year","Scenario","low1","median","high1")


df_temp <- data.frame(
  Temp = df.vars.combined.median$EMB_STA ,
  EMB = omega_emb_median,
  LoanFailure = omega_loan_median,
  Nordhaus = omega_nordhaus_median
)

df_temp_long <- tidyr::pivot_longer(
  df_temp,
  cols = -Temp,
  names_to = "Scenario",
  values_to = "Omega"
)

# ---------------------------------------------------------------------
# Plot (panel a)
# ---------------------------------------------------------------------

p1 <- ggplot(df_time_final) +
  geom_line(mapping=aes(x = Year, y = median, color = Scenario), linewidth = 1.2) +
  labs(
    title = "Growth v/s Level effects: FRIDA-v2.1 and Nordhaus",
    y = "Output ratio Ω(t)",
    x = "Year"
  ) +
  scale_x_continuous(breaks=seq(1980,2150,by=20))+
  scale_color_manual(values=c("black","#f39865","blue"),
                     breaks = c("EMB","LoanFailure","Nordhaus"),
                     labels= c("AllImpacts/NoImpacts","FRoL/NoImpacts","Nordhaus DICE-2016R level damage"))+
  ylim(0, 1.05) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "sans",size=12,angle=45, vjust=0.3, color="black"),
        axis.text.y = element_text(family = "sans",size=12, vjust=0.3, color="black"),
        axis.title = element_text(family = "sans",size=12, vjust=0.3, color="black"),
        plot.title = element_text(family = "sans",size=12, vjust=0.3, color="black"),
        panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
        panel.grid.minor = element_blank(),
        legend.direction = "vertical",
        legend.position = "inside",
        legend.position.inside = c(.3,.2),
        legend.title = element_blank(),
        legend.text = element_text(family = "sans",size=13, vjust=0.5, color="black"),
        legend.key.spacing.x = unit(2,"cm"),
        legend.key.width = unit(2, "lines"))

#print(p1)
# ---------------------------------------------------------------------
# Plot (panel b)
# ---------------------------------------------------------------------

p2 <- ggplot(df_temp_long, aes(x = Temp, y = Omega, color = Scenario)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "(b) As a function of temperature",
    x = "Temperature anomaly (°C)",
    y = NULL
  ) +
  theme_minimal()

# ---------------------------------------------------------------------
# Combine panels
# ---------------------------------------------------------------------



combined_plot <- p1 + p2 +
  plot_annotation(
    title = "Output ratios relative to the No_Impacts scenario"
  )

# Save figure
ggsave(
  OUTPUT_FILE,
  p1,
  width = FIGWIDTH,
  height = FIGHEIGHT,
  dpi = DPI
)

#print(combined_plot)

cat("Saved figure to:", normalizePath(OUTPUT_FILE), "\n")
