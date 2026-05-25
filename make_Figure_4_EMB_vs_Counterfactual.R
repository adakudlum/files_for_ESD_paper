# Script to make the EMB v/s counterfactual figure (Figure #4) in the paper
#
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
#                                  year 
#                                  low1 
#                                  median 
#                                  high1 
#                   per.difference (percent differences between every ensemble member) -- 
#                                  |   
#                                  year 
#                                  low1 
#                                  median 
#                                  high1 
#            variable 2 -- and so on
#
#------------ How to execute -------------
# This script can be run on a standard Rstudio terminal, visual code terminal, or a Linux terminal
# with the command - source('make_Figure_4_EMB_vs_Counterfactual.R')
#
# Muralidhar Adakudlu, 20/5/2026
# Norwegian Meteorological Institute
#--------------------------------------------------------------------------------------------------
#install.packages(c(
#  "readxl",
#  "tidyverse",
#  "reshape2",
#  "stringr",
#  "extrafont",
#  "showtext",
#  "latex2exp",
#  "ggpubr",
#  "abind"
#))
#
library("readxl")
library(tidyverse)
library(reshape2)
library(stringr)
library(extrafont)
library(showtext)
require(latex2exp)
require(ggpubr)
library(abind)

# ---------------------------------------------------------------------
# Set working directory to script location
# ---------------------------------------------------------------------
remove(list=ls())
script_path <- normalizePath("make_Figure_4_EMB_vs_Counterfactual.R")
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
  df.calib     <- read.csv2(file.path(data_dir,"Calibration Data.csv"), sep=",")[,-c(46:52)]
} else {
  stop("I/p directory is missing or is empty.")
}

# ---------------------------------------------------------------------
# Set the variables required
# ---------------------------------------------------------------------
variables.list <- c(# Finance
  # Economy #
  "demographics_real_gdp_per_person",
  "government_government_consumption",
  "employment_realised_productivity",
  ### Demography ###
  "demographics_population",
  "demographics_total_deaths",
  "demographics_births",
  ### Energy ###
  "energy_demand_demand_for_energy",
  ### Human behaviour ###
  "food_demand_direct_food_demand_per_person_per_day",
  ### Resources
  "concrete_total_yearly_concrete_use",
  ### Land use and agriculture ###
  "freshwater_agricultural_water_withdrawal",
  "crop_crop_yield",
  ### Climate ###
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

subset.years <- as.character(seq(1980,2150,by=1))

# ---------------------------------------------------------------------
# Process the model data
# ---------------------------------------------------------------------
data.absolute <- sapply(variables.list, function(k) {
  map_df(.x=list("counterfactual"=df.emb[[k]]$counterfactual,
                 "emb"=df.emb[[k]]$baseline
                 ),
         .f=bind_rows,
         .id="Experiment")}, simplify = FALSE)

# ---------------------------------------------------------------------
# Estimate total GHG emissions from individual sources
# ---------------------------------------------------------------------
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
  summarise(low2=sum(low2*multiply), 
            low1=sum(low1*multiply),  
            median=sum(median*multiply), 
            high1=sum(high1*multiply),
            high2=sum(high2*multiply)
  )
data.absolute.df <- do.call(rbind, data.absolute)
data.absolute.df$variable <- unlist(lapply(strsplit(rownames(data.absolute.df),"\\."), '[[', 1))

cat("Model data processed. Continuing to calibration data ....", "\n")

#----------------------------------------------------------------------------------------------------------
#  Process the calibration data 
#----------------------------------------------------------------------------------------------------------
len.frida.data   <- length(seq(1980,2150,1))
names(df.calib)  <- substring(names(df.calib),2,5)   # keep last 4 letters in the column names (YYYY format)

energy.demand.calibration          <- filter(df.calib, if_any(everything(),~str_detect(.x,"energy demand.demand for energy")))
food.demand.calibration            <- filter(df.calib, if_any(everything(),~str_detect(.x,"Food Demand.direct food demand per person per day")))
concrete.use.calibration           <- filter(df.calib, if_any(everything(),~str_detect(.x,"Concrete.total yearly concrete use")))
births.calibration                 <- filter(df.calib, if_any(everything(),~str_detect(.x,"Demographics.Births")))
mortality.calibration              <- filter(df.calib, if_any(everything(),~str_detect(.x,"Demographics.Total Deaths")))
population.calibration             <- filter(df.calib, if_any(everything(),~str_detect(.x,"Demographics.Population")))
gdp.per.person.calibration         <- filter(df.calib, if_any(everything(),~str_detect(.x,"real GDP per person")))
govt.consumption.calibration       <- filter(df.calib, if_any(everything(),~str_detect(.x,"Government.Government Consumption")))
sta.calibration                    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Surface Temperature Anomaly")))
slr.calibration                    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Total global sea level anomaly")))
total.co2.emissions.calibration    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.Total CO2 Emissions")))
total.ch4.emissions.calibration    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.Total CH4 Emissions")))
total.n2o.emissions.calibration    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.Total N2O Emissions")))
total.so2.emissions.calibration    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.Total SO2 Emissions")))
total.nox.emissions.calibration    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.Total NOx Emissions")))
total.vox.emissions.calibration    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.VOC Emissions")))
total.co.emissions.calibration     <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.CO Emissions")))
total.hfc.emissions.calibration    <- filter(df.calib, if_any(everything(),~str_detect(.x,"Emissions.HFC134a eq Emissions")))
total.ghg.emissions.calibration    <- rbind(as.numeric(total.co2.emissions.calibration[,-1]),
                                            as.numeric(total.ch4.emissions.calibration[,-1]),
                                            as.numeric(total.n2o.emissions.calibration[,-1])*1e-3,
                                            as.numeric(total.so2.emissions.calibration[,-1]),
                                            as.numeric(total.nox.emissions.calibration[,-1]),
                                            as.numeric(total.vox.emissions.calibration[,-1]),
                                            as.numeric(total.co.emissions.calibration[,-1]),
                                            as.numeric(total.hfc.emissions.calibration[,-1])*1e-3)
total.ghg.emissions.calibration <- apply(total.ghg.emissions.calibration,2,function(x) sum(x, na.rm=TRUE))

# ---------------------------------------------------------------------
# reformat calibration data
# ---------------------------------------------------------------------

energy.demand.calibration          <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(energy.demand.calibration[,-1])),rep(NA,len.frida.data-44)))
food.demand.calibration            <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(food.demand.calibration[,-1])),rep(NA,len.frida.data-44)))
concrete.use.calibration           <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(concrete.use.calibration[,-1])),rep(NA,len.frida.data-44)))
births.calibration                 <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(births.calibration[,-1])),rep(NA,len.frida.data-44)))
population.calibration             <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(population.calibration[,-1])),rep(NA,len.frida.data-44)))
mortality.calibration              <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(mortality.calibration[,-1])),rep(NA,len.frida.data-44)))
crop.yield.calibration             <- data.frame(as.factor(seq(1980,2150)),rep(NA,len.frida.data))
water.use.calibration              <- data.frame(as.factor(seq(1980,2150)),rep(NA,len.frida.data))
govt.consumption.calibration       <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(govt.consumption.calibration[,-1])),rep(NA,len.frida.data-44)))
gdp.per.person.calibration         <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(gdp.per.person.calibration[,-1])),rep(NA,len.frida.data-44)))
productivity.calibration           <- data.frame(as.factor(seq(1980,2150)),rep(NA,len.frida.data))
sta.calibration                    <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(sta.calibration[,-1])),rep(NA,len.frida.data-44)))
total.ghg.emissions.calibration    <- data.frame(as.factor(seq(1980,2150)),abind(total.ghg.emissions.calibration,rep(NA,len.frida.data-44)))
slr.calibration                    <- data.frame(as.factor(seq(1980,2150)),abind(as.numeric(t(slr.calibration[,-1])),rep(NA,len.frida.data-44)))

colnames(energy.demand.calibration) <-  c("year","median")
  colnames(food.demand.calibration) <- c("year","median")
  colnames(concrete.use.calibration) <- colnames(births.calibration) <- colnames(population.calibration) <- colnames(mortality.calibration) <- colnames(govt.consumption.calibration) <- c("year","median")
  colnames(sta.calibration) <-  colnames(gdp.per.person.calibration) <- colnames(water.use.calibration) <- c("year","median")
  colnames(crop.yield.calibration) <- colnames(total.ghg.emissions.calibration) <-  c("year","median")
  colnames(slr.calibration) <- colnames(productivity.calibration) <- c("year","median")

calibration.data <- map_df(.x=list("energy_demand_demand_for_energy"=energy.demand.calibration,
                                   "food_demand_direct_food_demand_per_person_per_day"=food.demand.calibration,
                                   "concrete_total_yearly_concrete_use"=concrete.use.calibration,
                                   "demographics_births"=births.calibration,
                                   "demographics_population"=population.calibration,
                                   "demographics_total_deaths"=mortality.calibration,
                                   "demographics_real_gdp_per_person"=gdp.per.person.calibration,
                                   "employment_realised_productivity"=productivity.calibration,
                                   "government_government_consumption"=govt.consumption.calibration,
                                    "crop_crop_yield"=crop.yield.calibration,
                                    "freshwater_agricultural_water_withdrawal" =water.use.calibration,
                                    "emissions_total_ghg_emissions"=total.ghg.emissions.calibration,
                                     "sea_level_total_global_sea_level_anomaly"=slr.calibration,
                                    "energy_balance_model_surface_temperature_anomaly"=sta.calibration),
                            .f=bind_rows, .id="variable")  
  
cat("Calibration data processed. Continuing to create plots ....", "\n")

# ---------------------------------------------------------------------
# Plotting part
# ---------------------------------------------------------------------

showtext_opts(dpi = 300)
showtext_auto()

# Plotting function 
plot.fun <- function(model.data,calibration.data,multiplier,y.label,title.label){
  ggplot()+
  geom_line(data=model.data,
            mapping=aes(x=year,y=median*multiplier, color=Experiment, group = Experiment),linewidth=1)+
  geom_point(data=calibration.data,
             mapping=aes(x=as.numeric(as.character(year)),y=median*multiplier, shape=variable),color="darkgreen",size=2)+
  geom_ribbon(data=model.data,
              mapping=aes(x=year,ymax=high1*multiplier,ymin=low1*multiplier,fill=Experiment,group = Experiment),alpha=0.2)+
  theme_bw()+
  labs(x=NULL,y=TeX(y.label),title=title.label)+
  scale_x_continuous(breaks=seq(1980,2150,by=20), expand=c(0,0))+
  scale_shape_manual(values = 1, labels=c("Calibration"),guide="legend") +
  scale_color_manual(values=c("black","red"),
                     breaks=c("emb","counterfactual"),
                     labels=c("AllImpacts","NoImpacts"))+
  scale_fill_discrete(type=c("red","black"),
                      breaks=c("emb","counterfactual"),
                      labels=c("AllImpacts","NoImpacts"))+
  theme(axis.text.x = element_text(family = "sans",size=11,angle=45, vjust=0.3, color="black"),
        axis.text.y = element_text(family = "sans",size=12, vjust=0.3, color="black"),
        axis.title = element_text(family = "sans",size=14, vjust=0.3, color="black"),
        plot.title = element_text(family = "sans",size=14, vjust=0.3, color="black"),
        panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA),
        strip.placement = "outside",
        panel.spacing.x = unit(0,"lines"),
        panel.spacing.y = unit(0,"lines"),
        legend.direction = "vertical",
        legend.position = "inside",
        legend.position.inside = c(.1,.5),
        legend.justification = "left",
        legend.title = element_blank(),
        legend.text = element_text(family = "sans",size=14, vjust=0.5, color="black"),
        legend.key.spacing.x = unit(1.5,"cm"))+
  guides(colour =guide_legend(ncol=1), fill=guide_legend(ncol=1),linetype=guide_legend(ncol=1),
         shape = guide_legend(override.aes = list(color="darkgreen",size=3)))}

plot.fun.govsp <- function(model.data,calibration.data,multiplier,y.label,title.label){
  ggplot()+
    geom_line(data=model.data,
              mapping=aes(x=year,y=median*multiplier, color=Experiment, group = Experiment),linewidth=1)+
    geom_point(data=calibration.data,
               mapping=aes(x=as.numeric(as.character(year)),y=median*multiplier, shape=variable),color="darkgreen",size=2)+
    geom_ribbon(data=model.data,
                mapping=aes(x=year,ymax=high1*multiplier,ymin=low1*multiplier,fill=Experiment,group = Experiment),alpha=0.2)+
    theme_bw()+
    labs(x=NULL,y=TeX(y.label),title=title.label)+
    scale_x_continuous(breaks=seq(1980,2150,by=20), expand=c(0,0))+
    scale_y_log10()+
    scale_shape_manual(values = 1, labels=c("Calibration"),guide="legend") +
    scale_color_manual(values=c("black","red"),
                       breaks=c("emb","counterfactual"),
                       labels=c("AllImpacts","NoImpacts"))+
    scale_fill_discrete(type=c("red","black"),
                        breaks=c("emb","counterfactual"),
                        labels=c("AllImpacts","NoImpacts"))+
    # plot only EMB
    theme(axis.text.x = element_text(family = "sans",size=11,angle=45, vjust=0.3, color="black"),
          axis.text.y = element_text(family = "sans",size=12, vjust=0.3, color="black"),
          axis.title = element_text(family = "sans",size=14, vjust=0.3, color="black"),
          plot.title = element_text(family = "sans",size=14, vjust=0.3, color="black"),
          panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill = NA),
          strip.placement = "outside",
          panel.spacing.x = unit(0,"lines"),
          panel.spacing.y = unit(0,"lines"),
          #panel.background = "orange",
          legend.direction = "vertical",
          legend.position = "inside",
          legend.position.inside = c(.1,.5),
          legend.justification = "left",
          legend.title = element_blank(),
          legend.text = element_text(family = "sans",size=13, vjust=0.5, color="black"),
          legend.key.spacing.x = unit(1.5,"cm"))+
    guides(colour =guide_legend(ncol=1), fill=guide_legend(ncol=1),linetype=guide_legend(ncol=1),
           shape = guide_legend(override.aes = list(color="darkgreen",size=3)))}

# Plots 
gdp.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "demographics_real_gdp_per_person",],
                     calibration.data=calibration.data[calibration.data$variable == "demographics_real_gdp_per_person" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                     multiplier=1,
                     y.label=r'(Bill.\$ $Mp^{-1}$ $Yr^{-1}$)',
                     title.label="(a) Real GDP per capita in 2021$")

productivity.plot <- plot.fun.govsp(model.data=data.absolute.df[data.absolute.df$variable == "employment_realised_productivity",],
                              calibration.data=calibration.data[calibration.data$variable == "employment_realised_productivity" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                              multiplier=1,
                              y.label=r'(dmnl)',
                              title.label="(b) Labour productivity")
consumption.plot <- plot.fun.govsp(model.data=data.absolute.df[data.absolute.df$variable == "government_government_consumption",],
                             calibration.data=calibration.data[calibration.data$variable == "government_government_consumption" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                             multiplier=1,
                             y.label=r'(Bill.\$ $Yr^{-1}$)',
                             title.label="(c) Government expenditure in 2021$")

population.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "demographics_population",],
                            calibration.data=calibration.data[calibration.data$variable == "demographics_population" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                            multiplier=1,
                            y.label=r'(Mp $Yr^{-1}$)',
                            title.label="(d) Total Population")

mortality.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "demographics_total_deaths",],
                           calibration.data=calibration.data[calibration.data$variable == "demographics_total_deaths" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                           multiplier=1,
                           y.label=r'(Mp $Yr^{-1}$)',
                           title.label="(e) Mortality")

births.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "demographics_births",],
                           calibration.data=calibration.data[calibration.data$variable == "demographics_births" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                           multiplier=1,
                           y.label=r'(Mp $Yr^{-1}$)',
                           title.label="(f) Births")


energy.demand.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "energy_demand_demand_for_energy",],
                     calibration.data=calibration.data[calibration.data$variable == "energy_demand_demand_for_energy" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                     multiplier=1e-3,
                     y.label=r'(PWh $Yr^{-1}$)',
                     title.label="(g) Demand for energy")

food.demand.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "food_demand_direct_food_demand_per_person_per_day",],
                               calibration.data=calibration.data[calibration.data$variable == "food_demand_direct_food_demand_per_person_per_day" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                               multiplier=1e-3,
                               y.label=r'(MCal $Person^{-1}$ $Day^{-1}$)',
                               title.label="(h) Demand for food")

concrete.use.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "concrete_total_yearly_concrete_use",],
                     calibration.data=calibration.data[calibration.data$variable == "concrete_total_yearly_concrete_use" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                     multiplier=1e-3,
                     y.label=r'(Gt $Yr^{-1}$)',
                     title.label="(i) Concrete use")


crop.yield.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "crop_crop_yield",],
                            calibration.data=calibration.data[calibration.data$variable == "crop_crop_yield" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                            multiplier=1,
                            y.label=r'(PCal $MHa^{-1}$ $Yr^{-1}$)',
                            title.label="(j) Crop yield")

water.use.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable ==  "freshwater_agricultural_water_withdrawal" ,],
                           calibration.data=calibration.data[calibration.data$variable ==  "freshwater_agricultural_water_withdrawal"  & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                           multiplier=1,
                           y.label=r'($m^{3}$ $Yr^{-1}$)',
                           title.label="(k) Water use")

emissions.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "emissions_total_ghg_emissions",],
                     calibration.data=calibration.data[calibration.data$variable == "emissions_total_ghg_emissions" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                     multiplier=1e-3,
                     y.label=r'(Gt $CO_{2}$ eq. $Yr^{-1}$)',
                     title.label="(l)Total GHG emissions")

sta.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "energy_balance_model_surface_temperature_anomaly" ,],
                     calibration.data=calibration.data[calibration.data$variable == "energy_balance_model_surface_temperature_anomaly"  & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                     multiplier=1,
                     y.label=r'(°C)',
                     title.label="(m) Surface temperature anomaly")

slr.plot <- plot.fun(model.data=data.absolute.df[data.absolute.df$variable == "sea_level_total_global_sea_level_anomaly",],
                     calibration.data=calibration.data[calibration.data$variable == "sea_level_total_global_sea_level_anomaly" & calibration.data$year %in% as.factor(seq(1980,2025,by=5)),],
                     multiplier=1,
                     y.label=r'(m)',
                     title.label="(n) Sea level anomaly")

# Create a dummy plot with the legend, we need this as a placeholder for the legend
my_legend <- get_legend(sta.plot)
empty_plot <- ggplot() + theme_void()+ annotation_custom(my_legend)

draw.plot <- suppressWarnings({
                ggarrange(gdp.plot, productivity.plot, consumption.plot,
                       population.plot, mortality.plot, births.plot, 
                       energy.demand.plot, food.demand.plot,concrete.use.plot,
                       crop.yield.plot, water.use.plot,
                       emissions.plot, sta.plot, slr.plot, empty_plot,
                       nrow=5, ncol=3,
                       heights= c(1,1,1),widths= c(1,1,1),
                       align= "v", legend = "none")
}) 

suppressWarnings({
  ggsave(
  filename = file.path(figures_dir, paste0("f04",".png")),
  plot = draw.plot,
  width = 13,
  height = 12)
})

cat("Script executed successfully. \n
    Figure saved to:", normalizePath(figures_dir), "\n")
