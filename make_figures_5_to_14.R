# Creates the plots #5 to #13 

# Input:
# 
# Data should be in RDS format, separate files for every experiment listed in Table 2 of the paper
# Each input file contains a nested list of variables required for the paper with
#  4 metrics (actual run, NoImpacts counterfactual, absolute difference, and percent difference),
#  each having the corresponding median, 67% bound and the 95% bound
# The input files come from the post-processing of the raw uncertainty sampling with 100,000 members of FRIDAv2.1

# Author:
# Muralidhar Adakudlu, Norwegian Meteorological Institute

library(tidyverse)
library(ggpubr)
require(latex2exp)
library(abind)

setwd('G:/My Drive/R/r-scripts/worldTrans/make_plots_nonlinearity_paper')
remove(list=ls())
filepath.emb <- 'G:/My Drive/R/r-scripts/worldTrans/WorldTransFRIDA-aggregateDamages/outputData'

emb.cfb.on         <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_emb-ClimateSTAOverride_Off_new_sampling.RDS'))
emb.cfb.finance <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_FinanceImpact-ClimateSTAOverride_Off.RDS'))
emb.cfb.labor <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_LabourImpact-ClimateSTAOverride_Off.RDS'))
emb.cfb.govt <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_GovernmentImpact-ClimateSTAOverride_Off.RDS'))
emb.cfb.demography <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_DemographyImpact-ClimateSTAOverride_Off.RDS'))
emb.cfb.landuse <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_LanduseImpact-ClimateSTAOverride_Off.RDS'))
emb.cfb.behavior <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_BehavioralImpact-ClimateSTAOverride_Off_testing.RDS'))
emb.cfb.energy <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_EnergyImpact-ClimateSTAOverride_Off.RDS'))
emb.cfb.concrete <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_ConcreteImpact-ClimateSTAOverride_Off.RDS'))
emb.cfb.slr <- readRDS(file.path(filepath.emb,'dataForCompoundDam-policy_EMB-ClimateFeedback_SLRImpact-ClimateSTAOverride_Off.RDS'))

variables.list <- c(
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

# Put units here only, but for titles, names and units can be separated
title.list <- c("GDP per capita", #"Government consumption", "Productivity",
                "Population","Mortality","Births",
                "Demand for energy",
                "Total food demand",
                "Total yearly concrete use",
                "Water use","Crop yield",
                "Surface Temperature anomaly",
                "Total CO2 emissions","Total CH4 emissions","Total N2O emissions","Total SO2 emissions",
                "Total NOx emissions","Total VOC emissions","Total CO emissions","Minor GHG emissions",
                "Sea_level_anomaly")
                
data.absolute <- sapply(variables.list, function(k) {
                 map_df(.x=list("counterfactual"=emb.cfb.on[[k]]$counterfactual,
                 "emb"=emb.cfb.on[[k]]$baseline,
                 "finance"=emb.cfb.finance[[k]]$baseline,
                 "labor"=emb.cfb.labor[[k]]$baseline,
                 "govt"=emb.cfb.govt[[k]]$baseline,
                 "demography"=emb.cfb.demography[[k]]$baseline,
                 "landuse"=emb.cfb.landuse[[k]]$baseline,
                 "behavior"=emb.cfb.behavior[[k]]$baseline,
                 "energy"=emb.cfb.energy[[k]]$baseline,
                 "concrete"=emb.cfb.concrete[[k]]$baseline,
                 "slr"=emb.cfb.slr[[k]]$baseline),
         .f=bind_rows,
         .id="Experiment")}, simplify = FALSE)

# This set has the uncertainty range per ensemble member. Looks unrealistic in the plots
data.difference <- sapply(variables.list, function(k) {
  map_df(.x=list("emb"=emb.cfb.on[[k]]$per.difference,
                 "finance"=emb.cfb.finance[[k]]$per.difference,
                 "labor"=emb.cfb.labor[[k]]$per.difference,
                 "govt"=emb.cfb.govt[[k]]$per.difference,
                 "demography"=emb.cfb.demography[[k]]$per.difference,
                 "landuse"=emb.cfb.landuse[[k]]$per.difference,
                 "behavior"=emb.cfb.behavior[[k]]$per.difference,
                 "energy"=emb.cfb.energy[[k]]$per.difference,
                 "concrete"=emb.cfb.concrete[[k]]$per.difference,
                 "slr"=emb.cfb.slr[[k]]$per.difference),
         .f=bind_rows,
         .id="Experiment")}, simplify = FALSE)

# Add new item, total GHG emissions, to the lists above
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

# Plotting function 
plot.fun <- function(model.data, shading.data, errorbar.data,  multiplier, 
                     values.linetype, values.color, values.fill,
                     breaks.plot, 
                     labels.plot,
                     y.label,title.label){
  ggplot()+
    geom_line(data=model.data,
              mapping=aes(x=as.character(year),y=median*multiplier, linetype=Experiment,color=Experiment, group = Experiment), linewidth=.7)+
    geom_ribbon(data=shading.data,
                mapping=aes(x=as.character(year),ymax=high1*multiplier,ymin=low1*multiplier,fill=Experiment,group = Experiment),alpha=0.2)+
    geom_errorbar(data=errorbar.data,
                mapping=aes(x=as.character(year),ymax=high1*multiplier,ymin=low1*multiplier,width=4, color= Experiment, group = Experiment, linetype=Experiment),alpha=0.9)+geom_jitter()+
  
    theme_bw()+
    labs(x=NULL,y=TeX(y.label),title=title.label)+
    scale_x_discrete(breaks=seq(1980,2150,by=20))+
    scale_shape_manual(values = 1, labels=c("Calibration"),guide="none") +
    scale_linetype_manual(values=values.linetype,
                          breaks = breaks.plot,
                          labels=labels.plot)+
    scale_color_manual(values=values.color,
                       breaks = breaks.plot,
                       labels= labels.plot)+
    scale_fill_discrete(type=values.fill,
                        breaks = breaks.plot,
                        labels=labels.plot)+
    theme(axis.text.x = element_text(family = "sans",size=10,angle=45, vjust=0.3, color="black"),
          axis.text.y = element_text(family = "sans",size=12, vjust=0.3, color="black"),
          axis.title = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          plot.title = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill = NA),
          strip.placement = "outside",
          panel.spacing.x = unit(0,"lines"),
          panel.spacing.y = unit(0,"lines"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "sans",size=12, vjust=0.5, color="black"),
          legend.key.spacing.x = unit(2,"cm"),
          legend.key.width = unit(2, "lines"))+
    guides(colour =guide_legend(ncol=4), fill=guide_legend(ncol=1),linetype=guide_legend(ncol=4))}

plot.fun.logy <- function(model.data, shading.data, errorbar.data,  multiplier, 
                     values.linetype, values.color, values.fill,
                     breaks.plot, 
                     labels.plot,
                     y.label,title.label){
  ggplot()+
    geom_line(data=model.data,
              mapping=aes(x=as.character(year),y=median*multiplier, linetype=Experiment,color=Experiment, group = Experiment), linewidth=.7)+
    geom_ribbon(data=shading.data,
                mapping=aes(x=as.character(year),ymax=high1*multiplier,ymin=low1*multiplier,fill=Experiment,group = Experiment),alpha=0.2)+
    geom_errorbar(data=errorbar.data,
                  mapping=aes(x=as.character(year),ymax=high1*multiplier,ymin=low1*multiplier,width=4, color= Experiment, group = Experiment, linetype=Experiment),alpha=0.9)+geom_jitter()+
    
    theme_bw()+
    scale_y_log10()+
    labs(x=NULL,y=TeX(y.label),title=title.label)+
    scale_x_discrete(breaks=seq(1980,2150,by=20))+
    scale_shape_manual(values = 1, labels=c("Calibration"),guide="none") +
    scale_linetype_manual(values=values.linetype,
                          breaks = breaks.plot,
                          labels=labels.plot)+
    scale_color_manual(values=values.color,
                       breaks = breaks.plot,
                       labels= labels.plot)+
    scale_fill_discrete(type=values.fill,
                        breaks = breaks.plot,
                        labels=labels.plot)+
    theme(axis.text.x = element_text(family = "sans",size=10,angle=45, vjust=0.3, color="black"),
          axis.text.y = element_text(family = "sans",size=12, vjust=0.3, color="black"),
          axis.title = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          plot.title = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill = NA),
          strip.placement = "outside",
          panel.spacing.x = unit(0,"lines"),
          panel.spacing.y = unit(0,"lines"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "sans",size=12, vjust=0.5, color="black"),
          legend.key.spacing.x = unit(2,"cm"),
          legend.key.width = unit(2, "lines"))+
    guides(colour =guide_legend(ncol=4), fill=guide_legend(ncol=1),linetype=guide_legend(ncol=4))}

plot.diff <- function(model.data, multiplier, 
                     values.linetype, values.color, values.fill,
                     breaks.plot, 
                     labels.plot,
                     y.label,title.label){
  ggplot()+
    geom_line(data=model.data,
              mapping=aes(x=as.character(year),y=median*multiplier, linetype=Experiment,color=Experiment, group = Experiment), linewidth=.7)+
    theme_bw()+
    labs(x=NULL,y=TeX(y.label),title=paste(title.label, "(deviations from NoImpacts)")) +
    scale_x_discrete(breaks=seq(1980,2150,by=20))+
    scale_shape_manual(values = 1, labels=c("Calibration"),guide="none") +
    scale_linetype_manual(values=values.linetype,
                          breaks = breaks.plot,
                          labels=labels.plot)+
    scale_color_manual(values=values.color,
                       breaks = breaks.plot,
                       labels= labels.plot,)+
    scale_fill_discrete(type=values.fill,
                        breaks = breaks.plot,
                        labels=labels.plot)+
    theme(axis.text.x = element_text(family = "sans",size=10,angle=45, vjust=0.3, color="black"),
          axis.text.y = element_text(family = "sans",size=12, vjust=0.3, color="black"),
          axis.title = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          plot.title = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill = NA),
          strip.placement = "outside",
          panel.spacing.x = unit(0,"lines"),
          panel.spacing.y = unit(0,"lines"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "sans",size=12, vjust=0.5, color="black"),
          legend.key.spacing.x = unit(2,"cm"),
          legend.key.width = unit(2, "lines"))+
    guides(colour =guide_legend(ncol=4), fill=guide_legend(ncol=1),linetype=guide_legend(ncol=4))}

data.absolute.4.errorbar <- function(data){as.data.frame(data) %>% mutate(low1 = ifelse(Experiment %in% "finance" & year %in% seq(2020,2150,by=20), low1, 
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
                                                                                                                        )}

library(extrafont)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto()

#******* GDP and Econ variables *********************
data.to.plot <- data.absolute$demographics_real_gdp_per_person
gdp.per.capita.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
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
gdp.per.capita.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
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
govt.plot.abs <- plot.fun.logy(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy", "slr"),],
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
govt.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy", "slr"),],
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
labour.plot.abs <- plot.fun.logy(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
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
labour.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% c("counterfactual","emb","finance","labor","govt", "energy","slr"),],
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
print(panel.finance.sector)
ggsave(filename=paste0("figure_5_finance_impacts",".png"),
       plot=panel.finance.sector,
       height = 8, width=10)

#************************* Mortality **************************
data.to.plot <- data.absolute$demographics_total_deaths
cases.to.plot <- c("counterfactual","emb","finance","labor","govt","demography")
mortality.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
mortality.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
print(panel.mortality.sector)
ggsave(filename=paste0("figure_6_Mortality_Impact",".png"),
       plot=panel.mortality.sector,
       height =5 , width=9.5)

#****************** Energy demand ********************
data.to.plot <- data.absolute$energy_demand_demand_for_energy
cases.to.plot <- c("counterfactual","emb","finance","labor","landuse", "energy","slr")
energy.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
energy.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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

print(panel.energy.sector)
ggsave(filename=paste0("figure_7_Energy_Impact",".png"),
       plot=panel.energy.sector,
       height = 4, width=10)

#*************** Food demand ************************

data.to.plot <- data.absolute$food_demand_direct_food_demand_per_person_per_day
cases.to.plot <- c("counterfactual","emb","finance","labor","govt","landuse", "behavior", "energy","slr")
food.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
food.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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

print(panel.food.sector)
ggsave(filename=paste0("figure_8_food_impacts",".png"),
       plot=panel.food.sector,
       height = 5, width=10)

#************* Resources *****************

data.to.plot <- data.absolute$concrete_total_yearly_concrete_use
cases.to.plot <- c("counterfactual","emb","finance","labor","govt", "energy","concrete","slr")
concrete.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
concrete.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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

print(panel.concrete.sector)
ggsave(filename=paste0("figure_9_concrete_impacts",".png"),
       plot=panel.concrete.sector,
       height =5 , width=10)


#************************* Crop yield and freshwater **************************

data.to.plot <- data.absolute$crop_crop_yield
cases.to.plot <- c("counterfactual","emb","finance","govt","landuse","behavior", "labor","energy")
crop.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
crop.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
water.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
water.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
print(panel.landuse.sector)
ggsave(filename=paste0("figure_10_LandUse_Impact",".png"),
       plot=panel.landuse.sector,
       height =8 , width=11)

##******************************** Feedbacks to climate **###

data.to.plot <- data.absolute$energy_balance_model_surface_temperature_anomaly
cases.to.plot <- c("counterfactual","emb","finance","labor", "government","demography","energy","behavior","concrete","slr")
sta.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
sta.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
emissions.plot.abs <- plot.fun(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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
emissions.plot.diff <- plot.diff(model.data=data.to.plot[data.to.plot$Experiment %in% cases.to.plot,],
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

print(panel.climate.sector)
ggsave(filename=paste0("figure_11_feedbacks_to_climate",".png"),
       plot=panel.climate.sector,
       height =8 , width=12)

##**** Plotting nonlinearities *****#############
# For nonlinearity, we consider absolute differences

data.difference <- sapply(variables.list, function(k) {
  map_df(.x=list("emb"=emb.cfb.on[[k]]$abs.difference,
                 "finance"=emb.cfb.finance[[k]]$abs.difference,
                 "labor"=emb.cfb.labor[[k]]$abs.difference,
                 "govt"=emb.cfb.govt[[k]]$abs.difference,
                 "demography"=emb.cfb.demography[[k]]$abs.difference,
                 "landuse"=emb.cfb.landuse[[k]]$abs.difference,
                 "behavior"=emb.cfb.behavior[[k]]$abs.difference,
                 "energy"=emb.cfb.energy[[k]]$abs.difference,
                 "concrete"=emb.cfb.concrete[[k]]$abs.difference,
                 "slr"=emb.cfb.slr[[k]]$abs.difference),
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

plot.diff <- function(model.data, multiplier, 
                      values.linetype, values.color, values.fill,
                      breaks.plot, 
                      labels.plot,
                      y.label,title.label){
  ggplot()+
    geom_line(data=model.data,
              mapping=aes(x=as.character(year),y=median*multiplier, linetype=Experiment,color=Experiment, group = Experiment), linewidth=.7)+
    theme_bw()+
    labs(x=NULL,y=TeX(y.label),title=title.label) +
    scale_x_discrete(breaks=seq(1980,2150,by=20))+
    scale_shape_manual(values = 1, labels=c("Calibration"),guide="none") +
    scale_linetype_manual(values=values.linetype,
                          breaks = breaks.plot,
                          labels=labels.plot)+
    scale_color_manual(values=values.color,
                       breaks = breaks.plot,
                       labels= labels.plot,)+
    scale_fill_discrete(type=values.fill,
                        breaks = breaks.plot,
                        labels=labels.plot)+
    theme(axis.text.x = element_text(family = "sans",size=12,angle=45, vjust=0.3, color="black"),
          axis.text.y = element_text(family = "sans",size=15, vjust=0.3, color="black"),
          axis.title = element_text(family = "sans",size=16, vjust=0.3, color="black"),
          plot.title = element_text(family = "sans",size=16, vjust=0.3, color="black"),
          panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill = NA),
          plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, "cm"),
          strip.placement = "outside",
          panel.spacing.x = unit(0,"lines"),
          panel.spacing.y = unit(0,"lines"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "sans",size=15, vjust=0.5, color="black"),
          legend.key.spacing.x = unit(.5,"cm"),
          legend.key.width = unit(2, "cm"))+
    guides(colour =guide_legend(ncol=4), linetype=guide_legend(ncol=4))}


# Plots

data.to.plot <- data.difference$demographics_real_gdp_per_person
gdp.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
productivity.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
govt.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
population.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
mortality.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
energy.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
food.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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

print(food.plot.nonlinearity)

data.to.plot <- data.difference$concrete_total_yearly_concrete_use
concrete.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
crop.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
water.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
emissions.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
sta.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
print(panel.nonlinearity)

ggsave(filename=paste0("figure_13_panel_nonliearity",".png"),
       plot=panel.nonlinearity,
       height =14.5 , width=15)

#-----------------------------
# Plot the nonlinearities in individual emissions sources

data.to.plot <- data.difference$emissions_total_co2_emissions
co2.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
ch4.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
n2o.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
SO2.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
nox.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
voc.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
co.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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
hfc.plot.nonlinearity <- plot.diff(model.data=data.to.plot,
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

ggsave(filename=paste0("figure_14_emissions_nonliearity",".png"),
       plot=panel.emissions,
       height =8 , width=16)

