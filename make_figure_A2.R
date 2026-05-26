# Script to make the Figure A2 - to show nonlinearities exist even without the
# dominant impact channel, FRoL
#
# Input data should be in RDS format
#
#------------ How to execute -------------
# source('make_figure_A2.R')
#
# Muralidhar Adakudlu, 20/5/2016
# Norwegian Meteorological Institute
#-----------------------------------------------------------

#install.packages("xlsx")

library(tidyverse)
library(ggpubr)
require(latex2exp)
library(abind)
library("readxl")
library(openxlsx)
library(purrr)

# ---------------------------------------------------------------------
# Set working directory to script location
# ---------------------------------------------------------------------
remove(list=ls())
script_path <- normalizePath("make_figure_A2.R")
script_dir  <- dirname(script_path)
setwd(script_dir)
data_dir    <- file.path(script_dir,'supplementary_data')
figures_dir <- file.path(script_dir, 'supplementary_figures')

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
  df.noFRoL <- readRDS(file.path(data_dir,'dataForCompoundDam-ClimateFeedback_noFRoL.RDS'))
  cat("Input data read successfully")
} else {
  stop("I/p directory is missing or is empty.")
}

# We are interested in the following variables
variables.list <- c("food_demand_direct_food_demand_per_person_per_day",
                   "crop_crop_yield")

#----------------------------------------------------------------------------------------
# Re-arrage and create a list
#------------------------------------------------------------------------------------
data.difference <- sapply(variables.list, function(k) {
  map_df(.x=list("NoImpacts"=df.emb[[k]]$counterfactual,            # This is only a place holder to put the sum of individual exps.
                 "AllImpacts"=df.emb[[k]]$abs.difference,
                 "FRoL"=df.finance[[k]]$abs.difference,
                 "LaPr"=df.labour[[k]]$abs.difference,
                 "GovSp"=df.govsp[[k]]$abs.difference,
                 "Demo"=df.mortality[[k]]$abs.difference,
                 "CrFw"=df.landuse[[k]]$abs.difference,
                 "ClimExHB"=df.behaviour[[k]]$abs.difference,
                 "En"=df.energy[[k]]$abs.difference,
                 "DuCn"=df.concrete[[k]]$abs.difference,
                 "SLR"=df.slr[[k]]$abs.difference,
                 "Demo_LaPr_DuCn_GovSp_SLR_CrFw_ClimExHB_En"=df.noFRoL[[k]]$abs.difference
                  ),
         .f=bind_rows,
         .id="Experiment")}, simplify = FALSE)

# Extract the median (if it throws error, check the column number of "median". Below it is #5) and combine the variables
data.df <- do.call(rbind,data.difference)[,c(1,2,5)]
data.df$variable <- unlist(lapply(strsplit(rownames(data.df),"\\."), '[[', 1))

# Separate the single channel experiments and the new experiment without FRoL
runs <- unique(data.difference[[variables.list[1]]]$Experiment)
experiments <- runs[!(runs %in% c("NoImpacts", 
                                  "AllImpacts",
                                  "GovSp",
                                  "Demo",
                                  "DuCn",
                                  "ClimExHB",
                                  "En",
                                  "CrFw",
                                  "FRoL",
                                  "LaPr",
                                  "SLR"))]

#----------------------------------------------------------------------------------------
# Function to estimate the sum of individuals in the additional experiments 
# Right now, we have only one - noFRoL
#-------------------------------------------------------------------------------------- 

generate.df <- function(exp.name, df) {
  exp.runs <- abind(unlist(strsplit(exp.name,"_")),exp.name)
  exp.df <- df[df$Experiment %in% c("NoImpacts",exp.runs),] %>% group_by(variable,year) %>%
    mutate(sum_of_each=case_when(
      Experiment == "NoImpacts" ~ sum(median[Experiment %in% exp.runs[!(exp.runs %in% exp.name)]],na.rm=TRUE),
      TRUE ~ NA_real_
    )) %>% group_by(variable,year) %>%
    mutate(FinalValue=ifelse(
      Experiment == "NoImpacts", sum_of_each, median))  }

#-------------------------------------------------------------------------------------------
# Find the sum of individual impact channels corresponding to each targeted experiment (in this case only 1) 
# and arrange them by rows.
#-----------------------------------------------------------------------------------------
experiments.df <- lapply(experiments, function(i) {
  exp.id <- generate.df(i,data.df)})
names(experiments.df) <- experiments
experiments.merge <- bind_rows(experiments.df, .id="ExpName")


#---------------------------------------------------------------------------------------------------------
# To plot EMB black and grey lines on top of each panel for each experiments, we need to separately find
# sum of individuals  for the AllImpacts case
#-----------------------------------------------------------------------------------------------
emb.runs <- runs[!(runs %in% c("NoImpacts","AllImpacts", experiments))] # this will pick up all the single-channel experiments
emb.df <- data.df[!(data.df$Experiment %in% experiments),] %>% group_by(variable,year) %>%
  mutate(FinalValue=ifelse(
    Experiment == "NoImpacts", sum(median[Experiment %in% emb.runs],na.rm=TRUE), median))

# Keep only AllImpacts and NoImpacts data, remove the rest
emb.df.final <- map_dfr(1:length(experiments), ~ as.data.frame(emb.df[!(emb.df$Experiment %in% emb.runs),]), .id ="ExpName") %>% 
   mutate(ExpName=case_when(ExpName == "1"~experiments[1],
                            TRUE~NA_character_)) %>% 
  mutate(Experiment=ifelse(Experiment == "NoImpacts", "sum of all individuals impacts",Experiment))

#--------------------------------------------------------------------------------------------------------
# Merge the individual experiments and the EMB cases to plot
#------------------------------------------------------------------------------------------------------
merge.data <- rbind(experiments.merge[, (!colnames(experiments.merge) %in% "sum_of_each")],emb.df.final)
  
#-------------------------------------------------------------------------------------------------------
# Plotter function
#----------------------------------------------------------------------------------------------------

plot.fun <- function(plot.data,multiplier,
                     plot.var,
                     values.linetype, 
                     values.color,
                     breaks.plot, 
                     labels.plot,
                     y.label,title.label){
  ggplot()+
    geom_line(data=plot.data,
              mapping=aes(x=year,y=FinalValue*multiplier, linetype=Experiment,color=Experiment, group = Experiment), linewidth=1)+
    theme_bw()+
    labs(x=NULL,y=TeX(y.label),title=title.label) +
    scale_x_continuous(breaks=seq(1980,2150,by=20))+
    scale_linetype_manual(values=values.linetype,
                          breaks = breaks.plot,
                          labels=labels.plot)+
    scale_color_manual(values=values.color,
                       breaks = breaks.plot,
                       labels= labels.plot,)+
    theme(axis.text.x = element_text(family = "sans",size=13,angle=45, vjust=0.3, color="black"),
          axis.text.y = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          axis.title = element_text(family = "sans",size=15, vjust=0.3, color="black"),
          plot.title = element_text(family = "sans",size=15, vjust=0.3, color="black"),
          panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill = NA),
          strip.placement = "outside",
          strip.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(family = "sans",size=13, vjust=0.3, color="black"),
          panel.spacing.x = unit(0,"lines"),
          panel.spacing.y = unit(0,"lines"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "sans",size=15, vjust=0.5, color="black"),
          legend.key.spacing.x = unit(.5,"cm"),
          legend.key.width = unit(2, "lines"),
          legend.background = element_rect(fill = "transparent") )+
    guides(colour =guide_legend(ncol=8), linetype=guide_legend(ncol=8))}

#------------------------------------------------------------------------------------------------------------
# Create plots
#--------------------------------------------------------------------------------------------------------
p1 <- plot.fun(plot.data=merge.data[merge.data$ExpName == experiments[1] &
                                      merge.data$variable %in% c("food_demand_direct_food_demand_per_person_per_day"),],
               multiplier=1e-3,
               values.linetype = c("twodash","twodash","solid","solid","solid","solid","solid","solid","dashed","dotted","solid", "solid"),
               values.color = c("black","grey","#c6dda4","#cbbdde","#f9e593","#93a9d8","black","#cba880","#f39865","#f39865","grey","#75a593"),
               breaks.plot = c( "AllImpacts","sum of all individuals impacts","CrFw","ClimExHB","En","Demo",experiments[1],"DuCn","GovSp","LaPr","NoImpacts","SLR"),
               labels.plot = c("EMB","Sum of ALL individuals impacts","CrFw","ClimExHB","En","Mortality","Coupled effect", "DuCn","GovSp","LaPr","Sum of individual impacts","SLR"),
               y.label = r'(MCal $Person^{-1}$ $Day^{-1}$)',
               title.label = "(a) Food demand" )

p2 <- plot.fun(plot.data=merge.data[merge.data$ExpName == experiments[1] &
                                      merge.data$variable %in% c("crop_crop_yield"),],
               multiplier=1,
               values.linetype = c("twodash","twodash","solid","solid","solid","solid","solid","solid","dashed","dotted","solid", "solid"),
               values.color = c("black","grey","#c6dda4","#cbbdde","#f9e593","#93a9d8","black","#cba880","#f39865","#f39865","grey","#75a593"),
               breaks.plot = c( "AllImpacts","sum of all individuals impacts","CrFw","ClimExHB","En","Demo",experiments[1],"DuCn","GovSp","LaPr","NoImpacts","SLR"),
               labels.plot = c("EMB","Sum of ALL individuals impacts","CrFw","ClimExHB","En","Mortality","Coupled effect", "DuCn","GovSp","LaPr","Sum of individual impacts","SLR"),
               y.label = r'(PCal $MHa^{-1}$ $Yr^{-1}$)',
               title.label = "(b) Crop Yield" )

panel.A2 <- ggarrange(p1,p2,
                      nrow=1, ncol=2,
                      heights= c(1,1,1),widths= c(1,1,1),
                      align= "v",legend = "bottom",
                      common.legend = T)

#-----------------------------------------------------------------------------------
# Save the figures
#---------------------------------------------------------------------------------
ggsave(filename=file.path(figures_dir, paste0("fig_A2",".png")),
       plot=panel.A2,
       height=6 , width=15)

cat("Script executed successfully. \n
    Figures saved to:", normalizePath(figures_dir), "\n")
