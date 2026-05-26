# Script to make the air to sea flux figure (Figure A1)
#
# Input data should be in RDS format
#
#------------ How to execute -------------
# source('make_figure_A1.R')
#
# Muralidhar Adakudlu, 20/5/2016
# Norwegian Meteorological Institute
#-----------------------------------------------------------

library(tidyverse)
library(ggpubr)
require(latex2exp)
library(abind)
library(extrafont)
library(showtext)
showtext_opts(dpi = 300)
showtext_auto()

# ---------------------------------------------------------------------
# Set working directory to script location
# ---------------------------------------------------------------------
remove(list=ls())
script_path <- normalizePath("make_figure_A1.R")
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
  cat("Input data read successfully")
} else {
  stop("I/p directory is missing or is empty.")
}

# Assign which variable we want
variables.list <- c("ocean_air_sea_co2_flux")

#----------------------------------------------------------------------------------------
# Re-arrange and create a list
#------------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------------
# sum of all individual impacts 
#---------------------------------------------------------------------------------------

data.difference.sum.of.individuals <- sapply(variables.list, function(k) {
  dummy1 <- data.difference[[k]]
  dummy2 <- dummy1[dummy1$Experiment != "emb",] %>% group_by(year) %>% summarise(low1=sum(low1),
                                                                                 median=sum(median),
                                                                                 high1=sum(high1)
                                                                                 )
  dummy2$Experiment <- "sum of individuals"
  return(dummy2)
}, simplify=FALSE)

#----------------------------------------------------------------------------------------------
# combine the sum of individuals to the data frame of differences
#-----------------------------------------------------------------------------------------------

data.difference <- as.list(do.call(bind_rows, list(data.difference,data.difference.sum.of.individuals)))

#-------------------------------------------------------------------------------------------------------
# Plotter function
#----------------------------------------------------------------------------------------------------

plot.diff <- function(model.data, multiplier, 
                      values.linetype, values.color, values.fill,
                      breaks.plot, 
                      labels.plot,
                      y.label,title.label){
  ggplot()+
    geom_line(data=model.data,
              mapping=aes(x=as.character(year),y=median*multiplier, linetype=Experiment,color=Experiment, group = Experiment), linewidth=.7)+
    theme_bw()+
    labs(x=NULL,y=TeX(y.label),title=TeX(title.label)) +
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
          axis.text.y = element_text(family = "sans",size=14, vjust=0.3, color="black"),
          axis.title = element_text(family = "sans",size=14, vjust=0.3, color="black"),
          plot.title = element_text(family = "sans",size=14, vjust=0.3, color="black"),
          panel.grid.major = element_line(color="grey",linewidth=0.5,linetype=3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "grey", fill = NA),
          strip.placement = "outside",
          panel.spacing.x = unit(0,"lines"),
          panel.spacing.y = unit(0,"lines"),
          legend.direction = "horizontal",
          legend.position = "inside",
          legend.position.inside = c(.4,.25),
          legend.title = element_blank(),
          legend.text = element_text(family = "sans",size=12, vjust=0.5, color="black"),
          legend.key.spacing.x = unit(.5,"cm"),
          legend.key.width = unit(2, "lines"),
          legend.background = element_rect(fill = "transparent"))+
    guides(colour =guide_legend(ncol=2), linetype=guide_legend(ncol=2))}

#------------------------------------------------------------------------------------------------------------
# Create plots
#--------------------------------------------------------------------------------------------------------
data.to.plot <- data.difference$ocean_air_sea_co2_flux
air.sea.flux <- plot.diff(model.data=data.to.plot,
                                   multiplier=1,
                                   values.linetype=c("solid","solid","solid","dotted","dashed","solid","solid","solid","solid","solid","solid"),
                                   values.color=c("black","grey","#f39865","#f39865","#f39865","#93a9d8","#c6dda4","#cbbdde","#f9e593","#cba880","#75a593"),
                                   values.fill=c("black"),
                                   breaks.plot=c("emb","sum of individuals","finance","labor","govt","demography","landuse","behavior","energy","concrete","slr"),
                                   #labels.plot=c("AllImpacts","sum of individual impacts",
                                   #               "FRoL (B1/B2/R1)",
                                   #               "LaPr (B3)","GovSp (B4)","Mortality (B5)","CrFw (B11/R6)","ClimExHB (B8/R4)","En (B6/B7)","DuCn (B10)","SLR"),
                                   labels.plot=c("AllImpacts","sum of individual impacts",
                                         "FRoL","LaPr","GovSp","Mortality","CrFw","ClimExHB","En","DuCn","SLR"),
                                   y.label=r'(GtC $Yr^{-1}$)',
                                   title.label=r'(Air to Sea $CO_{2}$ flux)')

ggsave(filename=file.path(figures_dir, paste0("fig_A1",".png")),
       plot=air.sea.flux,
       height =5 , width=7)

cat("Script executed successfully. \n
    Figures saved to:", normalizePath(figures_dir), "\n")