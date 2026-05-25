#------------------------------------------------------------------------------------------
# This script contains functions used in the script "make_figures_5_to_14.R" to plot the figures
# in the paper Adakudlu et al., 2026
#------------------------------------------------------------------------------------------

# ------------------ Plots absolute values -------------------------------------------

plot.absolute <- function(model.data, shading.data, errorbar.data,  multiplier, 
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

#------For government expenditure/labour productivity, the y-scale is very large, making it hard to
#      follow the values in the EMB trajectory, so we transform the y-scale to log10

plot.absolute.logy <- function(model.data, shading.data, errorbar.data,  multiplier, 
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


#----------- Plot the differences without the sum of individuals --------------------------

plot.difference.no.grey.line <- function(model.data, multiplier, 
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


plot.difference.with.grey.line <- function(model.data, multiplier, 
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

