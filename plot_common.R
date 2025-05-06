#############################
# plots look and feel settings and functions
# Hutchens/Aomura/Munhall
#

#REQUIRED libraries

#if( !require(tidyverse) ){install.packages("tidyverse"); require(tidyverse)}
if( !require(ggpubr) ){install.packages("ggpubr"); require(ggpubr)}
if( !require(ggsignif) ){install.packages("ggsignif"); require(ggsignif)}
if( !require(systemfonts) ){install.packages("systemfonts"); require(systemfonts)}
if( !require(ragg) ){install.packages("ragg"); require(ragg)}

#deal with the need for unicode font accessibility on windows and perhaps other architectures
fonts <- systemfonts::system_fonts()
font_light <- "LucidaSansUnicode"
font_light_info <- fonts[fonts$name == "Lucida Sans Unicode", ]
systemfonts::register_font(
  font_light, 
  plain = list(font_light_info$path, font_light_info$index)
)


plot_theme_wide<-list(
  theme_bw(),
  theme(
    aspect.ratio=0.5,
    plot.title=element_text(size=16, face="bold"),
      legend.position = c(0.95,0.99),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(1, 1, 1, 1),
      legend.text=element_text(size=6),
      legend.title = element_blank(),
      legend.key.size = unit(0.5,"line"),
    axis.text=element_text(size=10),
    axis.title=element_text(size=8, face="bold"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.title.x=element_text(color="black"),
    axis.title.y=element_text(color="black")
  )
)
linesize=0.7
pointsize=1.0
barsize=1.0

plot_theme_squareish<-list(
  theme_bw(),
  theme(
    aspect.ratio=1.2,
    plot.title=element_text(size=16, face="bold"),
    legend.title=element_blank(),
    legend.text=element_text(size=18),
    axis.text=element_text(size=16),
    axis.title=element_text(size=18, face="bold"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.title.x=element_text(color="black"),
    axis.title.y=element_text(color="black"),
    
  )
)


plot_theme_squareish2<-list(
  theme_bw(),
  theme(
    aspect.ratio=1.2,
    plot.title=element_text(size=16, face="bold"),
    legend.position = c(0.95,0.99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(1, 1, 1, 1),
    legend.text=element_text(size=6),
    legend.title = element_blank(),
    legend.key.size = unit(0.5,"line"),
    axis.text=element_text(size=10),
    axis.title=element_text(size=8, face="bold"),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    axis.title.x=element_text(color="black"),
    axis.title.y=element_text(color="black")
  )
)

linesize=1.1
pointsize=2.0
barsize=1.2


left_legend<-c(0.25,0.95)


#might help with unicode in plots
if( !require(extrafont) ){install.packages("extrafont"); require(extrafont)}
library(extrafont)
loadfonts(device = "win")

############################################################
#
# OPTIONAL: custom color palettes
#
# if use_my_colors is TRUE
use_my_colors=T
# then scale_color_manual() will be used to set colors to those assigned to treatments in a
# named vector "gamut".

#gamut_shamveh<-c( 'sham'='#ee0000FF' ,'veh'= '#3b4992ff')
gamut_cilveh<-c( 'cil'='#ee0000FF' ,'veh'= '#3b4992ff')

gamut<-c( 'cil'='#008b45ff', 'calcil'='#631879ff', 'veh'='#3b4992ff' )
# copy of gamut for when we want "veh" to appear as 'ctrl'
gamut_ctrl<-c( 'sham'='#88CCEE', 'cil'='#DDCC77', 'ctrl'='#CC6677' )
# other colors #'teal'='#44AA99' #'cobalt blue'='#332288'

# misc color settings
darkcyan<-"#008b8b"
bluedarkcyan<-"#00688b"
default_color=gamut["veh"]
overlay_point_color="darkslateblue" #default color for overlays
vars5_cil_activity=c("mediumblue",bluedarkcyan,darkcyan,"darkslateblue",'#3b4992ff')
csDA1 <- c("mediumblue","red3") 
csDA2 <- c("red3","mediumblue") 
cil_calcil_veh <- c('#631879ff',"mediumblue","red3") 
sham_cil_veh <- c(bluedarkcyan,"mediumblue","red3") 
cil_veh <- c("mediumblue","red3") 
veh_cil <- c("red3","mediumblue") 
sham_veh <- c(bluedarkcyan,"red3") 
redblue<-csDA1
scalefillDA <-  scale_fill_manual(values = c(csDA1))
scalecolorDA <-  scale_color_manual(values = c(csDA1))
scalefillSV<-  scale_fill_manual(values = c(sham_veh))
scalecolorSV<-  scale_color_manual(values = c(sham_veh))
scalefillCV <-  scale_fill_manual(values = c("red3","mediumblue"))
scalecolorCV <-  scale_color_manual(values =  c("red3","mediumblue"))
scalefillCCV <-  scale_fill_manual(values = c(cil_calcil_veh))
scalecolorCCV <-  scale_color_manual(values = c(cil_calcil_veh))
scalefillSCV <-  scale_fill_manual(values = c(sham_cil_veh))
scalecolorSCV <-  scale_color_manual(values = c(sham_cil_veh))
scalefillcilact <-  scale_fill_manual(values = c(vars5_cil_activity))
scalecolorcilact <-  scale_color_manual(values = c(vars5_cil_activity))
cilblue<-"mediumblue"
vehred<-"red3"


#plot text settings
plot_text_settings_DA <- theme(plot.title = element_text(size=36, hjust=0.5, face="bold"),legend.position="none"
                               , axis.text.x=element_text(size=14, colour = "black"), axis.text.y=element_text(size=9, colour = "black")
                               , axis.title.y  = element_text(size = 14))

plot_text_settings_DA_small <- theme(plot.title = element_text(size=30, hjust=0.5, face="bold"),legend.position="none"
                               , axis.text.x=element_text(size=12, colour = "black"), axis.text.y=element_text(size=9, colour = "black")
                               , axis.title.y  = element_text(size = 10))
#plot col-error-scatter
column_scatter_errorbar_plot<-function(d,x,y,groupvar,themevar,scalefill=scalefillDA, scalecolor= scalecolorDA,
                                       p_brackets=FALSE,p_bracketsdata=NULL,bracket_nudge=0,
                                       xlabel,ylabel,xlimit,ylimit){
  
  if(! exists("d") ){
    cat("column_scatter_errorbar_plot: ERROR: data frame not passed\n")
    return(NULL)
  }
  if(! exists("xlabel") ){ xlabel<-"x axis"}
  if(! exists("ylabel") ){ xlabel<-"y axis"}


  gp<-ggplot({{d}},aes({{x}},{{y}},group={{groupvar}},fill={{groupvar}},color={{groupvar}}))+
    scalefill+
    scalecolor+
    geom_bar(stat="summary",  position = "dodge")+
    geom_point(alpha=1, position = position_jitterdodge(jitter.width=0.3,dodge.width = 0.9),
                size=1, stroke=0.3,shape="circle filled",fill="white")+
    geom_errorbar(stat='summary', position= position_dodge(width = 0.9),width=.3, color="black", size=0.3)+
    xlab(xlabel)+
    ylab(ylabel)+
    themevar
  
  if(p_brackets){

    gp<-gp+geom_signif(xmin=p_bracketsdata$signif_bracket_xmin,
                                    xmax=p_bracketsdata$signif_bracket_xmax,
                                   y_position=(p_bracketsdata$y.position+bracket_nudge),
                                   annotation=p_bracketsdata$signif,
                                   tip_length = 0.025,
                                    color="black")

  }
  return(gp) 

  }

column_scatter_errorbar_plot_many<-function(d,x,y,groupvar,themevar,p_brackets=FALSE,p_bracketsdata=NULL,bracket_nudge=0,
                                       xlabel,ylabel,xlimit,ylimit){
  
  if(! exists("d") ){
    cat("col_er_scat: ERROR: data frame not passed\n")
    return(NULL)
  }
  if(! exists("xlabel") ){ xlabel<-"x axis"}
  if(! exists("ylabel") ){ xlabel<-"y axis"}
  
  gp<-ggplot({{d}},aes({{x}},{{y}},group={{groupvar}},fill={{groupvar}},color={{groupvar}}))+
    scale_fill_brewer()+
    scale_color_brewer()+
    geom_bar(stat="summary",  position = "dodge")+
    geom_point(alpha=1, position = position_jitterdodge(jitter.width=0.3,dodge.width = 0.9),
               size=1, stroke=0.3,shape="circle filled",fill="white")+
    geom_errorbar(stat='summary', position= position_dodge(width = 0.9),width=.3, color="black", size=0.3)+
    xlab(xlabel)+
    ylab(ylabel)+
    themevar
  
  if(p_brackets){
    
    gp<-gp+geom_signif(xmin=p_bracketsdata$signif_bracket_xmin,
                       xmax=p_bracketsdata$signif_bracket_xmax,
                       y_position=(p_bracketsdata$y.position+bracket_nudge),
                       annotation=p_bracketsdata$signif,
                       tip_length = 0.025,
                       color="black")
    
  }
  return(gp) 
  
}


#set the upper Ylim based on the (provided) max data point
setmaxY<-function(maxdata, round=TRUE){
  if (maxdata<1){return(1)}
  if(round){
    return(round(round(maxdata,-1)/10*11,-1))}
  else {return(maxdata/10*11)}
}


#convert numeric p value to stars for plots
pstar<-function(p.value,write_out_ns=TRUE, simple=FALSE){
  if (write_out_ns==TRUE){
    ns="ns"
  }
  else{
    (ns="")
    }
  if(simple==TRUE){
    stars=ifelse(p.value<0.05,"\U204E","")
  }
  else{
  stars=(ifelse(p.value<0.05,
                (ifelse(p.value<0.01, 
                        (ifelse(p.value<0.001,"\U2042","\U2051")),
                        "\U204E")),"\U0020"))
}
  return(stars)
}

pstar_horiz<-function(p.value,write_out_ns=TRUE){
  if (write_out_ns==TRUE){
    ns="ns"
  }
  else{
    (ns="")
  }
stars=ifelse(p.value<0.05,
             (ifelse(p.value<0.01, 
                     (ifelse(p.value<0.001,"***","**")),
                     "*")),ns)
  return(stars)
}

pval_to_significance_table<-function(pval,maxy, signif, bracket_minx, bracket_maxx){
  #takes a single pval calculated elsewhere, and the maximum y value in a dataset, 
  #and bracket coordinates 
  #and returns a dataframe with the expected values in order to place a custom 
  #comparison bracket
  #since it's a 
  significance_table<-data.frame(
    p.value=pval,
    signif=signif,
    y.position=(maxy*1.1),
    signif_bracket_xmin=bracket_minx,
    signif_bracket_xmax=bracket_maxx
  )
  return(significance_table)
}


pairs_to_significance_table<-function(pair_summary,maxy){
  #takes a pairs (emmeans) model as summary(pairs)
  #and returns a table of significance values which can be used to plot
  #with geom_signif
  significance_table<-pair_summary%>%group_by(x)%>%mutate(y.position=(maxy*1.1))%>%
    summarize(
      contrast=contrast,
      x=x,
      p.value=p.value,
      y.position=y.position,
      signif=star
  )
  significance_table<-significance_table%>%mutate(index=row_number(),
                                                signif_bracket_xmin=ifelse(p.value>=0.05,NA,index-0.15) ,
                                                signif_bracket_xmax=ifelse(p.value>=0.05,NA,index+0.15))
  
  #significance_table<-significance_table%>%mutate(signif=pstar(p.value))
  return(significance_table)
  
}

pairs_to_significance_table_no_grouping<-function(pair_summary){

  significance_table<-pair_summary%>%mutate(y.position=(max(emm_means$emmean)*1.3)
  )%>%summarize(
    contrast=contrast,
    p.value=p.value,
    y.position=y.position
  )
  significance_table<-significance_table%>%mutate(index=row_number(),
                                                  signif_bracket_xmin=ifelse(p.value>=0.05,NA,index-0.15) ,
                                                  signif_bracket_xmax=ifelse(p.value>=0.05,NA,index+0.15))
  
  significance_table<-significance_table%>%mutate(signif=pstar(p.value))
  return(significance_table)
  
}


line_scatter_errorbar_plot<-function(d,x,y,groupvar,themevar,scalefill=scalefillDA, scalecolor= scalecolorDA,
                                     scale_timepoint_breaks=waiver(),
                                     p_brackets=FALSE,p_bracketsdata=NULL,bracket_nudge=0,
                                       xlabel,ylabel,xlimit,ylimit){
  
  if(! exists("d") ){
    cat("column_scatter_errorbar_plot: ERROR: data frame not passed\n")
    return(NULL)
  }
  if(! exists("xlabel") ){ xlabel<-"x axis"}
  if(! exists("ylabel") ){ xlabel<-"y axis"}
  
  gp<-ggplot({{d}},aes({{x}},{{y}},group={{groupvar}},fill={{groupvar}},color={{groupvar}}))+
    scalefill+
    scalecolor+
    geom_point(alpha=0.4, position = position_jitterdodge(jitter.width=0.3,dodge.width = 0.9),
               size=0.7, stroke=0.2,shape="circle filled",fill="white")+
    geom_line(stat="summary",  position= position_dodge(width = 0.1), linewidth=1)+
    geom_errorbar(stat='summary', position= position_dodge(width = 0.1),width=.3, color="black", size=0.3)+
    xlab(xlabel)+
    ylab(ylabel)+
    themevar+
    scale_x_discrete(breaks=scale_timepoint_breaks)
  
  if(p_brackets){
    
    gp<-gp+geom_signif(xmin=p_bracketsdata$signif_bracket_xmin,
                       xmax=p_bracketsdata$signif_bracket_xmax,
                       y_position=(p_bracketsdata$y.position+bracket_nudge),
                       annotation=p_bracketsdata$signif,
                       tip_length = 0.025,
                       color="black")
    
  }
  return(gp) 
  
}


get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

get_plot_ymax <- function(plot) {
  gb = ggplot_build(plot)
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  return(ymax)
}


get_plot_ymin <- function(plot) {
  gb = ggplot_build(plot)
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  return(ymin)
}

get_star_y<- function(plot) {
  gb = ggplot_build(plot)
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  star.y<-ymax-(0.1*(ymax-ymin))
  return(star.y)
}