# todo: consider storing the theme as one object, then call it in each myplot function
# ggplot tips: multiplot.R, fullrange=TRUE to extend reg line
require(reshape2)
require(stringr)
source("munge/RMSE.R")

# No 1:1 abline, no reg line --------------------------------------------------
my_ggplot <- function(data, xvar, yvar, group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                         yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                         shape.col, legend.pos, shape.pch){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) + #  expand=c(0,0)
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + #  expand=c(0,0)
    # geom_smooth(method='lm', se=FALSE, col="grey50", size=0.5, linetype=2) +
    geom_point(aes_string(shape = group, col = group), cex=pts) + # ), cex = 0.6
    geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12) + # base_family
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col)
  
  return(plot)
}


# With 1:1 line, no reg line ----------------------------------------------
my_ggplot_1to1 <- function(data, xvar, yvar, group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                   yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                   shape.col, legend.pos, shape.pch){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  rmse <- round(RMSE(data[,xvar], data[,yvar]), 2)
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
    geom_abline(slope=1, intercept=0, size=0.2) + #  ADD 1:1 line
    geom_point(aes_string(shape = group, col = group), cex=pts) + # ), cex = 0.6
    # geom_smooth(method='lm', se=FALSE, col="black") +
    # geom_text(x = text.x, y = text.y, label = paste("RMSE = ",rmse,sep="")) + # ), cex = 2
    geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12, base_family = "Helvetica") +
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col)
  
  return(plot)
}



# No 1:1 abline, with reg line --------------------------------------------------
my_ggplot_lm <- function(data, xvar, yvar, group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                           yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                           shape.col, legend.pos){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
    geom_smooth(method='lm', se=FALSE, col="grey50", size=0.5, linetype=2) +
    geom_point(aes_string(shape = group, col = group), cex=pts) + # ), cex = 0.6
    geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12) + # base_family
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=1:ngroup) + 
    scale_colour_manual(name="", values=shape.col)
  
  return(plot)
}



# No 1:1 abline, by Type and Forest Quality --------------------------------------------------
my_ggplot_lm_qual <- function(data, xvar, yvar, group1, group2, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                         yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                         legend.pos){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
    geom_smooth(method='lm', se=FALSE, col="grey50", size=0.5, linetype=2) +
    geom_point(aes_string(shape = group1, col = group2), cex=pts) + # ), cex = 0.6
    geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12) + # base_family
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) 
  return(plot)
}




# Plot PARAS vs Landsat, save pdf -----------------------------------------
myplot_PARASvsLandsat <- 
  function(paras.res, black.soil, xaxis.lims = list(green=c(0,1), red=c(0,1), nir=c(0,1), swir1=c(0,1), swir2=c(0,1)),
           yaxis.lims = list(green=c(0,1), red=c(0,1), nir=c(0,1), swir1=c(0,1), swir2=c(0,1)),
           xaxis.breaks = list(green=seq(0,1,0.1), red=seq(0,1,0.1), nir=seq(0,1,0.1), swir1=seq(0,1,0.1), swir2=seq(0,1,0.1)), 
           yaxis.breaks = list(green=seq(0,1,0.1), red=seq(0,1,0.1), nir=seq(0,1,0.1), swir1=seq(0,1,0.1), swir2=seq(0,1,0.1)),
           text.xs = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8), 
           text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
           out.dir, title) {
    
    
    if (black.soil) {                                  # If to print black soil case figures: BRF_b2_bs
      
      PARAS_Landsat_byType <- list(
        green = my_ggplot_1to1(paras.res, "SR_B2_green", "BRF_b2_bs", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                               "LANDSAT green", "PARAS green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                               xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                               text.x = text.xs$green, text.y = text.ys$green,
                               shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        red = my_ggplot_1to1(paras.res, "SR_B3_red", "BRF_b3_bs", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                             "LANDSAT red", "PARAS red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                             xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                             text.x = text.xs$red, text.y = text.ys$red,
                             shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        nir = my_ggplot_1to1(paras.res, "SR_B4_nir", "BRF_b4_bs", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                             "LANDSAT nir", "PARAS nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                             xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                             text.x = text.xs$nir, text.y = text.ys$nir,
                             shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        swir1 = my_ggplot_1to1(paras.res, "SR_B5_swir1", "BRF_b5_bs", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                               "LANDSAT swir1", "PARAS swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                               xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                               text.x = text.xs$swir1, text.y = text.ys$swir1,
                               shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        swir2 = my_ggplot_1to1(paras.res, "SR_B7_swir2", "BRF_b7_bs", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                               "LANDSAT swir2", "PARAS swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                               xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                               text.x = text.xs$swir2, text.y = text.ys$swir2,
                               shape.col = c(2:4,6,8,9), legend.pos = "right")
      )
      
    } else {
      
      PARAS_Landsat_byType <- list(
        green = my_ggplot_1to1(paras.res, "SR_B2_green", "BRF_b2_s", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                               "LANDSAT green", "PARAS green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                               xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                               text.x = text.xs$green, text.y = text.ys$green,
                               shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        red = my_ggplot_1to1(paras.res, "SR_B3_red", "BRF_b3_s", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                             "LANDSAT red", "PARAS red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                             xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                             text.x = text.xs$red, text.y = text.ys$red,
                             shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        nir = my_ggplot_1to1(paras.res, "SR_B4_nir", "BRF_b4_s", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                             "LANDSAT nir", "PARAS nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                             xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                             text.x = text.xs$nir, text.y = text.ys$nir,
                             shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        swir1 = my_ggplot_1to1(paras.res, "SR_B5_swir1", "BRF_b5_s", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                               "LANDSAT swir1", "PARAS swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                               xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                               text.x = text.xs$swir1, text.y = text.ys$swir1,
                               shape.col = c(2:4,6,8,9), legend.pos = "right"),
        
        swir2 = my_ggplot_1to1(paras.res, "SR_B7_swir2", "BRF_b7_s", "Type", ngroup = 5, pts = 2, # my_ggplot_lm function above
                               "LANDSAT swir2", "PARAS swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                               xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                               text.x = text.xs$swir2, text.y = text.ys$swir2,
                               shape.col = c(2:4,6,8,9), legend.pos = "right")
      )
    }
    

    
  # For now print the plots (bands) separately, later ask Aarne how to print multiple plots
  pdf(paste(c(out.dir, "/L7vsPARAS_green.pdf"), collapse = ""))                             # green
  print(PARAS_Landsat_byType$green + ggtitle(title))
  dev.off()
  
  pdf(paste(c(out.dir, "/L7vsPARAS_red.pdf"), collapse = ""))                                 # red
  print(PARAS_Landsat_byType$red + ggtitle(title))
  dev.off()
  
  pdf(paste(c(out.dir, "/L7vsPARAS_nir.pdf"), collapse = ""))                                 # nir
  print(PARAS_Landsat_byType$nir + ggtitle(title))
  dev.off()
  
  pdf(paste(c(out.dir, "/L7vsPARAS_swir1.pdf"), collapse = ""))                              # swir1
  print(PARAS_Landsat_byType$swir1 + ggtitle(title))
  dev.off()
  
  pdf(paste(c(out.dir, "/L7vsPARAS_swir2.pdf"), collapse = ""))                              # swir2
  print(PARAS_Landsat_byType$swir2 + ggtitle(title))
  dev.off()

  return(1)
}






# No 1:1 abline, with spline line --------------------------------------------------
require(mgcv)
my_ggplot_spl <- function(data, xvar, yvar, shp.group, col.group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                         yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                         shape.col, legend.pos, label = c("Landsat", "PARAS")){
  # r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar, color = col.group)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
    stat_smooth(method = "gam", formula = y ~ s(x, k=3), se=FALSE, size=0.5, linetype=2) +
    geom_point(aes_string(shape = shp.group, col = col.group), cex=pts) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12) + # base_family
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=1:ngroup) + 
    scale_colour_manual(name="", values=shape.col, label = label)
  
  return(plot)
}

my_ggplot_spl_nolabel <- function(data, xvar, yvar, shp.group, col.group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                          yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                          shape.col, legend.pos){
  # r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar, color = col.group)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
    stat_smooth(method = "gam", formula = y ~ s(x, k=3), se=FALSE, size=0.5, linetype=2) +
    geom_point(aes_string(shape = shp.group, col = col.group), cex=pts) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12) + # base_family
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=1:ngroup) + 
    scale_colour_manual(name="", values=shape.col)
  
  return(plot)
}



# Plot PARAS & Landsat vs LAIt / ECC, save pdf -----------------------------------------
myplot_PARASvsLandsat_forvar <- function(paras.res, forest.var, 
      xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
      yaxis.lims = list(green=c(0,0.6), red=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.6)),
      xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
      yaxis.breaks = list(green=seq(0,0.6,0.1), red=seq(0,0.6,0.1), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.6,0.1)),
      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
      out.dir, title, xlab, black.soil) {
  
  if (black.soil) {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_bs, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                      xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                      text.x = text.xs$green, text.y = text.ys$green,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_bs, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                      xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                      text.x = text.xs$red, text.y = text.ys$red,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_bs, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                      xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                      text.x = text.xs$nir, text.y = text.ys$nir,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_bs, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                      xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                      text.x = text.xs$swir1, text.y = text.ys$swir1,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_bs, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, # my_ggplot_lm function above
                      xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                      xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                      text.x = text.xs$swir2, text.y = text.ys$swir2,
                      shape.col = c("magenta", "blue"), legend.pos = "right")
    )
    
  } else {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_s, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                      xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                      text.x = text.xs$green, text.y = text.ys$green,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_s, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                      xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                      text.x = text.xs$red, text.y = text.ys$red,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_s, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                      xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                      text.x = text.xs$nir, text.y = text.ys$nir,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_s, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                      xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                      xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                      text.x = text.xs$swir1, text.y = text.ys$swir1,
                      shape.col = c("magenta", "blue"), legend.pos = "right"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_s, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, # my_ggplot_lm function above
                      xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                      xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                      text.x = text.xs$swir2, text.y = text.ys$swir2,
                      shape.col = c("magenta", "blue"), legend.pos = "right")
    )
    
  }

  # For now print the plots (bands) separately, later ask Aarne how to print multiple plots
  
  if (!is.na(str_match(forest.var, "LAIt")[1,1])) {                            # In the pdf name needs to mention LAIt or ECC: if LAIt
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  } else {                                                                    # If ECC
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  }
  
  return(1)
    
}










# No 1:1 abline, with spline line and error bars  -------------------------------------------------------------------
my_ggplot_spl_errbar <- function(data, xvar, yvar, shp.group, col.group, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                          yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                          shape.col, legend.pos, yminvar, ymaxvar, dataerr, xerr, yerr, yminerr, ymaxerr){
  # r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar, color = col.group)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) +       # Expand
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
    geom_errorbar(data = dataerr, aes_string(x = xerr, y = yerr, ymin = yminerr, ymax = ymaxerr), width = 0.05, col = shape.col[2], size = 0.2) +                              # Add error bar
    
    stat_smooth(method = "gam", formula = y ~ s(x, k=3), se=FALSE, size=0.5, linetype=2) +
    geom_point(aes_string(shape = shp.group, col = col.group), cex=pts) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12) + # base_family
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=c(0,1,2,5,6)) + 
    scale_colour_manual(name="", values=shape.col, label = c("Landsat", "PARAS"))
  
  return(plot)
}

# No 1:1 abline, with spline line and 2 error bars  -------------------------------------------------------------------
my_ggplot_spl_2errbar <- function(data, xvar, yvar, shp.group, col.group, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                                 yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                                 shape.col, legend.pos, yminvar, ymaxvar, dataerr, xerr, yerr, yminerr, ymaxerr,
                                 yerr2, yminerr2, ymaxerr2){
  # r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar, color = col.group)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) +       # Expand
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
    geom_errorbar(data = dataerr, aes_string(x = xerr, y = yerr, ymin = yminerr, ymax = ymaxerr), width = 0.05, col = shape.col[2], size = 0.2) +  
    geom_errorbar(data = dataerr, aes_string(x = xerr, y = yerr2, ymin = yminerr2, ymax = ymaxerr2), width = 0.05, col = shape.col[1], size = 0.2) +                              # Add error bar
    # Add error bar
    
    stat_smooth(method = "gam", formula = y ~ s(x, k=3), se=FALSE, size=0.5, linetype=2) +
    geom_point(aes_string(shape = shp.group, col = col.group), cex=pts) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 12) + # base_family
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=c(0,1,2,5,6)) + 
    scale_colour_manual(name="", values=shape.col, label = c("Landsat", "PARAS"))
  
  return(plot)
}


# Plot PARAS & Landsat vs LAIt / ECC, save pdf, with error bar = sd ------------
myplot_PARASvsLandsat_forvar_errbar <- function(paras.res, forest.var, 
                                         xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                                         yaxis.lims = list(green=c(-0.05,0.1), red=c(-0.05,0.1), nir=c(-0.05,0.55), swir1=c(-0.05,0.3), swir2=c(-0.05,0.3)),
                                         xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                                         yaxis.breaks = list(green=seq(0,0.1,0.05), red=seq(0,0.1,0.05), nir=seq(0,0.55,0.1), swir1=seq(0,0.3,0.05), swir2=seq(0,0.3,0.05)),
                                         text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                         text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                         out.dir, title, xlab, black.soil) {
  
  if (black.soil) {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                      xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                      text.x = text.xs$green, text.y = text.ys$green,
                      shape.col = c("magenta", "blue"), legend.pos = "right", 
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_bs.mean", yminerr = "BRF_b2_bs.meanminsd", ymaxerr = "BRF_b2_bs.meanplussd"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                      xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                      text.x = text.xs$red, text.y = text.ys$red,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_bs.mean", yminerr = "BRF_b3_bs.meanminsd", ymaxerr = "BRF_b3_bs.meanplussd"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                      xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                      text.x = text.xs$nir, text.y = text.ys$nir,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_bs.mean", yminerr = "BRF_b4_bs.meanminsd", ymaxerr = "BRF_b4_bs.meanplussd"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                      xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                      text.x = text.xs$swir1, text.y = text.ys$swir1,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_bs.mean", yminerr = "BRF_b5_bs.meanminsd", ymaxerr = "BRF_b5_bs.meanplussd"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                      xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                      text.x = text.xs$swir2, text.y = text.ys$swir2,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b7_bs.mean", yminerr = "BRF_b7_bs.meanminsd", ymaxerr = "BRF_b7_bs.meanplussd")
    )
    
  } else {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                      xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                      text.x = text.xs$green, text.y = text.ys$green,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_s.mean", yminerr = "BRF_b2_s.meanminsd", ymaxerr = "BRF_b2_s.meanplussd"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                      xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                      text.x = text.xs$red, text.y = text.ys$red,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_s.mean", yminerr = "BRF_b3_s.meanminsd", ymaxerr = "BRF_b3_s.meanplussd"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                      xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                      text.x = text.xs$nir, text.y = text.ys$nir,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_s.mean", yminerr = "BRF_b4_s.meanminsd", ymaxerr = "BRF_b4_s.meanplussd"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                      xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                      xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                      text.x = text.xs$swir1, text.y = text.ys$swir1,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_s.mean", yminerr = "BRF_b5_s.meanminsd", ymaxerr = "BRF_b5_s.meanplussd"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, # my_ggplot_lm function above
                      xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                      xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                      text.x = text.xs$swir2, text.y = text.ys$swir2,
                      shape.col = c("magenta", "blue"), legend.pos = "right",
                      dataerr = paras.res, xerr = forest.var, yerr = "BRF_b7_s.mean", yminerr = "BRF_b7_s.meanminsd", ymaxerr = "BRF_b7_s.meanplussd")
    )
    
  }
  
  # For now print the plots (bands) separately, later ask Aarne how to print multiple plots
  
  if (!is.na(str_match(forest.var, "LAIt")[1,1])) {                            # In the pdf name needs to mention LAIt or ECC: if LAIt
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  } else {                                                                    # If ECC
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  }
  
  return(1)
  
}





# Plot PARAS & Landsat vs LAIt / ECC, save pdf, with 2 error bar = sd ------------
myplot_PARASvsLandsat_forvar_2errbar <- function(paras.res, forest.var, 
                                                xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                                                yaxis.lims = list(green=c(-0.05,0.1), red=c(-0.05,0.1), nir=c(-0.05,0.55), swir1=c(-0.05,0.3), swir2=c(-0.05,0.3)),
                                                xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                                                yaxis.breaks = list(green=seq(0,0.1,0.05), red=seq(0,0.1,0.05), nir=seq(0,0.55,0.1), swir1=seq(0,0.3,0.05), swir2=seq(0,0.3,0.05)),
                                                text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                                text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                                out.dir, title, xlab, black.soil) {
  
  if (black.soil) {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                             xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                             text.x = text.xs$green, text.y = text.ys$green,
                             shape.col = c("magenta", "blue"), legend.pos = "right", 
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_bs.mean", yminerr = "BRF_b2_bs.meanminsd", ymaxerr = "BRF_b2_bs.meanplussd",
                             yerr2 = "SR_B2_green", yminerr2 = "SR_B2_green.minerr", ymaxerr2 = "SR_B2_green.pluserr"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                             xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                             text.x = text.xs$red, text.y = text.ys$red,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_bs.mean", yminerr = "BRF_b3_bs.meanminsd", ymaxerr = "BRF_b3_bs.meanplussd",
                             yerr2 = "SR_B3_red", yminerr2 = "SR_B3_red.minerr", ymaxerr2 = "SR_B3_red.pluserr"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                             xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                             text.x = text.xs$nir, text.y = text.ys$nir,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_bs.mean", yminerr = "BRF_b4_bs.meanminsd", ymaxerr = "BRF_b4_bs.meanplussd",
                             yerr2 = "SR_B4_nir", yminerr2 = "SR_B4_nir.minerr", ymaxerr2 = "SR_B4_nir.pluserr"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                             xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                             text.x = text.xs$swir1, text.y = text.ys$swir1,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_bs.mean", yminerr = "BRF_b5_bs.meanminsd", ymaxerr = "BRF_b5_bs.meanplussd",
                             yerr2 = "SR_B5_swir1", yminerr2 = "SR_B5_swir1.minerr", ymaxerr2 = "SR_B5_swir1.pluserr"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                             xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                             text.x = text.xs$swir2, text.y = text.ys$swir2,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b7_bs.mean", yminerr = "BRF_b7_bs.meanminsd", ymaxerr = "BRF_b7_bs.meanplussd",
                             yerr2 = "SR_B7_swir2", yminerr2 = "SR_B7_swir2.minerr", ymaxerr2 = "SR_B7_swir2.pluserr")
    )
    
  } else {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                             xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                             text.x = text.xs$green, text.y = text.ys$green,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_s.mean", yminerr = "BRF_b2_s.meanminsd", ymaxerr = "BRF_b2_s.meanplussd",
                             yerr2 = "SR_B2_green", yminerr2 = "SR_B2_green.minerr", ymaxerr2 = "SR_B2_green.pluserr"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                             xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                             text.x = text.xs$red, text.y = text.ys$red,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_s.mean", yminerr = "BRF_b3_s.meanminsd", ymaxerr = "BRF_b3_s.meanplussd",
                             yerr2 = "SR_B3_red", yminerr2 = "SR_B3_red.minerr", ymaxerr2 = "SR_B3_red.pluserr"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                             xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                             text.x = text.xs$nir, text.y = text.ys$nir,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_s.mean", yminerr = "BRF_b4_s.meanminsd", ymaxerr = "BRF_b4_s.meanplussd",
                             yerr2 = "SR_B4_nir", yminerr2 = "SR_B4_nir.minerr", ymaxerr2 = "SR_B4_nir.pluserr"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                             xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                             text.x = text.xs$swir1, text.y = text.ys$swir1,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_s.mean", yminerr = "BRF_b5_s.meanminsd", ymaxerr = "BRF_b5_s.meanplussd",
                             yerr2 = "SR_B5_swir1", yminerr2 = "SR_B5_swir1.minerr", ymaxerr2 = "SR_B5_swir1.pluserr"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_2errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, # my_ggplot_lm function above
                             xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                             xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                             text.x = text.xs$swir2, text.y = text.ys$swir2,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b7_s.mean", yminerr = "BRF_b7_s.meanminsd", ymaxerr = "BRF_b7_s.meanplussd",
                             yerr2 = "SR_B7_swir2", yminerr2 = "SR_B7_swir2.minerr", ymaxerr2 = "SR_B7_swir2.pluserr")
    )
    
  }
  
  # For now print the plots (bands) separately, later ask Aarne how to print multiple plots
  
  if (!is.na(str_match(forest.var, "LAIt")[1,1])) {                            # In the pdf name needs to mention LAIt or ECC: if LAIt
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  } else {                                                                    # If ECC
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  }
  
  return(1)
  
}
# Plot PARAS & Landsat vs LAIt / ECC, save pdf, with error bar = minmax --------

myplot_PARASvsLandsat_forvar_errbar_minmax <- function(paras.res, forest.var, 
                                                xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                                                yaxis.lims = list(green=c(0,0.6), red=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.6)),
                                                xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                                                yaxis.breaks = list(green=seq(0,0.6,0.1), red=seq(0,0.6,0.1), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.6,0.1)),
                                                text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                                text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                                out.dir, title, xlab, black.soil) {
  
  if (black.soil) {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                             xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                             text.x = text.xs$green, text.y = text.ys$green,
                             shape.col = c("magenta", "blue"), legend.pos = "right", 
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_bs.mean", yminerr = "BRF_b2_bs.min", ymaxerr = "BRF_b2_bs.max"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                             xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                             text.x = text.xs$red, text.y = text.ys$red,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_bs.mean", yminerr = "BRF_b3_bs.min", ymaxerr = "BRF_b3_bs.max"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                             xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                             text.x = text.xs$nir, text.y = text.ys$nir,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_bs.mean", yminerr = "BRF_b4_bs.min", ymaxerr = "BRF_b4_bs.max"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                             xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                             text.x = text.xs$swir1, text.y = text.ys$swir1,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_bs.mean", yminerr = "BRF_b5_bs.min", ymaxerr = "BRF_b5_bs.max"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_bs.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                             xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                             text.x = text.xs$swir2, text.y = text.ys$swir2,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b7_bs.mean", yminerr = "BRF_b7_bs.min", ymaxerr = "BRF_b7_bs.max")
    )
    
  } else {
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                             xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                             text.x = text.xs$green, text.y = text.ys$green,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_s.mean", yminerr = "BRF_b2_s.min", ymaxerr = "BRF_b2_s.max"),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                             xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                             text.x = text.xs$red, text.y = text.ys$red,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_s.mean", yminerr = "BRF_b3_s.min", ymaxerr = "BRF_b3_s.max"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                             xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                             text.x = text.xs$nir, text.y = text.ys$nir,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_s.mean", yminerr = "BRF_b4_s.min", ymaxerr = "BRF_b4_s.max"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, 
                             xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                             xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                             text.x = text.xs$swir1, text.y = text.ys$swir1,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_s.mean", yminerr = "BRF_b5_s.min", ymaxerr = "BRF_b5_s.max"),
      
      swir2 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B7_swir2, BRF_b7_s.mean, Type) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type")) %>% collect %>%  
        my_ggplot_spl_errbar(., xvar = forest.var, yvar = "value", shp.group = "Type", col.group = "variable", pts = 2, # my_ggplot_lm function above
                             xlab, "BRF swir2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                             xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                             text.x = text.xs$swir2, text.y = text.ys$swir2,
                             shape.col = c("magenta", "blue"), legend.pos = "right",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b7_s.mean", yminerr = "BRF_b7_s.min", ymaxerr = "BRF_b7_s.max")
    )
    
  }
  
  # For now print the plots (bands) separately, later ask Aarne how to print multiple plots
  
  if (!is.na(str_match(forest.var, "LAIt")[1,1])) {                            # In the pdf name needs to mention LAIt or ECC: if LAIt
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_LAIt_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  } else {                                                                    # If ECC
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_green.pdf"), collapse = ""))                             # green
    print(plots.list$green + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_red.pdf"), collapse = ""))                                 # red
    print(plots.list$red + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_nir.pdf"), collapse = ""))                                 # nir
    print(plots.list$nir + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir1.pdf"), collapse = ""))                              # swir1
    print(plots.list$swir1 + ggtitle(title))
    dev.off()
    
    pdf(paste(c(out.dir, "/L7vsPARAS_ECC_swir2.pdf"), collapse = ""))                              # swir2
    print(plots.list$swir2 + ggtitle(title))
    dev.off()
    
  }
  
  return(1)
  
}

# No grouping, with error bar ---------------------------------------------

my_ggplot_justerrbar <- function(data, xvar, yvar, xaxis.lab, yaxis.lab, xaxis.lim, 
                                 yaxis.lim, xaxis.break, yaxis.break, 
                                 legend.pos, dataerr, xerr, yerr, yminerr, ymaxerr){

  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + geom_point() +
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, expand=c(0,0)) +       # Expand
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break, expand=c(0,0)) + 
        geom_errorbar(data = dataerr, aes_string(x = xerr, y = yerr, ymin = yminerr, ymax = ymaxerr), width = 0.05, size = 0.2) +                              # Add error bar
        theme_bw(base_size = 12) + 
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=14,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + guides(colour = guide_legend(override.aes = list(size=1.5)))
    return(plot)
}

