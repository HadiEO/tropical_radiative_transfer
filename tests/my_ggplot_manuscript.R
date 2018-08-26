# No 1:1 abline, no reg line --------------------------------------------------
my_ggplot_ms <- function(data, xvar, yvar, group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                      yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                      shape.col, legend.pos, shape.pch, 
                      text){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) + #  expand=c(0,0)
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + #  expand=c(0,0)
    # geom_smooth(method='lm', se=FALSE, col="grey50", size=0.5, linetype=2) +
    geom_point(aes_string(shape = group, col = group), cex=1.3) + # ), cex = 0.6
    geom_text(x = text.x, y = text.y, label = text, cex = 3) + # ), cex = 2    # ADD DURING REVISION
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 8, base_family = "Helvetica") + # base_family
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6; legend.text=element_text(size=8)
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col)
  
  return(plot)
}


# col.group separate from shape.group -------------------------------------
fmt_dcimals <- function(decimals=2){                                                                      # 
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}

my_ggplot_ms_colgroup <- function(data, xvar, yvar, group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                         yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                         shape.col, legend.pos, shape.pch, col.group, col.label = "", legend.just = c(1,1), legend.box = "vertical", 
                         shape.guide = TRUE, col.guide = TRUE){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, labels = fmt_dcimals(2)) + #  expand=c(0,0)
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + #  expand=c(0,0)
    # geom_smooth(method='lm', se=FALSE, col="grey50", size=0.5, linetype=2) +
    geom_point(aes_string(shape = group, col = col.group), cex=1.3) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 8, base_family = "Helvetica") + # base_family
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos, legend.justification=legend.just, legend.box=legend.box) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch, guide=shape.guide) + 
    scale_colour_manual(name="", values=shape.col, labels=col.label, guide=col.guide)
  
  return(plot)
}


# With 1:1 line, no reg line ----------------------------------------------
my_ggplot_1to1_ms <- function(data, xvar, yvar, group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                           yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                           shape.col, legend.pos, shape.pch,
                           text){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  rmse <- round(RMSE(data[,xvar], data[,yvar]), 2)
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + 
    geom_abline(slope=1, intercept=0, size=0.1) + #  ADD 1:1 line
    geom_point(aes_string(shape = group, col = group), cex=1.3) + # ), cex = 0.6
    # geom_smooth(method='lm', se=FALSE, col="black") +
    # geom_text(x = text.x, y = text.y, label = paste("RMSE = ",rmse,sep="")) + # ), cex = 2
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    geom_text(x = text.x, y = text.y, label=text, cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 8, base_family = "Helvetica") +
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=6), legend.title=element_text(size=7), # size = 6; legend.text=element_text(size=8)
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col)  
  return(plot)
}


# Plot PARAS & Landsat vs LAIt / ECC, save pdf -----------------------------------------

myplot_PARASvsLandsat_forvar_ms <- function(paras.res, forest.var, shape.col, shape.pch, xlab,
                                         xaxis.lims = list(green=c(2.5,5), red=c(2.5,5), nir=c(2.5,5), swir1=c(2.5,5)),
                                         yaxis.lims = list(green=c(0,0.07), red=c(0,0.07), nir=c(0,0.55), swir1=c(0,0.25)),
                                         xaxis.breaks = list(green=seq(2.5,5,0.5), red=seq(2.5,5,0.5), nir=seq(2.5,5,0.5), swir1=seq(2.5,5,0.5)), 
                                         yaxis.breaks = list(green=seq(0,0.07,0.01), red=seq(0,0.07,0.01), nir=seq(0,0.55,0.10), swir1=seq(0,0.25,0.05))
                                         ) {
  
  
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_s, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Type.new", col.group = "variable", ngroup = 6, pts = 2, 
                      xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                      xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                      shape.col = shape.col, legend.pos = "none", legend.just = c(0.9,0.7), shape.pch = shape.pch, # legend.pos = c(1,1)
                      col.label = c("Observed (ETM+)", "Modelled (PARAS)"), legend.box = "vertical") +
        guides(col = guide_legend(label.theme = element_text(size=7, angle=0)),
               shape = FALSE), 
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_s, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Type.new", col.group = "variable", ngroup = 6, pts = 2, 
                     xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                     xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                     shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.9,0.8), shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                     col.label = c("Observed (ETM+)", "Modelled (PARAS)"), legend.box = "vertical") + 
        guides(col = FALSE, # col = guide_legend(label.theme = element_text(size=7, angle=0)
               shape = guide_legend(label.theme = element_text(size=7, angle=0))),        
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_s, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Type.new", col.group = "variable", ngroup = 6, pts = 2, 
                     xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                     xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                     shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_s, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Type.new", col.group = "variable", ngroup = 6, pts = 2, 
                     xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                     xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                     shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch)
        )
    
    return(plots.list)
    
  }

  

# Plot PARAS & Landsat vs LAIt / ECC, save pdf, with error bar = minmax --------
my_ggplot_errbar_ms <- function(data, xvar, yvar, shp.group, col.group, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                                 yaxis.lim, xaxis.break, yaxis.break,
                                 shape.col, shape.pch, dataerr, xerr, yerr, yminerr, ymaxerr,
                                legend.pos = "none", legend.just = c(1,1), legend.box = "vertical"){
  # r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar, color = col.group)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) +       # Expand
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + 
    geom_errorbar(data = dataerr, aes_string(x = xerr, y = yerr, ymin = yminerr, ymax = ymaxerr),         # Add error bar
                  width = 0.05, col = shape.col[2], size = 0.2, alpha = 0.7) +                   # alpha to "lighten" the colour?                         

    geom_point(aes_string(shape = shp.group, col = col.group), cex=pts) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 8, base_family = "Helvetica") +
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos, legend.justification=legend.just, legend.box=legend.box) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col, labels = c("Observed (ETM+)", "Modelled (PARAS)"))
  
  return(plot)
}

myplot_PARASvsLandsat_forvar_errbar_minmax_ms <- 
  function(paras.res, forest.var, 
           xaxis.lims = list(green=c(2.5,5), red=c(2.5,5), nir=c(2.5,5), swir1=c(2.5,5)),
           yaxis.lims = list(green=c(0,0.07), red=c(0,0.07), nir=c(0,0.55), swir1=c(0,0.25)),
           xaxis.breaks = list(green=seq(2.5,5,0.5), red=seq(2.5,5,0.5), nir=seq(2.5,5,0.5), swir1=seq(2.5,5,0.5)), 
           yaxis.breaks = list(green=seq(0,0.07,0.01), red=seq(0,0.07,0.01), nir=seq(0,0.55,0.10), swir1=seq(0,0.25,0.05)),
           xlab, shape.pch, shape.col, pts.cex) {
  
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                             xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                             xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                             shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none", legend.just = c(0.9,0.7),   # legend.pos = c(1,1)
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_s.mean", yminerr = "BRF_b2_s.min", ymaxerr = "BRF_b2_s.max") +
                guides(col = guide_legend(label.theme = element_text(size=7, angle=0)), shape = FALSE),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                             xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                             xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                             shape.col = shape.col, shape.pch = shape.pch, legend.pos = c(1,1), legend.just = c(0.9,0.8),
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_s.mean", yminerr = "BRF_b3_s.min", ymaxerr = "BRF_b3_s.max"),
               guides(col = FALSE, shape = guide_legend(label.theme = element_text(size=7, angle=0))),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                             xlab, "BRF nir", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                             xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                             shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_s.mean", yminerr = "BRF_b4_s.min", ymaxerr = "BRF_b4_s.max"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                             xlab, "BRF swir1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                             xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                             shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                             dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_s.mean", yminerr = "BRF_b5_s.min", ymaxerr = "BRF_b5_s.max")
      
       )

}

# Error bar 1std
myplot_PARASvsLandsat_forvar_errbar_1std_ms <- 
  function(paras.res, forest.var, 
           xaxis.lims = list(green=c(2.5,5), red=c(2.5,5), nir=c(2.5,5), swir1=c(2.5,5)),
           yaxis.lims = list(green=c(0,0.07), red=c(0,0.07), nir=c(0,0.55), swir1=c(0,0.25)),
           xaxis.breaks = list(green=seq(2.5,5,0.5), red=seq(2.5,5,0.5), nir=seq(2.5,5,0.5), swir1=seq(2.5,5,0.5)), 
           yaxis.breaks = list(green=seq(0,0.07,0.01), red=seq(0,0.07,0.01), nir=seq(0,0.55,0.10), swir1=seq(0,0.25,0.05)),
           xlab, shape.pch, shape.col, pts.cex) {
    
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), SR_B2_green, BRF_b2_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                            xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none", legend.just = c(0.9,0.7), # legend.pos = c(1,1)
                            dataerr = paras.res, xerr = forest.var, yerr = "BRF_b2_s.mean", yminerr = "BRF_b2_s.meanminsd", ymaxerr = "BRF_b2_s.meanplussd") +
        guides(col = guide_legend(label.theme = element_text(size=7, angle=0)), shape = FALSE),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), SR_B3_red, BRF_b3_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                            xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = c(1,1), legend.just = c(0.9,0.8),
                            dataerr = paras.res, xerr = forest.var, yerr = "BRF_b3_s.mean", yminerr = "BRF_b3_s.meanminsd", ymaxerr = "BRF_b3_s.meanplussd") +
      guides(col = FALSE, shape = guide_legend(label.theme = element_text(size=7, angle=0))),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), SR_B4_nir, BRF_b4_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                            xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                            dataerr = paras.res, xerr = forest.var, yerr = "BRF_b4_s.mean", yminerr = "BRF_b4_s.meanminsd", ymaxerr = "BRF_b4_s.meanplussd"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), SR_B5_swir1, BRF_b5_s.mean, Type.new) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Type.new")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Type.new", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                            xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                            dataerr = paras.res, xerr = forest.var, yerr = "BRF_b5_s.mean", yminerr = "BRF_b5_s.meanminsd", ymaxerr = "BRF_b5_s.meanplussd")
      
    )
    
  }
