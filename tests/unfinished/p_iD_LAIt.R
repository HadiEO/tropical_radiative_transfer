sim.LAIt = seq(0.5,5,by=0.5)
sim.iD = seq(0.05,0.95,by=0.1) 
z <- expand.grid(sim.LAIt, sim.iD)
sim <- tibble(sim.LAIt = z$Var1, sim.iD = z$Var2) %>%  
  mutate(sim.p = 1 - (sim.iD / sim.LAIt))

# sim %>% dplyr::filter(sim.p > 0) %>%  dplyr::filter(sim.p < 1) %>% collect %>% 
#   levelplot(sim.p ~ sim.LAIt*sim.iD, grid = ., xlab="LAIt",
#             ylab="iD", colorkey = TRUE, region = TRUE)



sim.p_aggr <- sim %>% dplyr::filter(sim.p > 0) %>%  dplyr::filter(sim.p < 1) %>% collect %>% 
  aggregate(sim.p~sim.LAIt+sim.iD, ., 'mean')


# library(RColorBrewer)
ggplot(sim.p_aggr) + 
    geom_tile(aes(x=sim.LAIt, y=sim.iD, fill=sim.p))+
    # scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkred", limits=c(0,1)) + 
    scale_fill_manual(values=diverge_hsv(8)) +
    theme_bw() + theme(aspect.ratio=1)

