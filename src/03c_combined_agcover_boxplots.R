boxplots_ndetects
boxplots_concs_sum

#combined agcover boxplots
agcover_boxplots_combined <- ggarrange(boxplots_ndetects, boxplots_concs_sum, 
                                       ncol = 2, 
                                       labels = c("A", "B")
                                       )

agcover_boxplots_combined
