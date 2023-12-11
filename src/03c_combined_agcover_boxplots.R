# this is figure 3 in the Glinski manuscript as of 12/11/2023

boxplots_ndetects
boxplots_concs_sum

#combined agcover boxplots
agcover_boxplots_combined <- ggarrange(boxplots_ndetects, boxplots_concs_sum, 
                                       ncol = 2, 
                                       labels = c("A", "B"),
                                       widths=c(0.415,0.585)
                                       )

agcover_boxplots_combined

# this is figure 3 in the Glinski manuscript as of 12/11/2023
gbm_agcover_boxplots_combined_filename <- paste(gbm_graphics,"/gbm_agcover_boxplots_combined.jpg",sep="")
jpeg(gbm_agcover_boxplots_combined_filename, width = 5, height = 8, units = "in",res=600)
  agcover_boxplots_combined
dev.off()
