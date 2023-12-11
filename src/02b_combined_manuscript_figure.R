## Glinski manuscript figure 5 on 12/13/2023

gg_gbm_heatmap_all_ag
gg_gbm_heatmap_detfreqall_ag

#combined gsh figure jpg
concs_dets_combined <- ggarrange(gbm_summary_plot_matrices,                                                 # First row with scatter plot
          ggarrange(gg_gbm_heatmap_all_ag, gg_gbm_heatmap_detfreqall_ag, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
          nrow = 2, 
          heights=c(2,3),
          labels = "A"                                        # Labels of the scatter plot
) 
concs_dets_combined

## Glinski manuscript figure 5 on 12/13/2023
gbm_concs_detects_filename <- paste(gbm_graphics,"/gbm_combined_concs_detects.jpg",sep="")
jpeg(gbm_concs_detects_filename, width = 8.5, height = 11, units = "in",res=600)
  concs_dets_combined
dev.off()
