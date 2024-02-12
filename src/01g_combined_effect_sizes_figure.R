#combined figure jpg

agcover_grob = grid.grabExpr(draw(agcover_heatmap)) 
date_compare_grob = grid.grabExpr(draw(date_compare_heatmap))

effect_sizes_combined <- ggarrange(agcover_grob, date_compare_grob, heights = c(4, 4), widths=c(3.78,3.22),
                                labels = c("A", "B"),
                                ncol = 2, nrow = 1)
effect_sizes_combined

#write to png 
png(file.path(gbm_graphics, "heatmap_agcover_date_effect_sizes.png"),
    width=7, height=5, units="in", res=300  )
      ggarrange(agcover_grob, date_compare_grob, heights = c(4, 4), widths=c(3.78,3.22),
                labels = c("A", "B"),
                ncol = 2, nrow = 1)
dev.off()
