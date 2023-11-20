# create a dataset
ag_cover <- c(rep("soybean_corn" , 10) , rep("other_crop" , 10) , rep("green_space" , 10) , rep("developed" , 10) )

location <- rep(c("HR","BR","FSR","MO", "WB","TV","SC","IB","MB","DS"), 4)
value <-        c(0.80,0.770,0.74 ,0.71, 0.70,0.68,0.49, 0.46,0.42,0.010,
                  0.06,0.075,0.03 ,0.07, 0.11,0.12,0.14, 0.11,0.10,0.005,
                  0.09,0.095,0.21 ,0.18, 0.14,0.13,0.18, 0.35,0.32,0.165,
                  0.05,0.060,0.02 ,0.04, 0.05,0.07,0.19, 0.08,0.16,0.820)
ag_cover_tall <- data.frame(ag_cover, location, value)

#View(ag_cover_tall)
location <- rep(c("HR","BR","FSR","MO","WB","TV","SC","IB","MB","DS"), 4)

# sort levels for display
lvls <- names(sort(table(ag_cover_tall[ag_cover_tall$ag_cover == "soybean_corn", "location"])))
ag_cover_tall$location <- factor(ag_cover_tall$location, levels=c("HR","BR","FSR","MO","WB","TV","SC","IB","MB","DS"))

# Stacked + percent
land_cover_stacked <- ggplot(ag_cover_tall, aes(factor(location, levels = lvls), fill=ag_cover, y=value, x=location)) + 
  geom_bar(position="fill", stat="identity") +
  ylab("Land Cover Proportion") +
  xlab("Location") +
  guides(fill=guide_legend(title="Land Cover")) +
  theme_classic()

land_cover_stacked

gbm_land_cover_stacked_filename <- paste(gbm_graphics,"/gbm_land_cover_stacked.jpg",sep="")
jpeg(gbm_land_cover_stacked_filename, width = 7, height = 5, units = "in",res=600)
  land_cover_stacked
dev.off()
  