# creating boxplots of chemical class mean concentrations by media

#View(gbm_heatmap_data_all)
dim(gbm_heatmap_data_all)
colnames(gbm_heatmap_data_all)

#View(pesticide_key)
dim(pesticide_key)
colnames(pesticide_key)

#rename data set for this figure and explicitly add a chemical field
gbm_boxplot_data <- gbm_heatmap_data_all
gbm_boxplot_data$chemical <- rownames(gbm_heatmap_data_all)

# merge so we have access to the chemical classes
gbm_boxplot_data_class <- merge(gbm_boxplot_data, pesticide_key, by='chemical', all=T)
colnames(gbm_boxplot_data_class)

# make it long to add media field
gbm_boxplot_data_long <-gbm_boxplot_data_class %>%
  gather(key="Media", value="value", DBT:IHNB)

# reorder Media factor to match text
unique(gbm_boxplot_data_long$Media)
levels(gbm_boxplot_data_long$Media)
new_media_types <- c("FP", "DBT", "IHNB", "IHL", "IHBB", "IHH")
gbm_boxplot_data_long$Media <- factor(gbm_boxplot_data_long$Media, levels = new_media_types)
levels(gbm_boxplot_data_long$Media)


colnames(gbm_boxplot_data_long)
boxplots_chem_classes <- ggplot(gbm_boxplot_data_long, aes(x = log(value), fill = Media)) + 
  geom_boxplot(position="dodge") +
  labs(y = "Media Type", x = "log(Mean Concentration (ng/g))", color = "Media") +
  facet_wrap(~pest_type, ncol=1) +# group by factor and wrap plots in a grid
  coord_flip() +
  theme_classic_custom() 
boxplots_chem_classes

### this is figure 4 of the Glinski manuscript as of 12/11/2023
gbm_boxplots_chem_classes_filename <- paste(gbm_graphics,"/gbm_boxplots_chem_classes.jpg",sep="")
jpeg(gbm_boxplots_chem_classes_filename, width = 4, height = 7, units = "in",res=600)
  boxplots_chem_classes
dev.off()
