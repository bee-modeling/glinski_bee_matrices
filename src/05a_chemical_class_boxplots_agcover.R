#### all data together
# proxy values for nondetects?
dim(gbm_data) # [1] 306  35
colnames(gbm_data)
gbm_mean_concs_agcover <- gbm_data %>%
  group_by(Media, ag_cover) %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), 
                   mean,
                   na.rm = TRUE))
colnames(gbm_mean_concs_agcover)
dim(gbm_mean_concs_agcover) # [1] 12 33
#View(gbm_mean_concs_agcover)

# drop date and weight
gbm_mean_concs_agcover2 <- gbm_mean_concs_agcover[,-c(3:4)]
colnames(gbm_mean_concs_agcover2)

#media
media_names <- gbm_mean_concs_agcover2$Media
media_names
ag_cover <- gbm_mean_concs_agcover2$ag_cover
ag_cover

# extract chemical concentrations for high ag cover
colnames(gbm_mean_concs_agcover2)

# gather to long
gbm_mean_concs_agcover_long <- gbm_mean_concs_agcover2 %>%
  pivot_longer(cols=Alachlor:Thiacloprid, 
               names_to="chemical", values_to="value")
dim(gbm_mean_concs_agcover_long)

# merge to get pest_type
gbm_boxplot_data_agcover <- merge(gbm_mean_concs_agcover_long, pesticide_key, by='chemical', all=T)
colnames(gbm_boxplot_data_agcover)
min_proxy <- min(gbm_boxplot_data_agcover$value, na.rm=T)

# proxies for NAs
gbm_boxplot_data_agcover <- replace(gbm_boxplot_data_agcover, is.na(gbm_boxplot_data_agcover), min_proxy)

# create boxplots on ag cover and media types
boxplots_chem_classes_agcover <- ggplot(gbm_boxplot_data_agcover, aes(x = log(value), y=ag_cover, fill = Media)) + 
  geom_boxplot(position="dodge") +
  labs(y = "Media Type", x = "log(Mean Concentration (ng/g))", color = "Ag cover") +
  facet_wrap(~pest_type, ncol=1) +# group by factor and wrap plots in a grid
  coord_flip() +
  theme_classic() 
boxplots_chem_classes_agcover

# save to graphics image
gbm_boxplots_chem_classes_agcover_filename <- paste(gbm_graphics,"/gbm_boxplots_chem_classes_agcover.jpg",sep="")
jpeg(gbm_boxplots_chem_classes_agcover_filename, width = 6, height = 7, units = "in",res=600)
  boxplots_chem_classes_agcover
dev.off()
