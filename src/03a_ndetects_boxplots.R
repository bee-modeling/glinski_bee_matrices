# convert data set to nondetect/detect binary
#View(gbm_data)
unique(gbm_data$Date)

#assume that lowest concentration observed is the detection limit (per DAG)
# anything greater than the lowest concentration is a detect
colnames(gbm_data)
cols_to_convert <- 6:34 # not 7:35 because of group command
gbm_binary_withNAs <- gbm_data %>%
  group_by(Media) %>%
  mutate(across(all_of(cols_to_convert), ~ifelse(. == min(., na.rm=T), 0, 1))) %>%
  ungroup()
  
#View(gbm_binary_withNAs)
  
# Convert NA values to 0s using mutate()
gbm_binary <- gbm_binary_withNAs %>% mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
# fix.restore the factors
gbm_binary[,1:6] <- gbm_data[,1:6]
#View(gbm_binary)

#now sum the number of detects in each sample in each media
gbm_binary_sum <- gbm_binary %>%
  #group_by(group) %>%
  mutate(n_detects = rowSums(.[7:35]))

#View(gbm_binary_sum)

# frequency polygon
ggplot(gbm_binary_sum, aes(x = n_detects, color = Media, fill = Media)) +
  geom_freqpoly(binwidth = 1, alpha = 0.5) +
  scale_color_discrete(name = "Group") +
  scale_fill_discrete(name = "Group") +
  xlab("# Detects") +
  ylab("Frequency") +
  ggtitle("Frequency polygon of number of detects in each media by sample") +
  theme_classic()

# relative frequency polygon
ggplot(gbm_binary_sum, aes(x = n_detects, color = Media, fill = Media)) +
  geom_freqpoly(aes(y = ..density..), binwidth = 1, alpha = 0.5) +
  scale_color_discrete(name = "Group") +
  scale_fill_discrete(name = "Group") +
  xlab("# Detects") +
  ylab("Frequency") +
  ggtitle("Relative frequency polygon of number of detects in each media by sample") +
  theme_classic()

# Create cumulative relative frequency polygon plot
ggplot(gbm_binary_sum, aes(x = n_detects, color = Media, fill = Media)) +
  stat_ecdf(aes(y = ..y.., group = Media), geom = "step") +
  scale_color_discrete(name = "Group") +
  scale_fill_discrete(name = "Group") +
  xlab("Value") +
  ylab("Cumulative relative frequency") +
  ggtitle("Cumulative relative frequency polygon plot by group") +
  theme_classic()

# stacked histogram

# kernel density plots
gbm_binary_sum$det_prop <- gbm_binary_sum$n_detects/29
colnames(gbm_binary_sum)
# Create the kernel density plot
ggplot(gbm_binary_sum, aes(x = det_prop, fill = Media)) + 
  geom_density(alpha = 0.5) + # set transparency to see overlapping densities
  facet_wrap(~Media) +# group by factor and wrap plots in a grid
  theme_classic()

ggplot(gbm_binary_sum, aes(x = det_prop, fill = Media)) + 
  geom_density(alpha = 0.5) + # set transparency to see overlapping densities
  facet_wrap(~ag_cover, ncol=1) +# group by factor and wrap plots in a grid
  theme_classic()

ggplot(gbm_binary_sum, aes(x = det_prop, fill = ag_cover)) + 
  geom_density(alpha = 0.5) + # set transparency to see overlapping densities
  facet_wrap(~Media) +# group by factor and wrap plots in a grid
  theme_classic()

ggplot(gbm_binary_sum, aes(x = det_prop, fill = Media)) + 
  geom_density(alpha = 0.5) +
  theme_classic()

# reorder the ac_cover order so low is first
levels(gbm_binary_sum$ag_cover)
gbm_binary_sum$ag_cover <- factor(gbm_binary_sum$ag_cover, levels=c("low", "high"))

# reorder Media factor to match text
unique(gbm_binary_sum$Media)
levels(gbm_binary_sum$Media) <- c("DBT (p=0.384)", "FP (p=0.289)", "IHBB (p=0.053)", "IHH (p=0.39)", "IHL (p=0.71)", "IHNB (p=0.193)")
new_media_types <- c("FP (p=0.289)", "DBT (p=0.384)", "IHNB (p=0.193)", "IHL (p=0.71)", "IHBB (p=0.053)", "IHH (p=0.39)")
#new_media_types <- c("FP", "DBT", "IHNB", "IHL", "IHBB", "IHH")
gbm_binary_sum$Media <- factor(gbm_binary_sum$Media, levels = new_media_types)
levels(gbm_binary_sum$Media)
#View(gbm_binary_sum)

#this is the one for the manuscript
dim(gbm_binary_sum)
colnames(gbm_binary_sum)
boxplots_ndetects <- ggplot(gbm_binary_sum, aes(x = n_detects, fill = ag_cover)) + 
  geom_boxplot(position="dodge") +
  facet_wrap(~Media, ncol=1) +# group by factor and wrap plots in a grid
  labs(x = "# of detects") + # , color = "Media"
  coord_flip() +
  theme_classic_custom() + # defined in a different scrip--05b
  theme(legend.position="none")
boxplots_ndetects

gbm_ndetects_agcover_filename <- paste(gbm_graphics,"/gbm_ndetects_agcover.jpg",sep="")
jpeg(gbm_ndetects_agcover_filename, width = 4, height = 8, units = "in",res=600)
  boxplots_ndetects
dev.off()


#figure by date
unique(gbm_binary_sum$Date)
gbm_binary_sum$n_detects
#summarize n_detects by Meda and Date

gbm_ndet_means <- gbm_binary_sum %>% 
  group_by(Media, Date) %>% 
  summarize(mean_ndets = mean(n_detects))
gbm_ndet_means

# Create the scatterplot
ggplot(gbm_ndet_means, aes(x = Date, y = mean_ndets, color = Media)) +
  geom_point() + # add points
  theme_classic()
  #scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + # format x-axis as dates
  #facet_wrap(~Media, ncol = 3) # group by factor and wrap plots in a grid

