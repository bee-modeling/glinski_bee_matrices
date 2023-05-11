# convert data set to nondetect/detect binary
View(gbm_data)
unique(gbm_data$Date)

#assume that lowest concentration observed is the detection limit (per DAG)
# anything greater than the lowest concentration is a detect
colnames(gbm_data)
cols_to_convert <- 6:34 # not 7:35 because of group command
gbm_concs_withNAs <- gbm_data %>%
  group_by(Media) %>%
  mutate(across(all_of(cols_to_convert), ~ifelse(. == min(., na.rm=T), ./(sqrt(2)), .))) %>%
  ungroup()
dim(gbm_concs_withNAs)

# Convert NA values to 0s using mutate()
# should do this by columns and set to min of the column
gbm_concs <- gbm_concs_withNAs %>% mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
# fix.restore the factors
gbm_concs[,1:6] <- gbm_data[,1:6]
View(gbm_concs)

#now sum the number of detects in each sample in each media
gbm_concs_sum <- gbm_concs %>%
  #group_by(group) %>%
  mutate(concs_sum = rowSums(.[7:35]))

View(gbm_concs_sum)

# reorder the ac_cover order so low is first
levels(gbm_concs_sum$ag_cover)
gbm_concs_sum$ag_cover <- factor(gbm_concs_sum$ag_cover, levels=c("low", "high"))

# reorder Media factor to match text
unique(gbm_concs_sum$Media)
levels(gbm_concs_sum$Media)
new_media_types <- c("FP", "DBT", "IHNB", "IHL", "IHBB", "IHH")
gbm_concs_sum$Media <- factor(gbm_concs_sum$Media, levels = new_media_types)
levels(gbm_concs_sum$Media)

# reorder the ac_cover order so low is first
levels(gbm_concs_sum$ag_cover)
gbm_concs_sum$ag_cover <- factor(gbm_concs_sum$ag_cover, levels=c("low", "high"))

#this is the one for the manuscript
dim(gbm_concs_sum)
boxplots_concs_sum <- ggplot(gbm_concs_sum, aes(x = log(concs_sum), fill = ag_cover)) + 
  geom_boxplot(position="dodge") +
  facet_wrap(~Media, ncol=1) +# group by factor and wrap plots in a grid
  labs(x = "log(sum(Mean Concentration (ng/g)))") + #, color = "Media"
  coord_flip() +
  theme_classic()
boxplots_concs_sum

gbm_sumconcs_agcover_filename <- paste(gbm_graphics,"/gbm_sumconcs_agcover.jpg",sep="")
jpeg(gbm_sumconcs_agcover_filename, width = 4, height = 8, units = "in",res=600)
  boxplots_concs_sum
dev.off()

#figure by date
unique(gbm_concs_sum$Date)
gbm_concs_sum$concs_sum
#summarize n_detects by Meda and Date

gbm_concs_means <- gbm_concs_sum %>% 
  group_by(Media, Date) %>% 
  summarize(mean_concs = mean(concs_sum))
gbm_concs_means
hist(gbm_concs_means$Date)

#wrong approach, convert as month-day
gbm_concs_means$Date2 <- paste("0",as.character(gbm_concs_means$Date),sep="")
gbm_concs_means$Date2 <- as.Date(gbm_concs_means$Date2, "%m%d")

max(gbm_concs_means$mean_concs)


# reorder Media factor to match text
unique(gbm_concs_means$Media)
levels(gbm_concs_means$Media)
new_media_types <- c("FP", "DBT", "IHNB", "IHL", "IHBB", "IHH")
gbm_concs_means$Media <- factor(gbm_concs_means$Media, levels = new_media_types)
levels(gbm_concs_means$Media)

# Create the scatterplot
media_means_plot <- ggplot(gbm_concs_means, aes(x = Date2, y = mean_concs, group=Media, color = Media)) +
  geom_line() + # add points
  labs(x = "Date", y = "Mean Concentration (ng/g)") +
  scale_x_date(date_breaks = "weeks" , date_labels = "%b-%d") +
  theme_classic()
media_means_plot
#scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + # format x-axis as dates
#facet_wrap(~Media, ncol = 3) # group by factor and wrap plots in a grid

media_means_plot_log <- ggplot(gbm_concs_means, aes(x = Date2, y = log(mean_concs), group=Media, color = Media)) +
  geom_line() + # add points
  labs(x = "Date", y = "log(Mean Concentration (ng/g))") +
  scale_x_date(date_breaks = "weeks" , date_labels = "%b-%d") +
  theme_classic()
media_means_plot_log

gbm_media_means_filename <- paste(gbm_graphics,"/gbm_media_means.jpg",sep="")
jpeg(gbm_media_means_filename, width = 7, height = 5, units = "in",res=600)
  media_means_plot
dev.off()


# time series test