#View(gbm_data)
dim(gbm_data)
colnames(gbm_data)
gbm_data$Media

chemical_names <- colnames(gbm_data)[7:35]
chemical_names

#### all data together
# proxy values for nondetects?
gbm_mean_concs <- gbm_data %>%
  group_by(Media) %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), 
                   mean,
                   na.rm = TRUE))
# View(gbm_mean_concs)
dim(gbm_mean_concs)
#media
media_names <- gbm_mean_concs$Media
media_names

gbm_heatmap_data_all <- as.data.frame(t(gbm_mean_concs[,4:32]))
dim(gbm_heatmap_data_all)
colnames(gbm_heatmap_data_all) <- media_names
#View(gbm_heatmap_data_all)

data_min <- min(gbm_heatmap_data_all, na.rm=T)

#replace NaNs with NAs
gbm_heatmap_data_all <- gbm_heatmap_data_all %>% mutate_all(~ifelse(is.nan(.), data_min, .))
gbm_heatmap_data_all <- gbm_heatmap_data_all %>% mutate_all(~ifelse(is.na(.), data_min, .))
#View(gbm_heatmap_data_all)

gbm_heatmap_all <- pheatmap(log(gbm_heatmap_data_all))
gbm_heatmap_all

gbm_heatmap_filename <- paste(gbm_graphics,"/gbm_heatmap.jpg",sep="")
jpeg(gbm_heatmap_filename, width = 4, height = 7, units = "in",res=600)
  gbm_heatmap_all
dev.off()

#### split data into high and low ag cover sample sets
gbm_mean_concs_ag <- gbm_data %>%
  group_by(Media, ag_cover) %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), 
                   mean,
                   na.rm = TRUE))
View(gbm_mean_concs_ag)
dim(gbm_mean_concs_ag)
colnames(gbm_mean_concs_ag)
rownames(gbm_mean_concs_ag)
#media
media_names_ag <- paste0(gbm_mean_concs_ag$Media,"_",gbm_mean_concs_ag$ag_cover)
media_names_ag

gbm_heatmap_data_all_ag <- as.data.frame(t(gbm_mean_concs_ag[,5:33]))
dim(gbm_heatmap_data_all_ag)
colnames(gbm_heatmap_data_all_ag) <- media_names_ag
#View(gbm_heatmap_data_all_ag)

data_min <- min(gbm_heatmap_data_all_ag, na.rm=T)

#replace NaNs with NAs
gbm_heatmap_data_all_ag <- gbm_heatmap_data_all_ag %>% mutate_all(~ifelse(is.nan(.), data_min, .))
gbm_heatmap_data_all_ag <- gbm_heatmap_data_all_ag %>% mutate_all(~ifelse(is.na(.), data_min, .))
#View(gbm_heatmap_data_all)

# export gbm_heatmap_data_all_ag so the chemicals can be tagged with herbicide, insecticide, miticide, and fungicide
# also drop those chemicals that don't have > 10% detects in 2 or more media


gbm_heatmap_all_ag <- pheatmap(log(gbm_heatmap_data_all_ag))
gbm_heatmap_all_ag

gbm_heatmap_ag_filename <- paste(gbm_graphics,"/gbm_heatmap_ag_split.jpg",sep="")
jpeg(gbm_heatmap_ag_filename, width = 7, height = 7, units = "in",res=600)
  gbm_heatmap_all_ag
dev.off()

gg_gbm_heatmap_all_ag <- as.ggplot(pheatmap(log(gbm_heatmap_data_all_ag)), legend_side="bottom")
gg_gbm_heatmap_all_ag
