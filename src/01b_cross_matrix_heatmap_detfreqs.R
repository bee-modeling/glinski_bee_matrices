#### all data together
dim(gbm_data)
colnames(gbm_data)

# convert matrix to binary
gbm_data_binary <- gbm_data %>%
  group_by(Media) %>%
  #select(where(is.numeric)) %>%
  #summarize(det_freq = sum(!is.na(everything())) / n())
  summarize(det_freq = ifelse(is.na(Alachlor),0,1))
#summarise(across(everything(), 
#                 mean,
#                 na.rm = TRUE))
colnames(gbm_data_binary)
#View(gbm_data_binary)

# gbm_det_freqs

vars_to_process <- unlist(colnames(gbm_data)[7:35], recursive=F)
c(vars_to_process)
#gbm_data_binary <- gbm_data %>%
#  group_by(Media) %>%
#  summarize(.vars=c(vars_to_process), .fns=list(min_detect = ~ min(., na.rm = TRUE)))) # %>%
#mutate_at(.vars=vars_to_process, list(Binary = ~ifelse(. > 0.5, 1, 0))) %>%
#mutate_at(.vars=vars_to_process, list(Binary = ~ifelse(is.na(.), 0, 1)))

# Calculate the mean of non-missing values in "Value1", "Value2", and "Value3" within each group, and then subtract each mean value from the corresponding non-missing value for each row in that group
#gbm_data_binary  <- gbm_data %>%
#  group_by(Media) %>%
#  summarize(across(.cols = (vars_to_process), .fns = list(min_value = ~ min(., na.rm = TRUE)))) %>%
#  mutate(across(.cols = (vars_to_process), .fns = list(~ ifelse(!is.na(.), . - mean(.[!is.na(.)]), .), cur_column())))

# this works for z score
#gbm_data_binary  <- gbm_data %>%
#  group_by(Media) %>%
#  mutate(across(.cols = (vars_to_process), .fns = list(z_score = ~ ifelse(!is.na(.), (.-mean(.[!is.na(.)]))/sd(.[!is.na(.)]), .))))
colnames(gbm_data_binary)
gbm_data_binary  <- gbm_data %>%
  group_by(Media) %>%
  #mutate(across(.cols = (vars_to_process), .fns = list(conc = ~ ifelse(!is.na(.), (.-mean(.[!is.na(.)]))/sd(.[!is.na(.)]), .))))
  mutate(across(.cols = (vars_to_process), .fns = list(conc = ~ ifelse(!is.na(.), ifelse(.>min(.[!is.na(.)]),1,0), 0))))

dim(gbm_data)
dim(gbm_data_binary)
#View(gbm_data_binary)
colnames(gbm_data_binary)

gbm_data_binary2 <- gbm_data_binary[,-c(7:35)]
colnames(gbm_data_binary2)[7:35] <- chemical_names

#### all data together for detection frequency
gbm_det_freq <- gbm_data_binary2 %>%
  group_by(Media) %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), 
                   mean,
                   na.rm = TRUE))
dim(gbm_det_freq)
colnames(gbm_det_freq)

#check on this, no longer alphabetical?
gbm_heatmap_binary <- as.data.frame(t(gbm_det_freq[,4:32]))
dim(gbm_heatmap_binary)
colnames(gbm_heatmap_binary) <- media_names

gbm_heatmap_detfreq <- pheatmap(gbm_heatmap_binary)
gbm_heatmap_detfreq

gbm_heatmap_detfreq_filename <- paste(gbm_graphics,"/gbm_heatmap_detfreq.jpg",sep="")
jpeg(gbm_heatmap_detfreq_filename, width = 4, height = 7, units = "in",res=600)
  gbm_heatmap_detfreq
dev.off()

#### split data into high and low ag cover sample sets
#View(gbm_data_binary2)
gbm_data_binary_ag <- gbm_data_binary2 %>%
  group_by(Media, ag_cover) %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), 
                   mean,
                   na.rm = TRUE))

#View(gbm_data_binary_ag)
dim(gbm_data)
dim(gbm_data_binary_ag)

#media
media_names_binary_ag <- paste0(gbm_data_binary_ag$Media,"_",gbm_data_binary_ag$ag_cover)
media_names_binary_ag

#View(gbm_data_binary_ag)
colnames(gbm_data_binary_ag)
gbm_heatmap_detfreq_all_ag <- as.data.frame(t(gbm_data_binary_ag[,5:33]))
dim(gbm_heatmap_detfreq_all_ag)
colnames(gbm_heatmap_detfreq_all_ag) <- media_names_ag
colnames(gbm_heatmap_detfreq_all_ag)
#View(gbm_heatmap_detfreq_all_ag)


# this might be useful elsewhere? but orphaned here
#View(gbm_data_binary)
#colnames(gbm_data_binary_ag)
#gbm_data_binary2 <- gbm_data_binary[,-c(7:35)]
#colnames(gbm_data_binary2)[7:35] <- chemical_names
#data_min <- min(gbm_heatmap_data_all_ag, na.rm=T)

#replace NaNs with NAs
gbm_heatmap_detfreq_all_ag <- gbm_heatmap_detfreq_all_ag %>% mutate_all(~ifelse(is.nan(.), 0, .))
gbm_heatmap_detfreq_all_ag <- gbm_heatmap_detfreq_all_ag %>% mutate_all(~ifelse(is.na(.), 0, .))
max(gbm_heatmap_detfreq_all_ag)
#View(gbm_heatmap_detfreq_all_ag)

# export gbm_heatmap_data_all_ag so the chemicals can be tagged with herbicide, insecticide, miticide, and fungicide
# also drop those chemicals that don't have > 10% detects in 2 or more media


gbm_heatmap_detfreqall_ag <- pheatmap(gbm_heatmap_detfreq_all_ag)
gbm_heatmap_detfreqall_ag

gbm_heatmap_detfreq_ag_filename <- paste(gbm_graphics,"/gbm_heatmap_detfreq_ag_split.jpg",sep="")
jpeg(gbm_heatmap_detfreq_ag_filename, width = 7, height = 7, units = "in",res=600)
  gbm_heatmap_detfreqall_ag
dev.off()


gg_gbm_heatmap_detfreqall_ag <- as.ggplot(pheatmap(gbm_heatmap_detfreq_all_ag))

