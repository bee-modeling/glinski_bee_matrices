gbm_summary_data <- gbm_heatmap_data_all
gbm_summary_data$pesticide <- rownames(gbm_summary_data)
#View(gbm_summary_data)
dim(gbm_summary_data)
colnames(gbm_summary_data)
# [1] "DBT"  "FP"   "IHBB" "IHH"  "IHL"  "IHNB" "pesticide"
rownames(gbm_summary_data)
#[1] "Alachlor"              "Anthraquinone"        
#[3] "Atrazine"              "Bifenthrin"           
#[5] "Biphenyl"              "Carfentrazone.ethyl"  
#[7] "Chlorobenzilate"       "cis.Captafol"         
#[9] "Diazinon"              "Diphenylamine"        
#[11] "DMPF"                  "Malathion"            
#[13] "Metalaxyl"             "Metolachlor"          
#[15] "o.Hydroxybiphenyl"     "Penconazole"          
#[17] "Procymidone"           "Pyriproxyfen"         
#[19] "Terbacil"              "Terbuthylazine"       
#[21] "Tetrahydrophthalimide" "Vinclozoline"         
#[23] "Dinotefuran"           "Nitenpyram"           
#[25] "Thiamethoxam"          "Clothianidin"         
#[27] "Imidacloprid"          "Acetamiprid"          
#[29] "Thiacloprid" 
#sort so no funny business
gbm_summary_data2 <- gbm_summary_data[order(gbm_summary_data$pesticide),]

dim(pesticide_key)
pesticide_key$pesticide
colnames(pesticide_key)
pesticide_key$pesticide==gbm_summary_data2$pesticide
#gbm_summary_data2$pesticide[7]
#pesticide_key$pesticide[7]
gbm_summary_data3 <- merge(pesticide_key, gbm_summary_data2, by = "pesticide")
#View(gbm_summary_data3)

# combine concentration estimates and detection frequencies to make summary plots
# make them long and skinny and then merge on media and chemical
dim(gbm_mean_concs)
colnames(gbm_mean_concs)
gbm_mean_concs2 <- gbm_mean_concs[,-c(2:3)]
colnames(gbm_mean_concs2)
gbm_mean_concs_tall <- gbm_mean_concs2 %>%
  gather(key = "chemical", value = "mean", -Media)
gbm_mean_concs_tall
colnames(gbm_mean_concs_tall)

dim(gbm_det_freq)
colnames(gbm_det_freq)
gbm_det_freq2 <- gbm_det_freq[,-c(2:3)]
colnames(gbm_det_freq2)
gbm_det_freq_tall <- gbm_det_freq2 %>%
  gather(key = "chemical", value = "det_freq", -Media)
gbm_det_freq_tall
colnames(gbm_det_freq_tall)

gbm_data_summary <- as.data.frame(left_join(gbm_mean_concs_tall, gbm_det_freq_tall, by = c("Media","chemical")))
#View(gbm_data_summary)
min(gbm_data_summary$mean, na.rm=T)
dim(gbm_data_summary)
summary(gbm_data_summary)
class(gbm_data_summary)
class(gbm_data_summary$Media)
class(gbm_data_summary$chemical)
class(gbm_data_summary$mean)
class(gbm_data_summary$det_freq)
gbm_data_summary$chemical <- factor(gbm_data_summary$chemical)

# very weird work around to deal with the fact that is.nan does not handle data frames
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
min_observed <- min(gbm_data_summary$mean, na.rm=T)
min_observed #should not be 0
gbm_data_summary[is.nan(gbm_data_summary)] <- min_observed

# merge chemical with class from the key (forgot to bring it along)
colnames(gbm_data_summary)
colnames(pesticide_key) <- c("chemical", "pest_type")
gbm_data_summary_plot <- merge(gbm_data_summary, pesticide_key, by = "chemical", all.x = TRUE)
class(gbm_data_summary_plot$pest_type)

#drop chemical labels when log(conc) <0.25 and det_freq <0.25 
gbm_data_summary_plot$chemical_plot <- gbm_data_summary_plot$chemical

# ggplot
gbm_summary_plot_all <- ggplot(gbm_data_summary_plot, aes(x = det_freq, y = log(mean), color = pest_type, shape=Media)) +
  geom_point(size=2.5) +
  labs(x = "Detection Frequency (proportion)", y = "log(Mean Concentration (mg/kg?))", color = "Media") +
  theme_classic() +
  theme(legend.position = "bottom")
  
gbm_summary_plot_all
gbm_summary_plot_all_filename <- paste(gbm_graphics,"/gbm_summary_plot_all.jpg",sep="")
jpeg(gbm_summary_plot_all_filename, width = 8, height = 6, units = "in",res=600)
  gbm_summary_plot_all
dev.off()


# drop some chemical names for the plot
# drop if log(mean) < -2.5
# drop if det_freq < 0.25


gbm_summary_plot_matrices <- ggplot(gbm_data_summary_plot, aes(x = det_freq, y = log(mean), color = pest_type)) +
  geom_point(size=2.5) +
  #geom_text(aes(label = chemical), vjust = -1) +
  geom_text_repel(aes(label = chemical), size = 2) +
  labs(x = "Detection Frequency (proportion)", y = "log(Mean Concentration (mg/kg?))", color = "Media") +
  facet_wrap(~ Media) +
  theme_classic() +
  theme(legend.position = "none")
gbm_summary_plot_matrices
gbm_summary_plot_matrices_filename <- paste(gbm_graphics,"/gbm_summary_plot_matrices.jpg",sep="")
jpeg(gbm_summary_plot_matrices_filename, width = 8, height = 6, units = "in",res=600)
  gbm_summary_plot_matrices
dev.off()
