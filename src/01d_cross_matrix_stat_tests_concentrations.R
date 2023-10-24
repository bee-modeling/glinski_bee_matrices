gbm_heatmap_data_all_ag
colnames(gbm_heatmap_data_all_ag)

# dbt
wilcox.test(gbm_heatmap_data_all_ag[,1], gbm_heatmap_data_all_ag[,2], paired=TRUE, alternative = c("greater"))
# data:  gbm_heatmap_data_all_ag[, 1] and gbm_heatmap_data_all_ag[, 2]
# V = 168, p-value = 0.5805
# alternative hypothesis: true location shift is greater than 0

# fp
wilcox.test(gbm_heatmap_data_all_ag[,3], gbm_heatmap_data_all_ag[,4], paired=TRUE, alternative = c("greater"))
#data:  gbm_heatmap_data_all_ag[, 3] and gbm_heatmap_data_all_ag[, 4]
#V = 174, p-value = 0.251
#alternative hypothesis: true location shift is greater than 0

# ihbb
wilcox.test(gbm_heatmap_data_all_ag[,5], gbm_heatmap_data_all_ag[,6], paired=TRUE, alternative = c("greater"))
#data:  gbm_heatmap_data_all_ag[, 5] and gbm_heatmap_data_all_ag[, 6]
#V = 156, p-value = 0.007453
#alternative hypothesis: true location shift is greater than 0

# ihh
wilcox.test(gbm_heatmap_data_all_ag[,7], gbm_heatmap_data_all_ag[,8], paired=TRUE, alternative = c("greater"))
#data:  gbm_heatmap_data_all_ag[, 7] and gbm_heatmap_data_all_ag[, 8]
#V = 48, p-value = 0.4444
#alternative hypothesis: true location shift is greater than 0

# ihl
wilcox.test(gbm_heatmap_data_all_ag[,9], gbm_heatmap_data_all_ag[,10], paired=TRUE, alternative = c("greater"))
#data:  gbm_heatmap_data_all_ag[, 9] and gbm_heatmap_data_all_ag[, 10]
#V = 84, p-value = 0.02582
#alternative hypothesis: true location shift is greater than 0

# ihnb
wilcox.test(gbm_heatmap_data_all_ag[,11], gbm_heatmap_data_all_ag[,12], paired=TRUE, alternative = c("greater"))
#data:  gbm_heatmap_data_all_ag[, 11] and gbm_heatmap_data_all_ag[, 12]
#V = 166, p-value = 0.2015
#alternative hypothesis: true location shift is greater than 0

# all pairwise concentrations
dbt_concs <- as.data.frame(gbm_heatmap_data_all_ag[,1:2], col.names=c("high", "low"))
colnames(dbt_concs) <- c("high", "low")
fp_concs <- as.data.frame(gbm_heatmap_data_all_ag[,3:4]) 
colnames(fp_concs) <- c("high", "low")
ihbb_concs <- as.data.frame(gbm_heatmap_data_all_ag[,5:6])
colnames(ihbb_concs) <- c("high", "low")
ihh_concs <- as.data.frame(gbm_heatmap_data_all_ag[,7:8])
colnames(ihh_concs) <- c("high", "low")
ihl_concs <- as.data.frame(gbm_heatmap_data_all_ag[,9:10])
colnames(ihl_concs) <- c("high", "low")
ihnb_concs <- as.data.frame(gbm_heatmap_data_all_ag[,11:12])
colnames(ihnb_concs) <- c("high", "low")

all_concs <-rbind(dbt_concs, 
                  fp_concs,
                  ihbb_concs,
                  ihh_concs,
                  ihl_concs,
                  ihnb_concs)
dim(all_concs)
colnames(all_concs)
summary(all_concs)
which(rowSums(all_concs)==0)
all_concs$high > all_concs$low
wilcox.test(all_concs$high, all_concs$low, paired=TRUE, alternative = c("greater"))
#data:  all_concs$high and all_concs$low
#V = 4448, p-value = 0.009984
#alternative hypothesis: true location shift is greater than 0


media_names <- c("All", "Dead bee traps", "Field pollen", "Inhive bee bread",
                 "Inhive honey", "Inhive larvae", "Inhive nurse bees")
detfreq_p_values <- c(0.09, 0.384, 0.289, 0.053, 0.39, 0.71, 0.193)
conc_p_values <- c(0.001, 0.581, 0.251, 0.007, 0.444, 0.026, 0.202)

p_value_table <- tibble(Media = media_names, DetFreq = detfreq_p_values, Conc = conc_p_values)

library(gt)

p_value_table |> gt() |> tab_header(
  title = "WSR p values for high v low ag cover")
