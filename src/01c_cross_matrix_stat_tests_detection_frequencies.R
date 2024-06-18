gbm_data_binary_ag
#View(gbm_data_binary_ag)
colnames(gbm_data_binary_ag)

# Wilcoxon signed rank test on detection frequencies for each media
# dbt
dbt_dfs <- as.data.frame(cbind(as.vector(as.numeric(gbm_data_binary_ag[1,5:33])), 
                               as.vector(as.numeric(gbm_data_binary_ag[2,5:33]))))
summary(dbt_dfs)
which(rowSums(dbt_dfs)==0)
dbt_df_test <- dbt_dfs[-which(rowSums(dbt_dfs)==0),]
dbt_df_test <- dbt_df_test + 0.001
dbt_df_test$V1 > dbt_df_test$V2
wilcox.test(dbt_df_test[,1], dbt_df_test[,2], paired=TRUE, alternative = c("greater"))
#data:  dbt_df_test[, 1] and dbt_df_test[, 2]
#V = 174, p-value = 0.3836
#alternative hypothesis: true location shift is greater than 0

#FP
fp_dfs <- as.data.frame(cbind(as.vector(as.numeric(gbm_data_binary_ag[3,5:33])), 
                               as.vector(as.numeric(gbm_data_binary_ag[4,5:33]))))
summary(fp_dfs)
which(rowSums(fp_dfs)==0)
fp_df_test <- fp_dfs[-which(rowSums(fp_dfs)==0),]
fp_df_test <- fp_df_test + 0.001
fp_df_test$V1 > fp_df_test$V2
wilcox.test(fp_df_test[,1], fp_df_test[,2], paired=TRUE, alternative = c("greater"))
#data:  fp_df_test[, 1] and fp_df_test[, 2]
#V = 170, p-value = 0.2886
#alternative hypothesis: true location shift is greater than 0

#IHBB
ihbb_dfs <- as.data.frame(cbind(as.vector(as.numeric(gbm_data_binary_ag[5,5:33])), 
                              as.vector(as.numeric(gbm_data_binary_ag[6,5:33]))))
summary(ihbb_dfs)
which(rowSums(ihbb_dfs)==0)
ihbb_df_test <- ihbb_dfs[-which(rowSums(ihbb_dfs)==0),]
ihbb_df_test <- ihbb_df_test + 0.001
ihbb_df_test$V1 > ihbb_df_test$V2
wilcox.test(ihbb_df_test[,1], ihbb_df_test[,2], paired=TRUE, alternative = c("greater"))
#data:  ihbb_df_test[, 1] and ihbb_df_test[, 2]
#V = 123, p-value = 0.05334
#alternative hypothesis: true location shift is greater than 0

#IHH
ihh_dfs <- as.data.frame(cbind(as.vector(as.numeric(gbm_data_binary_ag[7,5:33])), 
                              as.vector(as.numeric(gbm_data_binary_ag[8,5:33]))))
summary(ihh_dfs)
which(rowSums(ihh_dfs)==0)
ihh_df_test <- ihh_dfs[-which(rowSums(ihh_dfs)==0),]
ihh_df_test <- ihh_df_test + 0.001
ihh_df_test$V1 > ihh_df_test$V2
wilcox.test(ihh_df_test[,1], ihh_df_test[,2], paired=TRUE, alternative = c("greater"))
#data:  ihh_df_test[, 1] and ihh_df_test[, 2]
#V = 50, p-value = 0.3897
#alternative hypothesis: true location shift is greater than 0

#IHL
ihl_dfs <- as.data.frame(cbind(as.vector(as.numeric(gbm_data_binary_ag[9,5:33])), 
                              as.vector(as.numeric(gbm_data_binary_ag[10,5:33]))))
summary(ihl_dfs)
which(rowSums(ihl_dfs)==0)
ihl_df_test <- ihl_dfs[-which(rowSums(ihl_dfs)==0),]
ihl_df_test <- ihl_df_test + 0.001
ihl_df_test$V1 > ihl_df_test$V2
wilcox.test(ihl_df_test[,1], ihl_df_test[,2], paired=TRUE, alternative = c("greater"))
#data:  ihl_df_test[, 1] and ihl_df_test[, 2]
#V = 38, p-value = 0.7124
#alternative hypothesis: true location shift is greater than 0

#IHNB
ihnb_dfs <- as.data.frame(cbind(as.vector(as.numeric(gbm_data_binary_ag[11,5:33])), 
                              as.vector(as.numeric(gbm_data_binary_ag[12,5:33]))))
summary(ihnb_dfs)
which(rowSums(ihnb_dfs)==0)
ihnb_df_test <- ihnb_dfs[-which(rowSums(ihnb_dfs)==0),]
ihnb_df_test <- ihnb_df_test + 0.001
ihnb_df_test$V1 > ihnb_df_test$V2
wilcox.test(ihnb_df_test[,1], ihnb_df_test[,2], paired=TRUE, alternative = c("greater"))
#data:  ihnb_df_test[, 1] and ihnb_df_test[, 2]
#V = 167, p-value = 0.1926
#alternative hypothesis: true location shift is greater than 0


# combine all matrices
all_dfs <-rbind(dbt_dfs, fp_dfs, ihbb_dfs, ihh_dfs, ihl_dfs, ihnb_dfs)
dim(all_dfs)
summary(all_dfs)
which(rowSums(all_dfs)==0)
all_df_test <- all_dfs[-which(rowSums(all_dfs)==0),]
dim(all_df_test)
all_df_test <- all_df_test + 0.001
all_df_test$V1 > all_df_test$V2
sum(all_df_test$V1 > all_df_test$V2)
wilcox.test(all_df_test[,1], all_df_test[,2], paired=TRUE, alternative = c("greater"))
#data:  all_df_test[, 1] and all_df_test[, 2]
#V = 3879, p-value = 0.09047
#alternative hypothesis: true location shift is greater than 0




