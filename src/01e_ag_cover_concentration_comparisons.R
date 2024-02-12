gbm_data_binary
#View(gbm_data_binary)
colnames(gbm_data_binary)

# starting with the untransformed gbm_data_binary
# replace all NAs with lowest concentration observed for that chemical 
# (across media types)
gbm_data_concs <- gbm_data_binary[,1:35]
#View(gbm_data_concs)
for(i in 7:35){
  # print(gbm_data_concs[,i])
  min_conc <- min(gbm_data_concs[,i], na.rm=T)/sqrt(2)
  # print(min_conc)
  # print(sum(is.na(gbm_data_concs[,i])))
  gbm_data_concs[is.na(gbm_data_concs[,i]),i] <- min_conc
}
#View(gbm_data_concs)

## compare high ag to low ag dead bees by chemical
# we don't compare about date yet and are grouping sites by ag cover
dim(gbm_data_concs)
gbm_data_concs$ag_cover
gbm_data_concs$Media
unique(gbm_data_concs$Date)

# DBT 
print("dead bee traps")
gbm_data_concs_dbt <- gbm_data_concs %>% filter(Media == "DBT")
dim(gbm_data_concs_dbt)
#View(gbm_data_concs_dbt)
dbt_ag_high <- which(gbm_data_concs_dbt$ag_cover == "high")
dbt_ag_low <- which(gbm_data_concs_dbt$ag_cover == "low")
dbt_agcover_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(dbt_agcover_tests) <- c("chemical","ag_low_mean","ag_low_sd","ag_high_mean","ag_high_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_dbt[dbt_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_dbt[dbt_ag_low, i]))
  #sd(low_ag_concs)
  dbt_agcover_tests$chemical[i-6] <- colnames(gbm_data_concs_dbt[i])
  dbt_agcover_tests$ag_low_mean[i-6] <- mean(low_ag_concs)
  dbt_agcover_tests$ag_low_sd[i-6] <- sd(low_ag_concs)
  dbt_agcover_tests$ag_high_mean[i-6] <- mean(high_ag_concs)
  dbt_agcover_tests$ag_high_sd[i-6] <- sd(high_ag_concs)  
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
            low_ag_concs, 
            hedges.correction = T)
    dbt_agcover_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    dbt_agcover_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_dbt[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
dbt_agcover_tests

# FP 
print("field pollen")
gbm_data_concs_fp <- gbm_data_concs %>% filter(Media == "FP")
dim(gbm_data_concs_fp)
#View(gbm_data_concs_fp)
fp_ag_high <- which(gbm_data_concs_fp$ag_cover == "high")
fp_ag_low <- which(gbm_data_concs_fp$ag_cover == "low")
fp_agcover_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(fp_agcover_tests) <- c("chemical","ag_low_mean","ag_low_sd","ag_high_mean","ag_high_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_fp[fp_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_fp[fp_ag_low, i]))
  #sd(low_ag_concs)
  fp_agcover_tests$chemical[i-6] <- colnames(gbm_data_concs_fp[i])
  fp_agcover_tests$ag_low_mean[i-6] <- mean(low_ag_concs)
  fp_agcover_tests$ag_low_sd[i-6] <- sd(low_ag_concs)
  fp_agcover_tests$ag_high_mean[i-6] <- mean(high_ag_concs)
  fp_agcover_tests$ag_high_sd[i-6] <- sd(high_ag_concs) 
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    fp_agcover_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    fp_agcover_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_fp[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
fp_agcover_tests

# IHBB 
print("in hive bee bread")
gbm_data_concs_ihbb <- gbm_data_concs %>% filter(Media == "IHBB")
dim(gbm_data_concs_ihbb)
#View(gbm_data_concs_ihbb)
ihbb_ag_high <- which(gbm_data_concs_ihbb$ag_cover == "high")
ihbb_ag_low <- which(gbm_data_concs_ihbb$ag_cover == "low")
ihbb_agcover_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihbb_agcover_tests) <- c("chemical","ag_low_mean","ag_low_sd","ag_high_mean","ag_high_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihbb[ihbb_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihbb[ihbb_ag_low, i]))
  #sd(low_ag_concs)
  ihbb_agcover_tests$chemical[i-6] <- colnames(gbm_data_concs_ihbb[i])
  ihbb_agcover_tests$ag_low_mean[i-6] <- mean(low_ag_concs)
  ihbb_agcover_tests$ag_low_sd[i-6] <- sd(low_ag_concs)
  ihbb_agcover_tests$ag_high_mean[i-6] <- mean(high_ag_concs)
  ihbb_agcover_tests$ag_high_sd[i-6] <- sd(high_ag_concs) 
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    ihbb_agcover_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihbb_agcover_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihbb[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihbb_agcover_tests

# IHH 
print("in hive honey")
gbm_data_concs_ihh <- gbm_data_concs %>% filter(Media == "IHH")
dim(gbm_data_concs_ihh)
#View(gbm_data_concs_ihh)
ihh_ag_high <- which(gbm_data_concs_ihh$ag_cover == "high")
ihh_ag_low <- which(gbm_data_concs_ihh$ag_cover == "low")
ihh_agcover_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihh_agcover_tests) <- c("chemical","ag_low_mean","ag_low_sd","ag_high_mean","ag_high_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihh[ihh_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihh[ihh_ag_low, i]))
  #sd(low_ag_concs)
  ihh_agcover_tests$chemical[i-6] <- colnames(gbm_data_concs_ihh[i])
  ihh_agcover_tests$ag_low_mean[i-6] <- mean(low_ag_concs)
  ihh_agcover_tests$ag_low_sd[i-6] <- sd(low_ag_concs)
  ihh_agcover_tests$ag_high_mean[i-6] <- mean(high_ag_concs)
  ihh_agcover_tests$ag_high_sd[i-6] <- sd(high_ag_concs) 
  if(sd(high_ag_concs)>0 | sd(high_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    ihh_agcover_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihh_agcover_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihh[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihh_agcover_tests

# IHL 
print("in hive larvae")
gbm_data_concs_ihl <- gbm_data_concs %>% filter(Media == "IHL")
dim(gbm_data_concs_ihl)
#View(gbm_data_concs_ihl)
ihl_ag_high <- which(gbm_data_concs_ihl$ag_cover == "high")
ihl_ag_low <- which(gbm_data_concs_ihl$ag_cover == "low")
ihl_agcover_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihl_agcover_tests) <- c("chemical","ag_low_mean","ag_low_sd","ag_high_mean","ag_high_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihl[ihl_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihl[ihl_ag_low, i]))
  #sd(low_ag_concs)
  ihl_agcover_tests$chemical[i-6] <- colnames(gbm_data_concs_ihl[i])
  ihl_agcover_tests$ag_low_mean[i-6] <- mean(low_ag_concs)
  ihl_agcover_tests$ag_low_sd[i-6] <- sd(low_ag_concs)
  ihl_agcover_tests$ag_high_mean[i-6] <- mean(high_ag_concs)
  ihl_agcover_tests$ag_high_sd[i-6] <- sd(high_ag_concs) 
  if(sd(high_ag_concs)>0 | sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    ihl_agcover_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihl_agcover_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihl[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihl_agcover_tests

# IHNB
print("in hive nurse bees")
gbm_data_concs_ihnb <- gbm_data_concs %>% filter(Media == "IHNB")
dim(gbm_data_concs_ihnb)
#View(gbm_data_concs_ihnb)
ihnb_ag_high <- which(gbm_data_concs_ihnb$ag_cover == "high")
ihnb_ag_low <- which(gbm_data_concs_ihnb$ag_cover == "low")
ihnb_agcover_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihnb_agcover_tests) <- c("chemical","ag_low_mean","ag_low_sd","ag_high_mean","ag_high_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihnb[ihnb_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihnb[ihnb_ag_low, i]))
  #sd(low_ag_concs)
  ihnb_agcover_tests$chemical[i-6] <- colnames(gbm_data_concs_ihnb[i])
  ihnb_agcover_tests$ag_low_mean[i-6] <- mean(low_ag_concs)
  ihnb_agcover_tests$ag_low_sd[i-6] <- sd(low_ag_concs)
  ihnb_agcover_tests$ag_high_mean[i-6] <- mean(high_ag_concs)
  ihnb_agcover_tests$ag_high_sd[i-6] <- sd(high_ag_concs) 
  if(sd(high_ag_concs)>0 | sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    ihnb_agcover_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihnb_agcover_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihnb[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihnb_agcover_tests

#create data frame with effects sizes by media and chemical
cohen_d_agcover <- cbind(dbt_agcover_tests$cohens_d, 
                           fp_agcover_tests$cohens_d, 
                           ihbb_agcover_tests$cohens_d, 
                           ihh_agcover_tests$cohens_d, 
                           ihl_agcover_tests$cohens_d, 
                           ihnb_agcover_tests$cohens_d)
rownames(cohen_d_agcover) <- dbt_agcover_tests$chemical
colnames(cohen_d_agcover) <- c("dbt", "fp", "ihbb", "ihh", "ihl", "ihnb")
# replace NAs with zeros
cohen_d_agcover[is.na(cohen_d_agcover)] <- 0

# write to file
cohen_d_agcover_file <- paste(gbm_data_out,"/cohen_d_agcover.csv",sep="")
write.csv(cohen_d_agcover, cohen_d_agcover_file)


col_fun = colorRamp2(c(-1, 0, 1), c("red", "white", "green"))
col_fun(seq(-1, 1))
agcover_heatmap <- Heatmap(cohen_d_agcover, name = "Cohen's d", col = col_fun)
agcover_heatmap

# non parametric tests
#View(cohen_d_agcover)
# within each matrix type
colnames(cohen_d_agcover)
for(i in 1:6){
  print(colnames(cohen_d_agcover)[i])
  print(as.vector(unlist(cohen_d_agcover[which(cohen_d_agcover[,i]!=0),i])))
  n_positive <- length(which(cohen_d_agcover[,i]>0))
  n_total <- length(which(cohen_d_agcover[,i]!=0))
  print(paste(n_positive, n_total))
  print(binom.test(n_positive, n_total)) 
}

# for the entire matrix
n_positive <- length(which(cohen_d_agcover>0))
n_total <- length(which(cohen_d_agcover!=0))
print(paste(n_positive, n_total)) 
print(binom.test(n_positive, n_total))

# an attempt at a hierarchical approach
# does not work, not sure if it makes sense
library(coin)
class(cohen_d_agcover)
cohen_d_agcover_skinny <- pivot_longer(as.data.frame(cohen_d_agcover), cols=1:6, names_to = "matrices")
cohen_d_agcover_skinny$matrices <- as.factor(cohen_d_agcover_skinny$matrices)
class(cohen_d_agcover_skinny)
wilcoxsign_test(value ~ matrices, data=cohen_d_agcover_skinny)
