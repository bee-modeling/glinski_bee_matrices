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
gbm_data_concs$Date
gbm_data_concs$Media
unique(gbm_data_concs$Date)

# DBT 
print("dead bee traps")
gbm_data_concs_dbt <- gbm_data_concs %>% filter(Media == "DBT")
dim(gbm_data_concs_dbt)
#View(gbm_data_concs_dbt)
dbt_pre_plant <- which(gbm_data_concs_dbt$Date == "427" |gbm_data_concs_dbt$Date == "428" | gbm_data_concs_dbt$Date == "429" |
                       gbm_data_concs_dbt$Date == "430" | gbm_data_concs_dbt$Date == "501" )
dbt_post_plant <- which(gbm_data_concs_dbt$Date == "502" | gbm_data_concs_dbt$Date == "505" |
                      gbm_data_concs_dbt$Date == "506" | gbm_data_concs_dbt$Date == "507" |
                      gbm_data_concs_dbt$Date == "508" | gbm_data_concs_dbt$Date == "510" |
                      gbm_data_concs_dbt$Date == "511" | gbm_data_concs_dbt$Date == "512" |
                      gbm_data_concs_dbt$Date == "513" | gbm_data_concs_dbt$Date == "514" |
                      gbm_data_concs_dbt$Date == "516" | gbm_data_concs_dbt$Date == "519" |
                      gbm_data_concs_dbt$Date == "520" | gbm_data_concs_dbt$Date == "521" |
                      gbm_data_concs_dbt$Date == "522" | gbm_data_concs_dbt$Date == "523" |
                      gbm_data_concs_dbt$Date == "524" | gbm_data_concs_dbt$Date == "527" | gbm_data_concs_dbt$Date == "603"
                      )
dbt_plantdate_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(dbt_plantdate_tests) <- c("chemical","post_plant_mean","post_plant_sd","pre_plant_mean","pre_plant_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  pre_plant_concs <- as.numeric(unlist(gbm_data_concs_dbt[dbt_pre_plant, i]))
  pre_plant_concs
  #sd(pre_plant_concs)
  post_plant_concs <- as.numeric(unlist(gbm_data_concs_dbt[dbt_post_plant, i]))
  post_plant_concs
  #sd(post_plant_concs)
  dbt_plantdate_tests$chemical[i-6] <- colnames(gbm_data_concs_dbt[i])
  dbt_plantdate_tests$post_plant_mean[i-6] <- mean(post_plant_concs)
  dbt_plantdate_tests$post_plant_sd[i-6] <- sd(post_plant_concs)
  dbt_plantdate_tests$pre_plant_mean[i-6] <- mean(pre_plant_concs)
  dbt_plantdate_tests$pre_plant_sd[i-6] <- sd(pre_plant_concs)  
  if(sd(pre_plant_concs)>0 & sd(post_plant_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(post_plant_concs, pre_plant_concs)
    conc_cohens_d <- cohen.d(post_plant_concs, 
                             pre_plant_concs, 
                             hedges.correction = T)
    dbt_plantdate_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    dbt_plantdate_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_dbt[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
dbt_plantdate_tests

# FP 
print("field pollen")
gbm_data_concs_fp <- gbm_data_concs %>% filter(Media == "FP")
dim(gbm_data_concs_fp)
#View(gbm_data_concs_fp)
fp_pre_plant <- which(gbm_data_concs_fp$Date == "427" |gbm_data_concs_fp$Date == "428" | gbm_data_concs_fp$Date == "429" |
                         gbm_data_concs_fp$Date == "430" | gbm_data_concs_fp$Date == "501" )
fp_post_plant <- which(gbm_data_concs_fp$Date == "502" | gbm_data_concs_fp$Date == "505" |
                          gbm_data_concs_fp$Date == "506" | gbm_data_concs_fp$Date == "507" |
                          gbm_data_concs_fp$Date == "508" | gbm_data_concs_fp$Date == "510" |
                          gbm_data_concs_fp$Date == "511" | gbm_data_concs_fp$Date == "512" |
                          gbm_data_concs_fp$Date == "513" | gbm_data_concs_fp$Date == "514" |
                          gbm_data_concs_fp$Date == "516" | gbm_data_concs_fp$Date == "519" |
                          gbm_data_concs_fp$Date == "520" | gbm_data_concs_fp$Date == "521" |
                          gbm_data_concs_fp$Date == "522" | gbm_data_concs_fp$Date == "523" |
                          gbm_data_concs_fp$Date == "524" | gbm_data_concs_fp$Date == "527" | gbm_data_concs_fp$Date == "603"
)
fp_plantdate_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(fp_plantdate_tests) <- c("chemical","post_plant_mean","post_plant_sd","pre_plant_mean","pre_plant_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  pre_plant_concs <- as.numeric(unlist(gbm_data_concs_fp[fp_pre_plant, i]))
  #sd(pre_plant_concs)
  post_plant_concs <- as.numeric(unlist(gbm_data_concs_fp[fp_post_plant, i]))
  #sd(post_plant_concs)
  fp_plantdate_tests$chemical[i-6] <- colnames(gbm_data_concs_fp[i])
  fp_plantdate_tests$post_plant_mean[i-6] <- mean(post_plant_concs)
  fp_plantdate_tests$post_plant_sd[i-6] <- sd(post_plant_concs)
  fp_plantdate_tests$pre_plant_mean[i-6] <- mean(pre_plant_concs)
  fp_plantdate_tests$pre_plant_sd[i-6] <- sd(pre_plant_concs) 
  if(sd(pre_plant_concs)>0 & sd(post_plant_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(post_plant_concs, pre_plant_concs)
    conc_cohens_d <- cohen.d(post_plant_concs, 
                             pre_plant_concs, 
                             hedges.correction = T)
    fp_plantdate_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    fp_plantdate_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_fp[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
fp_plantdate_tests

# IHBB 
print("in hive bee bread")
gbm_data_concs_ihbb <- gbm_data_concs %>% filter(Media == "IHBB")
dim(gbm_data_concs_ihbb)
#View(gbm_data_concs_ihbb)
ihbb_pre_plant <- which(gbm_data_concs_ihbb$Date == "427" |gbm_data_concs_ihbb$Date == "428" | gbm_data_concs_ihbb$Date == "429" |
                        gbm_data_concs_ihbb$Date == "430" | gbm_data_concs_ihbb$Date == "501" )
ihbb_post_plant <- which(gbm_data_concs_ihbb$Date == "502" | gbm_data_concs_ihbb$Date == "505" |
                         gbm_data_concs_ihbb$Date == "506" | gbm_data_concs_ihbb$Date == "507" |
                         gbm_data_concs_ihbb$Date == "508" | gbm_data_concs_ihbb$Date == "510" |
                         gbm_data_concs_ihbb$Date == "511" | gbm_data_concs_ihbb$Date == "512" |
                         gbm_data_concs_ihbb$Date == "513" | gbm_data_concs_ihbb$Date == "514" |
                         gbm_data_concs_ihbb$Date == "516" | gbm_data_concs_ihbb$Date == "519" |
                         gbm_data_concs_ihbb$Date == "520" | gbm_data_concs_ihbb$Date == "521" |
                         gbm_data_concs_ihbb$Date == "522" | gbm_data_concs_ihbb$Date == "523" |
                         gbm_data_concs_ihbb$Date == "524" | gbm_data_concs_ihbb$Date == "527" | gbm_data_concs_ihbb$Date == "603"
)
ihbb_plantdate_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihbb_plantdate_tests) <- c("chemical","post_plant_mean","post_plant_sd","pre_plant_mean","pre_plant_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  pre_plant_concs <- as.numeric(unlist(gbm_data_concs_ihbb[ihbb_pre_plant, i]))
  #sd(pre_plant_concs)
  post_plant_concs <- as.numeric(unlist(gbm_data_concs_ihbb[ihbb_post_plant, i]))
  #sd(post_plant_concs)
  ihbb_plantdate_tests$chemical[i-6] <- colnames(gbm_data_concs_ihbb[i])
  ihbb_plantdate_tests$post_plant_mean[i-6] <- mean(post_plant_concs)
  ihbb_plantdate_tests$post_plant_sd[i-6] <- sd(post_plant_concs)
  ihbb_plantdate_tests$pre_plant_mean[i-6] <- mean(pre_plant_concs)
  ihbb_plantdate_tests$pre_plant_sd[i-6] <- sd(pre_plant_concs) 
  if(sd(pre_plant_concs)>0 & sd(post_plant_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(post_plant_concs, pre_plant_concs)
    conc_cohens_d <- cohen.d(post_plant_concs, 
                             pre_plant_concs, 
                             hedges.correction = T)
    ihbb_plantdate_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihbb_plantdate_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihbb[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihbb_plantdate_tests

# IHH 
print("in hive honey")
gbm_data_concs_ihh <- gbm_data_concs %>% filter(Media == "IHH")
dim(gbm_data_concs_ihh)
#View(gbm_data_concs_ihh)
ihh_pre_plant <- which(gbm_data_concs_ihh$Date == "427" |gbm_data_concs_ihh$Date == "428" | gbm_data_concs_ihh$Date == "429" |
                          gbm_data_concs_ihh$Date == "430" | gbm_data_concs_ihh$Date == "501" )
ihh_post_plant <- which(gbm_data_concs_ihh$Date == "502" | gbm_data_concs_ihh$Date == "505" |
                           gbm_data_concs_ihh$Date == "506" | gbm_data_concs_ihh$Date == "507" |
                           gbm_data_concs_ihh$Date == "508" | gbm_data_concs_ihh$Date == "510" |
                           gbm_data_concs_ihh$Date == "511" | gbm_data_concs_ihh$Date == "512" |
                           gbm_data_concs_ihh$Date == "513" | gbm_data_concs_ihh$Date == "514" |
                           gbm_data_concs_ihh$Date == "516" | gbm_data_concs_ihh$Date == "519" |
                           gbm_data_concs_ihh$Date == "520" | gbm_data_concs_ihh$Date == "521" |
                           gbm_data_concs_ihh$Date == "522" | gbm_data_concs_ihh$Date == "523" |
                           gbm_data_concs_ihh$Date == "524" | gbm_data_concs_ihh$Date == "527" | gbm_data_concs_ihh$Date == "603"
)
ihh_plantdate_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihh_plantdate_tests) <- c("chemical","post_plant_mean","post_plant_sd","pre_plant_mean","pre_plant_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  pre_plant_concs <- as.numeric(unlist(gbm_data_concs_ihh[ihh_pre_plant, i]))
  #sd(pre_plant_concs)
  post_plant_concs <- as.numeric(unlist(gbm_data_concs_ihh[ihh_post_plant, i]))
  #sd(post_plant_concs)
  ihh_plantdate_tests$chemical[i-6] <- colnames(gbm_data_concs_ihh[i])
  ihh_plantdate_tests$post_plant_mean[i-6] <- mean(post_plant_concs)
  ihh_plantdate_tests$post_plant_sd[i-6] <- sd(post_plant_concs)
  ihh_plantdate_tests$pre_plant_mean[i-6] <- mean(pre_plant_concs)
  ihh_plantdate_tests$pre_plant_sd[i-6] <- sd(pre_plant_concs) 
  if(sd(pre_plant_concs)>0 | sd(pre_plant_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(post_plant_concs, pre_plant_concs)
    conc_cohens_d <- cohen.d(post_plant_concs, 
                             pre_plant_concs, 
                             hedges.correction = T)
    ihh_plantdate_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihh_plantdate_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihh[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihh_plantdate_tests

# IHL 
print("in hive larvae")
gbm_data_concs_ihl <- gbm_data_concs %>% filter(Media == "IHL")
dim(gbm_data_concs_ihl)
#View(gbm_data_concs_ihl)
ihl_pre_plant <- which(gbm_data_concs_ihl$Date == "427" |gbm_data_concs_ihl$Date == "428" | gbm_data_concs_ihl$Date == "429" |
                          gbm_data_concs_ihl$Date == "430" | gbm_data_concs_ihl$Date == "501" )
ihl_post_plant <- which(gbm_data_concs_ihl$Date == "502" | gbm_data_concs_ihl$Date == "505" |
                           gbm_data_concs_ihl$Date == "506" | gbm_data_concs_ihl$Date == "507" |
                           gbm_data_concs_ihl$Date == "508" | gbm_data_concs_ihl$Date == "510" |
                           gbm_data_concs_ihl$Date == "511" | gbm_data_concs_ihl$Date == "512" |
                           gbm_data_concs_ihl$Date == "513" | gbm_data_concs_ihl$Date == "514" |
                           gbm_data_concs_ihl$Date == "516" | gbm_data_concs_ihl$Date == "519" |
                           gbm_data_concs_ihl$Date == "520" | gbm_data_concs_ihl$Date == "521" |
                           gbm_data_concs_ihl$Date == "522" | gbm_data_concs_ihl$Date == "523" |
                           gbm_data_concs_ihl$Date == "524" | gbm_data_concs_ihl$Date == "527" | gbm_data_concs_ihl$Date == "603"
)
ihl_plantdate_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihl_plantdate_tests) <- c("chemical","post_plant_mean","post_plant_sd","pre_plant_mean","pre_plant_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  pre_plant_concs <- as.numeric(unlist(gbm_data_concs_ihl[ihl_pre_plant, i]))
  #sd(pre_plant_concs)
  post_plant_concs <- as.numeric(unlist(gbm_data_concs_ihl[ihl_post_plant, i]))
  #sd(post_plant_concs)
  ihl_plantdate_tests$chemical[i-6] <- colnames(gbm_data_concs_ihl[i])
  ihl_plantdate_tests$post_plant_mean[i-6] <- mean(post_plant_concs)
  ihl_plantdate_tests$post_plant_sd[i-6] <- sd(post_plant_concs)
  ihl_plantdate_tests$pre_plant_mean[i-6] <- mean(pre_plant_concs)
  ihl_plantdate_tests$pre_plant_sd[i-6] <- sd(pre_plant_concs) 
  if(sd(pre_plant_concs)>0 | sd(post_plant_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(post_plant_concs, pre_plant_concs)
    conc_cohens_d <- cohen.d(post_plant_concs, 
                             pre_plant_concs, 
                             hedges.correction = T)
    ihl_plantdate_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihl_plantdate_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihl[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihl_plantdate_tests

# IHNB
print("in hive nurse bees")
gbm_data_concs_ihnb <- gbm_data_concs %>% filter(Media == "IHNB")
dim(gbm_data_concs_ihnb)
#View(gbm_data_concs_ihnb)
ihnb_pre_plant <- which(gbm_data_concs_ihnb$Date == "427" |gbm_data_concs_ihnb$Date == "428" | gbm_data_concs_ihnb$Date == "429" |
                          gbm_data_concs_ihnb$Date == "430" | gbm_data_concs_ihnb$Date == "501" )
ihnb_post_plant <- which(gbm_data_concs_ihnb$Date == "502" | gbm_data_concs_ihnb$Date == "505" |
                           gbm_data_concs_ihnb$Date == "506" | gbm_data_concs_ihnb$Date == "507" |
                           gbm_data_concs_ihnb$Date == "508" | gbm_data_concs_ihnb$Date == "510" |
                           gbm_data_concs_ihnb$Date == "511" | gbm_data_concs_ihnb$Date == "512" |
                           gbm_data_concs_ihnb$Date == "513" | gbm_data_concs_ihnb$Date == "514" |
                           gbm_data_concs_ihnb$Date == "516" | gbm_data_concs_ihnb$Date == "519" |
                           gbm_data_concs_ihnb$Date == "520" | gbm_data_concs_ihnb$Date == "521" |
                           gbm_data_concs_ihnb$Date == "522" | gbm_data_concs_ihnb$Date == "523" |
                           gbm_data_concs_ihnb$Date == "524" | gbm_data_concs_ihnb$Date == "527" | gbm_data_concs_ihnb$Date == "603"
)
ihnb_plantdate_tests <- data.frame(matrix(NA, nrow=29, ncol=7))
colnames(ihnb_plantdate_tests) <- c("chemical","post_plant_mean","post_plant_sd","pre_plant_mean","pre_plant_sd","t_test_pvalue","cohens_d")
for(i in 7:35){
  pre_plant_concs <- as.numeric(unlist(gbm_data_concs_ihnb[ihnb_pre_plant, i]))
  #sd(pre_plant_concs)
  post_plant_concs <- as.numeric(unlist(gbm_data_concs_ihnb[ihnb_post_plant, i]))
  #sd(post_plant_concs)
  ihnb_plantdate_tests$chemical[i-6] <- colnames(gbm_data_concs_ihnb[i])
  ihnb_plantdate_tests$post_plant_mean[i-6] <- mean(post_plant_concs)
  ihnb_plantdate_tests$post_plant_sd[i-6] <- sd(post_plant_concs)
  ihnb_plantdate_tests$pre_plant_mean[i-6] <- mean(pre_plant_concs)
  ihnb_plantdate_tests$pre_plant_sd[i-6] <- sd(pre_plant_concs) 
  if(sd(pre_plant_concs)>0 | sd(post_plant_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(post_plant_concs, pre_plant_concs)
    conc_cohens_d <- cohen.d(post_plant_concs, 
                             pre_plant_concs, 
                             hedges.correction = T)
    ihnb_plantdate_tests$t_test_pvalue[i-6] <- conc_t_test$p.value
    ihnb_plantdate_tests$cohens_d[i-6] <- conc_cohens_d$estimate
    print(paste(colnames(gbm_data_concs_ihnb[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
ihnb_plantdate_tests

#create data frame with effects sizes by media and chemical
cohen_d_plantdate <- cbind(dbt_plantdate_tests$cohens_d, 
                         fp_plantdate_tests$cohens_d, 
                         ihbb_plantdate_tests$cohens_d, 
                         ihh_plantdate_tests$cohens_d, 
                         ihl_plantdate_tests$cohens_d, 
                         ihnb_plantdate_tests$cohens_d)
rownames(cohen_d_plantdate) <- dbt_plantdate_tests$chemical
colnames(cohen_d_plantdate) <- c("dbt", "fp", "ihbb", "ihh", "ihl", "ihnb")
# replace NAs with zeros
cohen_d_plantdate[is.na(cohen_d_plantdate)] <- 0

# write to file
cohen_d_plantdate_file <- paste(gbm_data_out,"/cohen_d_plantdate.csv",sep="")
write.csv(cohen_d_plantdate, cohen_d_plantdate_file)


col_fun = colorRamp2(c(-1, 0, 1), c("red", "white", "green"))
col_fun(seq(-1, 1))
date_compare_heatmap <- Heatmap(cohen_d_plantdate, name = "Cohen's d", col = col_fun)
date_compare_heatmap

# non parametric tests
#View(cohen_d_plantdate)
# within each matrix type
colnames(cohen_d_plantdate)
for(i in 1:6){
  print(colnames(cohen_d_plantdate)[i])
  print(as.vector(unlist(cohen_d_plantdate[which(cohen_d_plantdate[,i]!=0),i])))
  n_positive <- length(which(cohen_d_plantdate[,i]>0))
  n_total <- length(which(cohen_d_plantdate[,i]!=0))
  print(paste(n_positive, n_total))
  print(binom.test(n_positive, n_total)) 
}

# for the entire matrix
n_positive <- length(which(cohen_d_plantdate>0))
n_total <- length(which(cohen_d_plantdate!=0))
print(paste(n_positive, n_total)) 
print(binom.test(n_positive, n_total))
