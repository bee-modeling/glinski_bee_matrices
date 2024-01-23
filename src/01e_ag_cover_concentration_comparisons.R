gbm_data_binary
#View(gbm_data_binary)
colnames(gbm_data_binary)

# starting with the untransformed gbm_data_binary
# replace all NAs with lowest concentration observed for that chemical 
# (across media types)
gbm_data_concs <- gbm_data_binary[,1:35]
View(gbm_data_concs)
for(i in 7:35){
  # print(gbm_data_concs[,i])
  min_conc <- min(gbm_data_concs[,i], na.rm=T)/sqrt(2)
  # print(min_conc)
  # print(sum(is.na(gbm_data_concs[,i])))
  gbm_data_concs[is.na(gbm_data_concs[,i]),i] <- min_conc
}
View(gbm_data_concs)

## compare high ag to low ag dead bees by chemical
# we don't compare about date yet and are grouping sites by ag cover
dim(gbm_data_concs)
gbm_data_concs$ag_cover
gbm_data_concs$Media
unique(gbm_data_concs$Date)

# DBT 

gbm_data_concs_dbt <- gbm_data_concs %>% filter(Media == "DBT")
dim(gbm_data_concs_dbt)
#View(gbm_data_concs_dbt)
dbt_ag_high <- which(gbm_data_concs_dbt$ag_cover == "high")
dbt_ag_low <- which(gbm_data_concs_dbt$ag_cover == "low")
print("dead bee traps")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_dbt[dbt_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_dbt[dbt_ag_low, i]))
  #sd(low_ag_concs)
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
            low_ag_concs, 
            hedges.correction = T)
    print(paste(colnames(gbm_data_concs_dbt[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}

# FP 
gbm_data_concs_fp <- gbm_data_concs %>% filter(Media == "FP")
dim(gbm_data_concs_fp)
#View(gbm_data_concs_fp)
fp_ag_high <- which(gbm_data_concs_fp$ag_cover == "high")
fp_ag_low <- which(gbm_data_concs_fp$ag_cover == "low")
print("field pollen")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_fp[fp_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_fp[fp_ag_low, i]))
  #sd(low_ag_concs)
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    print(paste(colnames(gbm_data_concs_fp[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}

# IHBB 
gbm_data_concs_ihbb <- gbm_data_concs %>% filter(Media == "IHBB")
dim(gbm_data_concs_ihbb)
#View(gbm_data_concs_ihbb)
ihbb_ag_high <- which(gbm_data_concs_ihbb$ag_cover == "high")
ihbb_ag_low <- which(gbm_data_concs_ihbb$ag_cover == "low")
print("in hive bee bread")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihbb[ihbb_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihbb[ihbb_ag_low, i]))
  #sd(low_ag_concs)
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    print(paste(colnames(gbm_data_concs_ihbb[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}

# IHH 
gbm_data_concs_ihh <- gbm_data_concs %>% filter(Media == "IHH")
dim(gbm_data_concs_ihh)
#View(gbm_data_concs_ihh)
ihh_ag_high <- which(gbm_data_concs_ihh$ag_cover == "high")
ihh_ag_low <- which(gbm_data_concs_ihh$ag_cover == "low")
print("in hive honey")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihh[ihh_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihh[ihh_ag_low, i]))
  #sd(low_ag_concs)
  if(sd(high_ag_concs)>0 & sd(high_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, low_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    print(paste(colnames(gbm_data_concs_ihh[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}

# IHL 
gbm_data_concs_ihl <- gbm_data_concs %>% filter(Media == "IHL")
dim(gbm_data_concs_ihl)
#View(gbm_data_concs_ihl)
ihl_ag_high <- which(gbm_data_concs_ihl$ag_cover == "high")
ihl_ag_low <- which(gbm_data_concs_ihl$ag_cover == "low")
print("in hive larvae")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihl[ihl_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihl[ihl_ag_low, i]))
  #sd(low_ag_concs)
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    print(paste(colnames(gbm_data_concs_ihl[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}

# IHNB
gbm_data_concs_ihnb <- gbm_data_concs %>% filter(Media == "IHNB")
dim(gbm_data_concs_ihnb)
#View(gbm_data_concs_ihnb)
ihnb_ag_high <- which(gbm_data_concs_ihnb$ag_cover == "high")
ihnb_ag_low <- which(gbm_data_concs_ihnb$ag_cover == "low")
print("in hive nurse bees")
for(i in 7:35){
  high_ag_concs <- as.numeric(unlist(gbm_data_concs_ihnb[ihnb_ag_high, i]))
  #sd(high_ag_concs)
  low_ag_concs <- as.numeric(unlist(gbm_data_concs_ihnb[ihnb_ag_low, i]))
  #sd(low_ag_concs)
  if(sd(high_ag_concs)>0 & sd(low_ag_concs)>0){
    # t.test from stats package
    conc_t_test <- t.test(low_ag_concs, high_ag_concs)
    conc_cohens_d <- cohen.d(high_ag_concs, 
                             low_ag_concs, 
                             hedges.correction = T)
    print(paste(colnames(gbm_data_concs_ihnb[i]), conc_t_test$p.value, conc_cohens_d$estimate))
  }
}
