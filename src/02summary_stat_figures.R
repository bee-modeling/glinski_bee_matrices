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

# get detection frequncies based on nas
