#Install and load supporting libraries.
print(Sys.info()[4])

R.Version()$version.string
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(knitr, quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(pheatmap)
library(tidyverse)
library(ggrepel)
library(ggplotify)
library(gt)
library(viridis)
library(hrbrthemes)
# hrbrthemes::import_roboto_condensed() #install required fonts if NA

print("list of loaded packages: ")
print((.packages()))

# map root directory to machine
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  gbm_root <- file.path("c:", "git", "glinski_bee_matrices")
}else if(Sys.info()[4]=="Stevens-Mac-Pro.local") {
  gbm_root <- file.path("~", "git", "glinski_bee_matrices")
}else if(Sys.info()[4]=="LZ2626UTPURUCKE"){
  gbm_root <- file.path("c:","git","glinski_bee_matrices")
} else if (Sys.info()[4]=="LZ26TPURUCKE-2"){ 
  # tom windows 2023 laptop
  gbm_root <- file.path("c:", "Users", "tpurucke", "git", "glinski_bee_matrices")
} else {
  print("fix root directory")
}

print(paste("Root directory location: ", gbm_root, sep=""))

gbm_data_in <- file.path(gbm_root, "data_in")
gbm_graphics <- file.path(gbm_root, "graphics")
gbm_src <- file.path(gbm_root, "src")

# reduced the bee data by detected in at least 2 media

#check to see if directories are accessible
boo = file.exists(file.path(gbm_data_in,"glinski_bee_matrix_data_reduced.csv"))
print(paste("check to see if R can access files OK: ", boo))

#cleaned up data set, manually reshaped
gbm_data <- read.csv(file.path(gbm_data_in,"glinski_bee_matrix_data_reduced.csv"), stringsAsFactors = TRUE)

dim(gbm_data)
summary(gbm_data)
colnames(gbm_data)
class(gbm_data$Media)
unique(gbm_data$Media)
class(gbm_data$ag_cover)
unique(gbm_data$ag_cover)
class(gbm_data$Site)
unique(gbm_data$Site)
class(gbm_data$Date)
unique(gbm_data$Date)
class(gbm_data$Sample.ID)
unique(gbm_data$Sample.ID)

#View(gbm_data)

# pesticide key for figures
pesticide_key <- read.csv(file.path(gbm_data_in,"pesticide_key.csv"), stringsAsFactors = TRUE)
dim(pesticide_key)

# run scripts
source(file.path(gbm_src, "00b_support_functions.R"))
source(file.path(gbm_src, "01a_cross_matrix_heatmap_concs.R"))
source(file.path(gbm_src, "01b_cross_matrix_heatmap_detfreqs.R"))
source(file.path(gbm_src, "01c_cross_matrix_stat_tests_detection_frequencies.R"))
source(file.path(gbm_src, "01d_cross_matrix_stat_tests_concentrations.R"))
source(file.path(gbm_src, "02a_summary_stat_figures.R"))
source(file.path(gbm_src, "02b_combined_manuscript_figure.R"))
source(file.path(gbm_src, "03a_ndetects_boxplots.R"))
source(file.path(gbm_src, "03b_cumulative_concs_boxplots.R"))
source(file.path(gbm_src, "03c_combined_agcover_boxplots.R"))
source(file.path(gbm_src, "04_stacked_boxplot_land_cover.R"))
source(file.path(gbm_src, "05a_chemical_class_boxplots_agcover.R"))
source(file.path(gbm_src, "05b_chemical_class_boxplots.R"))
