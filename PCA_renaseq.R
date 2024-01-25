
################################# 
#################################
# PCA and how is it used in the analyses 
# PCA: is demionstionality rredcuction technique 
# the varition is explained by variance and it is used to classify gennercs based on thier relevalmnce 
# variance: how spread two points are 
# it tries to captutre the variance in the best representative way
# eigen vectors are used in PCA
# the number of pPCA's are the number of dimension that we have 
# the numebr of pCA is determined by the data 
# the sacle is the square of standard devaitions 
# Eigen values are the squre roots of Sds
# pca can't count missing data



###############
library(tidyverse)

trans_cts<-read_csv("data_rnaseq/counts_transformed.csv")
sample_infor<-read_csv("data_rnaseq/sample_info.csv")

############ converts the data to matrix format  before running PCA

pca_matix<-trans_cts %>% 
  column_to_rownames("gene") %>% 
  # as matrix convers the data to matrix
  as.matrix() %>% 
  t()   
# t () transpose matix 

##### run PCA
sample_pca<-prcomp(pca_matix)
# explore the data 
class(sample_pca)
str(sample_pca)














