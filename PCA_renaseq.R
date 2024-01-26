
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
sample_info<-read_csv("data_rnaseq/sample_info.csv")

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
summary (sample_pca)
# the first 10 raws
pca_matix[1:10, 1:5]

as_tibble(pca_matix)
as_tibble(pca_matix, rownames("sample"))

#### eigen values
pc_eigenvalues<-sample_pca$sdev^2

# create table to plot 
pc_eigenvalues<-tibble(pc=factor(1:length(pc_eigenvalues)),
                       variance=pc_eigenvalues) %>% 
  mutate(pct=variance/sum(variance)*100) %>% 
  mutate(pct_cum=cumsum(pct))

###### pareto plot/chart is a combination of 
pc_eigenvalues %>% 
  ggplot(aes(x=pc)) +
  geom_col(aes(y=pct)) +
  geom_line(aes(y=pct_cum, group=1)) +
  geom_point(aes(y=pct_cum)) +
  # to know the number of PCAs that explain 90% of theb variance
  #geom_hline(yintercept = 90) +
  labs(x="principal component", y="Fraction variance explained")
  
######## visualize 
pc_scores<-sample_pca$x%>% 
  as_tibble(rownames="sample")
# x represents each of the samples in  36 dimensions 

######### 

pc_scores %>% 
  ggplot(aes(x=PC1, y=PC2)) +
  geom_point() 

### 
pca_plot<-pc_scores %>% 
  full_join(sample_info, by="sample")%>% 
  ggplot(aes(x=PC1, y=PC2,
             color=factor(minute),
            shape= strain)) +
  geom_point()

##########
pc_loadings<-sample_pca$rotation %>% 
as_tibble(rownames="gene")

top_genes<-pc_loadings%>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loading))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

pc_loadings<-pc_loadings%>%     # is used to store plots 
  filter(gene%in% top_genes)

loadings_plot<-ggplot(data=pc_loadings) +
  geom_segment(aes(x=0, y=0, xend=PC1, yend=PC2),
               arrow = arrow(length = unit(0.1,"in")),
              color="brown" )+
  geom_text(aes(x=PC1, y=PC2, label=gene),
            nudge_y = 0.005, size=3) +
  scale_x_continuous(expand = c(0.02, 0.02))
# creating pappers with multiple pannel

library(patchwork)

(pca_plot|pca_plot|pca_plot/loadings_plot) +
  plot_annotation(tag_levels = "A")

library(ggfortify)
autoplot(sample_pca)

autoplot(sample_pca, data = sample_info, 
         colour="minute", shape="strain")


####### short cut to compute results
library(broom) # provides a function called tidy

tidy(sample_pca, matrix="eigenvalues")

tidy (sample_pca, matrix="loadings")














