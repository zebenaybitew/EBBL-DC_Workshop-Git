
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

#########
autoplot(sample_pca, 
         data = sample_info %>% mutate(minute=as.factor(minute)), 
                                       colour="minute",
                                       shape="strain")

         
test_result<-read_csv("data_rnaseq/test_result.csv")  

#### differential expression results 
# gene column-> gene name
# basMean column-> normalized expression of a gene
#log2Foldachnage column-> amount of change between two conditions 
# lfcSE column-> standard error associated to log2FoldCahnge value 
# star column -> statistics value compurted as log2FoldChange/lfcSE compared to the stamndard normal distribution
# pvalue-> p-value associated with the change 
# padj-> p-value corrected for multiple hypothesis testing
# comparison-> comparison group
# genes with hihger basmean value tend to change less 



######### MA plot
# generate MA plot (baseMean vs log2FoldChange and compare by time point; Hint: consider log-transform baseMean)
test_result %>% 
ggplot(aes(x = log10(baseMean) , y =log2FoldChange )) +
  geom_point(alpha=0.1) +
  facet_wrap(facets = vars(comparison)) 

######## 
test_result %>% 
  mutate(sig=ifelse(padj<0.01, log2FoldChange, NA))  %>% 
  ggplot(aes(x = log10(baseMean) , y =log2FoldChange )) +
  geom_point(alpha=0.1) +
  geom_point(aes(y=sig), color="tomato", size=1) +
  geom_hline(yintercept = 0, color="dodgerblue")+
  facet_wrap(facets = vars(comparison)) 
  

ma_plot<-test_result %>% 
  mutate(sig=ifelse(padj<0.01, log2FoldChange, NA))  %>% 
  ggplot(aes(x = log10(baseMean) , y =log2FoldChange )) +
  geom_point(alpha=0.1) +
  geom_point(aes(y=sig), color="tomato", size=1) +
  geom_hline(yintercept = 0, color="dodgerblue")+
  facet_wrap(facets = vars(comparison)) 

### combining two plots 
(ma_plot|pca_plot)

##### genes significantly associaeted or candidate genes 


# visualizing expresssion trends 
# step 1: to get candidate genes (aka padj<0.01)
candidate_gene<-test_result %>% 
  filter(padj<0.01) %>% 
  pull(gene) %>% # test_results [, "gene] aka test_result$gene: extraxts one column in to a vector 
  unique()
# 1a. get the trans_cts_long
trans_cts_long<-trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample",
               values_to = "cts") %>% 
  full_join(sample_info, by="sample")

# 2. filter trans_cts_long for candidate genes and compute mean expression value for each in each tp and genotype

trans_cts_mean<-trans_cts_long %>% 
  filter(gene%in%candidate_gene) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts=mean(cts), nrep=n( )) %>% 
  ungroup()

# 3. plot trends 
trans_cts_mean %>% 
  ggplot(aes(x=minute, y=mean_cts)) +
  geom_line(aes(group=gene), alpha=0.3)+
  facet_grid(rows = vars(strain))

# scaling data to improve visualization: z-score transform and the mean of all genes will be zero
trans_cts_mean<-trans_cts_long %>% 
  filter(gene%in%candidate_gene) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled=(cts-mean(cts))/sd(cts)) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts_scaled=mean(cts_scaled), 
            nrep=n( )) %>% 
  ungroup()

#### plot the scald graph is used for defferential expression analysis 
trans_cts_mean %>% 
  ggplot(aes(x=minute, y=mean_cts_scaled)) +
  geom_line(aes(group=gene), alpha=0.3)+
  geom_hline(yintercept = 0, color="brown", linetype="dashed")+
  facet_grid(rows = vars(strain)) +
  scale_x_continuous (breaks=unique(trans_cts_mean$minute)) # used to chnage the baseline scales 



######## clustering analysis (supervised and non supervised analysis)
# clustering analysis has two main steps
# 1. measuring geometric distance or correlation distance matrix
# 2. 

# clustering 
trans_cts<-read_csv("data_rnaseq/counts_transformed.csv")
# 1. crreate a matrix of counts 
hclust_matrx<-trans_cts %>% 
  select(-gene) %>% 
  as.matrix()
rownames(hclust_matrx) <-trans_cts$gene
hclust_matrx<-hclust_matrx[candidate_gene,]

dim(hclust_matrx)



# z score transform and transpose it
hclust_matrx<-hclust_matrx %>% 
  t() %>% 
  scale() %>% 
  t()

dim(hclust_matrx)
# 2. calculate distance matrix for the candidate gene




















  
  
  
  
  
  
  
  





