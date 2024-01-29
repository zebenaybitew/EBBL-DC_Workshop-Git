########## Create four objects 
library(tidyverse) # should be the first thing to read the data 

raw_cts<-read_csv("data_rnaseq/counts_raw.csv")
trans_cts<-read_csv ("data_rnaseq/counts_transformed.csv")
sample_info<-read_csv ("data_rnaseq/sample_info.csv")
test_result<-read_csv("data_rnaseq/test_result.csv")

##### create a longer format 

trans_cts_long<-trans_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

####### combining tables using a join function in tidyverse 

trans_cts_long <-full_join(trans_cts_long, sample_info, by= "sample")

trans_cts_long %>% 
  ggplot(aes(x=cts)) +
  geom_freqpoly() 

####### separated by replicate 
trans_cts_long %>%
  ggplot(aes(x=cts, color=replicate)) +
  geom_freqpoly(binwidth=1) # use to adjust the binwidth

##### separate the plot based on the strain and teh time point using facet.grid
trans_cts_long %>%
  ggplot(aes(x=cts, color=replicate)) +
  geom_freqpoly(binwidth=1) +
  facet_grid(rows = vars(strain), cols=vars(minute))

##### challenge 1
raw_cts_long<-raw_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

##### combine the data using full join 
raw_cts_long <-full_join(raw_cts_long, sample_info, by= "sample")

##### plot a histogram using frequency polygon
raw_cts_long %>% 
  ggplot(aes(x=cts)) +
  geom_freqpoly() 

##########  plot a histogram using frequency polygon separating using replicate 
raw_cts_long %>%
  ggplot(aes(x=cts, color=replicate)) +
  geom_freqpoly()

############ grouping the data using facet.grid function 
raw_cts_long %>%
  ggplot(aes(x=cts, color=replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols=vars(minute))


########## transform it to log since the first polygon is not informative 
raw_cts_long %>%
  ggplot(aes(x=cts, color=replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols=vars(minute)) +
  scale_x_log10()

######### log transform the counts 
raw_cts_long %>%
  ggplot(aes(x=log10(cts), colour=replicate))+
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols=vars(minute))

#########remove the 645 warning messages, are due to zero values in the data 
#r considers log zero as an infinitive and it will pop is as an infinite value and will not be plotted 
log10(1) #in this situation we can change zero values to an artificial 1 and convert it to log10
log10(0)

############  removing the warnings by adding 1 at the aes
raw_cts_long %>%
  ggplot(aes(x=log10(cts+1), colour=replicate))+
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols=vars(minute))

######## adding binwidth in the graph 
raw_cts_long %>%
  ggplot(aes(x=log10(cts+1), colour=replicate))+
  geom_freqpoly(binwidth=1) +
  facet_grid(rows = vars(strain), cols=vars(minute))

########## instead of the frequency polygon lets try a box plot, box plot need box x and y-axis
# factor minute(): is used to change numeric to factor, coz it is a must 
# the color in histogram should be changed to fill function in the box plot 
raw_cts_long %>%
  ggplot(aes(x=factor(minute), y=log10(cts+1), fill=strain))+
  geom_boxplot() +
  facet_grid(cols=vars(replicate)) 

######### scatter plot : correlation between wt sample at T0 and T30
# using wide table is nice 
trans_cts %>% 
  ggplot(aes(x=wt_0_r1, y=wt_30_r1))+
  geom_point() +
  geom_abline(colour="brown")

######### check between replicates
trans_cts %>% 
  ggplot(aes(x=wt_0_r1, y=wt_0_r2))+
  geom_point() +
  geom_abline(colour="brown")

########## to look at the correlation of count data across all samples in our experiment 
trans_cts_corr<-trans_cts %>% 
  select(-gene) %>% 
  cor(method = "spearman")
  
####### to plot a correlation plot 
library(corrr) 

####### heatmap
rplot(trans_cts_corr)

####### to make the x-axis readable 
rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######  compare trans_cts and raw_cts 
summary(raw_cts_long$cts)
summary (trans_cts_long$cts)

raw_cts %>% 
  ggplot(aes(x=wt_0_r1, y=wt_0_r2)) +
  geom_point()
######## transforming the data in to log to to makle the data more understandably 
raw_cts %>% 
  ggplot(aes(x=wt_0_r1 +1 , y=wt_0_r2 +1)) +
  geom_point() +
  scale_x_continuous(trans="log2") +
  scale_y_continuous(trans="log2")

########## ploting mean on the x-axis and the variants in the y-axis 
raw_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts=mean(cts), var_cts= var(cts)) %>% 
  ggplot(aes(x=mean_cts, y=var_cts)) +
  geom_abline(colour="brown") +
  geom_point()+
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")

########
trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts=mean(cts), var_cts= var(cts)) %>% 
  ggplot(aes(x=mean_cts, y=var_cts)) +
  geom_point()

######## to color outliers above four
trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts=mean(cts), var_cts= var(cts)) %>% 
  mutate(above_four=var_cts>4) %>% 
  ggplot(aes(x=mean_cts, y=var_cts, colour= above_four)) +
  geom_point()










































