########### data visualization using ggplot2

library(ggplot2)
#### Assign the ggplot
plt<-ggplot(
  data = surveys_complete,
  mapping = aes(x=weight, y=hindfoot_length)
)
 str(plt)
 
#### to plot the graph, we can use 
 plt +
   geom_point()
 
 #### add titles 
 plt +
   geom_point() +
   ggtitle("My first plot")
 # geom_ stands for gemeotric function 
 
 plt<-ggplot(data = surveys_complete, mapping = aes(x=weight, y=hindfoot_length)) +
   geom_point() 
 plt +
   ggtitle("weight vs hindfoot_length") 
 
install.packages("hexbin")  # used for hexagonal heatmap of 2d bin counts 
library(hexbin)

###### it adds or counts the number of observation 
ggplot(data = surveys_complete, mapping = aes(x=weight, y=hindfoot_length)) +
  geom_hex()
####### alpha is used to check the trasparency of the plots 
ggplot(data = surveys_complete, mapping = aes(x=weight, y=hindfoot_length)) +
  geom_point(alpha= 0.1) 

######## change color 
ggplot(data = surveys_complete, mapping = aes(x=weight, y=hindfoot_length)) +
  geom_point(alpha= 0.1, color="blue") 

####### change color by variable name
ggplot(data = surveys_complete, mapping = aes(x=weight, y=hindfoot_length)) +
  geom_point(alpha= 0.25, aes(color=species_id)) 
####### or in more clear way 
ggplot(
  data = surveys_complete,
  mapping = aes(
    x=weight, 
    y=hindfoot_length,
    color=species_id
    )
  ) +
   geom_point(alpha= 0.25) 


####### challenge§ scatter plot of weightvs speciesId and color by plot_type

ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) +
  geom_point(alpha= 0.25, aes(color=plot_type)) 

#######  better to use box plot for the above plot 
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) +
  geom_boxplot() 

####### addind a liitle vale for each x value using  jitter
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) +
  geom_boxplot(outlier.shape = NA) +    # it removes the shapes of the outlines
  geom_jitter(alpha=0.3, color="salmon")
######## 
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) +
  geom_boxplot(outlier.shape = NA) +    # it removes the shapes of the outlines
  geom_jitter(alpha=0.3, aes(color=plot_type))
######### 
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) +
    geom_jitter(alpha=0.3, color="salmon") +
   geom_boxplot(outlier.shape = NA, fill=NA)

########### produce a violin plot of weight by species_id
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=weight)) +
  geom_violin ()+
  scale_y_log10() +# used to make it more visible 
  ylab("weight(log10)") 

###### challenge: make a box plot + jittered scatter plot of hindfoot_length by species_id: box plt should be in fromnt off the dot filled with white 
ggplot(data = surveys_complete, mapping = aes(x=species_id, y=hindfoot_length)) +
  geom_jitter(alpha=0.3, aes(color= factor(plot_id))) + # the factor function is added coz it considres plot_id as continous variable 
  geom_boxplot (outlier.shape = NA)

######## writing a line function nusing a time series data 
yearly_count<-surveys_complete %>% 
  count(year, genus)
###### a line function nfor a time series data 
ggplot(data=yearly_count, mapping = aes(x=year, y=n, color=genus, group=genus )) +
  geom_line() 
  
######## ggplot can be integrated to the tidyverese package or function will provide the same plot

yearly_count %>% 
  ggplot(data=yearly_count, mapping = aes(x=year, y=n, color=genus)) +
  geom_line() 

#######  the same graph combining ggplot with tidyverse
yearly_count_graph<-yearly_count %>% 
  count(year, genus) %>% 
  ggplot(data=yearly_count, mapping = aes(x=year, y=n, color=genus)) +
  geom_line() 
yearly_count_graph

 ####### a matrix of plots using faceting  gives separate graphs 
ggplot(data=yearly_count, mapping = aes(x=year, y=n)) +
  geom_line() +
  facet_wrap(facets = vars(genus))
  
######## drawing data classified based on gender 
surveys_complete %>% 
  count(year, genus, sex) %>% 
ggplot(mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus))

####### 

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_grid(rows = vars(sex), cols = vars(genus))

########

surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_grid(rows = vars(genus))

#########
surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_grid(rows = vars(genus))

########
surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  theme_bw() # improves the resolution of functions 

######## to change the base size 
surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(mapping = aes(x=year, y=n, color=sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  theme_bw(base_size = 18)
#########
plt<-surveys_complete %>% 
  count(year, genus, sex) %>% 
  ggplot(
    mapping = aes(
      x=year, 
      y=n, 
      color=sex)) +
  geom_line() +
  facet_wrap(facets = vars(genus)) +
  scale_color_manual(
    values = c("tomato", "dodgerblue"),
    labels= c("female", "male"),
    name="Sex") +
  xlab("year of observation") +
  ylab("number of individulas") +
  ggtitle("Observed genera over the time")
  theme_bw(base_size = 14) +
    theme(
      legend.position="bottom", 
      aspect.ratio=1, 
      axis.text.x=element_text(
        angle=45, 
        hjust=1
      ),
      plot.title = element_text(hjust = 0.5),
      panel.grid=element_blank(), 
      strip.background = element_blank()
         )
 plt
####### 
ggsave(filename = "data/plot.pdf", plot = plt, width = 20, height = 20)





#?rgb # used to know the color
#rgb(red=.3, green=.3, blue = .3)




 