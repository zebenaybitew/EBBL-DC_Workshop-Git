##################### Manipulating, analyzing and exploring data with tidyverse
#### dplyr and tidr
library(tidyverse)
surveys<-read_csv("data_raw/portal_data_joined.csv")
str(surveys)
select(surveys, plot_id, species_id, weight)

select (surveys, -record_id, -species_id) # selects two columns only 

filter(surveys, year==1995)# used to filter specific data  
filter(surveys, year==1995, sex=="M") # chooses only  ales in 1995

# to select variables with weight less than 5
surveys2<-filter(surveys, weight<5)
surveys_sml<- select(surveys2, species_id, sex, weight)
or
surveys_sml2<-select(filter(surveys, weight<5), species_id, sex, weight)

##### %>% the pipe function : similar to the above functions it choses species with weight less than five 
surveys %>%
  filter(weight<5) %>%
  select(species_id, sex, weight)

#### challenge , using pipes subset include animalsbefoire 1995 and only collumns year, sex, weight. 

surveys %>%
  filter(year<1995) %>%
  select(year, sex, weight)

##### the mutate function to create new columns in data 

surveys%>%
  mutate(weight_kg=weight/1000, weight_lb=weight_kg*2.2)%>%
  view()
##### remove nas
surveys%>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg=weight/1000, weight_lb=weight_kg*2.2)%>%
  view()

#######  to check if there are differences in wt between  male and female species 
# split-apply-combine-paradigm used to for this 
surveys%>%
filter(!is.na(sex))%>%
group_by(sex) %>%
  summarise(mean_weight=mean(weight, na.rm=T)) # removes nas in sex 

or 

surveys%>%
  filter(!is.na(weight))%>%
  group_by(sex) %>%
  summarise(mean_weight=mean(weight))
####### calculate mean using sex and species id
surveys%>%
  filter(!is.na(weight))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T))

###### print 15 lines
surveys%>%
  filter(!is.na(weight), !is.na(sex))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T))%>%
  print(n=15)

############ used to arrange in descendiong order 
surveys%>%
  filter(!is.na(weight), !is.na(sex))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T), min_weight=min(weight))%>%
  arrange(desc(min_weight))
############ used to arrange in ascending order 
surveys%>%
  filter(!is.na(weight), !is.na(sex))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T), min_weight=min(weight))%>%
  arrange(min_weight)


######### the count function 
surveys%>%
  count(sex, species)

##### arrange the data in descending order 
surveys%>%
  count(sex, species) %>%
  arrange(species, desc(n))

####### challenges how many animals were caught in the animal in each plot type 
surveys%>%
  count(plot_type)

####### challenges2 use group_by and summarise to commpute mean ,min and max of each speciciesusing species_id
surveys%>%
  filter(!is.na(hindfoot_length))%>%
  group_by(species) %>%
  summarize (
    mean_hindfoot_length = mean(hindfoot_length, na.rm=T), 
    min_hindfoot_length= min(hindfoot_length), 
    max_hindfoot_length= max(hindfoot_length),
    n=n()) %>%
  view()

####### challenges what is thew heaviest animal (year, genus, species,_id, and weight)

surveys%>%
  filter(!is.na(weight))%>%
  group_by(year) %>%
  filter(weight==max(weight)) %>%
  select(year, genus, species_id, weight) %>%
  arrange(year)%>%
  unique()
  



