##################### Manipulating, analyzing and exploring data with tidyverse
######## dplyr and tidr
library(tidyverse)

######## read the data
surveys<-read_csv("data_raw/portal_data_joined.csv")

######## Exploratory analysis of the survey data 
str(surveys)
select(surveys, plot_id, species_id, weight) # selects three columns form the survey data 

select (surveys, -record_id, -species_id) # selects two columns only 

filter(surveys, year==1995)  # used to filter specific data; it removes the NA values automatically 

filter(surveys, year==1995, sex=="M") # chooses only  males in 1995

######## to select variables with weight less than 5; filter or select functions can be used 

surveys2<-filter(surveys, weight<5)
surveys_sml<- select(surveys2, species_id, sex, weight)
or
surveys_sml2<-select(filter(surveys, weight<5), species_id, sex, weight)


####### %>% the pipe operator works with dolyr package : similar to the above functions it chooses species with weight less than five 
surveys %>%
  filter(weight<5) %>%
  select(species_id, sex, weight)

######## challenge , using pipes subset include animals before 1995 and only columns year, sex, weight. 
surveys %>%
  filter(year<1995) %>%
  select(year, sex, weight) 

######## the mutate function to create new columns in data 
surveys%>%
  mutate(weight_kg=weight/1000, weight_lb=weight_kg*2.2)%>%
  view()

######## remove "NAs"
surveys%>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg=weight/1000, weight_lb=weight_kg*2.2)%>%
  view()

######## to check if there are differences in wt between  male and female species 
#### split-apply-combine-paradigm used to for this 
surveys%>%
filter(!is.na(sex))%>%
group_by(sex) %>%
  summarise(mean_weight=mean(weight, na.rm=T)) # removes nas in sex 

or 

surveys%>%
  filter(!is.na(weight))%>%
  group_by(sex) %>%
  summarise(mean_weight=mean(weight))   # calculates the mean for NAs

######## calculate mean using sex and species id
surveys%>%
  filter(!is.na(weight))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T)) %>% 
  view()

###### print 15 lines
surveys%>%
  filter(!is.na(weight), !is.na(sex))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T))%>%
  print(n=15)

########  to arrange in descending order 
surveys%>%
  filter(!is.na(weight), !is.na(sex))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T), min_weight=min(weight))%>%
  arrange(desc(min_weight))

######## used to arrange in ascending order 
surveys%>%
  filter(!is.na(weight), !is.na(sex))%>%
  group_by(sex, species_id) %>%
  summarise(mean_weight=mean(weight, na.rm=T), min_weight=min(weight))%>% # na.rm removes NAs in species_ID
  arrange(min_weight) 
 
######## the count function 
surveys%>%
  count(sex, species, rm.na=T) 
####### remove NAs from sex and species 
surveys%>%
  filter(!is.na(sex), !is.na(species))%>%
  count(sex, species) %>% 
  view()

######## arrange the data in descending order 
surveys%>%
  count(sex, species) %>%
  arrange(species, desc(n)) 

######## challenge 1: how many animals were caught in the animal in each plot type 
surveys%>%
  count(plot_type)

####### challenges2: use group_by and summarize to commute mean ,min and max of hindfoot_length grouping by species 
surveys%>%
  filter(!is.na(hindfoot_length))%>%
  group_by(species) %>%
  summarize (
    mean_hindfoot_length = mean(hindfoot_length, na.rm=T), 
    min_hindfoot_length= min(hindfoot_length), 
    max_hindfoot_length= max(hindfoot_length),
    n=n()) %>%
  view()

####### challenge3: what is thew heaviest animal (year, genus, species,_id, and weight)

surveys%>%
  filter(!is.na(weight))%>%
  group_by(year) %>%
  filter(weight==max(weight)) %>%
  select(year, genus, species_id, weight) %>%
  arrange(year)%>%
  unique() # used to remove repeated files

#########  compare mean among groups of data using a wider data frame 
 
surveys_gw<-surveys %>%
  filter(!is.na(weight)) %>%
  group_by(plot_id, genus) %>%
  summarize(mean_weight=mean(weight)) %>% 
 view()

######## change the above result to a wider table 
surveys_gw %>%
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0) %>% 
  view()

######### how to go from wide to long format, the opposite of the above function 
survey_wide<-surveys_gw %>%
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)

Survey_long<-survey_wide %>% 
  pivot_longer(names_to =  "genus", values_to = "mean_weight", cols = -plot_id)
  
str(Survey_long)
str(surveys_gw)

######## challenges plot mean values of hindfoot_length and weight and the differences in measurements in each year using
#pivot_longer function

surves_long<-surveys %>%
  pivot_longer(names_to =  "measurement", values_to = "value", cols = c(hindfoot_length, weight))

######## challenge2 calculate the average of each measurement for each year based on plot type 

surves_long %>%
  group_by(year, measurement, plot_type) %>%
  summarize(mean_value=mean(value, na.rm = T)) %>% 
  pivot_wider(names_from = measurement, values_from = mean_value) %>% 
  view()


########  exporting data
surveys_complete<-surveys %>% 
  filter(!is.na(weight),
         !is.na(hindfoot_length),
         !is.na(sex))
write_csv(surveys_complete, file = "data_raw/surveys_complete.csv")


#########################################################################
#########################################################################
         ## END!!!























