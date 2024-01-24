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

