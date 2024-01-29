###################### tabular data or data frames: data.frames

#lets import some data is from online version: the data is nowhere in R
download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")


####### read data in R 
library(tidyverse)# this library is used to read this large data in R

### read the data using read_csv command 
surveys<- read_csv("data_raw/portal_data_joined.csv")

read_csv() # is used to read data in more clear format
read.csv() # is not robust as comared to read_csv

#### to check the first six columns of the data
head(surveys)
view(surveys)

##### to know the structure of the data
 str(surveys)

 ###### dim to know how long is the data.frame (the number of columns and rows)
 dim(surveys)
 ######## other exploratory analyses 
 ncol(surveys)
nrow(surveys) 
tail(surveys)
names(surveys) # gives the column names
colnames(surveys)
summary(surveys) # summarizes the column values 

######## indexing and subseting 
surveys[1,6] # to choose the first row and the six column 
surveys[1,] # will provide the whole first raw
surveys[,1]# gives the whole first column
surveys[c(1,2,3), c(5,6)] # choses the first three raws and the 5th and 6th column
surveys[1:3, 5:6] # choses the first three raws and the 5th and 6th column

surveys[1:3]# choses the whole three columns
surveys[,-1]# gives all data exept first column
surveys [,"sex" ] # to choose sex 
surveys["sex"]
surveys$plot_id
surveys$sex

###### challenge 
surveys_200<-surveys[200,] # only data chose the 200th raw

surveys[nrow(surveys),] # used to indicate the last row

nrow(surveys)/2
surveys[nrow(surveys)/2, ]# used to chose the middle data frame


my_list<-list(names=c("lora", "Lisanna", "Francesco"), 
              money=c(1,6,7,3,5,8))
my_list[[1]]
my_list$names


### changing variables to factors  : useful to handle data frames and it only contains the pre-defined levels
str(surveys)
surveys$sex<-factor(surveys$sex) #it will automatically level female 1 and male 2 sequentially 
levels(surveys$sex)
nlevels(surveys$sex)# will provide us the number of levels 

sex<-factor(c("male", "female", "female", "male"))
sex<-factor(sex, levels = c("male", "female"))

######## challenge chamge taxa and genus to a factor 
surveys$taxa<- factor(surveys$taxa)
levels(surveys$taxa)
sum(surveys$taxa=="Rabbit") #to know the number of rabbits in taxa
summary(surveys$taxa)

surveys$genus<- factor(surveys$genus)
nlevels(surveys$genus)# 26 genus
levels(surveys$genus)

######## convert factors
as.character(sex)
year_fct<-factor(c(1990, 1983, 1977, 1997))
as.numeric(year_fct) # 
# we can always change numeric to character with out losing the data 
as.numeric(as.character(year_fct))
as.numeric(levels(year_fct)) [year_fct] # the squre bracket provides the levels for each factor

########## remaining factors 
plot(surveys$sex)
summary(surveys$sex)
sex<-surveys$sex
levels(sex)
sex<-addNA(sex)# to add NA as a level
levels(sex)

levels(sex) [3]<-"undetermined" # is used to substitute NA to Undetermined 

levels(sex)
plot(sex)

####### challenge 

levels(sex) [1:2]<-c("female","male")
plot(sex)

sex<-factor(sex, levels=c("undetermined","female" , "male")) # used to rearrange the order of the levels or charterers 

plot(sex)


