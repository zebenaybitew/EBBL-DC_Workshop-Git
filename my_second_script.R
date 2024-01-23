########### vector 
weight_g<-c(50,60,65,82)# all eleements in a vcector should be the same type 
animals<-c("mouse", "rat", "dog")

length(animals)# is used to know teh number of values in a vector 

class(animals) # is used to know the type of data 
 
str(animals) # is use dto know the structure of the data 

#how to add an element tot he begning of the vector 
animals<-c("cincilla", animals)
animals<-c(animals,"frog")


typeof(animals) # similar with class
 
##### challenge 
num_char<-c(1,2,3,"a") # it choses characters 
class(num_char)
num_logical<-c(1,2,3, TRUE) # converst to numeric 
char_logical<-c("a", "b", "c", T)# converts to characters 
tricky<-c(1,2,3,"4")# converts to character 

# logical;<_numeric<-chracter # the opposite won't work
# logical<-character # the opposite won't work

############ subseting vectors or accessing specific values in the vector 

animals[2] # we use the square parenthesis 
animals[c(1,2)]# is used subset out of animals 

more_animals<-animals[c(1,2,3,2,1,4)]# will write animals sequentially using the mother documemnt 

weight_g[c(FALSE, FALSE, TRUE, TRUE)]# will only keep the last two elements 

weight_g>63

weight_g[weight_g>63] # choose only two values that are greater than 63

weight_g[weight_g>63 & weight_g<80]
weight_g[weight_g<58|weight_g>80]
# logica;l operators 
#<, >, ==, !=(different), <=, >=
weight_g==65
animals[animals=="rat"| animals=="frog"]
# %in% helps us find alll elements corresponding to a venctor of elememnts of our choices

animals%in% c("rat", "frog", "cat", "duck", "dog") # shows values irrespective of their positions 

animals[animals%in% c("rat", "frog", "cat", "duck", "dog")] # shows exact names 


# an example of a vector with missing data 

heights<-c(2, 4, 4, NA, 6)

mean(heights, na.rm=TRUE)# will remove NA before the calculation 
max(heights,na.rm=TRUE )
# identify the missing data
is.na(heights) 
heights[!is.na(heights)] # used to remove "NAs" in the data
# ommit the missing data
na.omit(heights)
# extract the complete cases
heights[complete.cases(heights)]

heights<-c(63, 69, 60, 65, NA, 68, 61, 70, 61, 59, 64, 69, 63, 6)
heights_no_na<-heights[!is.na(heights)] # used to choose comlete data 
median(heights, na.rm = TRUE) # used to calculate the median
median(heights_no_na) # used to calculate the median
heights_no_na[heights_no_na>67] # used to choose people greater that height 67 inches 

length (heights_no_na[heights_no_na>67]) # is use to count the number of values 

