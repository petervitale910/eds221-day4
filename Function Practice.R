
#### Creating Function #### 

#all in base

#adds up birds and dogs
birddog_sum <- function(bird, dog){
  pets <- bird + dog
  return(pets)
}

#use it
total_pets <- birddog_sum(4,7)


#create function to double values 
double_up <- function(x){
  print(2* x)
}

double_up(34.5)

# write a function with conditionals
# example is converting animals' ages
animal_age <- function(animal, age){
if (animal == "dog"){
  print(age * 7)
} else if (animal == "goat"){
  print(age * 4.7)
}else{
  paste0("WARNING ", animal, " ", "not accepted")
}
}

#try using for 8 year old dog
animal_age(animal = "dog", age = 8)

#lets try using for a cow
animal_age("cow", 8) #no error message

#I like my error solution but it doesnt stop the code

animal_age_stop <- function (animal, age){
  if (!animal %in% c("dog", "goat")){
    stop("Sorry we only can process dogs and Goats at this time")
  }
  if (is.numeric(age)== FALSE){
    stop("Age must be a number")
  }
  if (age <= 0 | age > 50){
    warning("Are you sure?")
  }
  if (animal == "dog"){
    print(age * 7)
  } else if (animal == "goat"){
    print(age * 4.7)
  }else{
    paste0("WARNING ", animal, " ", "not accepted")
  }
}

animal_age_stop("dog", -10)

# Functions with for loops

# all data frames in function called df
df_means <- function(df){
for(i in 1:ncol(df)){
  if (is.numeric((df[[i]])) == FALSE){
    print(paste(colnames(df[[i]]), " Is not numeric: cant compute mean "))
  } else{
  col_mean <- mean(df[[i]], na.rm = TRUE)
  column_name <- colnames(df[i])
  print(paste("mean value of", column_name, "is", col_mean))
  }
}
}

df_means(palmerpenguins::penguins)

##### Logistic Growth function ####

legistic_growth <- function(N0, K, R, time){
  NT <- K / (1+((K-N0))/N0 * exp(-R * time))
             print(NT)}
#Check for one set of vals
legistic_growth(100, 6000, .27, 40)


#working on an example just dealing with time
time_vec <-  seq(from = 0, 
                 to = 35,
                 by = .1)
#apply logistic growth function to the vector

pop_35 <- legistic_growth(100, 6000, .27, time_vec)

#combine time steps and pop size
pop_time_34 <- data.frame(time_vec, pop_35)

#plot it

pacman::p_load("tidyverse")
ggplot(pop_time_34, aes(x= time_vec, y = pop_35))+
  geom_line(size = .5)

#alternitavely with an internal loop
#storage for output
pop_35_vec <- vector(mode = "numeric", length = length(time_vec))
for(i in seq_along(time_vec)){
  pop_35 <- legistic_growth(100, 6000, .27, time_vec[i])
  pop_35_vec[i] <- population
  
}

#Now building to estimating over growth rates

r_seq <- seq(.2,
             .4,
             .01)

out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq)) ###series of growth rates
pop_35_mat <- vector(mode = "numeric", length = length(time_vec))
for (j in seq_along(r_seq)){
for(i in seq_along(time_vec)){
  pop_35 <- legistic_growth(100, 6000, r_seq[j], time_vec[i])
  out_matrix[i,j]<- pop_35
}
}


#data wrangling to plot
#add time as a variable and wrangle into df
out_df <-  data.frame(out_matrix, time = time_vec)

#Update colnames
colnames(out_df) <- c( paste0("gr_",r_seq),"time")

out_df_long <- pivot_longer(out_df, cols = -time, names_to = "growth_rate", values_to = "population")

#Plot it
ggplot(out_df_long, aes(x = time, y = population))+
  geom_line(aes(color = growth_rate))+
  theme_linedraw()+
  theme(legend.position = "none")
