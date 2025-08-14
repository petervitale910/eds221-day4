
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
