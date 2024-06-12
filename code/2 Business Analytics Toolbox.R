#' chapter 2 - Business Analytics Toolbox
#' contact: Dr. Dominik Jung, dominik.jung@junglivet.com

# libraries ---------------------------------------------------------------
library("dplyr")
library("ggplot2")
library("readxl")
library("dstools")

# first Steps in R-Studio to run R code -----------------------------------
1 + 1

string = "Hello World"
nchar(string)

string_2 = "Goodbye"
nchar(string_2)

# data structures ---------------------------------------------------------
X = 101
X = as.character(X)
is.numeric(X)

is.charcter(X)

my_vector = c(2,1,3,4)
funky_vector = c(TRUE, TRUE, FALSE)

really_funky_vector = c(my_vector, funky_vector)
really_funky_vector 

my_vector = c(2,1,3,4) # write your comment here 
my_vector

X = c(TRUE, 2, 3, 4)
X

my_vector = NULL #deletes the poor guy
my_vector

1/0
0/0

my_funky_vector2 = c(A=1, B=2.3, C=100)
my_funky_vector2

length(my_funky_vector2)

mode(my_funky_vector2)

c(1,2,3,4) + c(1,2)

my_factor = factor(c("medium", "rare", "rare"),
                  levels = c("blue", "rare", "medium", "medium rare", "well-done"))

unclass(my_factor)

table(my_factor)

my_matrix = matrix(
  + c(1, 2, 1, 1, 2, 2), # the elements
  + nrow=3, # number of rows
  + ncol=2, # number of columns
  + byrow= TRUE) # fill matrix by rows

my_matrix

my_array = array(1:24, dim=c(3,4,2))
my_array

funny_dog_names= c("Winnie, the Poodle", "Bark Twain", "OzzyPawsborne")
random_numbers= c(4,8,15,16,23,42)
coin_flips= c(TRUE, FALSE, FALSE, TRUE, TRUE)
stuff = list(funny_dog_names, random_numbers, coin_flips, 7)
stuff

my_data_frame = data.frame(Numbers=c(0,1,2), Booleans=c(TRUE, FALSE, TRUE))
my_data_frame

head(dataset)

# work with data ----------------------------------------------------------
dataset = read_excel("productionlog_sample.xlsx")

# or if dstools is installed and loaded just run
data("productionlog_sample")

View(productionlog_sample)

str(productionlog_sample)

# 1. possibility
my_vector = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# 2. possibility
my_vector = c(seq(10, 100, by=10))
my_vector

my_vector[1]
my_vector[2]

my_vector [seq(1, 3)]

my_vector[c(1,3,2)]

my_vector[-1] 

my_vector[c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)]

my_vector[my_vector >= 90]

names(my_vector) = c("Holger", "Samantha", "Bernd", "Anna", "Fred", "Nicky", "Louis", "Jennifer", "Jack", "Zoe")
my_vector
my_vector["Zoe"]
my_vector[c("Zoe")]

beauty_contest = list(dogs=funnyDogNames, rating=coinFlips)
beauty_contest$dogs

beauty_contest[[1]]

my_matrix = matrix(1:9, nrow = 3)
my_matrix

my_matrix[1, 2]

my_dataframe = data.frame(x = c(1,1,2), y = c(2,1,2))
my_dataframe[my_dataframe$x== 2, ]

my_dataframe$x

# control flows -----------------------------------------------------------
a=2
b=1
c=0

if(a>b){
  print("a>b")
}

ifelse(a>b, "Yo", "No")

if(a>b){
  print("1")
}else{
  print("2")
}

if(a>b){
  print("1")
}else if(a>c){
  print("2")
}else{
  print("3")
}

#structure
foo = switch(
  expression,
  case1,
  ...,
  casen
)
#example
foo = switch(
  2,
  "red",
  "green",
  "blue")
foo

for(i in 1:3){
  print("ho")
}

#structure
repeat {
  statement
}
#example
a = 2
repeat {
  print(a)
  a=a+1
  if(a==5){
    break
  }
}

#structure
while(cond){
  doing
}
#example
a = 2
while (a < 6) {
  print(a)
  a = a+1
}

# manage your projects ----------------------------------------------------
# convertion of dataframe and tibble
dataset = read_excel("productionlog_sample.xlsx")
head(dataset) #tibble

dataset_new = as.data.frame(dataset)
head(dataset_new) #df

# useful functions --------------------------------------------------------
a = 1
b = 2
print(a,b)

paste(a,b)

my_date = as.Date("2022-11-22")

as.Date("8/17/2023",format="%m/%d/%Y")
as.Date("April 20, 1989",format="%B %d, %Y")

my_date = as.Date("2022-11-22")
weekdays(my_date)

