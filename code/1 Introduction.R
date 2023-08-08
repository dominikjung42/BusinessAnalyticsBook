#' chapter 1 - Introduction
#' contact: Dr. Dominik Jung, dominik.jung@junglivet.com

# libraries ---------------------------------------------------------------
# you can skip line 6-10 if the libraries are already installed
install.packages("devtools")
devtools::install_github("dominikjung42/dstools")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")

# load the libraries after installation into your R environment
library("dstools")
library("dplyr")
library("ggplot2")
library("readxl")

# business data understanding ---------------------------------------------
# 1. way to load the data_
dataset = read_excel("productionlog_sample.xlsx")

# 2. way to lad the data:
# dataset = data("productionlog_sample")

# Inspect the first 6 rows of your dataset
head(dataset)

# Show the whole dataset in tabular form
View(dataset)

# Compute some descriptive statistics
summary(dataset)

# business data preprocessing ---------------------------------------------
# Omit the column MONTH
dataset = subset(dataset, select = -c(MONTH))
names(dataset)

# Omit row 11 which has no values
dataset = na.omit(dataset)
nrow(dataset)

# business analytics ------------------------------------------------------
# plot suppliers
boxplot(TASTING ~ MALTING, data = dataset)

# compare suppliers by quality values
dataset_burns = subset(dataset, MALTING==c("Burns Best Ltd."))
mean(dataset_burns$TASTING)

dataset_inhouse <- subset(dataset, MALTING==c("Inhouse"))
mean(dataset_inhouse$TASTING)

plot(x=dataset$COLOR, y=dataset$TASTING)

# exercises ---------------------------------------------------------------
# Investigate other possible reasons for bad quality
boxplot(TASTING ~ SHIFT, data = dataset)
boxplot(TASTING ~ MALTING, data = dataset)
boxplot(TASTING ~ MANUFACTURER, data = dataset)

