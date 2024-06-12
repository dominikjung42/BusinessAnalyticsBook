#' chapter 6 - Business Data Products
#' contact: Dr. Dominik Jung, dominik.jung@junglivet.com

# libraries ---------------------------------------------------------------
# General libraries for this chapter
library("dplyr")
library("ggplot2")
library("readxl")

# automated reportings ----------------------------------------------------
install.packages("officer", dependencies = TRUE)
library(officer)

# read in our pptx template 
template_pptx = read_pptx("Projectslides Officer Example.pptx")
layout_summary(template_pptx)

# add a new slide with a title
template_pptx = read_pptx("Projectslides Officer Example.pptx")

template_pptx = add_slide(template_pptx, layout = "r_content_1", master = "business_data_analytics") 

template_pptx = ph_with(
  x = template_pptx, value = "Onlineshop Payment Methods",
  location = ph_location_label(ph_label = "title")
) 

print(template_pptx, target = "reporting.pptx")


# add full reporting
template_pptx = read_pptx("Projectslides Officer Example.pptx")

onlineshop = read_excel("onlineshop.xlsx")

plot_payments = ggplot(data=onlineshop) +
  aes(x=PAYMENT_METHOD) +
  geom_bar() +
  labs(x="", y="Frecquency") +
  ggtitle("Prefered Payment Methods in Junglivet Onlineshop")

plot_description = paste("The plot shows the amount of payments per payment method from ",
                         min(onlineshop$DATE),
                         " to ",
                         max(onlineshop$DATE),
                         sep="")

template_pptx <- add_slide(template_pptx, layout="r_content_1", master="business_data_analytics") 

template_pptx <- ph_with(
  x=template_pptx,
  value="Onlineshop Payment Methods",
  location=ph_location_label(ph_label = "title")
)

template_pptx <- ph_with(
  x=template_pptx,
  value=plot_payments,
  location=ph_location_label(ph_label="plot")
)

template_pptx <- ph_with(
  x=template_pptx,
  value=plot_description,
  location=ph_location_label(ph_label = "text")
)

print(template_pptx, target = "reporting.pptx")


# posters -----------------------------------------------------------------
# there is no code in this section

# notebooks ---------------------------------------------------------------
# the code for this section is in a seperate file "5 notebooks.rmd"

# dashboards --------------------------------------------------------------
# the code for this section is in a seperate file "5 dashboards.rmd"
install.packages("flexdashboard", dependencies = TRUE)


# recommender system ------------------------------------------------------
# install.packages("recommenderlab", dependencies = TRUE)
library(recommenderlab)

data(MovieLense)
MovieLense

head(names(colCounts(MovieLense)))

ratings = MovieLense[rowCounts(MovieLense) > 10, colCounts(MovieLense) > 100]
dim(ratings)

ratings = normalize(ratings)

ratings_sets = evaluationScheme(data=ratings, method="split",
                             train=0.8,
                             given=15,
                             goodRating=3,
                             k=1)


# UBFC
recommender_ubcf = Recommender(data=getData(ratings_sets, "train"),
                               method="UBCF",
                               parameter=NULL)

recommendations = predict(object=recommender_ubcf,
                          newdata=getData(ratings_sets, "known"),
                          n=10,
                          type="ratings")

eval_accuracy = calcPredictionAccuracy(x=recommendations,
                                       data=getData(ratings_sets, "unknown"),
                                       byUser=TRUE)
head(eval_accuracy)


# IBFC
recommender_ibcf = Recommender(data = getData(ratings_sets, "train"),
                               method = "IBCF",
                               parameter = NULL)

recommendations = predict(object=recommender_ibcf,
                          newdata=getData(ratings_sets, "known"),
                          n=10,
                          type="ratings")

eval_accuracy = calcPredictionAccuracy(x=recommendations,
                                       data=getData(ratings_sets, "unknown"),
                                       byUser=TRUE)
head(eval_accuracy)


# API ---------------------------------------------------------------------
# install.packages("plumber", dependencies = TRUE)
library(plumber)

# the code for the API is in a seperate file "5 plumber.R"
# you can test it with the following commands

root = pr("5 plumber.R")
root
root %>% pr_run()








