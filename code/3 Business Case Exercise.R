#' chapter 3 - Business Case Exercise
#' contact: Dr. Dominik Jung, dominik.jung@junglivet.com

# libraries ---------------------------------------------------------------
library("dplyr")
library("ggplot2")
library("readxl")
library("dstools")

# load data ---------------------------------------------------------------
whisky_collection = read_excel("whiskycollection.xlsx")

# or if dstools is installed and loaded just run
data("whisky_collection")

# investigate data --------------------------------------------------------




<- ggplot(mtcars2) +
  geom_point(aes(x = wt, y = mpg, colour = gear)) +
  labs(
    title = "Fuel economy declines as weight increases",
    subtitle = "(1973-74)",
    caption = "Data from the 1974 Motor Trend US magazine.",
    tag = "Figure 1",
    x = "Weight (1000 lbs)",
    y = "Fuel economy (mpg)",
    colour = "Gears"
  ) + facet_grid(vs ~ am) + theme_bw()

