#' chapter 4 - Business Data Understanding
#' contact: Dr. Dominik Jung, dominik.jung@junglivet.com

# libraries ---------------------------------------------------------------
# General libraries for this chapter
library("dplyr")
library("dstools")
library("ggplot2")
library("readxl")

# introduction ------------------------------------------------------------
ggplot(data=iris) +
  aes(x=Petal.Length, y=Petal.Width) +
  geom_point(aes(color=Species, shape=Species), size=3)

# flat files --------------------------------------------------------------
data("whisky_collection")

#read csv
whisky_collection_csv = read.csv("whisky_collection.csv")

#read delimeter separated values
library(readr)
whisky_collection_dsv = read_delim("whisky_collection.dsv", delim=";")

# read RData
load(file="whiskycollection.RData")

# save as RData
save(whisky_collection, file="whisky_collection.RData")

# webdata -----------------------------------------------------------------
install.packages("readr", dependencies = TRUE)
library(readr)

# Read from the web
urlfile="https://raw.githubusercontent.com/dominikjung42/BusinessAnalyticsBook/main/data/whisky_collection.csv"
dataset = read_csv(url(urlfile))

# Download it and read the file locally
download.file(urlfile, destfile="./downloaded_whisky_collection.csv")
dataset <- read_csv("downloaded_whisky_collection.csv")

install.packages("rvest", dependencies = TRUE)
library(rvest)

# Specify the url for desired website to be scraped
url = "https://en.wikipedia.org/wiki/List_of_whisky_distilleries_in_Scotland"
website = read_html(url)
tables = html_nodes(website, "table.wikitable")
tables = html_table(tables, header = TRUE)

tables[4]

# apis --------------------------------------------------------------------
# Generate your own API key here: http://www.omdbapi.com/apikey.aspx
library(httr)
library(jsonlite)

api_key = "enter your api key here"

movies_raw = GET(url = api_key)

# get status
status_code(movies_raw)

# download
str(content(movies_raw))
movies_text = content(movies_raw, "text", encoding = "UTF-8")
str(movies_text)

# parsing json
movies_json = fromJSON(movies_text, flatten = TRUE)
movies_json

# convert to data frame
movies_dataframe = as.data.frame(movies_json)
movies_dataframe

# databases ---------------------------------------------------------------
library(dplyr)
library(dbplyr)
library(DBI)
library(RSQLite)

# download
urlfile="https://raw.githubusercontent.com/dominikjung42/BusinessAnalyticsBook/main/data/whiskycollection.csv"
download.file(urlfile, destfile="./downloaded_whisky_collection.db")

# connect to db
con = dbConnect(SQLite(), "downloaded_whisky_collection.db")

as.data.frame(dbListTables(con))

tbl(con, sql("SELECT NAME, RATING FROM whisky_collection"))

db = tbl(con, "whisky_collection")
head(db)

nrow(db)

# if you are done, disconnect
dbDisconnect(con)

# corrupted dataset -------------------------------------------------------
# load from library dstools
data("whisky_collection_corrupted")
whisky_collection_corrupted

# load from github
download.file(urlfile, destfile="./whisky_collection_corrupted.xlsx")
whisky_collection_corrupted = read_excel("whisky_collection_corrupted.xlsx")
whisky_collection_corrupted


# fix corrupted values ----------------------------------------------------
library(ggplot2)
whisky_collection_corrupted

whisky_collection_corrupted %>% mutate(FOUNDATION = ifelse((FOUNDATION <0 | FOUNDATION >2050), NA, FOUNDATION))

table(whisky_collection_corrupted$RATING)

whisky_collection_pre = whisky_collection_corrupted
whisky_collection_pre$RATING = as.numeric(whisky_collection_corrupted$RATING)

summary(whisky_collection_corrupted)
summary(whisky_collection_corrupted[,c(6,14)])
glimpse(whisky_collection_corrupted)

summary(whisky_collection_corrupted$FOUNDATION)

ggplot(data=whisky_collection_corrupted) +
  aes(x=PRICE) +
  geom_density()

ggplot(data=whisky_collection_corrupted) +
  aes(x=FOUNDATION) +
  geom_density()

whisky_collection_pre[7,14] = median(whisky_collection_corrupted$PRICE, na.rm = TRUE)
whisky_collection_pre[15,6] = median(whisky_collection_corrupted$FOUNDATION, na.rm = TRUE)

whisky_collection_pre[3,3] = "Scotland"
whisky_collection_pre[15,3] = "Scotland"
#Other ones are from the us
  

whisky_collection_pre$PRICE = 
whisky_collection_pre$FOUNDATION = median(whisky_collection_corrupted$FOUNDATION, na.rm = TRUE)

        


# reduce noise ------------------------------------------------------------
duplicated(whisky_collection_corrupted)

rows = duplicated(whisky_collection_corrupted)
whisky_collection_corrupted[rows,1]

whisky_collection_pre = whisky_collection_corrupted[!rows,]
whisky_collection_pre = unique(whisky_collection_corrupted)
whisky_collection_pre = whisky_collection_corrupted %>% 
  distinct()

whisky_collection_corrupted %>%
  mutate(RATING_CAT = ifelse(REVIEWS < 9, "normal", "excellent"))

whisky_collection_corrupted %>% mutate(REVIEWS_CAT = cut(REVIEWS, breaks=2))

bins = 2
reviews_min = min(whisky_collection_corrupted$REVIEWS)
reviews_max = max(whisky_collection_corrupted$REVIEWS)
width = (reviews_max - reviews_min)/bins

whisky_collection_corrupted %>% mutate(REVIEWS_CAT = cut(REVIEWS, breaks=seq(reviews_min, reviews_max, width)))

# unify data structure ----------------------------------------------------
library(tidyr)

names(whisky_collection_corrupted)
whisky_collection_corrupted = make_names(whisky_collection_corrupted)
names(whisky_collection_corrupted)

# do not use, deprecated
#whisky_collection_new = make.names(whisky_collection)
#head(whisky_collection_new)

whisky_collection = read_excel("whisky_collection.xlsx")
head(whisky_collection)

whisky_ratings = whisky_collection %>%
  group_by(LOCATION, TYPE) %>%
  summarize(MEAN_RATING=mean(RATING))

head(whisky_ratings)

# Pivoting from long to wide format
whisky_ratings_wide = whisky_ratings %>%
  pivot_wider(names_from=TYPE, values_from=MEAN_RATING)

whisky_ratings_wide

whisky_ratings_long = whisky_ratings_wide %>%
  pivot_longer(names_to="TYPE", values_to="MEAN_RATING", cols=-LOCATION)
whisky_ratings_long

# feature engineering -----------------------------------------------------
install.packages("caret", dependencies = TRUE)
library(caret)

data("whisky_collection")
whisky_collection

whisky_collection_new = whisky_collection %>% 
  mutate(AGE = 2023 - FOUNDATION)
head(whisky_collection_new)

filter(whisky_collection_corrupted, NAME==c("Glendalough")) %>% 
  select(NAME, DISTILLERY, LOCATION, FOUNDATION)
whisky_collection_corrupted

foundation = whisky_collection_corrupted[32,6]
foundation = 20111

get_age = function(foundation){
  actual_year = as.integer(format(Sys.time(), "%Y"))
  foundation = ifelse(foundation <= 1608, 1608,
         ifelse(foundation > actual_year, actual_year, foundation))
  age = actual_year - foundation
  return(age)
}

whisky_collection_new = whisky_collection_corrupted %>% 
  mutate(AGE = get_age(FOUNDATION))

whisky_collection %>%
  group_by(REGION) %>%
  arrange(REGION, desc(PRICE)) %>%
  mutate(DIF_PRICE = PRICE - lag(PRICE))

entropy(whisky_collection$REGION)
entropy(whisky_collection$LOCATION)

information_gain(whisky_collection, "RATING", "PRICE", bins=5)

cor(whisky_collection$RATING, whisky_collection$PRICE)

numeric_features = whisky_collection[,c(6, 9:13)]
correlation_matrix = cor(numeric_features)
correlation_matrix

best = findCorrelation(correlation_matrix, cutoff=0.25)
print(names(numeric_features)[best])

chisq.test(whisky_collection$TYPE, whisky_collection$LOCATION)

control = rfeControl(functions=rfFuncs, method="cv", number=10)
results = rfe(whisky_collection[,1:13], whisky_collection[,14], sizes=c(1:13), rfeControl=control)
print(results)

tasting_pca = whisky_collection %>% 
  select(RATING, REVIEWS, CRITIQUES, ) %>% 
  prcomp(., scale = TRUE)

summary(tasting_pca)

plot(predict(tasting_pca))

getwd()
#path = paste0(getwd(), "/productionlogs/")
path = "C:/users/dominik/productionlogs/"
data_all = read_dir(dir = path, type = "xlsx")

# data integration --------------------------------------------------------
data("productionlog_sample")
head(productionlog_sample)

data("productionlog_extension")
head(productionlog_extension)

left_join(productionlog_sample, productionlog_extension, by="DAY")
productionlog_all = left_join(productionlog_sample,
                              productionlog_extension,
                              by = c("DAY" = "DAY", "SHIFT" = "DAYTIME"))
glimpse(productionlog_all)

productionlog_all = right_join(productionlog_sample,
                               productionlog_extension,
                               by = c("DAY" = "DAY", "SHIFT" = "DAYTIME"))
nrow(productionlog_all)

inner_join(productionlog_sample,
           productionlog_extension,
           by = c("DAY" = "DAY", "SHIFT" = "DAYTIME"))

full_join(productionlog_sample,
          productionlog_extension,
          by = c("DAY" = "DAY", "SHIFT" = "DAYTIME"))

# saving data -------------------------------------------------------------
save(whisky_ratings, file="data_export/whisky_ratings.RData")

library(readr)
write_csv(whisky_ratings, file="data_export/whisky_ratings.csv")
write_delim(whisky_ratings, file="data_export/whisky_ratings.csv", delim="|")


# useful functions --------------------------------------------------------
list.files()

path = paste0(getwd(), "/productionlogs/")
list.files(path=path, pattern="\\.xlsx")



