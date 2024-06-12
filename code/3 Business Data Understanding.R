#' chapter 3 - Business Data Understanding
#' contact: Dr. Dominik Jung, dominik.jung@junglivet.com

# libraries ---------------------------------------------------------------
# General libraries for this chapter
library("dplyr")
library("dstools")
library("ggplot2")
library("readxl")

# loading business data ---------------------------------------------------
whisky_collection = read_excel("whiskycollection.xlsx")

# or if dstools is installed and loaded just run
data("whisky_collection")

View(whisky_collection)
head(whisky_collection)

# business data manipulation ----------------------------------------------
filter(whisky_collection, LOCATION == "Scotland")

filter(whisky_collection, LOCATION %in% c("Scotland", "USA"))

filter(whisky_collection, LOCATION == "Scotland" | (LOCATION == "USA" & TYPE == "Single Malt"))


subset_scotland = filter(whisky_collection, LOCATION == "Scotland")
subset_scotland_red = select(subset_scotland, NAME, RATING)
subset_scotland_red_final = filter(subset_scotland_red, RATING >= 4)

whisky_collection %>% 
  filter(LOCATION == "Scotland") %>% 
  select(NAME, RATING) %>% 
  filter(RATING >= 4)

whisky_collection_new = whisky_collection %>% 
  filter(LOCATION == "Scotland") %>% 
  select(NAME, RATING) %>% 
  filter(RATING >= 4) %>% 
  mutate(FAVOURITE = 1)

whisky_collection_new = whisky_collection %>% 
  mutate(AGE = 2023 - FOUNDATION)

whisky_collection_new = whisky_collection %>% 
  mutate(FAVORITE = ifelse(LOCATION == "Scotland", TRUE,
                           ifelse(RATING >= 4, TRUE, FALSE)))

whisky_collection_new = whisky_collection %>%
  group_by(LOCATION) %>% 
  mutate(BENCHMARK = mean(RATING))

whisky_collection_new = whisky_collection %>%
  group_by(LOCATION) %>% 
  summarize(BENCHMARK = mean(RATING))

whisky_collection_new = whisky_collection %>%
  group_by(LOCATION) %>%
  summarize(BENCHMARK = mean(RATING), NUM = n())



# ploting -----------------------------------------------------------------
whisky_collection
summary(whisky_collection)
plot(x=whisky_collection$REVIEWS, y=whisky_collection$CRITIQUES)

plot(x=whisky_collection$REVIEWS,
     y=whisky_collection$CRITIQUES,
     xlab="Average online review rating",
     ylab="Average critic score",
     xlim = c(5, 10),
     ylim = c(70, 100),
     main = "Relationship between critic and user ratings")

library(ggplot2)
ggplot(data=whisky_collection) +
  aes(x=REVIEWS, y=CRITIQUES) +
  geom_point()


#Advanced ggplot2 plot
ggplot(data = whisky_collection) +
  aes(x = REVIEWS, y = CRITIQUES, color=LOCATION) +
  geom_point(size=3) +
  theme_bw() +
  labs(x = "Average online review rating",
       y = "Average critic score") +
  ggtitle("Relationship between critic and user ratings")



# histogramm --------------------------------------------------------------
hist(whisky_collection$CRITIQUES,
     xlab="Rating from critics",
     main="Whisky ratings from critics")

ggplot(data=whisky_collection) +
  aes(x=CRITIQUES) + 
  geom_histogram() +
  labs(x="Rating from critics") +
  ggtitle("Whisky ratings from critics")


# density plot ------------------------------------------------------------
foundation_density = density(whisky_collection$REVIEWS) # returns density
plot(foundation_density,
     xlab="Online reviews (0-10 points)",
     main = "User rating distribution in whisky shops")

ggplot(data=whisky_collection) +
  aes(x=REVIEWS) +
  geom_density() +
  labs(x = "Online reviews (0-10 points)") +
  ggtitle("User rating distribution in whisky shops")

ggplot(data=whisky_collection) +
  aes(x=REVIEWS, fill=TYPE) +
  geom_density(alpha=0.4) +
  labs(x="Online reviews (0-10 points)") +
  ggtitle("User ratings by whisky type")


# boxplot -----------------------------------------------------------------
# No outlier removal
boxplot(PRICE ~ LOCATION,
        data = whisky_collection,
        xlab="Country",
        ylab="Price in €",
        main = "User rating distribution in whisky shops")

ggplot(data=whisky_collection) +
  aes(x=LOCATION,y=PRICE) +
  geom_boxplot() +
  labs(x="Country", y="Price in €") +
  ggtitle("Whisky price distribution per country")

# With removed outliers
boxplot(PRICE ~ LOCATION,
        data = whisky_collection,
        outline=FALSE,
        xlab="Country",
        ylab="Price in €",
        main = "User rating distribution in whisky shops")

ggplot(data=whisky_collection) +
  aes(x=LOCATION, y=PRICE) +
  geom_boxplot(outlier.shape = NA) + 
  coord_cartesian(ylim =  c(0, 750)) +
  labs(x="Country", y="Price in €") +
  ggtitle("Whisky price distribution per country")


# line chart --------------------------------------------------------------
# Make new dataset
distillery_foundations = whisky_collection %>%
  select(DISTILLERY, FOUNDATION) %>%
  mutate(YEAR = as.numeric(substr(FOUNDATION, 1, 2))) %>% 
  group_by(YEAR) %>% 
  summarize(NUM = n())
distillery_foundations

# Plot line charts
plot(distillery_foundations$YEAR,
     distillery_foundations$NUM,
     type="l",
     xaxp = c(16, 20, 4),
     xlab="Century",
     ylab="Number of distilleries",
     main = "Distillery foundations across the last centuries")

ggplot(data=distillery_foundations) +
  aes(x=YEAR, y=NUM) +
  geom_line() +
  geom_point(size=5) +
  labs(x="Century", y="Number of distilleries") +
  ggtitle("Distillery foundations across the last centuries")

# Stacked line chart
distillery_foundations = whisky_collection %>%
  select(DISTILLERY, TYPE, FOUNDATION) %>%
  mutate(YEAR = as.numeric(substr(FOUNDATION, 1, 2))) %>% 
  group_by(YEAR, TYPE) %>% 
  summarize(NUM = n())
distillery_foundations
ggplot(data=distillery_foundations) +
  aes(x=YEAR, y=NUM, color=TYPE) +
  geom_line() +
  labs(x="Century", y="Number of distilleries") +
  ggtitle("Distillery foundations by type across the last centuries")



# area chart --------------------------------------------------------------
# Make new dataset
distillery_foundations = whisky_collection %>%
  select(DISTILLERY, FOUNDATION) %>%
  mutate(YEAR = as.numeric(substr(FOUNDATION, 1, 2))) %>% 
  group_by(YEAR) %>% 
  summarize(NUM = n())
distillery_foundations

# Make area chart with base R
plot(distillery_foundations$YEAR,
     distillery_foundations$NUM,
     type="o",
     xaxp = c(16, 20, 4),
     xlab="Century",
     ylab="Number of distilleries",
     main = "Distillery foundations across the last centuries")

# Fill the area under the line
vertice_x=c(min(distillery_foundations$YEAR),
              distillery_foundations$YEAR,
              max(distillery_foundations$YEAR)) 
vertice_y=c(min(distillery_foundations$NUM),
               distillery_foundations$NUM,
               min(distillery_foundations$NUM))
polygon(vertice_x,
        vertice_y, 
        col=rgb(0.3,0.3,0.3,0.3),
        border=F)

# ggplot2 area chart
ggplot(data=distillery_foundations) +
  aes(x=YEAR, y=NUM) +
  geom_area() +
  labs(x="Century", y="Number of distilleries") +
  ggtitle("Distillery foundations by type across the last centuries")

# stacked area chart
distillery_foundations = whisky_collection %>%
  select(DISTILLERY, TYPE, FOUNDATION) %>%
  mutate(YEAR = as.numeric(substr(FOUNDATION, 1, 2))) %>% 
  group_by(YEAR, TYPE) %>% 
  summarize(NUM = n())
distillery_foundations
ggplot(data=distillery_foundations) +
  aes(x=YEAR, y=NUM, fill=TYPE) +
  geom_area(alpha=0.4) +
  labs(x="Century", y="Number of distilleries") +
  ggtitle("Distillery foundations by type across the last centuries")



# regression plot ---------------------------------------------------------
plot(whisky_collection$CRITIQUES,
     whisky_collection$REVIEWS,
     pch=19,
     frame=FALSE,
     xlab="Expert rating",
     ylab="User rating",
     main = "Prediction of the consumer rating using expert ratings")
abline(lm(REVIEWS ~ CRITIQUES, data=whisky_collection), col = "grey")


ggplot(data=whisky_collection) +
  aes(x=CRITIQUES, y=REVIEWS) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x="Expert rating", y="User rating") +
  ggtitle("Prediction of the consumer rating using expert ratings")



# sankey diagramm ---------------------------------------------------------
if (!require(devtools)) install.packages("devtools")
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# Simple sankey
df = whisky_collection %>%
  make_long(SMOKNESS, RICHNESS, RATING)

ggplot(df) +
  aes(x=x,
      next_x=next_x,
      node=node,
      next_node=next_node,
      fill=factor(node)) +
  geom_sankey()

# Complexe sankey
df <- whisky_collection %>%
  make_long(REGION, TYPE, CRITIQUES, REVIEWS)

ggplot(df) +
  aes(x=x,
      next_x=next_x,
      node=node,
      next_node=next_node,
      fill=factor(node),
      label=node) +
  geom_sankey(flow.alpha=.6,
              node.color="gray") +
  geom_sankey_label(size=3, color="white", fill="black") +
  scale_fill_viridis_d() +
  theme_sankey(base_size=18) +
  labs(x=NULL) +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=.5)) +
  ggtitle("Whisky Quality Sankey")


# venn diagramms ----------------------------------------------------------
if (!require(devtools)) install.packages("devtools")
devtools::install_github("gaospecial/ggVennDiagram")
library(tidyr)
library(ggvenn)

whisky_characteristics <- whisky_collection %>%
  filter(LOCATION=="Scotland") %>%
  select(NAME, TYPE, CRITIQUES) %>%
  mutate(SINGLE_MALT=ifelse(TYPE=="Single Malt",TRUE,FALSE)) %>%
  mutate(BLENDED=ifelse(TYPE=="Blended",TRUE,FALSE)) %>%
  mutate(AWARD=ifelse(CRITIQUES >= 90,TRUE,FALSE))

whisky_characteristics %>% 
  select(AWARD, SINGLE_MALT) %>% 
  ggvenn()


# Generate fast dummies
install.packages("fastDummies")
library(fastDummies)

whisky_characteristics <- whisky_collection %>%
  filter(LOCATION=="Scotland") %>% 
  select(NAME, TYPE) %>%
  dummy_cols(select_columns=c("TYPE"))
head(whisky_characteristics)


# heatmaps ----------------------------------------------------------------
# base R
whisky_collection_matrix = whisky_collection %>% 
  select(RATING, REVIEWS, CRITIQUES, PRICE) %>%
  as.matrix() %>% 
  scale()

row.names(whisky_collection_matrix) = whisky_collection$NAME

heatmap(whisky_collection_matrix)

# ggplot2
distillery_foundations = whisky_collection %>%
  filter(TYPE %in% c("Single Malt", "Blended")) %>% 
  select(DISTILLERY, TYPE, FOUNDATION) %>%
  mutate(YEAR = as.numeric(substr(FOUNDATION, 1, 2))) %>% 
  group_by(YEAR, TYPE) %>% 
  summarize(NUM = n())
distillery_foundations

ggplot(distillery_foundations) +
  aes(x=YEAR, y=TYPE, fill=NUM) +
  geom_tile() +
  labs(x="Expert rating", y="User rating") +
  ggtitle("Prediction of the consumer rating using expert ratings")


# treemaps ----------------------------------------------------------------
install.packages("treemapify", dependencies = TRUE)
library(treemapify)

# Simple Treemap
distilleries = whisky_collection %>%
  group_by(LOCATION) %>% 
  summarize(PRICE=mean(PRICE), REVIEWS=mean(REVIEWS), NUM=n())

ggplot(data=distilleries) +
  aes(area=NUM, fill=REVIEWS , label=LOCATION) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE)

# With subgroups  
distilleries = whisky_collection %>%
    group_by(LOCATION, REGION) %>% 
    summarize(PRICE=mean(PRICE), REVIEWS= mean(REVIEWS), NUM=n())
  
ggplot(data=distilleries) +
    aes(area=NUM,
        fill=REVIEWS,
        label=LOCATION,
        subgroup=REGION) +
    geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place="centre",
                             grow=T,
                             alpha=0.5,
                             colour="black",
                             fontface="italic",
                             min.size=0) +
  geom_treemap_text(colour="white",
                    place="topleft",
                    reflow=T)



# barplots ----------------------------------------------------------------
distilleries = whisky_collection %>%
  group_by(LOCATION) %>% 
  summarize(NUM=n())

barplot(height=distilleries$NUM,
        names=distilleries$LOCATION,
        xlab="Countries",
        ylab="Frecquency",
        main = "Number of whisky distilleries by country")

ggplot(data=whisky_collection) +
  aes(x=LOCATION) +
  geom_bar() +
  labs(x="Countries", y="Frecquency") +
  ggtitle("Number of whisky distilleries by country")

ggplot(data=whisky_collection) +
  aes(x=LOCATION, fill=TYPE) +
  geom_bar() +
  labs(x="Countries", y="Frecquency") +
  ggtitle("Number of whisky distilleries by country")


# pie chart ---------------------------------------------------------------
distilleries = whisky_collection %>%
  group_by(LOCATION) %>% 
  summarize(NUM=n())

pie(distilleries$NUM,
    labels = distilleries$LOCATION,
    main="Pie Chart of Countries")

ggplot(data=distilleries) +
  aes(x="", y=NUM, fill=LOCATION, label=LOCATION) +
  geom_bar(stat="identity") +
  geom_text(position=position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  labs(x="", y="") +
  ggtitle("Number of whisky distilleries by country")
  


# parallel coordinates ----------------------------------------------------
install.packages("GGally", dependencies = TRUE)
library(GGally)

ggparcoord(whisky_collection,
           columns = c(9:14),
           groupColumn = "LOCATION",
           showPoints = TRUE,
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3) +
  labs(x="Countries", y="Frecquency") +
  ggtitle("Whisky characteristics by country")


# radar charts ------------------------------------------------------------
if (!require(devtools)) install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar")

library(ggradar)
library(dplyr)
library(ggplot2)
library(dstools)

whisky_characteristics = whisky_collection %>% 
  select(LOCATION, RATING:PRICE) %>% 
  group_by(LOCATION) %>%
  summarise(across(RATING:PRICE, mean)) %>% 
  ungroup() %>%
  mutate_at(vars(-LOCATION), normalize)

ggradar(whisky_characteristics) +
  ggtitle("Whisky characteristics by country")


# maps --------------------------------------------------------------------
install.packages("maps", dependencies = TRUE)
library(maps)

# load map from ggplot2
world_map <- map_data("world")

ggplot() +
  # geom_map() function takes world map as input 
  geom_map(
    data = world_map, map = world_map,
    aes(long, lat, map_id = region))

head(whisky_collection$COORDINATES)

# prepare data
library(tidyr)
mapdata = separate_wider_delim(whisky_collection,
                               COORDINATES,
                               names=c("LATITUDE", "LONGITUDE"),
                               delim = ",")

mapdata$LONGITUDE = as.numeric(mapdata$LONGITUDE)
mapdata$LATITUDE = as.numeric(mapdata$LATITUDE)

# updated map
ggplot() +
  geom_map(
    data = world_map, map = world_map,
    aes(long, lat, map_id = region)) +
  geom_point(
  data = mapdata,
  aes(LONGITUDE, LATITUDE, color = LOCATION, size=2)
)

# export plots ------------------------------------------------------------
plot_final = ggplot(data=whisky_collection) +
  aes(x=LOCATION) +
  geom_bar()
ggsave("whisky_distilleries.png", plot_final, width = 15, height = 10)

ggplot(data=whisky_collection) +
  aes(x=LOCATION) +
  geom_bar()
png("whisky_distilleries.png")
dev.off()


# descriptive statistics --------------------------------------------------
library(dstools)
data("whisky_collection")
max(whisky_collection$PRICE)

scottish_whiskies = subset(whisky_collection, LOCATION=="Scotland")
min(scottish_whiskies$RATING)

table(whisky_collection$LOCATION)

# summary statistics  -----------------------------------------------------
max(whisky_collection$PRICE)
min(whisky_collection$PRICE)
quantile(whisky_collection$PRICE)
mean(whisky_collection$PRICE)

summary(whisky_collection)

# if you want install the psych library 
install.packages("psych", dependencies = TRUE)
library("psych")

describe(whisky_collection[,c(1,6,14)])

# describe numeric features -----------------------------------------------

table(whisky_collection$LOCATION)

table(whisky_collection$LOCATION, whisky_collection$RATING)

whisky_ratings = table(whisky_collection$LOCATION, whisky_collection$RATING)
prop.table(whisky_ratings)

statmode(whisky_collection$LOCATION)
statmode(whisky_collection$RATING)

unique(whisky_collection$LOCATION)

# validate your business data ---------------------------------------------

glimpse(whisky_collection)

head(whisky_collection)
tail(whisky_collection)

check = (nrow(whisky_collection) == 40)
ifelse(check, "Data complete", "Data incomplete")

length(whisky_collection)

junglivet = whisky_collection[27,]
nchar(junglivet$WIKIPEDIA)

whisky_collection_sorted = whisky_collection %>%
  select(NAME, PRICE) %>% 
  arrange(PRICE)
head(whisky_collection_sorted)

whisky_collection_sorted = whisky_collection %>%
  select(NAME, desc(PRICE)) %>% 
  arrange(PRICE)
head(whisky_collection_sorted)

sort(whisky_collection$PRICE, decreasing = FALSE)


# useful functions --------------------------------------------------------
ggplot(data=whisky_collection) +
  aes(x=REVIEWS, y=CRITIQUES) +
  geom_point()

ggplot(data=whisky_collection) +
  aes(x=REVIEWS, y=CRITIQUES) +
  geom_point() +
  theme(panel.background=element_rect(fill="white", colour="black"))

ggplot(whisky_collection) +
  aes(LOCATION, fill=LOCATION) +
  geom_bar()

ggplot(whisky_collection) +
  aes(LOCATION, fill=LOCATION) +
  geom_bar() +
  scale_fill_manual(values = use_pal(name="bootstrap"))

list_pals()


ggplot(data=whisky_collection) +
  aes(x=REVIEWS, y=CRITIQUES, shape=LOCATION) +
  geom_point() +
  guides(shape=guide_legend(title="Awesome Plot"))

library(GGally)
ggpairs(whisky_collection[, c("LOCATION", "TYPE", "RATING", "REVIEWS", "CRITIQUES")])

ggplot(data=whisky_collection) +
  aes(x=REVIEWS, y=CRITIQUES, shape=LOCATION) +
  geom_point() +
  facet_wrap( ~ LOCATION)




