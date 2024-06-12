#' chapter 5 - Business Data Analytics Modelling
#' contact: Dr. Dominik Jung, dominik.jung@junglivet.com

# libraries ---------------------------------------------------------------
# General libraries for this chapter
library("dplyr")
library("dstools")
library("ggplot2")
library("readxl")

# hypothesis testing ------------------------------------------------------
pollution = data.frame(Supplier=c("New Supplier", "Old Supplier"), Cadmium=c(99,5))

barplot(height=pollution$Cadmium,
        names=pollution$Supplier,
        xlab="",
        ylab="",
        main = "")

shapiro.test(onlineshop$TURNOVER)

t.test(onlineshop$TURNOVER, mu=50)
# t.test(x, mu = 0, alternative = "two.sided")

t.test(onlineshop$TURNOVER, mu=50, alternative="greater")

woman = onlineshop %>% 
  filter(GENDER=="female")
man = onlineshop %>% 
  filter(GENDER=="male")

nrow(woman)
nrow(man)

# A/B Testing -------------------------------------------------------------
# overview of sales
onlineshop = read_excel("onlineshop.xlsx") %>% 
  select(USER_ID, DATE, VIDEO_AD, CONVERTED)

onlineshop = onlineshop %>%
  mutate(MONTH = months(DATE)) %>%
  group_by(VIDEO_AD, MONTH) %>%
  summarize(WHISKY_CONVERSION_RATE = mean(CONVERTED))

# run this to order the months correctly, if you use a non-english OS
# onlineshop$MONTH = factor(onlineshop$MONTH, levels=c("Oktober","November", "Dezember"))

ggplot(onlineshop) +
  aes(x = MONTH,
      y = WHISKY_CONVERSION_RATE,
      color = VIDEO_AD,
      group = VIDEO_AD) +
  geom_point() +
  geom_line() 

# test conditions
onlineshop_abs = read_excel("onlineshop.xlsx") %>% 
  select(VIDEO_AD, CONVERTED) %>% 
  table() %>% 
  addmargins()
onlineshop_abs

onlineshop_rel = read_excel("onlineshop.xlsx") %>% 
  select(VIDEO_AD, CONVERTED) %>% 
  table() %>% 
  prop.table(., 1)
onlineshop_rel = round(addmargins(onlineshop_rel, 2), 2)
onlineshop_rel

read_excel("onlineshop.xlsx") %>% 
  select(VIDEO_AD, CONVERTED) %>% 
  table() %>% 
  prop.test()

# glm(CONVERTED ~ VIDEO_AD,
#     family = "binomial",
#     data = onlineshop) %>%
#   tidy()

# clustering --------------------------------------------------------------
onlineshop = read_excel("onlineshop.xlsx") %>%
  group_by(USER_ID) %>% 
  mutate(EXPENDITURE = round(mean(TURNOVER)), INTERACTIONS = n()) %>%
  select(AGE, GENDER, EXPENDITURE, INTERACTIONS) %>% 
  distinct()

onlineshop

onlineshop_pre = onlineshop %>% 
  mutate(across(c(is.double), as.integer)) %>%
  ungroup() %>%
  select(AGE, EXPENDITURE, INTERACTIONS)

# onlineshop_pre = onlineshop %>% 
#   mutate(AGE = as.integer(AGE)) %>% 
#   mutate(AGE = as.integer(EXPENDITURE))

fit = kmeans(onlineshop_pre, centers = 3, nstart = 25)

fit$centers

onlineshop$CLUSTER = as.factor(fit$cluster)

ggplot(onlineshop) +
  aes(x=INTERACTIONS, y=EXPENDITURE, color=CLUSTER, shape = GENDER) +
  geom_point() +
  labs(x="Number of interactions", y="Expenditure in EUR") +
  ggtitle("Customer types in the Junglivet onlineshop")

# second run with scaled features
onlineshop_pre = onlineshop %>% 
  ungroup() %>%
  select(AGE, EXPENDITURE, INTERACTIONS) %>% 
  mutate(across(c(is.double), as.integer)) %>%
  mutate(across(everything(), scale))
fit = kmeans(onlineshop_pre, centers = 3, nstart = 25)
onlineshop$CLUSTER = as.factor(fit$cluster)

ggplot(onlineshop) +
  aes(x=INTERACTIONS, y=EXPENDITURE, color=CLUSTER, shape = GENDER) +
  geom_point() +
  labs(x="Number of interactions", y="Expenditure in EUR") +
  ggtitle("Customer types in the Junglivet onlineshop")

# prepare dataset for k-means
onlineshop_pre = onlineshop %>% 
  mutate(across(c(is.double), as.integer)) %>%
  ungroup() %>%
  select(AGE, EXPENDITURE, INTERACTIONS)

# weighted sum squares (wss) for all cluster numbers from 2 to 15
wss = c()
for(k in 2:10){
  wss[k-1] = sum(
    kmeans(
      onlineshop_pre,
      nstart=1000,
      centers=i
    )$withinss
  )
}

wss_clusters = data.frame(centers = 2:10, wss)

# make plot
ggplot(wss_clusters, aes(x = centers, y = wss)) + 
  geom_point() + 
  geom_line() +
  xlab("Number of Clusters") + 
  ylab("Within groups sum of squares")

if (!require(class)) install.packages("class")
library(class)

onlineshop = read_excel("onlineshop.xlsx") %>%
  group_by(USER_ID) %>% 
  mutate(EXPENDITURE = round(mean(TURNOVER)), INTERACTIONS = n()) %>%
  ungroup() %>% 
  select(AGE, LIFETIME, CREDIT_SCORE, EXPENDITURE, INTERACTIONS, TYPE) %>% 
  distinct()

onlineshop$AGE = normalize(onlineshop$AGE)
onlineshop$EXPENDITURE = normalize(onlineshop$EXPENDITURE)
onlineshop$INTERACTIONS = normalize(onlineshop$INTERACTIONS)
onlineshop$TYPE = as.factor(onlineshop$TYPE)

customers_unknown = filter(onlineshop, is.na(TYPE))
customers_known = filter(onlineshop, !is.na(TYPE))

set.seed(1)
split = sample(c(TRUE, FALSE), nrow(customers_known), replace=TRUE, prob=c(0.7,0.3))
customers_train_types = customers_known[split, ]$TYPE
customers_train = customers_known[split, ] %>% select(., -TYPE)
customers_test_types = customers_known[!split, ]$TYPE
customers_test = customers_known[!split, ] %>% select(.,-TYPE)

customers_train
customers_test

fit = knn(train=customers_train,
          test=customers_test,
          cl=customers_train_types,
          k=1)
fit

table(customers_test_types, fit)

misClassError = mean(fit != customers_test_types)
print(paste('Accuracy =', 1-misClassError))

customers_unknown = customers_unknown %>% select(.,-TYPE)
fit = knn(train=customers_train,
    test=customers_unknown,
    cl=customers_train_types,
    k=1)
fit

# rules and correlations --------------------------------------------------
install.packages(c("ggcorrplot"), dependencies = TRUE)
library(ggcorrplot)

onlineshop = read_excel("onlineshop.xlsx")

cor(onlineshop$AGE, onlineshop$LIFETIME)

onlineshop_selection = onlineshop %>%
  select(AGE, LIFETIME, CREDIT_SCORE, TURNOVER)
correlation_matrix = round(cor(onlineshop_selection), 2)
correlation_matrix

ggcorrplot(correlation_matrix)

# association rules -------------------------------------------------------
install.packages(c("arules", "arulesViz"), dependencies = TRUE)
library(arules)
library(arulesViz)

data("supermarket")
class(supermarket)

inspect(head(supermarket))

dim(supermarket)

itemLabels(supermarket)

summary(supermarket)

itemFrequencyPlot(supermarket, topN=10,  cex.names=1)

rules = apriori(data=supermarket,
                parameter=list(supp=0.001,conf = 0.001), 
                appearance=list(rhs="whisky"),
                control=list(verbose=F))

rules = sort(rules, decreasing=TRUE,by="confidence")
inspect(rules)

ruleExplorer(rules)


# trees -------------------------------------------------------------------
install.packages("C50", dependencies = TRUE)
library(C50)

onlineshop_sample = onlineshop %>% 
  mutate(CREDIT_SCORE = as.factor(CREDIT_SCORE)) %>%
  distinct() %>% 
  select(AGE, GENDER, TURNOVER, CREDIT_SCORE)

set.seed(1)
ids = sample(c(TRUE, FALSE), nrow(onlineshop_sample), replace=TRUE, prob=c(0.7,0.3))
train = onlineshop_sample[ids,]
test = onlineshop_sample[!ids,]

fit = C5.0(x=select(train,-CREDIT_SCORE), y=train$CREDIT_SCORE)

summary(fit)

plot(fit)

fit_rules = C5.0(x=train[,-4], y=train$CREDIT_SCORE, rules=TRUE)

predict(fit_rules, newdata=test)
predict(fit, newdata=test)

predict(fit, newdata=test)

predict(fit, newdata=test, type="prob")

# random forest
install.packages("randomForest", dependencies = TRUE)
library(randomForest)

onlineshop_sample = onlineshop %>% 
  mutate(CREDIT_SCORE=as.factor(CREDIT_SCORE)) %>% 
  distinct() %>% 
  select(AGE, GENDER, LIFETIME, CREDIT_SCORE)

set.seed(1)
ids = sample(c(TRUE, FALSE), nrow(onlineshop_sample), replace=TRUE, prob=c(0.7,0.3))
train = onlineshop_sample[ids,]
test = onlineshop_sample[!ids,]

fit = randomForest(x=train[,-4], y=train$CREDIT_SCORE, ntree = 500)

predictions = predict(fit, newdata=test[, -4])
predictions

#xgboost
install.packages("xgboost", dependencies = TRUE)
library(xgboost)

onlineshop_sample = onlineshop %>% 
  select(AGE, GENDER, LIFETIME, CREDIT_SCORE) %>% 
  distinct() %>% 
  mutate(GENDER = ifelse(GENDER == "male",1,0)) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.matrix()

set.seed(1)
ids = sample(c(TRUE, FALSE), nrow(onlineshop_sample), replace=TRUE, prob=c(0.7,0.3))
train = onlineshop_sample[ids,]
test = onlineshop_sample[!ids,]

fit = xgboost(data=train[, -4], label=train[, 4], nrounds = 10)

predictions = predict(fit, newdata = test[, -4])
predictions

# regression --------------------------------------------------------------
#linear
onlineshop_sample = onlineshop %>% 
  group_by(USER_ID) %>%
  arrange(DATE) %>% 
  mutate(LAST_TURNOVER = lag(TURNOVER)) %>% 
  ungroup()

tail(onlineshop_sample[,c(1,2,3,13)])
head(onlineshop_sample)

onlineshop_sample = onlineshop_sample %>% 
  filter(!is.na(LAST_TURNOVER))

fit = lm(TURNOVER ~ LAST_TURNOVER, data= onlineshop_sample)

summary(fit) 

ggplot(fit, aes(x = LAST_TURNOVER, y = TURNOVER)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE)

#polynominal
fit = lm(TURNOVER ~ ., data=select(onlineshop_sample, -USER_ID))
summary(fit)

fit = lm(TURNOVER ~ GENDER + TYPE + LIFETIME + PAYMENT_METHOD, data=onlineshop_sample)
summary(fit)

fit = lm(TURNOVER ~ ., data=select(onlineshop_sample, -USER_ID))
summary(fit)

fit = glm(SENDBACK ~ GENDER + TURNOVER, data = onlineshop, family = binomial)
summary(fit)

# time series -------------------------------------------------------------
install.packages(c("forecast", "prophet"), dependencies = TRUE)
library(forecast)
library(prophet)

costs = read_excel("costs.xlsx")
head(costs)

costs = costs %>% 
  mutate(DATE=paste(MONTH, DAY, YEAR, sep="/")) %>% 
  mutate(DATE=as.Date(DATE, format="%m/%d/%Y")) %>% 
  select(DATE, COSTS)

costs_ts = ts(costs$COSTS, frequency=12, start=c(2015,1))
plot(costs_ts)

ggplot(costs) +
  aes(x=DATE, y=COSTS) +
  geom_line() + 
  xlab("")

ggplot(costs) +
  aes(x=DATE, y=COSTS) +
  geom_line() + 
  xlab("") + 
  scale_x_date(date_labels="%m-%Y")

# arima forecasting 
costs_ts = ts(costs$COSTS, frequency=12, start=c(2015,1))

fit = auto.arima(costs_ts)

costs_prediction = forecast(fit, 6)

# prophet forecasting
names(costs) = c("ds", "y")

fit = prophet(costs)

future = make_future_dataframe(fit, periods = 36)
prediction = predict(fit, future)

head(prediction)

plot(prediction)
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

prophet_plot_components(fit, prediction)





