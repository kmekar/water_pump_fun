

library(tidyverse)
library(lubridate)
library(googleVis)
library(caret)




#### Import Data ####

# Import train data
# train_url <- "http://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv"
# download.file(train_url, "./data/train.csv")
data_raw <- read.csv("./data/train.csv")

# Import train labels
# train_labels_url <- "http://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv"
# download.file(train_labels_url, "./data/train_labels.csv")
data_labels <- read.csv("./data/train_labels.csv")

# Join train data and labels
data_raw <- left_join(data_labels, data_raw, by="id")
data <- data_raw  # dataframe for transformation below
data_head <- data[1:1000,]

# Import test data
# remove from final project
# test_url <- "http://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv"
# download.file(test_url, "./data/test.csv")
# test <- read.csv("./data/test.csv")


#### Explore Data ####
### Examine all 41 variables:
str(data)
table(is.na(data))  # no NAs, though there is missing data - zeros
n <- dim(data)[1]  # number of observations

## "variable name" ##

## id ##
# Well ID

## status_group ##
# Functional status of the well
# Outcome variable
table(data_raw$status_group)
prop.table(table(data$status_group))

## amount_tsh ##
# payment amount in Tsh - associated with payment_type
# 70% zeros
sum(data$amount_tsh == 0)/n
summary(data$amount_tsh)
data %>% group_by(payment_type) %>%
  summarize(n=n(), n_pay = sum(amount_tsh > 0), median(amount_tsh), max(amount_tsh))
# payment_type["never pay"] shows 93 payments (out of 25k)
data %>% filter(amount_tsh > 0) %>% 
  ggplot(aes(amount_tsh)) +
  geom_histogram() +
  scale_x_log10()
# log10 transform is closer to normal dist
data$amount_log <- ifelse(data$amount_tsh > 0, log10(data$amount_tsh), 0)

## date_recorded ##
# Date of well assessment
# y-m-d
sum(data$date_recorded == 0)/n  # no zeros
qplot(year(data$date_recorded))  # mostly recorded between 2011 and 2013
min(year(data$date_recorded))  # recorded as early as 2002
# convert to year recorded for later manipulation
data$year_recorded <- as.integer(year(data$date_recorded))

## funder ##
# who funded the well
# probably not as important as "installer"
length(unique(data$funder))
# table of top funders - #2 is blank
data %>% group_by(funder) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head(20)


## gps_height ##
# Altitude of the well
summary(data$gps_height)
hist(data$gps_height)

## installer ##
# who installed the well
length(unique(data$installer))
# table of top installers - #2 is blank
data %>% group_by(installer) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head(10)

### Location Variables
## longitude
## latitude
sum(data$longitude == 0)/n
# basic map by status
data %>% filter(latitude < 0 & longitude > 0) %>%
  ggplot(aes(x =latitude, y = longitude, color = status_group)) + 
  geom_point(shape = 1) + 
  theme(legend.position = "top")
# googleVis interactive map
n = 1000  # number of wells to map
wells_map <- data[1:n,] %>%
  mutate(latlong = paste(round(latitude,2), round(longitude, 2), sep = ":"),
         size = rep(1, n)) %>% # marker size
  gvisGeoChart(locationvar = "latlong",
               colorvar = "status_group",
               sizevar = "size",
               options = list(region = "TZ", 
                              colorAxis= "{colors:[\'green', \'yellow', \'red']}"))
plot(wells_map)
## basin ## - geographic water basin
## subvillage ##
## region ##
## region_code ##
data$region_code <- as.factor(data$region_code)
## district_code ##
data$district_code <- as.factor(data$district_code)
## ward ##
## lga ##
# todo: decide which location variables to use
# end of Location Variables

## wpt_name ##
# name of the waterpoint
# redundant (id), delete
data <- data %>% select(-wpt_name)

## num_private ##
# unknown variable, delete
summary(data$num_private)
data <- data %>% select(-num_private)

## population ##
# Population around the well 
summary(data$population)
# almost half zeros and ones
sum(data$population < 2)/n
data %>% filter(population > 1) %>%
  ggplot(aes(population)) +
  geom_histogram(bins=30) +
  scale_x_log10()
# log scale looks normally distributed
data$pop_log <- log10(data$population)
# todo: impute zeros and ones based on subvillage or ward median

## public_meeting ##
# meaning is unclear
table(data$public_meeting)

## recorded_by ##
# who recorded the data
# only one level, delete
unique(data$recorded_by)
data <- data %>% select(-recorded_by)

## scheme_management ##
# who operates the well
length(unique(data$scheme_management))
# sorted table of installers - #3 is blank
data %>% group_by(scheme_management) %>% summarize(n=n()) %>% arrange(desc(n))

## scheme_name ##
# meaning unclear
length(unique(data$scheme_name))
# sorted table of installers - #3 is blank
data %>% group_by(scheme_name) %>% summarize(n=n()) %>% arrange(desc(n))
# mostly blanks, otherwise a large number of unique names, so delete
data <- data %>% select(-scheme_name)

## permit ##
# True if the well was permitted
table(data$permit)

## construction_year ##
# Year the well was constructed
sum(data$construction_year == 0)/n  # ~one-third missing data
# histogram for construction_year grouped by status_group
data %>% filter(construction_year > 0) %>%
  ggplot(aes(construction_year)) + 
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)
# create variable for years between construction and date_recorded
year_med <- data %>% filter(construction_year > 0) %>% summarize(m = median(construction_year)) %>% .$m
data$years_op <- ifelse(data$construction_year == 0,
              data$year_recorded - year_med,  # temporarily impute median construction year
              data$year_recorded - data$construction_year)
qplot(data$years_op, geom="histogram", bins=20)
# todo: impute construction year by location and/or other variables

## extraction_type ##
## extraction_type_group ##
## extraction_type_class ##
table(data$extraction_type)
table(data$extraction_type_group)
table(data$extraction_type_class)
# only use one of the three 

## management ##
## management_group ##
# who manages the well
data %>% group_by(management) %>% count() %>% arrange(desc(n))
data %>% group_by(management_group) %>% count() %>% arrange(desc(n))

## payment ##
## payment type ##
# If and how payments are made
data %>% group_by(payment) %>% count() %>% arrange(desc(n))
data %>% group_by(payment_type) %>% count() %>% arrange(desc(n))
prop.table(table(data$payment_type, data$status_group), margin=1) %>% round(2)
# payment and payment type are identical, except for the names
# e.g. "pay annually" VS "annually", so delete
data <- data %>% select(-payment)

## water_quality ##
## quality_group ##
# quality of the water coming from the well
# water_quality may be data leakage, because some levels specify "abandoned", e.g. "salty abandoned"
# the association between "abandoned"  in "water_quality" and status_group is not clear,
# but use "quality_group" anyways
data %>% group_by(water_quality) %>% count() %>% arrange(desc(n))
data %>% group_by(quality_group) %>% count() %>% arrange(desc(n))
prop.table(table(data$quality_group, data$status_group), margin = 1) %>% round(2)

## quantity ##
## quantity_group ##
# quantity may be data leakage - dry wells are almost always non-functional
data %>% group_by(quantity) %>% count() %>% arrange(desc(n))
data %>% group_by(quantity_group) %>% count() %>% arrange(desc(n))
prop.table(table(data$quantity, data$status_group), margin=1) %>% round(2)
# plot quantity stacked by status group
qplot(quantity, data=data, geom="bar", fill=status_group)
# quantity and quantity_group are identical, so delete one
data <- data %>% select(-quantity_group)

## source ##
## source_type ##  <- my inital preference
## source_class ##
# Water source for the well
data %>% group_by(source) %>% count() %>% arrange(desc(n))
data %>% group_by(source_type) %>% count() %>% arrange(desc(n))
data %>% group_by(source_class) %>% count() %>% arrange(desc(n))

## waterpoint_type ##
## waterpoint_type_group ## 
# what kind of well/how the water is accessed
# waterpoint_type_group only combines "communal standpipe" and "communal standpipe multiple"
data %>% group_by(waterpoint_type) %>% count() %>% arrange(desc(n))
data %>% group_by(waterpoint_type_group) %>% count() %>% arrange(desc(n))
# plot waterpoint_type stacked by status group
data %>% ggplot(aes(waterpoint_type, fill=status_group)) +
  geom_bar() +
  theme(legend.position = "top") +
  theme(axis.text.x=element_text(angle = -20, hjust = 0))

#### Further Variable Manipulation ####
# tweak variables that couldn't be tweaked in a line or two in the section above
### todo:
# code blanks?
  # e.g. installer_name, permit

## Installer variable ##
summary(data$installer)
# same installer with different names in many instances
# e.g. 'Government','Gover', 'GOVER', 'Govt', 'Central government'
# Make installer lowercase
data$installer2 <- tolower(data$installer)
summary(as.factor(data$installer2))
# label blanks as unknown
data$installer2[data$installer2 %in% c(" ", "", "0", "_", "-")] <- "unknown"

## government
# combine repeated instances of government installer with regex
# include "district" and "council" as forms of government
# check the strings where "gov","dist", or "cent" is detected 
summary(as.factor(data$installer2[str_detect(data$installer2, pattern="gov")]))
summary(as.factor(data$installer2[str_detect(data$installer2, pattern="dist")]))
summary(as.factor(data$installer2[str_detect(data$installer2, pattern="cent")]))
# few/negligible instances of non-Tanzania government, e.g. italian gov - 4, japan gov - 1
# one instance of non-districts: methodist church - 1 
# no instances of non-council/gov/dist
# So, the government related regex pattern is:
gov <- "(gov)|(dist)|(coun)"  # also add "cent"?
# the pattern to remove these false positives is: 
non_gov <- "(ital)|(japan)|(iran)|(egypt)|(methodist)"
# replace strings that contain gov pattern but not non_gov pattern with "gov"
data$installer2[str_detect(data$installer2, pattern=gov)
                & !str_detect(data$installer2, pattern=non_gov)] <- "gov"

## community
# combine repeated instances of community installer with regex
# include "villages" as forms of communities
# check the strings where "comm" or "vill" is detected 
summary(as.factor(data$installer2[str_detect(data$installer2, pattern="comm")]))
summary(as.factor(data$installer2[str_detect(data$installer2, pattern="vill")]))
comm <- "(comm)|(vill)"
non_comm <- "(committe)|(bank)"
# replace strings that contain comm pattern but not non_comm pattern with "comm"
data$installer2[str_detect(data$installer2, pattern=comm)
                & !str_detect(data$installer2, pattern=non_comm)] <- "comm"

# keep the the top 20 installers
summary(as.factor(data$installer2))[1:20]
install_top20 <- names(summary(as.factor(data$installer2)))[1:20]
data$installer2[!(data$installer2 %in% install_top20)] <- "other"
data$installer2 <- as.factor(data$installer2)
summary(data$installer2)

# Table of installer2 VS the well/pump status
prop.table(table(data$installer2, data$status_group), margin = 1) %>% round(2)


## population ##
# impute zeros and ones based on subvillage or ward median
data <- data %>% group_by(district_code) %>% 
  mutate(population2 = ifelse(population < 2, median(population), population))
data <- data %>% group_by(district_code) %>% 
  summarize(med = median(population)) %>%
  ungroup() %>%
  mutate(population2 = ifelse(population < 2, med, population))
# verify that imputation did not change ward medians
pop_med_2 <- data %>% group_by(district_code) %>% 
  summarize(before = median(population), after = median(population2))
sum(!pop_med$before == pop_med$after)  # one ward changed median from 15.5 to 21.25
# five of 20 districts have a median population of zero
sum(pop_med$before == 0)
# district is a subdivision of region, but we have more regions than districts in this dataset
length(unique(data$district_code))
length(unique(data$region_code))

tmp2 <- pop_med$before - pop_med$after
sum(pop_med$before == pop_med$after)

data %>% ggplot(aes(population2)) +
  geom_histogram() +
  scale_x_log10()

data %>% filter(population < 2) %>% count()

tmp <- apply(data$population, MARGIN=1, function(p){
  if (pop < 2){
    pop <- data %>% filter
  } else {
    pop <- p
  }
  return(pop)
})



#### Preliminary Modeling ####

### Split into test and train sets 


### Random Forest
library(randomForest)  # change to caret package RF later

# Set seed and create a random forest classifier
set.seed(42)
model_forest <- randomForest(as.factor(status_group) ~ longitude + latitude + 
                               extraction_type_group + quality_group + quantity + 
                               waterpoint_type + construction_year, 
                             data = data, 
                             importance = TRUE, 
                             ntree = 5, 
                             nodesize = 2)

# Use random forest to predict the values in train
pred_forest_train <- predict(model_forest, train)

# evaluate RF model with confusion matrix
confusionMatrix(pred_forest_train, train$status_group)

# plot variable importance 
importance(model_forest)
varImpPlot(model_forest)


#### Add features to model ####

# Installer variable
summary(data$installer)
# same installer with different names in many instances
# e.g. 'Government','Gover', 'GOVER', 'Govt', 'Central government' 

# Make installer lowercase, take first 3 letters as a sub string
train$install_3 <- substr(tolower(train$installer),1,3)
train$install_3[train$install_3 %in% c(" ", "", "0", "_", "-")] <- "other"

# Take the top 15 substrings from above by occurance frequency
install_top_15 <- names(summary(as.factor(train$install_3)))[1:15]
train$install_3[!(train$install_3 %in% install_top_15)] <- "other"
train$install_3 <- as.factor(train$install_3)

# Table of the install_3 variable vs the status of the pumps
table(train$install_3, train$status_group)
prop.table(table(train$install_3, train$status_group), margin = 1)

# Create install_3 for the test set using same top 15 from above
test$install_3 <- substr(tolower(test$installer),1,3)
test$install_3[test$install_3 %in% c(" ", "", "0", "_", "-")] <- "other"
test$install_3[!(test$install_3 %in% install_top_15)] <- "other"
test$install_3 <- as.factor(test$install_3)

# Random Forest with install_3
set.seed(42)
model_forest <- randomForest(as.factor(status_group) ~ longitude + latitude + extraction_type_group + quantity + waterpoint_type + construction_year + install_3,
                             data = train, importance = TRUE,
                             ntree = 5, nodesize = 2)

# Predict using the training values
pred_forest_train <- predict(model_forest, train)
importance(model_forest)
confusionMatrix(pred_forest_train, train$status_group)

# Predict using the test values
pred_forest_test <- predict(model_forest, test)

# Create submission data frame
submission <- data.frame(test$id)
submission$status_group <- pred_forest_test
names(submission)[1] <- "id"







