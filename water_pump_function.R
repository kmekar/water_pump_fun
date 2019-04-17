

library(tidyverse)
library(googleVis)
library(caret)




#### Import Data ####

# Import train data
train_url <- "http://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv"
download.file(train_url, "./data/train.csv")
train <- read.csv("./data/train.csv")

# Import train labels
train_labels_url <- "http://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv"
download.file(train_labels_url, "./data/train_labels.csv")
train_labels <- read.csv("./data/train_labels.csv")

# Join train data and labels
train <- left_join(train_labels, train, by="id")

# Import test data
test_url <- "http://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv"
download.file(test_url, "./data/test.csv")
test <- read.csv("./data/test.csv")


### Explore Data 
str(train)
table(train$status_group)
prop.table(table(train$status_group))
prop.table(table(train$quantity, train$status_group), margin = 1)  

# plot quantity stacked by status group
qplot(quantity, data=train, geom="bar", fill=status_group)
# quantity may be data leakage - dry wells are almost always non-functional...
prop.table(table(train$quantity, train$status_group), margin = 1)  

# plot waterpoint_type stacked by status group
train %>% ggplot(aes(waterpoint_type, fill=status_group)) +
  geom_bar() +
  theme(legend.position = "top") +
  theme(axis.text.x=element_text(angle = -20, hjust = 0))

# histogram for construction_year grouped by status_group
train %>% ggplot(aes(construction_year)) + 
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)
# lots of missing data (zeros) for construction_year
train %>% filter(construction_year > 0) %>%
  ggplot(aes(construction_year)) + 
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)

## Maps
train %>% filter(latitude < 0 & longitude > 0) %>%
  ggplot(aes(x =latitude, y = longitude, color = status_group)) + 
  geom_point(shape = 1) + 
  theme(legend.position = "top")

# googleVis interactive map
n = 1000  # number of wells to map
wells_map <- train[1:n,] %>%
  mutate(latlong = paste(round(latitude,2), round(longitude, 2), sep = ":"),
         size = rep(1, n)) %>% # marker size
  gvisGeoChart(locationvar = "latlong",
               colorvar = "status_group",
               sizevar = "size",
               options = list(region = "TZ", 
                              colorAxis= "{colors:[\'green', \'yellow', \'red']}"))
plot(wells_map)


#### Preliminary Modeling ####

### Random Forest
library(randomForest)  # change to caret package RF later

# Set seed and create a random forest classifier
set.seed(42)
model_forest <- randomForest(as.factor(status_group) ~ longitude + latitude + 
                               extraction_type_group + quality_group + quantity + 
                               waterpoint_type + construction_year, 
                             data = train, 
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
summary(train$installer)
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
