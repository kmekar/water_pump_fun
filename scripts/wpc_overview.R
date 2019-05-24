#############################################################%
##########   Water Pump Condition Master Script   ###########%
#############################################################%

### Overview:
# This script serves as a standalone abbreviated version 
# of the other wpc scripts. It has less detail, but is 
# more readable and has shorter computations
# For more details on the exploratory data analysis and preprocessing
# see wpc_eda.R (https://github.com/kykar/water_pump_condition/blob/master/scripts/wpc_eda.R)
# For more details on the modeling process see wpc_prelim_models.R
# (https://github.com/kykar/water_pump_condition/blob/master/scripts/wpc_prelim_models.R)
# For example, wpc_eda.R explores and transforms each of the 40 variables 
# one by one while this script only includes the necessary preprocessing,
# and wpc_prelim_models.R goes through the model tuning process
# while this script simply uses the final/best parameters

### Outline:
#  I. Import Data
#  II. Preprocessing
#  III. Visualization
#  IV. Modeling


### Load libraries ###
library(tidyverse)
library(lubridate)
library(lsr)
library(corrplot)
library(ggmap)
library(cowplot)
library(caret)
library(randomForest)
library(xgboost)

### Load custom functions ###
## correlation function
# computes the correlation coefficent (r or its equivalent) across data types, e.g.:
# numeric V numeric - Pearson’s r
# categorical V categorical - chi squared
# numeric V categorical/factor - linear regression correlation coefficient
if(!file.exists("./scripts/cor2Fun.R") & !file.exists("cor2Fun.R")){
  if(!dir.exists("./scripts")){dir.create("./scripts")}
  cor2fun_url <- "https://github.com/kykar/water_pump_condition/blob/master/scripts/cor2Fun.R"
  download.file(train_url, "./scripts/cor2Fun.R")
}
source("./scripts/cor2Fun.R")

### Define colors for waterpoint status
status_colors <- c("#0072B2", "#E69F00", "#CF3816")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  I. Import Data  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Download data if necessary
# Download train data
if(!file.exists("./data/train_raw.csv")){
  if(!dir.exists("./data")){dir.create("./data")}
  train_url <- "http://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv"
  download.file(train_url, "./data/train_raw.csv")
}
# Download train labels
if(!file.exists("./data/train_labels.csv")){
  train_labels_url <- "http://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv"
  download.file(train_labels_url, "./data/train_labels.csv")
}
# Download test data
if(!file.exists("./data/test_raw.csv")){
  test_url <- "http://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv"
  download.file(test_url, "./data/test.csv")
}
# Download basemap
if(!file.exists(".data/base_map.rds")){
  basemap_url <- "https://github.com/kykar/water_pump_condition/blob/master/reports_and_figures/base_map.rds"
  download.file(basemap_url, "./data/base_map.rds")
}


### Load raw data
# Load train data and labels
train_raw <- read.csv("./data/train_raw.csv")
train_labels <- read.csv("./data/train_labels.csv")
# Join train data and labels
train_raw <- left_join(train_labels, train_raw, by="id")
# Load test data
test_raw <- read.csv("./data/test_raw.csv")
# Copy train and test data for modification/preprocessing
train <- train_raw  
test <- test_raw


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  II. Preprocessing  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### log10 transforms 
# amount_log
train$amount_log <- ifelse(train$amount_tsh > 0, log10(train$amount_tsh), 0)
test$amount_log <- ifelse(test$amount_tsh > 0, log10(test$amount_tsh), 0)
# population
train$pop_log <- ifelse(train$population > 0, log10(train$population), 0)
test$pop_log <- ifelse(test$population > 0, log10(test$population), 0)

### change numeric to factor
# region_code
train$region_code <- as.factor(train$region_code)
test$region_code <- as.factor(test$region_code)
# district_code
train$district_code <- as.factor(train$district_code)
test$district_code <- as.factor(test$district_code)

### Delete redundant and low variance variables
# wpt_name - redundant with waterpoint id
train <- train %>% select(-wpt_name)
test <- test %>% select(-wpt_name)
# num_private - unknown variable
train <- train %>% select(-num_private)
test <- test %>% select(-num_private)
# public_meeting - meaning is unclear and little variance
train <- train %>% select(-public_meeting)
test <- test %>% select(-public_meeting)
# recorded_by - zero variance (only one level)
train <- train %>% select(-recorded_by)
test <- test %>% select(-recorded_by)
# scheme_name - mostly blanks and a large number of levels
train <- train %>% select(-scheme_name)
test <- test %>% select(-scheme_name)
# payment - identical to payment type, except for the names
train <- train %>% select(-payment)
test <- test %>% select(-payment)
# quantity_group - identical to quantity
train <- train %>% select(-quantity_group)
test <- test %>% select(-quantity_group)

### permit
# change blank level to "unknown"
levels(train$permit)[levels(train$permit)==""] <- "unknown"
levels(test$permit)[levels(test$permit)==""] <- "unknown"

### management
# combine some levels of management that have few observations
# for train set:
train$management <- as.character(train$management)
train$management[train$management=="company"] <- "private operator"
train$management[train$management=="other - school"] <- "other"
train$management[train$management=="trust"] <- "other"
train$management <- as.factor(train$management)
# for test set:
test$management <- as.character(test$management)
test$management[test$management=="company"] <- "private operator"
test$management[test$management=="other - school"] <- "other"
test$management[test$management=="trust"] <- "other"
test$management <- as.factor(test$management)

### scheme_management
# change to lowercase string
train$scheme_management <- tolower(train$scheme_management)
test$scheme_management <- tolower(test$scheme_management)
# change blank to unknown
train$scheme_management[train$scheme_management==""] <- "unknown"
test$scheme_management[test$scheme_management==""] <- "unknown"
# change levels with n < n(other) to other
train$scheme_management[train$scheme_management=="swc"|
                          train$scheme_management=="trust"|
                          train$scheme_management=="none"] <- "other"
test$scheme_management[test$scheme_management=="swc"|
                         test$scheme_management=="trust"] <- "other"
# change string back to factor
train$scheme_management <- as.factor(train$scheme_management)
test$scheme_management <- as.factor(test$scheme_management)

### management
# combine some levels of management for train set:
train$management <- as.character(train$management)
train$management[train$management=="company"] <- "private operator"
train$management[train$management=="other - school"] <- "other"
train$management[train$management=="trust"] <- "other"
train$management <- as.factor(train$management)
# combine some levels of management for test set:
test$management <- as.character(test$management)
test$management[test$management=="company"] <- "private operator"
test$management[test$management=="other - school"] <- "other"
test$management[test$management=="trust"] <- "other"
test$management <- as.factor(test$management)

### Create years_op (years operational variable)
# create year recorded feature
train$year_recorded <- as.integer(year(train$date_recorded))
test$year_recorded <- as.integer(year(test$date_recorded))
# calculate median construction year for (temporary) imputation
year_med <- train %>% filter(construction_year > 0) %>% summarize(m = median(construction_year)) %>% .$m
# create variable for years between construction and year_recorded
train$years_op <- ifelse(train$construction_year == 0,
                         train$year_recorded - year_med,  # temporarily impute median construction year
                         train$year_recorded - train$construction_year)
test$years_op <- ifelse(test$construction_year == 0,
                        test$year_recorded - year_med,  # temporarily impute median construction year
                        test$year_recorded - test$construction_year)
# plot histogram of years_op
qplot(train$years_op, geom="histogram", bins=20)
qplot(test$years_op, geom="histogram", bins=20)
# todo: impute construction year

### Funder
# same funder with different names in many instances
# especially government and community
# e.g. 'Government','Gover', 'GOVER', 'Govt', 'Central government'
summary(train$funder)
# Make funder lowercase
train$funder20 <- tolower(train$funder)
test$funder20 <- tolower(test$funder)
# label blanks as unknown
train$funder20[train$funder20 %in% c(" ", "", "0", "_", "-")] <- "unknown"
test$funder20[test$funder20 %in% c(" ", "", "0", "_", "-")] <- "unknown"

## government
# combine repeated instances of government funder with regex
# include "district" and "council" as forms of government
# check the strings where "gov","dist", or "coun" is detected 
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="gov")]))
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="dist")]))
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="coun")]))
# the government related regex pattern is:
gov <- "(gov)|(dist)|(coun)"
# the pattern to remove these false positives is: 
non_gov <- "(village)|(comm)|(china)|(belgian)|(finland)|(irish)|(ital)|(japan)|(iran)|(egypt)|(methodist)"
# replace strings that contain gov pattern but not non_gov pattern with "gov"
train$funder20[str_detect(train$funder20, pattern=gov)
               & !str_detect(train$funder20, pattern=non_gov)] <- "gov"
test$funder20[str_detect(test$funder20, pattern=gov)
              & !str_detect(test$funder20, pattern=non_gov)] <- "gov"

## community
# combine repeated instances of community funder with regex
# include "villages" as forms of communities
# check the strings where "comm" or "vill" is detected 
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="comm")]))
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="vill")]))
comm <- "(comm)|(vill)"
non_comm <- "(committe)|(bank)"
# replace strings that contain comm pattern but not non_comm pattern with "comm"
train$funder20[str_detect(train$funder20, pattern=comm)
               & !str_detect(train$funder20, pattern=non_comm)] <- "comm"
test$funder20[str_detect(test$funder20, pattern=comm)
              & !str_detect(test$funder20, pattern=non_comm)] <- "comm"

## create additional variable with only the top 10 funders
summary(as.factor(train$funder20))[1:10]
train$funder10 <- train$funder20
funder_top10 <- names(summary(as.factor(train$funder10)))[1:10]
train$funder10[!(train$funder10 %in% funder_top10)] <- "other"
train$funder10 <- as.factor(train$funder10)
# same for test set
test$funder10 <- test$funder20
test$funder10[!(test$funder10 %in% funder_top10)] <- "other"
test$funder10 <- as.factor(test$funder10)
# reduce funder20 to top 20 funders
funder_top20 <- names(summary(as.factor(train$funder20)))[1:20]
train$funder20[!(train$funder20 %in% funder_top20)] <- "other"
train$funder20 <- as.factor(train$funder20)
summary(train$funder20)
# same for test set
test$funder20[!(test$funder20 %in% funder_top20)] <- "other"
test$funder20 <- as.factor(test$funder20)
summary(test$funder20)


### Installer
# same installer with different names in many instances
# as with funder above
# e.g. 'Government','Gover', 'GOVER', 'Govt', 'Central government'
summary(train$installer)
# Make installer lowercase
train$installer20 <- tolower(train$installer)
test$installer20 <- tolower(test$installer)
summary(as.factor(train$installer20))
# label blanks as unknown
train$installer20[train$installer20 %in% c(" ", "", "0", "_", "-")] <- "unknown"
test$installer20[test$installer20 %in% c(" ", "", "0", "_", "-")] <- "unknown"

## government
# combine repeated instances of government installer with regex
# include "district" and "council" as forms of government
# check the strings where "gov","dist", or "coun" is detected 
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="gov")]))
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="dist")]))
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="coun")]))
# the government related regex pattern is:
gov <- "(gov)|(dist)|(coun)"
# the pattern to remove these false positives is: 
non_gov <- "(ital)|(japan)|(iran)|(egypt)|(methodist)"
# replace strings that contain gov pattern but not non_gov pattern with "gov"
train$installer20[str_detect(train$installer20, pattern=gov)
                  & !str_detect(train$installer20, pattern=non_gov)] <- "gov"
test$installer20[str_detect(test$installer20, pattern=gov)
                 & !str_detect(test$installer20, pattern=non_gov)] <- "gov"

## community
# combine repeated instances of community installer with regex
# include "villages" as forms of communities
# check the strings where "comm" or "vill" is detected 
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="comm")]))
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="vill")]))
comm <- "(comm)|(vill)"
non_comm <- "(committe)|(bank)"
# replace strings that contain comm pattern but not non_comm pattern with "comm"
train$installer20[str_detect(train$installer20, pattern=comm)
                  & !str_detect(train$installer20, pattern=non_comm)] <- "comm"
test$installer20[str_detect(test$installer20, pattern=comm)
                 & !str_detect(test$installer20, pattern=non_comm)] <- "comm"

## create additional variable with the top 10 installers
summary(as.factor(train$installer20))[1:10]
train$installer10 <- train$installer20
install_top10 <- names(summary(as.factor(train$installer20)))[1:10]
train$installer10[!(train$installer10 %in% install_top10)] <- "other"
summary(as.factor(train$installer10))
train$installer10 <- as.factor(train$installer10)
# same for test set
test$installer10 <- test$installer20
test$installer10[!(test$installer10 %in% install_top10)] <- "other"
summary(as.factor(test$installer10))
test$installer10 <- as.factor(test$installer10)
# reduce installer20 to top 20 installers
summary(as.factor(train$installer20))[1:20]
install_top20 <- names(summary(as.factor(train$installer20)))[1:20]
train$installer20[!(train$installer20 %in% install_top20)] <- "other"
train$installer20 <- as.factor(train$installer20)
summary(train$installer20)
# same for test set
test$installer20[!(test$installer20 %in% install_top20)] <- "other"
test$installer20 <- as.factor(test$installer20)
summary(test$installer20)


### Population
# impute missing population data
# almost half zeros and ones
sum(train$population < 2)/59400
sum(test$population < 2)/14850

## impute zeros and ones based on median within region_code
train <- train %>% group_by(region_code) %>% 
  mutate(population2 = ifelse(population < 2, median(population), population)) %>%
  ungroup()
test <- test %>% group_by(region_code) %>% 
  mutate(population2 = ifelse(population < 2, median(population), population)) %>%
  ungroup()
# verify that imputation did not change region medians significantly
pop_med_check <- train %>% group_by(region_code) %>% 
  summarize(before = median(population), after = median(population2))
pop_med_check[!pop_med_check$before == pop_med_check$after,]  
# one non-zero region changed median from 217.5 to 217.75
# reduced missing data  from 48% to 33%
sum(train$population2 < 2)/59400
# take log transform, and retain zeros
train <- train %>% mutate(pop_log2 = ifelse(population2 == 0, 0, log10(population2)))
test <- test %>% mutate(pop_log2 = ifelse(population2 == 0, 0, log10(population2)))


## Impute population with KNN
# create dataframe with features
x_pop <- train %>% filter(population > 1) %>% 
  select(latitude, longitude)
# create vector with target variable
y_pop <- train %>% filter(population > 1) %>% 
  pull(pop_log)  # using log results for a more accurate prediction
# define tune grid for k (number of neighbors)
grid_pop <- expand.grid(k=c(5,7,9,11))
# define training method - cross validation
train_control <- trainControl(method="cv", number=10, savePredictions="final")
# train KNN with cross validation to assess accuracy 
pop_knn <- train(x_pop, y_pop, method="knn", trControl=train_control, tuneGrid=grid_pop)
print(pop_knn)
# impute zeros with KNN model predictions
train <- train %>% mutate(pop_log3 = ifelse(pop_log==0, predict(pop_knn), pop_log))
test <- test %>% mutate(pop_log3 = ifelse(pop_log==0, predict(pop_knn), pop_log))
# check distribution 
# before
train %>% filter(pop_log > 0) %>%
  ggplot(aes(pop_log)) +
  geom_histogram(bins=30)
# after
train %>% ggplot(aes(pop_log3)) +
  geom_histogram(bins=30)
# very similar shape, looks successful

## Future:
# include population density data (based on the GPS coords) imputation
# for example, data available here: https://energydata.info/dataset/tanzania-high-resolution-settlement-layer-2016

### Save Preprocessed Data
# write.csv(train, file='./data/train.csv', row.names=F)
# write.csv(test, file='./data/test.csv', row.names=F)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
########   III. Visualizations   ##########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Target Variable
train %>% ggplot(aes(status_group)) +
  geom_bar(aes(y=(..count..)/sum(..count..), fill=status_group), width=0.7) +
  # ggtitle("Waterpoint Condition") +
  geom_text(stat='count', aes(y=(..count..)/sum(..count..), 
                              label=paste("n = ", ..count..)), vjust=-0.2) +
  xlab("") +
  ylab("proportion") +
  scale_x_discrete(breaks=levels(train$status_group), 
                   labels=c("functional", 
                            "functional,\nneeds repair",
                            "non-functional")) +
  scale_y_continuous(limits=c(0, 0.6)) +
  scale_fill_manual(values=status_colors) +
  theme(legend.position="none")

### Map
# load basemap
base_map <- readRDS("./reports_and_figures/base_map.rds")
# plot waterpoints on base map
ggmap(base_map) +
  geom_point(aes(x=longitude, y=latitude, color = status_group), data=train, 
             shape=16, alpha=0.35) +
  scale_color_manual(values=status_colors) +
  theme_void() +
  theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=3, alpha = 0.8)))

### Installer
train %>% group_by(installer10) %>% 
  mutate(installer_tot = n()) %>% ungroup() %>%
  ggplot(aes(x=reorder(installer10, status_group=="non-functional"), fill=status_group)) +
  geom_bar(position="fill") +
  geom_hline(yintercept=0.384, size=1) +
  geom_text(aes(label=paste("n = ", installer_tot), y=1), hjust=1) +
  coord_flip() +
  scale_fill_manual(values=status_colors) +
  ylab("proportion") +
  xlab("installer") +
  theme(legend.position=c(-0.3, 1.08), 
        legend.title = element_blank(), 
        legend.spacing.x=unit(2, "mm"),
        legend.direction="horizontal")

### Construction Year
# histogram for construction_year grouped by status_group
train %>% filter(construction_year > 0) %>%
  ggplot(aes(construction_year, fill=status_group)) + 
  geom_histogram(bins = 15, color="grey40") + 
  scale_fill_manual(values=status_colors) +
  xlab("construction year") +
  facet_grid( ~ status_group) +
  theme(legend.position = "none",
        panel.border = element_rect(color = "black", fill=NA, size=1))

### Source
train %>% group_by(source) %>% 
  mutate(tot = n()) %>% ungroup() %>%
  ggplot(aes(x=reorder(source, status_group=="non-functional"),
             fill=status_group)) +
  geom_bar(position="fill") +
  geom_hline(yintercept=0.384, size=1) +
  geom_text(aes(label=paste("n = ", tot), y=1), hjust=1) +
  coord_flip() +
  scale_fill_manual(values=status_colors) +
  ylab("proportion") +
  xlab("water source") +
  ggtitle("") + # needed for blank space for legend position
  theme(legend.title=element_blank(),
        legend.position=c(-0.3, 1.08),
        legend.spacing.x=unit(2, "mm"),
        legend.direction="horizontal")

### Extraction Type
train %>% group_by(extraction_type_class) %>% 
  mutate(tot = n()) %>% ungroup() %>%
  ggplot(aes(x=reorder(extraction_type_class, status_group=="non-functional"),
             fill=status_group)) +
  geom_bar(position="fill") +
  geom_hline(yintercept=0.384, size=1) +
  geom_text(aes(label=paste("n = ", tot), y=1), hjust=1) +
  coord_flip() +
  scale_fill_manual(values=status_colors) +
  ylab("proportion") +
  xlab("extraction  type") +
  ggtitle("") + # needed for blank space for legend position
  theme(legend.title=element_blank(),
        legend.position=c(-0.3, 1.08),
        legend.spacing.x=unit(2, "mm"),
        legend.direction="horizontal")

### Waterpoint Type
train %>% group_by(waterpoint_type) %>% 
  mutate(wp_type_tot = n()) %>% ungroup() %>%
  ggplot(aes(x=reorder(waterpoint_type, status_group=="non-functional"),
             fill=status_group)) +
  geom_bar(position="fill") +
  geom_hline(yintercept=0.384, size=1) +
  geom_text(aes(label=paste("n = ", wp_type_tot), y=1), hjust=1) +
  coord_flip() +
  scale_fill_manual(values=status_colors) +
  ylab("proportion") +
  xlab("waterpoint  type") +
  ggtitle("") + # needed for blank space for legend position
  theme(legend.title=element_blank(),
        legend.position = c(-0.4, 1.08),
        legend.spacing.x=unit(2, "mm"),
        legend.direction = "horizontal")

### Amount Tsh
# smoothed histogram
train %>% filter(amount_tsh > 0, !payment_type=="never pay", 
                 !payment_type=="other", !payment_type=="unknown") %>% 
  ggplot() +
  geom_density(aes(x=amount_tsh, y=(..count..)*2, fill=payment_type),  
               adjust=2, alpha=0.5) + # multiply count by adjust value to get correct count
  scale_x_log10(limits=c(1,350000)) +
  #theme_bw() +
  scale_fill_manual(values=c("darkturquoise", "red", "orange", "darkgrey"), name="payment type") +
  xlab("payment amount in Tsh (log scale)") +
  ylab("count")

### Correlations
# 13 initally modeled variables
modeled <- c("amount_tsh", "installer20", "longitude", "latitude", "gps_height", 
             "pop_log3", "permit", "years_op", "extraction_type_group",
             "management", "payment_type", "source_type", "waterpoint_type")
# calculate correlations for mixed feature types using cor2() function
corr_modeled <- train %>% select(modeled) %>% cor2()
# plot correlation matrix
corrplot.mixed(corr_modeled, tl.pos ="lt", tl.col="black")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#########     IV. Modeling     #########
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Note that the models below have already been tuned for accuracy
# using 10-fold cross validation in wpc_prelim_models.R
# cross validation is not used in this script for faster compute times

set.seed(1)

### Random Forest
## Without data leakage
# define features (x) and target (y)
y <- train$status_group
x <- train %>% select(installer20, funder20, longitude, latitude, gps_height, 
                      pop_log, years_op, extraction_type_group,
                      management, payment_type, waterpoint_type)
# fit random forest (can take several mins)
model_rf <- randomForest(x, y, ntree=201, mtry=3)
# summarize results
print(model_rf)
# plot variable importance 
varImpPlot(model_rf)

## with data leakage (can take several mins)
model_rf_leak <- x %>% mutate(quantity = train$quantity, quality = train$water_quality) %>%
 randomForest(., y, ntree=201, mtry=3)
print(model_rf_leak)
varImpPlot(model_rf_leak$finalModel)


### XGBoost
# define model formula
form_13 <- as.formula(status_group ~ amount_tsh + installer20 + 
                      longitude + latitude + gps_height + 
                      pop_log3 + permit + years_op + extraction_type_group +
                      management + payment_type + source_type + waterpoint_type)
# define model parameters
grid_xgb <- expand.grid(nrounds=250, max_depth=5, eta=0.4, gamma=0, colsample_bytree=0.8, 
                        min_child_weight=1, subsample=0.75)
# fit XGBoost model
model_xgb <- train(form_13, data=train, method="xgbTree", 
                   trControl=trainControl(method="none"), 
                   tuneGrid=grid_xgb)
# variable importance
varImp(model_xgb)


### Multinomial logistic regression
# define model formula
form_multinom <- as.formula(status_group ~ installer10 + region +
                            permit + extraction_type_group +
                            management + payment_type + source_type + 
                            waterpoint_type)
# multinomial logistic regression
model_multinom <- train(model_form_multinom, data=train, method="multinom", 
                        trControl=trainControl(method = "none"), 
                        tuneGrid=expand.grid(decay=0))
# variable importance
varImp(model_multinom)


### k-Nearest Neighbors 
# define and scale features (x)
x_scaled <- train %>% select(longitude, latitude, gps_height,
                             pop_log3, amount_log, years_op) %>% scale()
# KNN classification
model_knn <- knn3(x_scaled, y, k=7)
model_knn <- train(x_scaled, y, method="knn", 
                   trControl=trainControl(method = "none"),
                   tuneGrid=expand.grid(k=7))


### Majority Vote Ensemble
# create list will all relevant models
names_all <- c("model_rf", "model_xgb","model_multinom", "model_knn")
models_all <- lapply(names_all, get)
names(models_all) <- c("rf", "xgb", "log", "knn")
# make predictions
pred_train <- as.data.frame(sapply(models_all, function(x) predict(x, train)))
names(pred_train) <- c("rf", "xgb", "log", "knn")
# make ensemble
pred_train <- pred_train %>% mutate(vote = if_else(xgb==log & xgb==knn, xgb, rf))


### Stacked random forest ensemble
# add column with target variable
pred_train <- add_column(pred_train, target = train$status_group, .before=1)
# fit random forest
stack_rf <- randomForest(target ~ rf + xgb + log + knn, data=pred_train, mtry=2, ntrees=501)
# make predictions
pred_train$stack <- predict(stack_rf, pred_train)


### check accuracy on training set 
# not actually informative, but interesting
sum(pred_train$target==pred_train$rf)/59400
sum(pred_train$target==pred_train$xgb)/59400
sum(pred_train$target==pred_train$log)/59400
sum(pred_train$target==pred_train$knn)/59400
sum(pred_train$target==pred_train$vote)/59400
sum(pred_train$target==pred_train$stack)/59400
# see wpc_prelim_models for accuracy estimates from cross-validation


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  V. Predictions/Submissions  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

submission <- data.frame(test$id)
names(submission)[1] <- "id"

### Random forest
# prepare x_test for random forest
x_test <- test %>% select(installer20, funder20, longitude, latitude, gps_height, 
                          pop_log, years_op, extraction_type_group,
                          management, payment_type, waterpoint_type)
x_test_leak <- x_test %>% mutate(quantity = test$quantity, quality = test$water_quality)

# prediction/submission
submission_rf <- submission
submission_rf$status_group <- predict(model_rf, x_test)

# Random forest w/ leakage
submission_leak <- submission
submission_leak$status_group <- predict(model_rf_leak, x_test_leak)

### XGBoost
submission_xgb <- submission
submission_xgb$status_group <- predict(model_xgb, test)

### Majority vote ensemble
pred_test <- data.frame("rf" = submission_rf$status_group,
                        "xgb" = predict(model_xgb, test),
                        "log" = predict(model_multinom, test),
                        "knn" = predict(model_knn, test))
pred_test <- pred_test %>% mutate(vote = if_else(xgb==log & xgb==knn, xgb, rf))
submission_vote <- submission
submission_vote$status_group <- pred_test$vote

### Stacked random forest ensemble
submission_stack <- submission
submission_stack$status_group <- predict(stack_rf, pred_test)

### Save submissions
if(!dir.exists("./output")){dir.create("./output")}
write.csv(submission_rf, "./output/submission_rf.csv", row.names=F)
write.csv(submission_xgb, "./output/submission_xgb.csv", row.names=F)
write.csv(submission_leak, "./output/submission_leak.csv", row.names=F)
write.csv(submission_vote, "./output/submission_vote.csv", row.names=F)
write.csv(submission_stack, "./output/submission_stack.csv", row.names=F)

