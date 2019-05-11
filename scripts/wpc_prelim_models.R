#########################################################%
##########       Preliminary Modeling         ###########%
#########################################################%

## Overview:
# This script documents the various models I tried
# and the tweaks and tuning thereof.
# I recommend not to run it start to finish, 
# as it would take a very long time -
# it is more of a record for future reference.
# Most of the models have been saved to the 
# model folder for future use. 

## Outline:
#  I. Random Forest
#      - Variable Selection (recursive Feature Elimination)
#      - Tune Model Parameters (ntrees and mtry)
#      - Feature Substitution (try substituting similar/grouped features)
#      - Other Performance Tests (formula call method, 5 VS 10-fold cross validation)
#  II. XGBoost
#  III. Logistic Regression
#      - Logistic regression with simplified 2 level target 
#      - Multinomial logistic regression
#  IV. k-Nearest Neighbors
#  V. Competition Submission


### Load libraries ###
library(tidyverse)
library(caret)
library(randomForest)


### Import datasets ###
train <- read.csv(file="./data/train.csv", header=T)
test <- read.csv(file="./data/test.csv", header=T)







#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  I. Random Forest  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Variable Selection using Recursive Feature Elimination (RFE)  ####
## define RFE control
rfe_control <- rfeControl(functions=rfFuncs,   # use random forest for RFE
                          method='cv', number=10,  # 10-fold cross validation
                          saveDetails=T)  
# define features (x) and target (y)
y <- train$status_group
x <- train %>% select(amount_tsh, installer20, longitude, latitude, gps_height, 
                      pop_log3, permit, years_op, extraction_type_group,
                      management, payment_type, source_type, waterpoint_type)
# train RFE modwl
model_rfe <- rfe(x, y, sizes=c(2:6)*2, rfeControl=rfe_control)
# save model
saveRDS(model_rfe, "./models/feature_elimination.rds")

# Summarize results
print(model_rfe)
# list features selected
predictors(model_rfe)
# plot variable importance 
varImpPlot(model_rfe$fit)
# plot results
model_rfe$results %>% ggplot(aes(Variables, Accuracy)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=Accuracy-2*AccuracySD/sqrt(10), 
                    ymax=Accuracy+2*AccuracySD/sqrt(10))) +
  theme_bw()

## choose 10 feature model as the 12 feature model is not significantly better
# Define model features
x_10 <- x %>% select(-amount_tsh, -source_type, -permit)


#### Tune model parameters with cross validation ####
# define 10-fold cross-validation training method
train_control <- trainControl(method="cv", number=10, savePredictions="final")
# control <- trainControl(method="repeatedcv", number=10, repeats=10)

## Tune number of trees
ntree = c(51,101,201,301,501)
grid_rf <- expand.grid(mtry=3)
# Random Forest model
model_rf_trees <- lapply(ntree, function(ntree){
  train(x_10, y, method="rf", ntree=ntree,
        trControl=train_control, tuneGrid=grid_rf)
})
# name the resulting list items 
names(model_rf_trees) <- c("51 trees", "101 trees", "201 trees", "301 trees", "501 trees")
# view the results
sapply(model_rf_trees, `[[`, "results")
# 101 trees appears adequate (but 201 has lower SD)
ntree <- 101
# save model
saveRDS(model_rf_trees, "./models/rf_trees.rds")

## Tune mtry
modelLookup("rf")
grid_rf <- expand.grid(mtry=c(2,3,4,5))
# Random Forest model
model_rf_mtry <- train(x_10, y, method="rf", ntree=ntree,
                       trControl=train_control, tuneGrid=grid_rf)

print(model_rf_mtry)
# mtry=3 is not significantly better than mtry=4
grid_rf <- expand.grid(mtry=3)
# save model
saveRDS(model_rf_mtry, "./models/rf_mtry.rds")


#_________________________________
#### Try alternative features ####

## rerun baseline model
model_rf_baseline <- train(x_10, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_baseline)

## add pop_log3
x_alt <- x_10 %>% mutate(pop_log3 = train$pop_log3)
model_rf_alt1 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt1)
# not significantly better with pop_log3

## remove source_type and/or quality_group
x_alt <- x_10 %>% select(-source_type)
x_alt <- x_10 %>% select(-quality_group)
x_alt <- x_10 %>% select(-source_type, -quality_group)
model_rf_alt2 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt2)
# slightly worse without source_type and/or quality_group

## installer20 -> installer10
x_alt <- x_10 %>% select(-installer20) %>% mutate(installer10 = train$installer10)
model_rf_alt3 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt3)
# slightly worse, but keep the simpler feature?

## add funder20 or funder10
x_alt <- x_10 %>% mutate(funder20 = train$funder20)
model_rf_alt4 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt4)
# not a significant improvement, so leave out

## extraction_type_group -> extraction_type_class
x_alt <- x_10 %>% select(-extraction_type_group) %>% mutate(extraction_type_class = train$extraction_type_class)
model_rf_alt5 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt5)
# reduced accuracy slightly, but keep simpler feature?

## management -> management_group or scheme_management
x_alt <- x_10 %>% select(-management) %>% mutate(management_group = train$management_group)
model_rf_alt6 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt6)
# slightly reduced accuracy, so keep management

## source_type -> source or source_class
x_alt <- x_10 %>% select(-source_type) %>% mutate(source_class = train$source_class)
x_alt <- x_10 %>% select(-source_type) %>% mutate(source = train$source)
model_rf_alt7 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt7)
# slightly reduced accuracy

## payment_type -> amount_log
x_alt <- x_10 %>% select(-payment_type) %>% mutate(amount_log = train$amount_log)
model_rf_alt8 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt8)
# lower accuracy, so don't switch

## years_op -> construction year
x_alt <- x_10 %>% select(-years_op) %>% mutate(construction_year = train$construction_year)
model_rf_alt9 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt9)

## add gps_height
x_alt <- x_10 %>% mutate(gps_height = train$gps_height)
model_rf_alt10 <- train(x_alt, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_alt10)
varImpPlot(model_rf_alt10$finalModel)
# slightly improves model

## add quantity (data leakage) for comparison
x_quant <- x_alt %>% mutate(quantity = train$quantity)
model_rf_quant <- train(x_quant, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(model_rf_quant)
varImpPlot(model_rf_quant$finalModel)
# significantly improves model, as expected

#______________________________________
#### Final Model ####
# only gps_height perfroms significantly better than baseline model (besides quantity)
x_final <- x_alt  # with gps_height added
# define 10-fold cross-validation training method that saves predictions and probabilities
train_control <- trainControl(method="cv", number=10, savePredictions="all", classProbs=T)
# re-run final model (should be about the same as model_rf_alt10) 
model_rf_final <- train(x_final, y, method="rf", ntree=ntree, 
                        trControl=train_control, tuneGrid=grid_rf)
# print model
print(model_rf_final)
# variable importance
varImpPlot(model_rf_final$finalModel)
# confusion matrix
confusionMatrix(model_rf_final$pred$pred, reference=model_rf_final$pred$obs)


#______________________________________
#### Other Misc. Performance Tests ####

### model formula call VS x, y call ###
# x, y call is significantly more accurate for some reason...
# Random Forest with x, y call
model_rf_xy <- train(x_10, y, method="rf", ntree=ntree,
                       trControl=train_control, tuneGrid=grid_rf)
print(model_rf_xy)

# formula alternative to model call
model_form_10 <- as.formula(status_group ~ installer20 + longitude + latitude +
                              years_op + extraction_type_group + waterpoint_type +
                              payment_type + source_type +
                              quality_group + management)
# Random Forest with formula call
model_rf_form <- train(model_form_10, data=train, method="rf", ntree=ntree,
                       trControl=train_control, tuneGrid=grid_rf)
print(model_rf_form)

### 5-fold vs 10-fold C.V. ###
# define cross-validation training method
train_control <- trainControl(method="cv", number=5, savePredictions="final")
grid_rf <- expand.grid(mtry=3)
# Random Forest model
model_rf_5cv <- train(x_10, y, method="rf", ntree=ntree,
                        trControl=train_control, tuneGrid=grid_rf)
print(model_rf_5cv)
plot(model_rf_5cv$finalModel)
# plot variable importance 
varImpPlot(model_rf_5cv$finalModel)
# 5-fold appears adequate, but accuracy is slightly inflated , so continue to use 10-fold

#________________________________________________
#### save alternative and test model results ####
# manually select model names
model_rf_names <- c("model_rf_baseline", "model_rf_alt1","model_rf_alt2", "model_rf_alt3", 
                   "model_rf_alt4", "model_rf_alt5", "model_rf_alt6", "model_rf_alt7", 
                   "model_rf_alt8", "model_rf_alt9", "model_rf_alt10", "model_rf_quant", 
                   "model_rf_final", "model_rf_xy", "model_rf_form", "model_rf_5cv")
# create a list based on models names provided
model_rf_list <- lapply(model_rf_names, get)
# set names
names(model_rf_list) <- model_rf_names
# check the output
sapply(model_rf_list, `[[`, "results")
# save models
saveRDS(model_rf_list, "./models/rf_tests.rds")
# load models
# model_rf_list <- readRDS("./models/rf_tests.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%
######  II. XGBoost  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%

# define model formula
model_form_13 <- as.formula(status_group ~ amount_tsh, installer10, 
                            longitude, latitude, gps_height, 
                            pop_log3, permit, years_op, extraction_type_group,
                            management, payment_type, source_type, waterpoint_type)
# define tuning grid 
modelLookup("xgbTree")
# grid_xgb <- expand.grid()
# xgbTree model
model_xgb <- train(model_form_13, data=train, method="xgbTree", 
                        trControl=train_control, tuneLength=3)
# summary of results
print(model_xgb)
# save model
saveRDS(model_xgb, "./models/xgb.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  III. Logistic Regression  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Logistic regression ###
# with simplified 2 level target
# combine "functional needs repair" with "non functional" in new variable
train$status_group2 <- train$status_group
levels(train$status_group2)[levels(train$status_group2)=="functional needs repair"] <- "non functional"
# define model formula (with status_group2)
model_form_log <- as.formula(status_group2 ~ amount_tsh + installer10 + 
                             longitude + latitude + gps_height + permit +
                             pop_log3 + years_op + extraction_type_group +
                             management + payment_type + source_type + waterpoint_type)
# define 10-fold cross-validation training method
train_control <- trainControl(method="cv", number=10, savePredictions=T)
# logistic regression
model_log <- train(model_form_log, data=train, method="glm", family=binomial,
                   trControl=train_control)  # no tuning parameters
# summarize results
print(model_log)
imp_log <- varImp(model_log)

# define model formula using features with fewer levels
# and removing less important variables
model_form_log <- as.formula(status_group2 ~ installer10 + gps_height + 
                               years_op + payment_type + region +
                               source_type + waterpoint_type)
# logistic regression
model_log <- train(model_form_log, data=train, method="glm", family=binomial,
                   trControl=train_control)  # no tuning parameters
# summary of results
print(model_log)
# variable importance
imp_log <- varImp(model_log)$importance %>% rownames_to_column()
# confusion matrix
confusionMatrix(model_log, positive="non functional")
# save model
saveRDS(model_log, "./models/log.rds")


### Multinomial logistic regression ###
# define model formula
model_form_multinom <- as.formula(status_group ~ installer10 + region +
                                    permit + extraction_type_group +
                                    management + payment_type + source_type + 
                                    waterpoint_type)
# multinomial logistic regression
model_multinom <- train(model_form_multinom, data=train, method="multinom", 
                        trControl=train_control, tuneGrid=expand.grid(decay=0))
# summary of results
print(model_multinom)
# variable importance
imp_multinom <- varImp(model_multinom)$importance %>% rownames_to_column()
# confusion matrix
confusionMatrix(model_multinom)
# detailed confusion matrix
confusionMatrix(model_multinom$pred$pred, reference=model_multinom$pred$obs, 
                positive="non functional")
# save model
saveRDS(model_multinom, "./models/multinom.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  IV. k-Nearest Neighbors  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# define features (x) and target (y), and scale x
y <- train$status_group
x_scaled <- train %>% select(longitude, latitude, gps_height,
                             pop_log3, amount_log, years_op) %>% scale()
# define 10-fold cross-validation training method
train_control <- trainControl(method="cv", number=10, savePredictions="final")
# KNN classification
model_knn <- train(x_scaled, y, method="knn", 
                   trControl=train_control, tuneGrid=expand.grid(k=7))
# summary of results
print(model_knn)
# confusion matrix
confusionMatrix(model_knn)
# save model
saveRDS(model_knn, "./models/knn.rds")



# Naive Bayes?


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  V. Ensemble Models  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Majority Vote



### Stacked Model - 



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  V. Create submission  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
submission <- data.frame(test$id)
submission$status_group <- pred_forest_test
names(submission)[1] <- "id"
