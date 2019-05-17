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
# load model
# model_rfe <- readRDS("./models/feature_elimination.rds")
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


#### Tune model parameters ####
# define 10-fold cross-validation training method
train_control <- trainControl(method="cv", number=10, savePredictions="final")

### Tune number of trees
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
# 101 trees appears adequate (but will use 201 for good measure)
ntree <- 201
# save model
saveRDS(model_rf_trees, "./models/rf_trees.rds")

## save just the results
results_rf_trees <- lapply(model_rf_trees, `[[`, "results")
# pull accuracy and SD results into df 
results_rf_trees_acc <- sapply(results_rf_trees, `[[`, "Accuracy")
results_rf_trees_ci <- sapply(results_rf_trees, `[[`, "AccuracySD")*1.96/sqrt(10)
results_rf_trees <- data.frame("n_trees" = c(51,101,201,301,501),
                               "accuracy" = results_rf_trees_acc,
                               "ci" = results_rf_trees_ci)
rownames(results_rf_trees) <- NULL
# save
saveRDS(results_rf_trees, "./models/results_rf_trees.rds")

## load model or results
# model_rf_trees <- readRDS("./models/rf_trees.rds")
# results_rf_trees <- readRDS("./models/results_rf_trees.rds")

### Tune mtry
modelLookup("rf")
grid_rf <- expand.grid(mtry=c(2,3,4,5))
# Random Forest model
model_rf_mtry <- train(x_10, y, method="rf", ntree=ntree,
                       trControl=train_control, tuneGrid=grid_rf)
# summary
print(model_rf_mtry)
# none are significantly better than the others, but stick with mtry=3
grid_rf <- expand.grid(mtry=3)
# save model
saveRDS(model_rf_mtry, "./models/rf_mtry.rds")
# load model
# model_rf_mtry <- readRDS("./models/rf_mtry.rds")

#_________________________________
#### Try alternative features ####

## rerun baseline model
rf_baseline <- train(x_10, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_baseline)

## installer20 -> installer10
rf_inst <- x_10 %>% select(-installer20) %>% mutate(installer10 = train$installer10) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_inst)
# not significantly worse

## add funder20 or funder10
rf_fund <- x_10 %>% mutate(funder20 = train$funder20) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_fund)
# slight improvement?

## extraction_type_group -> extraction_type_class
rf_ext <- x_10 %>% select(-extraction_type_group) %>% mutate(extraction_type_class = train$extraction_type_class) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_ext)
# reduced accuracy slightly

## management -> management_group or scheme_management
rf_man <- x_10 %>% select(-management) %>% mutate(management_group = train$management_group) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_man)
# slightly reduced accuracy, so keep management

## pop_log3 -> pop_log or pop_log2
rf_pop <- x_10 %>% select(-pop_log3) %>% mutate(pop_log = train$pop_log) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_pop)
rf_pop2 <- x_10 %>% select(-pop_log3) %>% mutate(pop_log2 = train$pop_log2) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_pop2)
# slightly improved accuracy

## payment_type -> amount_log
rf_pay <- x_10 %>% select(-payment_type) %>% mutate(amount_log = train$amount_log) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_pay)
# lower accuracy

## years_op -> construction year
rf_year <- x_10 %>% select(-years_op) %>% mutate(construction_year = train$construction_year) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_year)
# not significantly different

## everything

#______________________________________
#### Final Model ####
# none of the variations (besides leakage) are significantly better than baseline
# try final model with variations that appear to make an improvement:
# pop_log3 -> plo_log & add funder20
x_final <- x_10 %>% select(-pop_log3) %>% mutate(pop_log = train$pop_log, funder20=train$funder20)
# define 10-fold cross-validation training method that saves predictions 
# and probabilities...
train_control <- trainControl(method="cv", number=10, savePredictions="final")#, classProbs=T)
# run final model
rf_final <- train(x_final, y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
rf_final_500 <- train(x_final, y, method="rf", ntree=501, trControl=train_control, tuneGrid=grid_rf)
# print model
print(rf_final)
print(rf_final_500)
# variable importance
varImpPlot(rf_final$finalModel)
# confusion matrix
confusionMatrix(rf_final$pred$pred, reference=rf_final$pred$obs)
# save final rf model
saveRDS(rf_final, "./models/rf_final.rds")

## add quantity and quality (data leakage) for comparison
rf_leak <- x_final %>% mutate(quantity = train$quantity, quality = train$water_quality) %>%
  train(., y, method="rf", ntree=ntree, trControl=train_control, tuneGrid=grid_rf)
print(rf_leak)
varImpPlot(rf_leak$finalModel)
# significantly improves model, as expected


#________________________________________________
#### Save random forest model results ####
# manually select model names
rf_tests_names <- c("rf_baseline", "rf_inst","rf_fund", "rf_ext", 
                    "rf_man", "rf_pop", "rf_pop2", "rf_pay", "rf_year", 
                    "rf_final", "rf_leak")
# create a list based on models names provided
model_rf_tests <- lapply(rf_tests_names, get)
# set names
names(model_rf_tests) <- rf_tests_names
# check the output results
sapply(model_rf_tests, `[[`, "results")
# save models
saveRDS(model_rf_tests, "./models/rf_tests.rds")

## save just the results
results_rf_tests <- lapply(model_rf_tests, `[[`, "results")
# pull accuracy and SD results into df (with stack)
results_rf_tests_acc <- stack(sapply(results_rf_tests, `[[`, "Accuracy"))[2:1]  # [2:1] changes order
results_rf_tests_ci <- stack(sapply(results_rf_tests, `[[`, "AccuracySD")*1.96/sqrt(10))[2:1]
# combine accuracy and SE into a single dataframe
results_rf_tests <- left_join(results_rf_tests_acc, results_rf_tests_ci, by="ind")
results_rf_tests <- setNames(results_rf_tests, c("test_name", "accuracy", "ci"))
saveRDS(results_rf_tests, "./models/results_rf_tests.rds")

## load models or results
# model_rf_tests <- readRDS("./models/rf_tests.rds")
# results_rf_tests <- readRDS("./models/results_rf_tests.rds")


#______________________________________
#### Other Misc. Performance Tests ####

### model formula call VS x, y call ###
# x, y call is significantly more accurate for some reason...
# i.e. train(x, y...) is more accurate than the formula call below
# compare below to model_rf_baseline

# formula alternative to model call
model_form_11 <- as.formula(status_group ~ installer20 + funder20 + longitude + latitude +
                            years_op + extraction_type_group + waterpoint_type +
                            payment_type + gps_height + pop_log + management)
# tune grid and train control
grid_rf_form <- expand.grid(mtry=c(5,10,15,20,25))
train_control <- trainControl(method="cv", number=10, savePredictions="final")
# Random Forest with formula call
rf_form <- train(model_form_11, data=train, method="rf", ntree=501,
                       trControl=train_control, tuneGrid=grid_rf_form)
# Summarize results
print(rf_form)
# plot variable importance 
varImpPlot(rf_form$finalModel)
# plot results
rf_form$results %>% ggplot(aes(mtry, Accuracy)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=Accuracy-2*AccuracySD/sqrt(10), 
                    ymax=Accuracy+2*AccuracySD/sqrt(10))) +
  theme_bw()
# save model
saveRDS(rf_form, "./models/rf_form.rds")

### 5-fold vs 10-fold C.V. ###
# define cross-validation training method
train_control <- trainControl(method="cv", number=5, savePredictions="final")
grid_rf <- expand.grid(mtry=3)
# Random Forest model
rf_5cv <- train(x_10, y, method="rf", ntree=ntree,
                        trControl=train_control, tuneGrid=grid_rf)
print(rf_5cv)
plot(rf_5cv$finalModel)
# plot variable importance 
varImpPlot(rf_5cv$finalModel)
# 5-fold appears adequate, but accuracy is slightly inflated , so continue to use 10-fold


#_________________________________________________
#### Plot Random Forest test/tune results ####

## Tests
results_rf_tests %>% filter(!test_name=="rf_leak") %>%
  ggplot(aes(test_name, accuracy)) +
  geom_point() +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci)) +
  theme_bw()

## Trees
results_rf_trees %>% ggplot(aes(n_trees, accuracy)) +
  geom_point() +
  geom_errorbar(aes(ymin=accuracy-ci, ymax=accuracy+ci)) +
  theme_bw()

## mtry
model_rf_mtry$results %>% ggplot(aes(mtry, Accuracy)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=Accuracy-2*AccuracySD/sqrt(10), 
                    ymax=Accuracy+2*AccuracySD/sqrt(10))) +
  theme_bw()


#%%%%%%%%%%%%%%%%%%%%%%%%%%
######  II. XGBoost  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%

train_control <- trainControl(method="cv", number=10, savePredictions="final")
# define model formula
model_form_13 <- as.formula(status_group ~ amount_tsh + installer20 + 
                            longitude + latitude + gps_height + 
                            pop_log3 + permit + years_op + extraction_type_group +
                            management + payment_type + source_type + waterpoint_type)
# define tuning grid 
modelLookup("xgbTree")
# grid_xgb <- expand.grid()
# xgbTree model
model_xgb <- train(model_form_13, data=train, method="xgbTree", 
                        trControl=train_control, tuneLength=5)
# summary of results
print(model_xgb)
# confusion matrix
confusionMatrix(model_xgb$pred$pred, reference=model_xgb$pred$obs)
# save model
saveRDS(model_xgb, "./models/xgb.rds")
# load model
# model_xgb <- readRDS("./models/xgb.rds")


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
# load model
# model_multinom <- readRDS("./models/multinom.rds")


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
# load model
# model_knn <- readRDS("./models/knn.rds")

#_________________________________________________
#### Save all model results ####

# create df with accuracy and confidence interval
results_all <- data.frame("model" = c("rf", "leak", "xgb", "multinom", "knn", "vote", "stack"),
                          "accuracy" = c(rf_final$results$Accuracy,
                                         rf_leak$results$Accuracy,
                                         max(model_xgb$results$Accuracy),
                                         model_multinom$results$Accuracy, 
                                         model_knn$results$Accuracy, NA, NA),
                          "ci" = 1.96/sqrt(10)* c(rf_final$results$AccuracySD,
                                                  rf_leak$results$AccuracySD,
                                                  model_xgb$results$AccuracySD[model_xgb$results$Accuracy==max(model_xgb$results$Accuracy)],
                                                  model_multinom$results$AccuracySD, 
                                                  model_knn$results$AccuracySD, NA, NA))
# saveRDS(results_all, "./models/results_all.rds")


#### ROC Curves ####

cm_rf <- confusionMatrix(rf_final$pred$pred, reference=rf_final$pred$obs)

# set non-functional to 1 and functonal and functional needs repair to zero
y_roc <- rf_final$finalModel$y
levels(y_roc)[levels(y_roc)=="non functional"] <- 1
levels(y_roc)[levels(y_roc)=="functional"] <- 0
levels(y_roc)[levels(y_roc)=="functional needs repair"] <- 0
# probabilities for fnon-functional
x_roc <- rf_final$finalModel$votes[,3]
# calculate and plot ROC curve
roc_rf <- roc(y_roc, x_roc)
ggroc(roc_rf, color="blue") +
  geom_point(aes(x=0.8910, y=0.7217), color="red")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  V. Ensemble Model  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Majority Vote
# create list will all relevant models
all_names <- c("rf_final", "model_xgb","model_multinom", "model_knn")
all_models <- lapply(all_names, get)
names(all_models) <- c("rf", "xgb", "log", "knn")
# make predictions
pred_train <- as.data.frame(sapply(models_all, function(x) predict(x, train)))
names(pred_train) <- c("rf", "xgb", "log", "knn")
# make ensemble
pred_train <- pred_train %>% mutate(vote = if_else(xgb==log & xgb==knn, xgb, rf))

### Stacked random forest
pred_train <- add_column(pred_train, target = train$status_group, .before=1)
train_control <- trainControl(method="cv", number=10, savePredictions="final")
stack_rf_tune <- train(target ~ rf + xgb + log + knn, data=pred_train, method="rf", 
                       trControl=train_control, tuneLength=5)
# use mtry=2
stack_rf <- randomForest(target ~ rf + xgb + log + knn, data=pred_train, mtry=2, ntrees=501)
pred_train$stack <- predict(stack_rf, pred_train)
print(stack_rf)

#________________________________________________
#### Update predictions with years_op to reflect current year ####
x_update <- x_final %>% mutate(years_op = years_op + (2019 - train$year_recorded))
mean(x_final$years_op)
mean(x_update$years_op)
pred_train$update <- predict(rf_final, x_update)
sum(!pred_train$update==pred_train$rf)
sum(pred_train$rf=="functional" & pred_train$update=="non functional")
sum(pred_train$rf=="functional" & pred_train$update=="functional needs repair")
sum(pred_train$rf=="functional needs repair" & pred_train$update=="non functional")
sum(pred_train$rf=="functional needs repair" & pred_train$update=="functional")
sum(pred_train$rf=="non functional" & pred_train$update=="functional")
sum(pred_train$rf=="non functional" & pred_train$update=="functional needs repair")

## Partial dependence plot
pdp_f <- partialPlot(rf_final$finalModel, x_final, x.var=years_op)
pdp_r <- partialPlot(rf_final$finalModel, x_final, x.var=years_op, which.class="functional needs repair")
pdp_n <- partialPlot(rf_final$finalModel, x_final, x.var=years_op, which.class="non functional")
grid.arrange(pdp_f, pdp_r, pdp_n, nrow=1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  V. Prepare submissions  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
submission <- data.frame(test$id)
names(submission)[1] <- "id"

# prepare x_test for random forest
x_test <- test %>% select(installer20,  longitude, latitude, gps_height, 
                          years_op, extraction_type_group, management, 
                          payment_type, waterpoint_type, pop_log, funder20)
levels(test$installer20)[levels(test$installer20)=="da"] <- "danid"
levels(x_test$installer20) <- levels(x_final$installer20)
x_test_leak <- x_test %>% mutate(quantity = test$quantity, quality = test$water_quality)

# Random forest
submission_rf <- submission
submission_rf$status_group <- predict(rf_final, x_test)

# Random forest w/ leakage
submission_leak <- submission
submission_leak$status_group <- predict(rf_leak, x_test_leak)

# XGBoost
submission_xgb <- submission
submission_xgb$status_group <- predict(model_xgb, test)

# Majority vote ensemble
pred_test <- data.frame("rf" = submission_rf$status_group,
                        "xgb" = predict(model_xgb, test),
                        "log" = predict(model_multinom, test),
                        "knn" = predict(model_knn, test))
pred_test <- pred_test %>% mutate(vote = if_else(xgb==log & xgb==knn, xgb, rf))
submission_vote <- submission
submission_vote$status_group <- pred_test$vote

# Stacked random forest ensemble
submission_stack <- submission
submission_stack$status_group <- predict(stack_rf, pred_test)

# Save submissions
write.csv(submission_rf, "./output/submission_rf.csv", row.names=F)
write.csv(submission_xgb, "./output/submission_xgb.csv", row.names=F)
write.csv(submission_leak, "./output/submission_leak.csv", row.names=F)
write.csv(submission_vote, "./output/submission_vote.csv", row.names=F)
write.csv(submission_stack, "./output/submission_stack.csv", row.names=F)

# save final results
results_all <- results_all %>% mutate(test = c(0.7801, 0.8125, 0.7601, NA, NA, 0.7728, 0.7800))
saveRDS(results_all, "./models/results_all.rds")



