# Predicting Waterpoint Condition
The purpose of this project is to predict the functionality of drinking water wells in Tanzania. The data comes from [drivendata.org](https://www.drivendata.org/competitions/7/pump-it-up-data-mining-the-water-table/)

## Directory Tree:

│--water_pump_condition.Rproj  
│  
│--data  
│------test.csv  
│------test_raw.csv  
│------train.csv  
│------train_labels.csv  
│------train_raw.csv  
│  
│--models  
│------feature_elimination.rds  
│------knn.rds  
│------multinom.rds  
│------rf_mtry.rds  
│------xgb_tune.rds  
│  
│--reports_and_figures  
│------wpc_report.docx  
│------wpc_report.html  
│------wpc_report.Rmd  
│  
│--scripts  
│------cor2Fun.R  
│------wpc_eda.R  
│------wpc_prelim_models.R  
        
### data

- test.csv  
preprocessed test data  
- test_raw.csv  
raw test data  
- train.csv  
preprocessed train data  
- train_labels.csv  
labels for target for train data  
- train_raw.csv  
raw train data  

### models
models are created in /scripts/wpc_prelim_models.R

- feature_elimination.rds  
recursive feature elimination with random forest  
- knn.rds  
k-nearest neighbors  
- multinom.rds  
multinomial logistic regression  
- rf_mtry.rds  
random forest tuning mtry (number of variables randomly sampled at each split)  
- xgb_tune.rds  
XGBoost tuning various parameters

### reports_and_figures

- report in .Rmd, .html, and .docx formats  
- images used in report

### scripts

- cor2Fun.R  
function that computes the correlation coefficients between different data types
- wpc_eda.R  
exploratry data analysis and data preprocessing
- wpc_prelim_models.R  
preliminary modeling
        