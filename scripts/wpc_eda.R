######################################################################%
##########   Preprocessing and Exploratory Data Analysis   ###########%
######################################################################%

### Load libraries ###
library(tidyverse)
library(lubridate)
library(googleVis)
library(lsr)
library(corrplot)
# devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)  # install most recent version
library(ggmap)
library(cowplot)

### Load custom functions ###
## correlation function
# computes the correlation coefficent (r or its equivalent) across data types, e.g.:
# continuous V continuous - 
# continuous V categorical/factor - 
# categorical V categorical - 
source("./scripts/cor2Fun.R")

### Define colors for waterpoint status
status_colors <- c("#0072B2", "#E69F00", "#CF3816")



#%%%%%%%%%%%%%%%%%%%%%%%%%%
######  Import Data  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%

### Download data if necessary
# Download train data
if(!file.exists("./data/train_raw.csv")){
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
  
### Load raw data
# Load train data
train_raw <- read.csv("./data/train_raw.csv")
# Load train labels
train_labels <- read.csv("./data/train_labels.csv")
# Join train data and labels
train_raw <- left_join(train_labels, train_raw, by="id")
# Copy train data for modification/preprocessing
train <- train_raw  
# Load test data
test_raw <- read.csv("./data/test_raw.csv")
# Copy test data for modification/preprocessing
test <- test_raw

#%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  Explore Data  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Examine all 41 variables:
str(train)
table(is.na(train))  # no NAs, though there is missing data - zeros or empty strings
# save number of observations
n <- dim(train)[1]  

## id ##___________________________________________________
# Well ID

## status_group ##_________________________________________
# Functional status of the well
# Outcome variable
table(train$status_group)
prop.table(table(train$status_group))
train %>% ggplot(aes(status_group)) +
  geom_bar(aes(y=(..count..)/sum(..count..), fill=status_group)) +
  ggtitle("Waterpoint Condition") +
  xlab("") +
  ylab("proportion") +
  theme(legend.position="none") +
  scale_fill_manual(values=status_colors) +
  theme(legend.position="none")


## amount_tsh ##___________________________________________
# payment amount in Tsh - associated with payment_type
# check blanks - 70% zeros
sum(train$amount_tsh == 0)/n
# print summary statistics
summary(train$amount_tsh)
# table of median, max, and number of payments based on payment type
train %>% group_by(payment_type) %>%
  summarize(n=n(), n_pay = sum(amount_tsh > 0), median(amount_tsh), max(amount_tsh))
# we see the large number of zeros makes sense, though there is not perfect agreement
# payment_type["never pay"] shows 93 payments (out of 25k)
# check for zeros in categories which should have payments - about 9%
train %>% summarize(blanks = sum(amount_tsh==0 & !(payment_type=="never pay" | payment_type=="unknown" | payment_type=="on failure"))/n)
# plot histogram of non-zero payments broken down by payment type
train %>% filter(amount_tsh > 0) %>% 
  ggplot(aes(amount_tsh, fill=payment_type)) +
  geom_histogram() +
  scale_x_log10() +
  theme_bw()
# log10 transform is closer to normal dist, so save new train and test variable
train$amount_log <- ifelse(train$amount_tsh > 0, log10(train$amount_tsh), 0)
test$amount_log <- ifelse(test$amount_tsh > 0, log10(test$amount_tsh), 0)

# NOTE: amount_tsh is listed as amount total static head on drivendata.org
# but I strongly believe this is incorrect
# total static head is essentially a measure of water pressure
# so quantity=="dry" wells should have zero pressure, but that is not the case
train %>% filter(amount_tsh > 0) %>%
  ggplot(aes(quantity, amount_tsh)) +
  geom_boxplot() +
  scale_y_log10()
train %>% group_by(quantity) %>%
  summarize(sum(amount_tsh == 0)/n())
train %>% group_by(extraction_type_class) %>%
  summarize(mean(amount_tsh))

## date_recorded ##________________________________________
# date of well assessment
# y-m-d
sum(train$date_recorded == 0)/n  # no zeros
qplot(year(train$date_recorded))  # mostly recorded between 2011 and 2013
min(year(train$date_recorded))  # recorded as early as 2002
# convert to year recorded for later manipulation
train$year_recorded <- as.integer(year(train$date_recorded))
test$year_recorded <- as.integer(year(test$date_recorded))


## funder ##_______________________________________________
# who funded the well
# probably not as important as "installer", but worth checking
# number of unique funders - 1898
length(unique(train$funder))
# table of top 20 funders - #2 is blank
train %>% group_by(funder) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head(20)
test %>% group_by(funder) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head(20)


## installer ##____________________________________________
# who installed the well
# number of unique installers - 2146
length(unique(train$installer))
# table of top 10 installers - #2 is blank
train %>% group_by(installer) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head(10)
test %>% group_by(installer) %>% summarize(n=n()) %>% arrange(desc(n)) %>% head(10)


## Longitude ##____________________________________________
## Latitude ##
# check missingness - only about 3% of longitude, no missing latitude 
sum(train$longitude == 0)/n
sum(train$latitude == 0)/n
## googleVis "interactive" map
w = 1000  # number of wells to map
wells_map <- train[1:w,] %>%
  mutate(latlong = paste(round(latitude,2), round(longitude, 2), sep = ":"),
         size = rep(1, w)) %>% # marker size
  gvisGeoChart(locationvar = "latlong",
               colorvar = "status_group",
               sizevar = "size",
               options = list(region = "TZ", 
                              colorAxis= "{colors:[\'green', \'yellow', \'red']}"))
plot(wells_map)
## ggmap
# set google API key
ggmap::register_google(key="SET YOUR API KEY HERE")  # deleted API key to prevent misuse
# get google base map
base_map <- get_googlemap(center="Tanzania",
                          zoom=6, size=c(640, 640), scale=2,
                          format='png8', maptype='roadmap', color='color')
# check base map
ggmap(base_map)
# save basemap
saveRDS(base_map, "./reports_and_figures/base_map.rds")
# plot waterpoints on base map
ggmap(base_map) +
  geom_point(aes(x=longitude, y=latitude, color = status_group), data=train, 
             shape=16, alpha=0.35) +
  scale_color_manual(values=status_colors) +
  theme(legend.position = "top", legend.title = element_blank())

## Other Location Variables _______________________________
## gps_height ## - altitude of the well
# summary statistics
summary(train$gps_height)
# histogram
hist(train$gps_height)

## basin ## - geographic water basin
# summary statistics
summary(train$basin)

### geographic divisions from smallest to largest 
## subvillage ##
length(unique(train$subvillage))
## ward ##
length(unique(train$ward))
## lga ##
length(unique(train$lga))
## region ##
length(unique(train$region))
## region_code ##
length(unique(train$region_code))
## district_code ##
length(unique(train$district_code))

# district should be a subdivision of region, 
# but we have more regions than districts in this dataset
# also a different number of unique regions and region codes
# so there is an issue with these divisions

# change region and district codes from numeric to factor
train$region_code <- as.factor(train$region_code)
train$district_code <- as.factor(train$district_code)
test$region_code <- as.factor(test$region_code)
test$district_code <- as.factor(test$district_code)


## wpt_name ##_____________________________________________
# name of the waterpoint
# number of unique names
length(unique(train_raw$wpt_name))  
# not all the names are unique, 
# but it is unnecessary since we have the waterpoint id, so delete
train <- train %>% select(-wpt_name)
test <- test %>% select(-wpt_name)


## num_private ##__________________________________________
# summary statistics
summary(train$num_private)
# unknown variable, delete
train <- train %>% select(-num_private)
test <- test %>% select(-num_private)


## population ##___________________________________________
# Population around the well 
# summary statistics
summary(train$population)
# check missing - almost half zeros and ones
sum(train$population < 2)/n
# plot population (>1) on log scale 
train %>% filter(population > 1) %>%
  ggplot(aes(population)) +
  geom_histogram(bins=30) +
  scale_x_log10()
# qqplot of population on log scale
qqnorm(log10(train$population[train$population > 1]))
qqline(log10(train$population[train$population > 1]), col = "blue", lwd = 2)
# log scale closer to normally distributed, so save new variable
train$pop_log <- ifelse(train$population > 0, log10(train$population), 0)
test$pop_log <- ifelse(test$population > 0, log10(test$population), 0)


## public_meeting ##_______________________________________
# table
table(train$public_meeting)
# meaning is unclear, and little variance, so delete
train <- train %>% select(-public_meeting)
test <- test %>% select(-public_meeting)


## recorded_by ##__________________________________________
# who recorded the data
unique(train$recorded_by)
# only one level, so delete
train <- train %>% select(-recorded_by)
test <- test %>% select(-recorded_by)


## scheme_management ##____________________________________
# who operates the well
# sorted table of scheme management - #3 is blank
train %>% group_by(scheme_management) %>% summarize(n=n()) %>% arrange(desc(n))
test %>% group_by(scheme_management) %>% summarize(n=n()) %>% arrange(desc(n))
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
test$scheme_management[train$scheme_management=="swc"|
                       train$scheme_management=="trust"] <- "other"
# change string back to factor
train$scheme_management <- as.factor(train$scheme_management)
test$scheme_management <- as.factor(test$scheme_management)


## scheme_name ##__________________________________________
# meaning unclear
# check number of unique names
length(unique(train$scheme_name))
# sorted table of scheme names
train %>% group_by(scheme_name) %>% summarize(n=n()) %>% arrange(desc(n))
# mostly blanks, otherwise a large number of unique names, so delete
train <- train %>% select(-scheme_name)
test <- test %>% select(-scheme_name)


## permit ##_______________________________________________
# True if the well was permitted
table(train$permit)
# change blank level to "unknown"
levels(train$permit)[levels(train$permit)==""] <- "unknown"
levels(test$permit)[levels(test$permit)==""] <- "unknown"
# table of permit by status group
prop.table(table(train$permit, train$status_group), margin = 1) %>% round(2)


## construction_year ##____________________________________
# Year the well was constructed
# check missing - approx one-third
sum(train$construction_year == 0)/n
# histogram for construction_year grouped by status_group
train %>% filter(construction_year > 0) %>%
  ggplot(aes(construction_year)) + 
  geom_histogram(bins = 20) + 
  facet_grid( ~ status_group)
# calculate median construction year for (temporary) imputation
year_med <- train %>% filter(construction_year > 0) %>% summarize(m = median(construction_year)) %>% .$m
# create variable for years between construction and date_recorded
train$years_op <- ifelse(train$construction_year == 0,
                         train$year_recorded - year_med,  # temporarily impute median construction year
                         train$year_recorded - train$construction_year)
test$years_op <- ifelse(test$construction_year == 0,
                        test$year_recorded - year_med,  # temporarily impute median construction year
                        test$year_recorded - test$construction_year)
# plot histogram of years_op
qplot(train$years_op, geom="histogram", bins=20)
qplot(test$years_op, geom="histogram", bins=20)
# todo: impute construction year by location and/or other variables


## extraction_type ##______________________________________
## extraction_type_group ##
## extraction_type_class ##
# tables of these overlapping variables
table(train$extraction_type)
table(train$extraction_type_group)
table(train$extraction_type_class)
# only use one of the three - initally extraction_type_group

## management ##___________________________________________
## management_group ##
# who manages the well
# similar to scheme_management?
# table of management and management_group levels:
train %>% group_by(management) %>% count() %>% arrange(desc(n))
test %>% group_by(management) %>% count() %>% arrange(desc(n))
train %>% group_by(management_group) %>% count() %>% arrange(desc(n))
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
# initally prefer management 


## payment ##______________________________________________
## payment_type ##
# If and how payments are made
# table of payment/payment type
train %>% group_by(payment) %>% count() %>% arrange(desc(n))
train %>% group_by(payment_type) %>% count() %>% arrange(desc(n))
prop.table(table(train$payment_type, train$status_group), margin=1) %>% round(2)
# payment and payment type are identical, except for the names
# e.g. "pay annually" VS "annually", so delete
train <- train %>% select(-payment)
test <- test %>% select(-payment)


## water_quality ##________________________________________
## quality_group ##
# quality of the water coming from the well
# water_quality may be data leakage, because some levels specify "abandoned", e.g. "salty abandoned"
# the association between "abandoned"  in "water_quality" and status_group is not clear,
# but use "quality_group" anyways
train %>% group_by(water_quality) %>% count() %>% arrange(desc(n))
train %>% group_by(quality_group) %>% count() %>% arrange(desc(n))
prop.table(table(train$water_quality, train$status_group), margin = 1) %>% round(2)
prop.table(table(train$quality_group, train$status_group), margin = 1) %>% round(2)


## quantity ##_____________________________________________
## quantity_group ##
# quantity may be data leakage - dry wells are almost always non-functional
train %>% group_by(quantity) %>% count() %>% arrange(desc(n))
train %>% group_by(quantity_group) %>% count() %>% arrange(desc(n))
prop.table(table(train$quantity, train$status_group), margin=1) %>% round(2)
# plot quantity stacked by status group
qplot(quantity, data=train, geom="bar", fill=status_group)
# quantity and quantity_group are identical, so delete one
train <- train %>% select(-quantity_group)
test <- test %>% select(-quantity_group)


## source ##_______________________________________________
## source_type ## 
## source_class ##
# Water source for the well
train %>% group_by(source) %>% count() %>% arrange(desc(n))
train %>% group_by(source_type) %>% count() %>% arrange(desc(n))
train %>% group_by(source_class) %>% count() %>% arrange(desc(n))
# initally prefer source_type


## waterpoint_type ##______________________________________
## waterpoint_type_group ## 
# what kind of well/how the water is accessed
# waterpoint_type_group only combines "communal standpipe" and "communal standpipe multiple"
train %>% group_by(waterpoint_type) %>% count() %>% arrange(desc(n))
train %>% group_by(waterpoint_type_group) %>% count() %>% arrange(desc(n))
# plot waterpoint_type stacked by status group
train %>% group_by(waterpoint_type) %>% 
  mutate(wp_type_tot = n()) %>% ungroup() %>%
  ggplot(aes(x=reorder(waterpoint_type, status_group=="non functional"),
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
        legend.position=c(-0.5, 1.08),
        legend.spacing.x=unit(2, "mm"),
        legend.direction = "horizontal")
# initally prefer waterpoint_type


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  Additional Feature Preprocessing  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# modify variables that couldn't be modified
# within a few lines of code in the section above

#________________________________________________
#### Funder ####
summary(train$funder)
# same funder with different names in many instances
# e.g. 'Government','Gover', 'GOVER', 'Govt', 'Central government'
# Make funder lowercase
train$funder20 <- tolower(train$funder)
test$funder20 <- tolower(test$funder)
# summary of funders
summary(as.factor(train$funder20))
# label blanks as unknown
train$funder20[train$funder20 %in% c(" ", "", "0", "_", "-")] <- "unknown"
test$funder20[test$funder20 %in% c(" ", "", "0", "_", "-")] <- "unknown"

## government
# combine repeated instances of government funder with regex
# include "district" and "council" as forms of government
# check the strings where "gov","dist", or "cent" is detected 
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="gov")]))
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="dist")]))
summary(as.factor(train$funder20[str_detect(train$funder20, pattern="coun")]))
# few/negligible instances of non-Tanzania government, e.g. italian gov - 4, japan gov - 1
# one instance of non-districts: methodist church - 1 
# no instances of non-council/gov/dist
# So, the government related regex pattern is:
gov <- "(gov)|(dist)|(coun)"  # also add "cent"?
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

# create additional variable with only the top 10 funders
summary(as.factor(train$funder20))[1:10]
train$funder10 <- train$funder20
funder_top10 <- names(summary(as.factor(train$funder10)))[1:10]
train$funder10[!(train$funder10 %in% funder_top10)] <- "other"
train$funder10 <- as.factor(train$funder10)
# same for test set
test$funder10 <- test$funder20
funder_top10 <- names(summary(as.factor(test$funder10)))[1:10]
test$funder10[!(test$funder10 %in% funder_top10)] <- "other"
test$funder10 <- as.factor(test$funder10)
# reduce funder20 to top 20 funders
funder_top20 <- names(summary(as.factor(train$funder20)))[1:20]
train$funder20[!(train$funder20 %in% funder_top20)] <- "other"
train$funder20 <- as.factor(train$funder20)
summary(train$funder20)
# same for test set
funder_top20 <- names(summary(as.factor(test$funder20)))[1:20]
test$funder20[!(test$funder20 %in% funder_top20)] <- "other"
test$funder20 <- as.factor(test$funder20)
summary(test$funder20)


#________________________________________________
#### Installer ####
summary(train$installer)
# same installer with different names in many instances
# e.g. 'Government','Gover', 'GOVER', 'Govt', 'Central government'
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
# check the strings where "gov","dist", or "cent" is detected 
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="gov")]))
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="dist")]))
summary(as.factor(train$installer20[str_detect(train$installer20, pattern="cent")]))
# few/negligible instances of non-Tanzania government, e.g. italian gov - 4, japan gov - 1
# one instance of non-districts: methodist church - 1 
# no instances of non-council/gov/dist
# So, the government related regex pattern is:
gov <- "(gov)|(dist)|(coun)"  # also add "cent"?
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

# create additional variable with the top 10 installers
summary(as.factor(train$installer20))[1:10]
train$installer10 <- train$installer20
install_top10 <- names(summary(as.factor(train$installer20)))[1:10]
train$installer10[!(train$installer10 %in% install_top10)] <- "other"
summary(as.factor(train$installer10))
train$installer10 <- as.factor(train$installer10)
# same for test set
test$installer10 <- test$installer20
install_top10 <- names(summary(as.factor(test$installer20)))[1:10]
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
install_top20 <- names(summary(as.factor(test$installer20)))[1:20]
test$installer20[!(test$installer20 %in% install_top20)] <- "other"
test$installer20 <- as.factor(test$installer20)
summary(test$installer20)

# Table of installer20 VS the well/pump status
prop.table(table(train$installer20, train$status_group), margin = 1) %>% round(2)
# bar chart of installer20 VS waterpoint status
train %>% 
  ggplot(aes(x=reorder(installer20, -(status_group=="non functional")), fill=status_group)) +
  geom_bar(position="fill")


#__________________________________________________________
#### Population ####
# almost half zeros and ones
sum(train$population < 2)/n
sum(test$population < 2)/14850

# check for group medians = 0
train %>% group_by(region_code) %>% summarize(med = median(population)) %>% 
  summarize(zeros = sum(med == 0)/length(med)) %>% pull(zeros)
# subvillage = 0.39 (proportion where medians = 0)
# ward = 0.39
# district_code = 0.25
# region = 0.29
# region code = 0.22

# impute zeros and ones based on median within region_code
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
sum(train$population2 < 2)/n
# plot new population variable
train %>% ggplot(aes(population2)) +
  geom_histogram() +
  scale_x_log10()  # log transform is somewhat normal looking
# take log transform, and retain zeros
train <- train %>% mutate(pop_log2 = ifelse(population2 == 0, 0, log10(population2)))
test <- test %>% mutate(pop_log2 = ifelse(population2 == 0, 0, log10(population2)))


### Impute population with KNN ###
# create dataframe with features
x_pop <- train %>% filter(population > 1) %>% 
  select(latitude, longitude)
# create vector with target variable
y_pop <- train %>% filter(population > 1) %>% 
  pull(pop_log)  # using log results in a more accurate prediction
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


## construction_year ______________________________________
# ~one-third missing data
sum(train$construction_year == 0)/n
# Future: 
# impute construction year based on location, installer, funder, etc.?


#### Save Preprocessed Data ###
write.csv(train, file='./data/train.csv', row.names=F)
write.csv(test, file='./data/test.csv', row.names=F)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%
######  Correlations  ######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Selected features
var <- c("amount_tsh", "amount_log", "gps_height", "installer20", "funder20",
         "longitude", "latitude", "population", "pop_log2", "pop_log3",
         "scheme_management", "permit", "construction_year", "years_op",
         "extraction_type_group", "management", "payment_type", 
         "source_type", "waterpoint_type", "quality_group", "quantity")
# calculate correlations for mixed feature types using cor2() function
corr <- train %>% select(var) %>% cor2()
# plot correlation matrix
corrplot.mixed(corr)

## correlated (R > 0.5):
# payment_type and (payment) amount_log (but not amount_tsh) 
# management and scheme_management (and management_group)
  # use management, because it has less unknowns than scheme_management
# pop_log and gps_height with construction year (but not years_op)


### Location Variables
loc <- c("longitude", "latitude", "region_code", "district_code", "lga",
         "gps_height", "basin")
# calculate correlations for mixed feature types using cor2() function
corr <- train %>% select(loc) %>% cor2()
# plot correlation matrix
corrplot.mixed(corr)


### amount_tsh
corr <- train %>% mutate(amount_zeros = ifelse(amount_tsh==0, 0, 1)) %>%
  select(amount_tsh, amount_log, amount_zeros,
         payment_type, quantity, status_group) %>% cor2()
# plot correlation matrix
corrplot.mixed(corr)

# var <- c("scheme_management", "management", "management_group")

