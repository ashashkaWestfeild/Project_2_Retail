
library(tidyverse)
library(tidymodels)
library(caTools)
library(car)

## load the data
path <- r'{C:\-\PROJECT\Project2- Retail\}'

store_train <- read.csv(paste0(path,'store_train.csv'))
store_test <- read.csv(paste0(path,'store_test.csv'))

setdiff(names(store_train), names(store_test)) # store

head(store_train$store) # 0 0 1 0 0 0 => Categorical

##------------DATA-DICTIONARY------------
# Id : Store ID 
# numeric sale figures for 5 types :
# sales0 : Numeric
# sales1 : Numeric
# sales2 : Numeric
# sales3 : Numeric
# sales4 : Numeric
# 
# country : categorical => coded values for country 
# State : categorical => coded values for State
# CouSub : numeric => subscription values at county level
# countyname : Categorical => county names
# storecode : categorical => store codes , this should not be used as is but can be source of a feature
# Areaname : categorical => name of the area , many times matches with county name
# countytownname : categorical => county town name
# population : numeric => population of the store area
# state_alpha : categorical => short codes for state
# store_Type : categorical => type of store 
# store : categorical 1/0 => target indicator var 1=opened 0=not opened 
##------------------------

## cleaning data-----------------
df <- store_train

View(df)
glimpse(df) # 3338 x 17

# check event rate
sum(df$store)/nrow(df) # 0.4382864
prop.table(table(df$store))

## checking all features

# sum(is.na(df$Id))
# sum(is.na(df$sales0))
# sum(is.na(df$sales1))
# sum(is.na(df$sales2))
# sum(is.na(df$sales3))
# sum(is.na(df$sales4))
# sum(is.na(df$country))
# sum(is.na(df$State))
# sum(is.na(df$CouSub))
# sum(is.na(df$countyname))
# sum(is.na(df$storecode))
# sum(is.na(df$Areaname))
# sum(is.na(df$countytownname))
# sum(is.na(df$population)) # 1
# sum(is.na(df$state_alpha))
# sum(is.na(df$store_Type))
# sum(is.na(df$store))
# 
# length(unique(df$country)) # 274
# country <- data.frame(prop.table(table(df$country)), table(df$country))[-3]
# View(country) # freq = 0.02
# 
# length(unique(df$State)) # 54
# state <- data.frame(prop.table(table(df$State)), table(df$State))[-3]
# View(state) # freq = 0.02
# 
# length(unique(df$CouSub)) # 1088
# cousub <- data.frame(prop.table(table(df$CouSub)), table(df$CouSub))[-3]
# View(cousub) # freq = 0.02
# 
# length(unique(df$countyname)) # 1491
# countyname <- data.frame(prop.table(table(df$countyname)), table(df$countyname))[-3]
# View(countyname) # freq = 0.01
# 
# length(unique(df$storecode)) # 1891
# storecode <- data.frame(prop.table(table(df$storecode)), table(df$storecode))[-3]
# View(storecode) # freq = 0.01
# 
# length(unique(df$Areaname)) # 1891
# areaname <- data.frame(prop.table(table(df$Areaname)), table(df$Areaname))[-3]
# View(areaname) # freq = 0.01
# 
# length(unique(df$countytownname)) # 2372
# countytownname <- data.frame(prop.table(table(df$countytownname)), table(df$countytownname))[-3]
# View(countytownname) # freq = 0.005
# 
# length(unique(df$state_alpha)) # 54
# state_alpha <- data.frame(prop.table(table(df$state_alpha)), table(df$state_alpha))[-3]
# View(state_alpha) # freq = 0.02
# 
# length(unique(df$store_Type)) # 4
# prop.table(table(df$store_Type))

# dummy to replace storecode
unique(substr(df$storecode,1,5)) # "NCNTY" "METRO"

df$storecode_NCNTY <- ((substr(df$storecode,1,5)=="NCNTY")*1)

store_test$storecode_NCNTY <- ((substr(store_test$storecode,1,5)=="NCNTY")*1)

sum(grepl('<', df$countyname)) # 10

quantile(df$population, na.rm = TRUE)
sum(df$population<10, na.rm = TRUE)
# df[is.na(df$population),'population']=median(df$population, na.rm = TRUE) 

# percentage increase between sales
(df$sales1-df$sales0)*100/df$sales0

df$PI_in_sales1 <- (df$sales1-df$sales0)*100/df$sales0
df$PI_in_sales2 <- (df$sales2-df$sales1)*100/df$sales1
df$PI_in_sales3 <- (df$sales3-df$sales2)*100/df$sales2
df$PI_in_sales4 <- (df$sales4-df$sales3)*100/df$sales3

store_test$PI_in_sales1 <- (store_test$sales1-store_test$sales0)*100/store_test$sales0
store_test$PI_in_sales2 <- (store_test$sales2-store_test$sales1)*100/store_test$sales1
store_test$PI_in_sales3 <- (store_test$sales3-store_test$sales2)*100/store_test$sales2
store_test$PI_in_sales4 <- (store_test$sales4-store_test$sales3)*100/store_test$sales3

hist(df$CouSub) 
hist(log(df$CouSub))

unique(df$store_Type)
# "Supermarket Type1", "Supermarket Type3", "Grocery Store", "Supermarket Type2"

df$country <- as.character(df$country)

## Preparing-----------------

df_pipe <- recipe(store~., data = df) %>% 
  update_role(Id, State, storecode, countytownname, Areaname, new_role = 'drop_vars') %>% 
  update_role(country, countyname, state_alpha, store_Type, new_role = 'to_dummy') %>% 
  update_role(population, new_role = 'NA_to_median') %>% 
  step_rm(has_role('drop_vars')) %>% 
  step_novel(has_role('to_dummy')) %>% 
  step_unknown(has_role('to_dummy'), new_level = '_missing_') %>% 
  step_other(has_role('to_dummy'), threshold = 0.02, other = '_other_') %>% 
  step_dummy(has_role('to_dummy')) %>% 
  step_mutate_at(CouSub, fn = log) %>% 
  step_impute_median(has_role('NA_to_median'), -all_outcomes())

df_pipe <- prep(df_pipe)  
  
train <- bake(df_pipe, new_data=NULL) 
test <- bake(df_pipe, new_data=store_test)

## Splitting 80-20
set.seed(3)
s <- sample(1:nrow(df), 0.8*nrow(df))
t1 <- train[s,]
t2 <- train[-s,]

## lm() to remove MULTICOLLINEARITY
# Iteration 0
vif_model <- lm(store~. , data = t1)

sort(vif(vif_model))
alias(vif_model)
  
# Iteration 1
vif_model <- lm(store~. -store_Type_X_other_, data = t1)

sort(vif(vif_model))

# Iteration 2 - Removing based on VIF value
vif_model <- lm(store~. -store_Type_X_other_ -sales0, data = t1)

sort(vif(vif_model))

# Iteration 3
vif_model <- lm(store~. -store_Type_X_other_ -sales0 -sales2, data = t1)

sort(vif(vif_model))

# Ieration 4
vif_model <- lm(store~. -store_Type_X_other_ -sales0 -sales2 -sales3, data = t1)

sort(vif(vif_model))

# Iteration 5
vif_model <- lm(store~. -store_Type_X_other_ -sales0 -sales2 -sales3 -sales1, data = t1)

sort(vif(vif_model))

# Iteration 6
vif_model <- lm(store~. -store_Type_X_other_ -sales0 -sales2 -sales3 -sales1 -state_alpha_X_other_, data = t1)

sort(vif(vif_model))

# Iteration 7
vif_model <- lm(store~. -store_Type_X_other_ -sales0 -sales2 -sales3 -sales1 -state_alpha_X_other_ -country_X_other_, data = t1)

sort(vif(vif_model))

# Iteration 8
vif_model <- lm(store~. -store_Type_X_other_ -sales0 -sales2 -sales3 -sales1 -state_alpha_X_other_ -country_X_other_ -PI_in_sales1, data = t1)

sort(vif(vif_model))

summary(vif_model)
# R-squared:  0.4816,	Adjusted R-squared:  0.4745 
# F-statistic: (p-value < 2.2e-16)
formula(vif_model)

## Logistic Regression
log_model1 <- glm(store~. -store_Type_X_other_ -sales0 -sales2 -sales3 -sales1 -state_alpha_X_other_ -country_X_other_ -PI_in_sales1, data = t1, family = "binomial")

summary(log_model1)

log_model1 <- stats::step(log_model1)

summary(log_model1)

formula(log_model1)
# store ~ population + storecode_NCNTY + country_X7 + state_alpha_IL + state_alpha_VT

log_model1 <- glm(store ~ population + storecode_NCNTY + country_X7 + state_alpha_IL + state_alpha_VT, data = t1, family = "binomial")

summary(log_model1)

## Saving Model
saveRDS(log_model1, file = paste0(path,'log_model1.RDS'))

## Predicting
t2_pred <- predict(log_model1, newdata = t2, type = 'response')
t1_pred <- predict(log_model1, newdata = t1, type = 'response')

pROC::auc(t2$store, t2_pred) # 0.8482
pROC::auc(t1$store, t1_pred) # 0.8488

## Now applying model on train data
log_model1 <- glm(store ~ population + storecode_NCNTY + country_X7 + state_alpha_IL + state_alpha_VT, data = train, family = "binomial")

summary(log_model1)

log_model1 <- stats:: step(log_model1)

summary(log_model1)

formula(log_model1)
# store ~ storecode_NCNTY + state_alpha_IL + state_alpha_VT

final_log_model1 <- glm(store ~ storecode_NCNTY + state_alpha_IL + state_alpha_VT, data = train, family = 'binomial')

train_pred <- predict(log_model1, newdata = train, type = 'response')

pROC::auc(train$store, train_pred) # 0.8485

## KS plot & cutoff
rocit <- ROCit::rocit(score = train_pred, class = train$store)
k_plot <- ROCit::ksplot(rocit, legend = FALSE)

cuttoff <- k_plot$`KS Cutoff`

## Final TEST value prediction
test_pred <- predict(final_log_model1, newdata = test, type = 'response')

final_test_pred <- as.numeric(test_pred>cuttoff)

write.csv(final_test_pred, file = paste0(path,'final_test_pred.csv'), row.names = FALSE)
















##------------------------------------------
# ## Building Model using DECISION TREE
# 
# tree_model <- decision_tree(
#   mode = "classification",
#   engine = "rpart",
#   cost_complexity = tune(),
#   tree_depth = tune(),
#   min_n = tune()
# )
# 
# # creating folds
# folds <- vfold_cv(t1, v=10)
# 
# tree_grid <- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 5)
# 
# dim(tree_grid) # 125
# 
# # Modelling
# dt_model <- tune_grid(
#   tree_model,
#   store~.,
#   resamples = folds,
#   grid = tree_grid,
#   metrics = metric_set(roc_auc),
#   control = control_grid(verbose = TRUE)
# )


# # check for constant and missing/NA
# 
# glimpse(t1)
# col_name <- colnames(t1)
# 
# for(i in col_name){
#   print(class(t1[,i]))
# } # "tbl_df", "tbl", "data.frame"
# 
# for (i in col_name) {
#   print(sum(is.na(t1[,i])))
# } # this returns 0 => means no NA 

##------------------------------------------
