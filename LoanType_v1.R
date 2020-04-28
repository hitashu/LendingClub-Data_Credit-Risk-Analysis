######## Classification for Loan Type ##############
## Good Loan --> Successfully Paid
## Bad Loan --> Charged Off


## This is the initial classification exercise for loan type without taking any knowledge from the business 
## domain or LendingClub processes


library(tidyverse)
setwd("~/Spring 2020/DS 5220 - Supervised ML/Project/Lending Club Loan/")
loan_data <- read_csv("lending-club-loan-data/loan.csv")
orig_cols <- colnames(loan_data)
orig_cols1 <- data.frame(orig_cols)
orig_cols1$orig_cols <- as.character(orig_cols1$orig_cols)
data_dict <- read_csv("dictionary.csv", col_names = c("field", "desc"))

col_desc <- orig_cols1 %>% left_join(data_dict, by = c("orig_cols"="field"))
col_desc <- col_desc %>% distinct()

# Remove columns with all NAs
# loan_data_1 <- loan_data %>%
#   map(~.x) %>%
#   discard(~all(is.na(.x))) %>%
#   map_df(~.x)
# cols_1 <- colnames(loan_data_1)
# allMissing <- setdiff(orig_cols,cols_1)
# # loan_data_1 <- na.omit(loan_data_1)
# columns <- colnames(loan_data_1)


# # Function to find columns having missing data from a data frame
# missingData <- function(df, columns) {
#   missing <- c()
#   for(col in columns){
#     if (any(is.na(df[[col]]))){
#       missing <- c(missing, col)
#     }
#   }
#   return(missing)
# }
# missing_columns <- missingData(loan_data_1, columns)


# filter for completed loan transactions
completed <- c("Fully Paid", "Charged Off")
loan_data_1 <- loan_data[loan_data$loan_status %in% completed,]

# Remove columns with more than 90% data missing
loan_data_90 <- loan_data_1[,which(colMeans(!is.na(loan_data_1)) > 0.1)]

# omit all rows with NA values
loan_data_90 <- na.omit(loan_data_90)
# distribution of classes
loan_data_90 %>% ggplot() + geom_bar(aes(x=loan_status))
table(loan_data_90$loan_status)
mean(loan_data_90$loan_status=="Fully Paid")
mean(loan_data_90$loan_status=="Charged Off")
removed_90 <- setdiff(orig_cols,colnames(loan_data_90))
removed_90

# Remove factor columns with single value
xx <- colnames(loan_data_90)
no_var <- c()
for(col in xx){
  if(length(unique(loan_data_90[[col]])) <= 1){
    no_var <- c(no_var, col)
  }
}
no_var

loan_data_90<- loan_data_90[,!names(loan_data_90) %in% no_var]

loan_data_90 <- as.data.frame(unclass(loan_data_90))
remove_cols <- c("emp_title","issue_d","zip_code","earliest_cr_line","last_pymnt_d","last_credit_pull_d")
loan_data_90 <- loan_data_90[,!names(loan_data_90) %in% remove_cols]

Y <- loan_data_90["loan_status"]
X <- model.matrix(loan_status ~ ., data = loan_data_90)
# Split train test data
set.seed(1)
train_90 = sample(nrow(loan_data_90), 0.8*nrow(loan_data_90))
loan_data_90_train <- loan_data_90[train_90,]
loan_data_90_test <- loan_data_90[-train_90,]
##############
set.seed(1)
# train_90 = sample(nrow(X), 0.8*nrow(X))
X_train <- X[train_90,]
Y_train <- Y[train_90,]
X_test <- X[-train_90,]
Y_test <- Y[-train_90,]




# Forward Selection selection
library(leaps)
fwd.fit.100 <- regsubsets(loan_status~., loan_data_90, method = "forward", nvmax = 100)
fwd.100.summary <- summary(fwd.fit.100)
fwd.100.summary$adjr2
which.max(fwd.100.summary$adjr2)
which.min(fwd.100.summary$cp)
which.min(fwd.100.summary$bic)

par(mfrow=c(2,2))
plot(fwd.100.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(fwd.100.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(58,fwd.100.summary$adjr2[11], col="red",cex=2,pch=20)
plot(fwd.100.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
points(56,fwd.100.summary$cp[10],col="red",cex=2,pch=20)
plot(fwd.100.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(45,fwd.100.summary$bic[6],col="red",cex=2,pch=20)
coef(fwd.fit.100, 14)
coef(fwd.fit.100, 85)
# write summary to file
sink("sum_fwd_213.txt")
print(summary(fwd.fit.213))
sink()

## Backward Selection
bwd.fit.100 <- regsubsets(loan_status~., loan_data_90, method = "backward", nvmax = 100)
bwd.100.summary <- summary(bwd.fit.100)
bwd.100.summary$adjr2
which.max(bwd.100.summary$adjr2)
which.min(bwd.100.summary$cp)
which.min(bwd.100.summary$bic)
# Plot
par(mfrow=c(2,2))
plot(bwd.100.summary$rss,xlab="Number of Variables",ylab="RSS1",type="l")
plot(bwd.100.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(60,bwd.100.summary$adjr2[11], col="red",cex=2,pch=20)
plot(bwd.100.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
points(56,bwd.100.summary$cp[10],col="red",cex=2,pch=20)
plot(bwd.100.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(45,bwd.100.summary$bic[6],col="red",cex=2,pch=20)
coef(bwd.fit.100, 14)
coef(bwd.fit.100, 85)


## Lasso Reg
library(glmnet)
grid <- 10 ^ seq(10,-2,length=100)
# X <- model.matrix(y ~ ., data = data.best.logistic)
# test_X <- model.matrix(loan_status ~ ., data = loan_data_90_test)
lasso.mod <- glmnet(x= X_train,y = Y_train,family = "binomial",alpha = 1,  lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.las <- cv.glmnet(X_train,Y_train, family = "binomial",alpha = 1)
par(mfrow=c(1,1))
plot(cv.las)
best.lam <- cv.las$lambda.min
model <- glmnet(X_train,Y_train, family = "binomial",alpha = 1, lambda = best.lam)
# pred.las <- predict(lasso.mod, s=best.lam, newx =X_test )
prob <- model %>% predict(newx = X_test, type = "response")
predicted.classes <- ifelse(prob > 0.60, "Fully Paid", "Charged Off")
mean(predicted.classes == Y_test)
table(Y_test, predicted.classes)
lasso.coef <- predict(lasso.mod, type = "coefficients")
coefs <- coef(cv.las, cv.las$lambda.min)


######## Resampling Data
library(DMwR)
library(ROSE)
# Oversampling
over_loan_train <- ovun.sample( loan_status ~ ., data = loan_data_90_train, method="over")$data
print(table(over_loan_train$loan_status))

# Undersampling
under_loan_train <- ovun.sample( loan_status ~ ., data = loan_data_90_train, method="under")$data
print(table(under_loan_train$loan_status))

# Mixed Sampling
both_loan_train <- ovun.sample(loan_status ~ ., data = loan_data_90_train, method="both", p=0.5, seed=222)$data
print(table(both_loan_train$loan_status))

# ROSE Sampling, this helps us to generate data synthetically
rose_loan_train <- ROSE(loan_status ~ ., data = loan_data_90_train,  seed=111)$data
print(table(rose_loan_train$loan_status))

# SMOTE(Synthetic Minority Over-sampling Technique) Sampling
# formula - relates how our dependent variable acts based on other independent variable.
# data - input data
# perc.over - controls the size of Minority class
# perc.under - controls the size of Majority class
# since my data has less Majority class, increasing it with 200 and keeping the minority class to 100.
smote_loan_train <- SMOTE(loan_status ~ ., data = loan_data_90_train, perc.over = 100, perc.under=200)
print(table(smote_loan_train$loan_status))


############################# LOGISTIC REGRESSION WITH RESAMPLED DATA ###########################
# Logistic classifier for Over sampling dataset
over_classifier = glm(formula = loan_status ~ ., family = binomial, data = over_loan_train)

# Logistic classifier for Under sampling dataset
under_classifier = glm(formula = loan_status ~ ., family = binomial, data = under_loan_train)

# Logistic classifier for Mixed sampling dataset
both_classifier = glm(formula = loan_status ~ ., family = binomial, data = both_loan_train)

#Logistic classifier for ROSE sampling dataset
rose_classifier = glm(formula = loan_status ~ ., family = binomial, data = rose_loan_train)

# Logistic classifier for SMOTE dataset
smote_classifier = glm(formula = loan_status ~ ., family = binomial, data = smote_loan_train)

#######PREDICT
over_probability_predict = predict(over_classifier, type = 'response', newdata = loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
y_pred_over = ifelse(over_probability_predict>0.5, 1, 0)

# Predicting the test set using Under sampling classifier
under_probability_predict = predict(under_classifier, type = 'response', newdata = loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
y_pred_under = ifelse(under_probability_predict>0.5, 1, 0)

# Predicting the test set using Mixed sampling classifier
both_probability_predict = predict(both_classifier, type = 'response', newdata = loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
y_pred_both = ifelse(both_probability_predict>0.5, 1, 0)

# Predicting the test set using ROSE classifier
rose_probability_predict = predict(rose_classifier, type = 'response', newdata = loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
y_pred_rose = ifelse(rose_probability_predict>0.5, 1, 0)

# Predicting the test set using SMOTE classifier
smote_probability_predict = predict(smote_classifier, type = 'response', newdata = loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
y_pred_smote = ifelse(smote_probability_predict>0.5, 1, 0)


# ROC for oversampling
roc_over <- roc.curve(loan_data_90_test$loan_status, y_pred_over)
print(roc_over)
table(loan_data_90_test$loan_status, y_pred_over)
# ROC curve of Under sampling data
roc_under <- roc.curve(loan_data_90_test$loan_status, y_pred_under)
print(roc_under)
table(loan_data_90_test$loan_status, y_pred_under)
# ROC curve of both sampling data
roc_both <- roc.curve(loan_data_90_test$loan_status, y_pred_both)
print(roc_both)
table(loan_data_90_test$loan_status, y_pred_both)
# ROC curve of ROSE sampling data
roc_rose <- roc.curve(loan_data_90_test$loan_status, y_pred_rose)
print(roc_rose)
table(loan_data_90_test$loan_status, y_pred_rose)
# ROC curve of SMOTE sampling data
roc_smote <- roc.curve(loan_data_90_test$loan_status, y_pred_smote)
print(roc_smote)
table(loan_data_90_test$loan_status, y_pred_smote)

####### Function for top 25 variables by random forest 
var_importance <- function(mod, n){
  var.imp <- importance(mod)
  var.imp <- sort(var.imp[,4], decreasing = T)
  top.n <- names(var.imp[1:n])
  return(top.n)
}

########################### RANDOM FOREST FOR IMBALANCED DATA #####################
library(randomForest)
set.seed(17)
rf.orig <- randomForest(loan_status ~ ., data = loan_data_90_train, importance = T, proximity = T)
pred.rf.orig <- predict(rf.orig, loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
table(loan_data_90_test$loan_status, pred.rf.orig)
mean(loan_data_90_test$loan_status==pred.rf.orig)
var.imp.rf.orig <- importance(rf.orig)
var.imp.rf.orig <- sort(var.imp.rf.orig[,4], decreasing = T)
top.25.orig <- var_importance(rf.orig, 25)
train_subset_orig <- loan_data_90_train[,c(top.25.orig,"loan_status")]
test_subset_orig <- loan_data_90_test[,c(top.25.orig,"loan_status")]
rf.orig.subset <- randomForest(loan_status ~ ., data = train_subset_orig, importance = T, proximity = T)
pred.rf.orig.subset <- predict(rf.orig.subset, test_subset_orig[,!names(test_subset_orig) %in% c("loan_status")])
table(test_subset_orig$loan_status, pred.rf.orig.subset)
mean(test_subset_orig$loan_status==pred.rf.orig.subset)





########################### RANDOM FOREST USING RESAMPLED DATA ######################
set.seed(17)
# Oversampling model
rf.over <- randomForest(loan_status ~ ., data = over_loan_train, importance = T, proximity = T)
pred.rf.over <- predict(rf.over, loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
table(loan_data_90_test$loan_status, pred.rf.over)
mean(loan_data_90_test$loan_status==pred.rf.over)
top.25.over <- var_importance(rf.over,25)

train_subset_over <- loan_data_90_train[,c(top.25.over,"loan_status")]
test_subset_over <- loan_data_90_test[,c(top.25.over,"loan_status")]
rf.over.subset <- randomForest(loan_status ~ ., data = train_subset_over, importance = T, proximity = T)
pred.rf.over.subset <- predict(rf.over.subset, test_subset_over[,!names(test_subset_over) %in% c("loan_status")])
table(test_subset_over$loan_status, pred.rf.over.subset)
mean(test_subset_over$loan_status==pred.rf.over.subset)


# Undersampling model
rf.under <- randomForest(loan_status ~ ., data = under_loan_train, importance = T, proximity = T)
pred.rf.under <- predict(rf.under, loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
table(loan_data_90_test$loan_status, pred.rf.under)
mean(loan_data_90_test$loan_status==pred.rf.under)
top.25.under <- var_importance(rf.under,25)

train_subset_under <- loan_data_90_train[,c(top.25.under,"loan_status")]
test_subset_under <- loan_data_90_test[,c(top.25.under,"loan_status")]
rf.under.subset <- randomForest(loan_status ~ ., data = train_subset_under, importance = T, proximity = T)
pred.rf.under.subset <- predict(rf.under.subset, test_subset_under[,!names(test_subset_under) %in% c("loan_status")])
table(test_subset_under$loan_status, pred.rf.under.subset)
mean(test_subset_under$loan_status==pred.rf.under.subset)

# Mixed-sampling model
rf.both <- randomForest(loan_status ~ ., data = both_loan_train, importance = T, proximity = T)
pred.rf.both <- predict(rf.both, loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
table(loan_data_90_test$loan_status, pred.rf.both)
mean(loan_data_90_test$loan_status==pred.rf.both)
top.25.both <- var_importance(rf.both,25)

train_subset_both <- loan_data_90_train[,c(top.25.both,"loan_status")]
test_subset_both <- loan_data_90_test[,c(top.25.both,"loan_status")]
rf.both.subset <- randomForest(loan_status ~ ., data = train_subset_both, importance = T, proximity = T)
pred.rf.both.subset <- predict(rf.both.subset, test_subset_both[,!names(test_subset_both) %in% c("loan_status")])
table(test_subset_both$loan_status, pred.rf.both.subset)
mean(test_subset_both$loan_status==pred.rf.both.subset)

# ROSE sampling model
rf.rose <- randomForest(loan_status ~ ., data = rose_loan_train, importance = T, proximity = T)
pred.rf.rose <- predict(rf.rose, loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
table(loan_data_90_test$loan_status, pred.rf.rose)
mean(loan_data_90_test$loan_status==pred.rf.rose)
top.25.rose <- var_importance(rf.rose,25)

train_subset_rose <- loan_data_90_train[,c(top.25.rose,"loan_status")]
test_subset_rose <- loan_data_90_test[,c(top.25.rose,"loan_status")]
rf.rose.subset <- randomForest(loan_status ~ ., data = train_subset_rose, importance = T, proximity = T)
pred.rf.rose.subset <- predict(rf.rose.subset, test_subset_rose[,!names(test_subset_rose) %in% c("loan_status")])
table(test_subset_rose$loan_status, pred.rf.rose.subset)
mean(test_subset_rose$loan_status==pred.rf.rose.subset)

# SMOTE sampling model
rf.smote <- randomForest(loan_status ~ ., data = smote_loan_train, importance = T, proximity = T)
pred.rf.smote <- predict(rf.smote, loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
table(loan_data_90_test$loan_status, pred.rf.smote)
mean(loan_data_90_test$loan_status==pred.rf.smote)
top.25.smote <- var_importance(rf.smote,25)

train_subset_smote <- loan_data_90_train[,c(top.25.smote,"loan_status")]
test_subset_smote <- loan_data_90_test[,c(top.25.smote,"loan_status")]
rf.smote.subset <- randomForest(loan_status ~ ., data = train_subset_smote, importance = T, proximity = T)
pred.rf.smote.subset <- predict(rf.smote.subset, test_subset_smote[,!names(test_subset_smote) %in% c("loan_status")])
table(test_subset_smote$loan_status, pred.rf.smote.subset)
mean(test_subset_smote$loan_status==pred.rf.smote.subset)

library(reshape2)
rf.var.imp <- data.frame(top.25.orig, top.25.over, top.25.under, top.25.both, top.25.rose, top.25.smote)



