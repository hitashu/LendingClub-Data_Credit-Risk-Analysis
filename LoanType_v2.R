######## Classification for Loan Type ##############
## Good Loan --> Successfully Paid
## Bad Loan --> Charged Off


## This is the classification exercise for loan type with subset of variables which are included at the application time
## Knowledge acquired from LendingClub's business


library(tidyverse)
setwd("~/Spring 2020/DS 5220 - Supervised ML/Project/Lending Club Loan/")
loan_data <- read_csv("lending-club-loan-data/loan.csv")
orig_cols <- colnames(loan_data)

# filter for completed loan transactions
completed <- c("Fully Paid", "Charged Off")
loan_data_1 <- loan_data[loan_data$loan_status %in% completed,]

column_vector <- c("loan_amnt", "term","int_rate", "installment", "grade","sub_grade", "emp_title","emp_length",
                   "home_ownership","annual_inc","verification_status", "purpose", "title", "addr_state", "dti",
                   "inq_last_6mths", "delinq_2yrs", "mths_since_last_delinq", "mths_since_last_record", "open_acc",
                   "pub_rec", "policy_code","annual_inc_joint", "dti_joint", "chargeoff_within_12_mths","loan_status")

loan_data_90


loan_data_1 <- loan_data_1[,column_vector]

# Remove columns with more than 90% data missing
loan_data_90 <- loan_data_1[,which(colMeans(!is.na(loan_data_1)) > 0.1)]
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
remove_cols <- c("emp_title","issue_d","zip_code","earliest_cr_line","last_pymnt_d","last_credit_pull_d","title","addr_state")
loan_data_90 <- loan_data_90[,!names(loan_data_90) %in% remove_cols]

Y <- loan_data_90["loan_status"]
X <- model.matrix(loan_status ~ ., data = loan_data_90)
# Split train test data
set.seed(1)
train_90 = sample(nrow(loan_data_90), 0.60*nrow(loan_data_90))
loan_data_90_train <- loan_data_90[train_90,]
loan_data_90_test <- loan_data_90[-train_90,]
##############
set.seed(1)
# train_90 = sample(nrow(X), 0.8*nrow(X))
X_train <- X[train_90,]
Y_train <- Y[train_90,]
X_test <- X[-train_90,]
Y_test <- Y[-train_90,]

## Lasso Reg
library(glmnet)
grid <- 10 ^ seq(10,-2,length=100)
# X <- model.matrix(y ~ ., data = data.best.logistic)
# test_X <- model.matrix(loan_status ~ ., data = loan_data_90_test)
lasso.mod <- glmnet(x= X_train,y = Y_train,family = "binomial",alpha = 1,  lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.las <- cv.glmnet(X_train,Y_train, family = "binomial",alpha = 1, trace.it = 1, parallel = T, nfolds = 4)
par(mfrow=c(1,1))
plot(cv.las)
best.lam <- cv.las$lambda.min
model <- glmnet(X_train,Y_train, family = "binomial",alpha = 1, lambda = best.lam, trace.it = 1)
# pred.las <- predict(lasso.mod, s=best.lam, newx =X_test )
prob <- model %>% predict(newx = X_test, type = "response")
predicted.classes <- ifelse(prob > 0.50, "Fully Paid", "Charged Off")
mean(predicted.classes == Y_test)
table(Y_test, predicted.classes)
lasso.coef <- predict(model, type = "coefficients")
coefs <- coef(cv.las, cv.las$lambda.min)


library(randomForest)
set.seed(17)
rf.new <- randomForest(loan_status ~ ., data = loan_data_90_train, importance = T)
pred.rf.new <- predict(rf.new, loan_data_90_test[,!names(loan_data_90_test) %in% c("loan_status")])
table(loan_data_90_test$loan_status, pred.rf.new)
mean(loan_data_90_test$loan_status==pred.rf.new)



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

