library(dplyr)
library(leaps)
library(tidyverse)


data <- read.csv('loan.csv')

#, loan_amnt, emp_length, home_ownership, annual_inc, verification_status, purpose, addr_state, dti
#, !is.na(annual_inc), !is.na(dti)

data_filtered = data %>% select(int_rate, grade, sub_grade, term, loan_amnt, emp_length, home_ownership, annual_inc, verification_status, purpose, addr_state, dti, application_type, open_acc, hardship_flag) %>% filter(!is.na(purpose))

best_Subset <- regsubsets(int_rate ~ ., data_filtered, method = 'forward', nvmax = 20)
best_Subset_Summary <- summary(best_Subset)

#names(best_Subset_Summary)

par(mfrow=c(1, 2))
plot(best_Subset_Summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l") 
plot(best_Subset_Summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
plot(best_Subset_Summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')

best_Subset_Summary$rss


k = 5
set.seed(1)
folds = sample(1:k,nrow(data_filtered),replace=TRUE)
table(folds)
cv.errors = matrix(NA,k,12, dimnames=list(NULL, paste(1:12)))

for(j in 1:k){
  best.fit = regsubsets(int_rate ~., data=data_filtered[folds != j,], nvmax = 12, really.big = T, method = 'forward')
  
  for (i in 1:12){
    pred = predict.regsubsets(best.fit, data_filtered[folds == j, ], id = i)
    cv.errors[j, i] = mean((data_filtered$int_rate[folds == j] - pred)^2)
  }
}

mean.cv.errors = apply(cv.errors, 2,mean)
mean.cv.errors

plot(mean.cv.errors, pch = 12, type = "b")