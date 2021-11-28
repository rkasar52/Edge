setwd("~/Edge")
library(glmnet)


all_data = read.csv("all_data.csv")
# head(all_data)

wait_df = all_data[which(all_data$increment == 1), ]
colnames(wait_df)

# Station will be a factor
wait_df$station = as.factor(wait_df$station)


# Day of Week Will be a factor
wait_df$day_of_week = as.factor(wait_df$day_of_week)
# hour will be a factor?
wait_df$hour = as.factor(wait_df$hour)


## Train Test Split
smp_size <- floor(0.75 * nrow(mtcars))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(wait_df)), size = smp_size)

train <- wait_df[train_ind, ]
test <- wait_df[-train_ind, ]


# First try linear regression without stations
linear_model = lm(wait_time ~ capacity + day_of_week + hour, data = train)
summary(linear_model)

# Alpha = 0 is ridge, 0.5 is elastic net, 1 is lasso
# Lasso (might need to make dummies)
#perform k-fold cross-validation to find optimal lambda value
train_matrix =  data.matrix(train[, c(2,8,11)])
cv_model <- cv.glmnet(train_matrix, train$wait_time, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_model <- glmnet(train_matrix, train$wait_time, alpha = 1, lambda = best_lambda)
coef(best_model)

