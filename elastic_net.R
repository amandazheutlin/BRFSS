libs <- c("gbm", "plyr", "dplyr", "psych", "ggplot2", "gdata", "RColorBrewer", "colorRamps", "survey",
          "caret", "glmnet", "doMC", "foreign")
invisible(lapply(libs, require, character.only = TRUE))
registerDoMC(cores = detectCores())

source("~/R/helpers.R")

find_me ("X_RFBING5")
brfss2015 <- read.xport("~/R/LLCP2015.XPT ")
brfss2013 <- read.xport("~/R/LLCP2013.XPT")


# brfss
depression_2015 <- clean_data_depression(brfss2015)
depression_2013 <- clean_data_depression(brfss2013)


dim (depression_2015)
dim (depression_2013)

############################
# Elastic net on depression#
############################
pred <- depression_2013[,-which(names(depression_2013)=="outcome")]
fitControl <- trainControl(method = "cv", savePredictions = TRUE)
eGrid      <- expand.grid(.alpha = seq(0,1,by=0.05), .lambda = seq(0,1,by=0.05))
enet       <- train(x= as.matrix(pred), y = as.numeric(depression_2013$outcome),
                    method = "glmnet", 
                    metric = "Rsquared",
                    #tuneGrid = eGrid,
                    trControl = fitControl)
plot(enet)
enet$bestTune

# find importance of top 10 variables
var_imp <- var_important (enet,10)

# draw a chart of importance:
chart(var_imp, "depression")

# apply best tune alpha and lambda to fit on brfss2015
predict_2015 <- predict(enet, newdata = as.matrix(depression_2015[,-which(names(depression_2015)=="outcome")]), type ="raw")
mean((predict_2015-depression_2015$outcome)^2)
# MSE of test set = 0.2, not too bad

################################
# Elastic net on binge drinking#
################################
binge_data_2013 <- clean_data_binge(brfss2013)
pred2    <- binge_data_2013 %>% dplyr::select(EXERANY2:X_PASTAE1, -X_RFBING5) #exclude X_RFBING5
outcome2 <- binge_data_2013[,"X_RFBING5"]
fitControl  <- trainControl(method = "cv", savePredictions = TRUE)
eGrid       <- expand.grid(.alpha = seq(0,1,by=0.05), .lambda = seq(0,1,by=0.05))
enet2       <- train(x= as.matrix(pred2), y = as.numeric(outcome2),
                     method = "glmnet", 
                     metric = "Rsquared",
                     #tuneGrid = eGrid, it seems that R has troubled fitting the model using this grid
                     #probably some linear algebra problem under the hood
                     trControl = fitControl)
plot(enet2)
enet2$bestTune

var_imp <- var_important (enet2,10)
chart(var_imp, "binge")

# Use model 2013 to predict 2015

binge_data_2015 <- clean_data_binge(brfss2015)
pred_2015 <- binge_data_2015 %>% dplyr::select(EXERANY2:X_PASTAE1, -X_RFBING5) 
predict_2015 <- predict(enet2, newdata = as.matrix(pred_2015), type ="raw")
mean((predict_2015-binge_data_2015$X_RFBING5)^2)

##########################################
# Multinomial Logistic with Ridge penalty#
##########################################
outcome2 <- as.factor(outcome2)
multinom_ridge <- cv.glmnet (as.matrix(pred2), outcome2, alpha = 0, nfolds = 10, family = "multinomial", type.measure = "class")
plot(multinom_ridge)               
lambda_min <- multinom_ridge$lambda.min
lambda_min
# minimum lambda is 106.3541

# Predict misclassification rate on 2015 data
predict_2015 <- predict(multinom_ridge, s = lambda_min, newx = as.matrix(pred_2015), type = "class")
mean(predict_2015 != binge_data_2015$X_RFBING5)
# Misclassification rate is about 17.2649% not bad at all


############################################
  # Multinomial Logistic with LASSO penalty#
############################################

multinom_lasso <- cv.glmnet (as.matrix(pred2), outcome2, alpha = 1, nfolds = 10, family = "multinomial", type.measure = "class")
plot(multinom_lasso)               
lambda_min <- multinom_lasso$lambda.min
lambda_min
# lambda_min is 0.0007679625

# Predict misclassification rate on 2015 data
predict_2015 <- predict(multinom_lasso, s = lambda_min, newx = as.matrix(pred_2015), type = "class")
mean(predict_2015 != binge_data_2015$X_RFBING5)
# Misclassification rate is about 17.20% 

#Find cofficients at lambda_min
# Find unique predictors across 3 multinomial logistic regression equations
coef_lasso <- predict(multinom_lasso, s = lambda_min, type = "coefficients")
coef_1<-rownames(coef_lasso$`1`)[which(!coef_lasso$`1` == 0)]
coef_2<-rownames(coef_lasso$`2`)[which(!coef_lasso$`2` == 0)]
coef_9<-rownames(coef_lasso$`9`)[which(!coef_lasso$`9` == 0)]

unique(c(coef_1, coef_2, coef_9)) # 26 unique variables in this model
