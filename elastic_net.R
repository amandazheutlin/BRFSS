libs <- c("gbm", "plyr", "dplyr", "psych", "ggplot2", "gdata", "RColorBrewer", "colorRamps", "survey",
          "caret", "glmnet", "doMC", "foreign", "tidyr", "MatchIt")
invisible(lapply(libs, require, character.only = TRUE))
registerDoMC(cores = detectCores())

source("~/R/helpers.R")

find_me("MENTHLTH")
find_me ("X_RFBING5")
brfss2015 <- read.xport("~/R/downloaded/LLCP2015.XPT ")
# replace NA in EXRACT11 and EXRACT12 by 100 so that we have column missing
brfss2015[is.na(brfss2015$EXRACT11), "EXRACT11"] <- 100
brfss2015[is.na(brfss2015$EXRACT21), "EXRACT21"] <- 100

brfss2015 <- brfss2015 %>% merge(activity_group1, by = "EXRACT11") %>% merge (activity_group2, by = "EXRACT21")

### propensity matching 
walking_vs_running <- subset(brfss2015, group1 %in% c("walking_jogging", "running")) %>%
  select(group1, CADULT,PHYSHLTH,SEX,MARITAL,EXEROFT1,EXERHMM1, MENTHLTH, METVL11_) %>% 
  na.omit() %>%
  subset((PHYSHLTH < 31 | PHYSHLTH == 88) & (MARITAL != 9) & (EXEROFT1 <= 299) &
           (EXERHMM1 <= 959 & EXERHMM1 != 777) & (MENTHLTH <31 | MENTHLTH == 88))

walking_vs_running$MENTHLTH <- ifelse(walking_vs_running$MENTHLTH ==88 ,0, walking_vs_running$MENTHLTH)
walking_vs_running$PHYSHLTH <- ifelse(walking_vs_running$PHYSHLTH ==88,0, walking_vs_running$PHYSHLTH)
walking_vs_running[,c("CADULT", "SEX", "MARITAL")] <- lapply(walking_vs_running[,c("CADULT", "SEX", "MARITAL")], factor)
walking_vs_running$group1 <- drop.levels(walking_vs_running$group1)
walking_vs_running$running <- as.integer(ifelse(as.character(walking_vs_running$group1) == "running",1,0))
summary(walking_vs_running)


match <- matchit(running ~  PHYSHLTH+EXERHMM1 + CADULT + SEX + MARITAL + EXEROFT1 + METVL11_, data = walking_vs_running, method ="nearest")
summary(match)
new_data <- match.data(match)
t.test(new_data$MENTHLTH ~ new_data$group1)


# General plotting
depression_2015<- spread_group(brfss2015) %>% clean_data_depression()

ggplot(depression_2015, aes(x= group1, y=outcome, color = group1)) + 
  geom_boxplot() +coord_flip() + ylab ("Days of depression") + xlab ("activity 1 type") + theme(legend.position = "none")
ggplot(depression_2015, aes(x= group2, y=outcome, color = group2)) + geom_boxplot() + 
  coord_flip() + ylab ("Days of depression") + xlab ("activity 2 type") + theme(legend.position = "none")

# simple analysis of mean
depression_1 <- subset (depression_2015, group1 != "missing")
anova(lm(outcome~group1, data = depression_1))

depression_2 <- subset (depression_2015, group2 != "missing")
anova(lm(outcome~group2, data = depression_2))

# with interaction
depression_2015 <- subset (depression_2015, group1 != "missing" || group2 != "missing" )
test <- aov(outcome~group1*group2,data=depression_2015)
summary(test)


#######2013 data

brfss2013 <- read.xport("~/R/downloaded/LLCP2013.XPT")
brfss2013[is.na(brfss2013$EXRACT11), "EXRACT11"] <- 100
brfss2013[is.na(brfss2013$EXRACT21), "EXRACT21"] <- 100
brfss2013 <- brfss2013 %>% merge(activity_group1, by = "EXRACT11") %>% merge (activity_group2, by = "EXRACT21")

depression_2013<- spread_group(brfss2013) %>% clean_data_depression()
ggplot(depression_2013, aes(x= group1, y=outcome, color = group1)) + 
  geom_boxplot() +coord_flip() + ylab ("Days of depression") + xlab ("activity 1 type") + theme(legend.position = "none")
ggplot(depression_2013, aes(x= group2, y=outcome, color = group2)) + geom_boxplot() + 
  coord_flip() + ylab ("Days of depression") + xlab ("activity 2 type") + theme(legend.position = "none")

# spread according to activity
activity_2015 <- spread_activity(brfss2015)
activity_2013 <- spread_activity(brfss2013)

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
enet
plot(enet)
enet$bestTune

# find importance of top 10 variables
var_imp <- var_important (enet,10)

# draw a chart of importance:
chart(var_imp, "depression")

# Another way to draw the chart of varImp:
plot <- ggplot(varImp(enet)) + 
        theme_bw() + 
        ggtitle("Variable Importance", subtitle = "elastic net on depression") + 
        theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

# apply best tune alpha and lambda to fit on brfss2015
predict_2015 <- predict(enet, newdata = as.matrix(depression_2015[,-which(names(depression_2015)=="outcome")]), type ="raw")
mean((predict_2015-depression_2015$outcome)^2)


########################################
# Logistic with Ridge and LASSO penalty#
########################################

### Clean data

binge_data_2015 <- clean_data_binge(brfss2015)
binge_data_2015 <- binge_data_2015[!binge_data_2015$X_RFBING5 == 9,]## omit Don't know/Refused/Missing


binge_data_2013 <- clean_data_binge(brfss2013)
binge_data_2013 <- binge_data_2013[!binge_data_2013$X_RFBING5 == 9,]## omit Don't know/Refused/Missing


################### doing by caret
binge_data_2015$X_RFBING5 <- as.factor(binge_data_2015$X_RFBING5)
binge_data_2013$X_RFBING5 <- as.factor(binge_data_2013$X_RFBING5)

summary(binge_data_2013$X_RFBING5)
summary(binge_data_2015$X_RFBING5)

glmnet_grid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                           lambda = seq(.01, .2, length = 20))
glmnet_ctrl <- trainControl(method = "cv", number = 10)
glmnet_fit <- train(X_RFBING5 ~ ., data = binge_data_2013,
                    method = "glmnet",
                    #tuneGrid = glmnet_grid,
                    trControl = glmnet_ctrl)
glmnet_fit
glmnet_fit$bestTune
coef(glmnet_fit$finalModel, glmnet_fit$bestTune$alpha, glmnet_fit$bestTune$lambda)

plot(glmnet_fit)

varImp(glmnet_fit, scale = FALSE)
ggplot(varImp(glmnet_fit, scale = FALSE)) + 
  theme_bw() + 
  ggtitle("Variable Importance", subtitle = "penalized logistic regression at best tune alpha, lambda") + 
  theme(panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())


pred_glm <- predict(glmnet_fit, new = binge_data_2015)
confusionMatrix(data = pred_glm, reference = binge_data_2015$X_RFBING5)
glm_probs <- predict(glmnet_fit, newdata = binge_data_2015, type = "prob")[,"1"]


#############################
# Normal logistic regression#

logistic <- glm(X_RFBING5 ~., data = binge_data_2013, family = "binomial")
pred_logistic <- predict(logistic, newdata = binge_data_2015, type ="response")
contrasts(binge_data_2013$X_RFBING5) # find how R encode contrast, so 1 is encoded as 0, 2 as 1.
pred_logistic <- factor(ifelse(pred_logistic>= 0.5, 2, 1))

confusionMatrix(data = pred_logistic, reference = binge_data_2015$X_RFBING5)

logistic_prob <- 1- predict(logistic, newdata = binge_data_2015, type ="response") # we want prob for class 1

#logistic is not a glmnet object so we have to process VarImp manually
varimp_logistic = cbind(variable = row.names (varImp(logistic, scale = TRUE)), varImp(logistic, scale = TRUE))
varimp_logistic <- merge(varimp_logistic, code_book ,by = "variable") %>%arrange(-Overall)
varimp_logistic <- varimp_logistic[1:10,]
names(varimp_logistic) <- c("name", "Importance", "label")
chart(varimp_logistic, "binge")


###################################
# Lift curve and calibration curve#

result <- data.frame(Class = binge_data_2015$X_RFBING5)
result$glm <- glm_probs
result$logistic <- logistic_prob

head(result)
trellis.par.set(caretTheme())
lift_obj <- lift(Class ~ glm + logistic, data = result, cuts =13)
plot(lift_obj, values =c(30,60), auto.key = list(columns = 2,
                                            lines = TRUE,
                                            points = FALSE))


trellis.par.set(caretTheme())
cal_obj <- calibration(Class ~ glm+logistic,
                       data = result,
                       cuts = 13)
plot(cal_obj, type = "l", auto.key = list(columns = 2,
                                          lines = TRUE,
                                          points = FALSE))
ggplot(cal_obj)

