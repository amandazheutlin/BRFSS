# behavioral risk factor surveillance system (BRFSS) survey for 2015
# nationally representative (US); 441,456 records
# evaluate effect of exercise + healthy food on depression, mental health

#### housekeeping
workdir <- "/data/swe_gwas/ABZ/Misc/"
setwd(workdir)

libs <- c("gbm", "dplyr", "psych", "ggplot2", "gdata", "RColorBrewer", "colorRamps", "survey",
          "caret", "glmnet", "doMC")
invisible(lapply(libs, require, character.only = TRUE))
registerDoMC(cores = detectCores())

#### get the data
# publicly available data from CDC
file.dl <- "https://www.cdc.gov/brfss/annual_data/2015/files/LLCP2015ASC.zip"
download.file(url = file.dl, destfile = "BRFSS_data.zip", method = "curl")

system("unzip -o BRFSS_data.zip") # unzip to wd
system("rm -rf BRFSS_data.zip")   # delete .zip


#### extract exercise variables
brfss2015 <- read.fwf("/data/swe_gwas/ABZ/Misc/LLCP2015.ASC",widths=2293)

exercise <- c("exerany2", "exract11", "exeroft1", "exerhmm1",
              "exract21", "exeroft2", "exerhmm2", "strength", 
              "metvl11_", "metvl21_", "fc60_",
              "actin11_", "actin21_", "padur1_", "padur2_",
              "pafreq1_", "pafreq2_", "_minac11", "_minac21",
              "strfreq_", "pamin11_", "pamin21_", "pa1min_",
              "pavig11_", "pavig21_", "pa1vigm_", "X_pacat1",
              "X_pastrng", "X_parec1", "X_pastae1")
vars     <- c("menthlth", exercise)

df.use          <- subset(brfss2015, menthlth < 31 | menthlth == 88, colnames(brfss2015) %in% vars)
df.use$mh.coded <- ifelse(df.use$menthlth == 88,0,df.use$menthlth)

# elastic net regression for emotional distress
df.na   <- df.use
df.na[is.na(df.na)] <- -1 # set missing to -1

df.mhonly <- df.na[df.na$mh.coded > 0,] # only ind who had any distress

pred    <- df.mhonly %>% dplyr::select(exerany2:X_pastae1)
outcome <- df.mhonly[complete.cases(df.mhonly$mh.coded),30]

fitControl <- trainControl(method = "cv", savePredictions = TRUE)
eGrid      <- expand.grid(.alpha = seq(1), .lambda = seq(0,1,by=0.05))
enet       <- train(x= as.matrix(pred), y = as.numeric(outcome),
                    method = "glmnet", 
                    metric = "Rsquared",
                    trControl = fitControl)
enet

gbmGrid <- expand.grid(.interaction.depth = c(1:4),
                       .n.trees = c(50,100, 200,300,400,600,800,1000,1500),
                       .n.minobsinnode = 25,
                       .shrinkage = c(0.1,0.01))

gbm_out       <- train(x= as.matrix(pred), y = as.numeric(outcome),
                    method = "gbm", 
                    metric = "Rsquared",
                    trControl = fitControl)
gbm_out



# variable importance
# importance df
fig1           <- caret::varImp(enet, scale=TRUE)[[1]] %>% as.data.frame
fig1           <- cbind(rownames(fig1), fig1) %>% arrange(-Overall)
names(fig1)    <- c("Predictor", "Importance")
fig1$Predictor <- factor(fig1$Predictor,
                         levels = fig1$Predictor[order(fig1$Importance)])

# show top 10
fig1.10        <- fig1[1:10,]
fig1.10$labels <- c("Activity 2 Intensity", "Activity 1 Intensity", 
                    "Any Exercise", "Muscle Strengthening Guideline", 
                    "Aerobic and Strengthening", "Aerobic and Strengthening Guideline",
                    "Physical Activity Category", "Activity 1 Metabolic Equivalent Task Value",
                    "Minutes of Activity 1", "Minutes of Activity 2")
fig1.10$labels   <- factor(fig1.10$labels,
                           levels = fig1.10$labels[order(fig1.10$Importance)])

# graph importance variables  
fig1.varimp <- ggplot(fig1.10, aes(x=labels,y=Importance)) + 
  geom_bar(fill="#0000CC",stat="identity",width=.85,alpha=.5) +
  theme(axis.title = element_text(size=15, face="bold"),
        axis.text = element_text(size=13,color="black"),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=11),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust=0,vjust=1,size=15,face="bold")) +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("What aspects of exercise improve depression?") +
  coord_flip(ylim=c(1,100))
fig1.varimp

ggsave(fig1.varimp, file = "cdc_exercise.varimp.pdf", 
       width = 220, height = 240, units = "mm")


## elastic net for binge drinking
vars2    <- c("X_rfbing5", exercise)
df.binge <- brfss2015[,colnames(brfss2015) %in% vars2]

# elastic net regression 
df.na2                <- df.binge
df.na2[is.na(df.na2)] <- -1 # set missing to -1

pred2    <- df.na2 %>% dplyr::select(exerany2:strength, metvl11_:X_pastae1)
outcome2 <- df.na2[,"X_rfbing5"]

fitControl  <- trainControl(method = "cv", savePredictions = TRUE)
eGrid       <- expand.grid(.alpha = seq(1), .lambda = seq(0,1,by=0.05))
enet2       <- train(x= as.matrix(pred2), y = as.numeric(outcome2),
                    method = "glmnet", 
                    metric = "Rsquared",
                    trControl = fitControl)
enet2

# variable importance
# importance df
fig2           <- caret::varImp(enet2, scale=TRUE)[[1]] %>% as.data.frame
fig2           <- cbind(rownames(fig2), fig2) %>% arrange(-Overall)
names(fig2)    <- c("Predictor", "Importance")
fig2$Predictor <- factor(fig2$Predictor,
                         levels = fig2$Predictor[order(fig2$Importance)])

# show top 10
fig2.10        <- fig2[1:10,]
fig2.10$labels <- c("Muscle Strengthening Guideline", "Aerobic and Strengthening Guideline",
                    "Physical Activity Category", "Any Exercise",
                    "Activity 1 Intensity", "Activity 2 Intensity", 
                    "Aerobic and Strengthening", "Type of Physical Activity",
                    "Activity 2 Metabolic Equivalent Task Value",
                    "Minutes of Activity 2")
fig2.10$labels   <- factor(fig2.10$labels,
                           levels = fig2.10$labels[order(fig2.10$Importance)])

# graph importance variables  
fig2.varimp <- ggplot(fig2.10, aes(x=labels,y=Importance)) + 
  geom_bar(fill="#339900",stat="identity",width=.85,alpha=.5) +
  theme(axis.title = element_text(size=15, face="bold"),
        axis.text = element_text(size=13,color="black"),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=11),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.title = element_text(hjust=0,vjust=1,size=15,face="bold")) +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("What aspects of exercise decrease binge drinking?") +
  coord_flip(ylim=c(1,100))
fig2.varimp

ggsave(fig2.varimp, file = "cdc_exercise.varimp2.pdf", 
       width = 230, height = 240, units = "mm")

  # options(survey.lonely.psu = "adjust")
# design <- svydesign(id = ~1,
#                     strata = ~X_strwt,
#                     weights = ~X_llcpwt,
#                     nest = TRUE,
#                     data = as.data.frame(brfss2015))

  
ggplot(df.mhonly, aes(x=actin21_,y=mh.coded)) + 
  geom_bar(stat="identity") +
  theme(axis.title = element_text(size=15, face="bold"),
        axis.text = element_text(size=13, color="black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=13, color="black"),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black")) +
  ylab("Days Feeling Depressed in Past Month") +
  xlab("Physical Activity Intensity")



