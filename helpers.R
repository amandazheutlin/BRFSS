code_book <- read.csv("./R/variable.csv")
state_code <- read.csv("./R/state_code.csv")
activity_code <- read.csv("./R/activity_encoded.csv")
activity_code <- activity_code[-nrow(activity_code),]
activity_code$Value <- as.character(activity_code$Value)
activity_code$Activity <- as.character(activity_code$Activity)


elastic_var <- c("EXERANY2", "EXRACT11", "EXEROFT1", "EXERHMM1",
              "EXRACT21", "EXEROFT2", "EXERHMM2", "STRENGTH", 
              "METVL11_", "METVL21_", "FC60_",
              "ACTIN11_", "ACTIN21_", "PADUR1_", "PADUR2_",
              "PAFREQ1_", "PAFREQ2_", "X_MINAC11", "X_MINAC21",
              "STRFREQ_", "PAMIN11_", "PAMIN21_", "PA1MIN_",
              "PAVIG11_", "PAVIG21_", "PA1VIGM_", "X_PACAT1",
              "X_PASTRNG", "X_PAREC1", "X_PASTAE1", "MENTHLTH","X_RFBING5")

find_me <- function (var_name){
  meaning <- code_book$meaning[code_book$name == var_name][1]
  return (meaning)
}

find_activity <- function (code){
  meaning <- activity_code$Activity[activity_code$Value == code][1]
  return (toString(meaning))
}

spread_activity <- function (data){
  result <- data
  result$activity1 <- sapply(result$EXRACT11, function (x) paste(find_activity(x), "_activity 1")) 
  result$activity2 <- sapply(result$EXRACT21, function (x) paste(find_activity(x), "_activity 2")) 
  
  result <- spread(result,key= activity1, value= METVL11_, fill =0) # drop = false will keep all the factor
  result <- spread(result,key= activity2, value= METVL21_, fill =0)
  return (result)
}
                             
clean_data_depression <- function (data){
  vars1 <- elastic_var[elastic_var != "X_RFBING5"]
  df.use <- subset(data, MENTHLTH < 31 | MENTHLTH == 88, colnames(data) %in% vars1)
  df.use$mh.coded <- ifelse(df.use$MENTHLTH == 88, 0, df.use$MENTHLTH)
  
  # elastic net regression for emotional distress, only use complete cases
  df.na <- df.use
  df.na [is.na(df.na)] <-1 # set NA to -1
  df.mhonly <- df.na[df.na$mh.coded>0,] # only those with distresses (ignore 0 and -1)
  
  pred    <- df.mhonly %>% dplyr::select(EXERANY2:X_PASTAE1) #predictors, ignore "MENTHLTH" and "md.coded"
  outcome <- df.mhonly[complete.cases(df.mhonly$mh.coded),30]
  
  result <- data.frame(pred,outcome)
  return (result)
}

clean_data_binge <- function (data){
  vars2 <- elastic_var[elastic_var != "MENTHLTH"]
  df.binge <- data[,colnames(data) %in% vars2]
  df.na2                <- df.binge
  df.na2[is.na(df.na2)] <- -1 # set missing to -1
  
  return (df.na2)
}

var_important <- function (elastic_model,n){
  fig1 <- caret::varImp(elastic_model, scale=TRUE)[[1]] %>% as.data.frame #return Overall vector and format to dataframe
  fig1 <- cbind(rownames(fig1), fig1) %>% arrange(-Overall)  # arange from large to small 
  names(fig1) <- c("name", "Importance")
  fig1.n  <- fig1[1:n,]
  fig1.n <- merge(fig1.n, code_book ,by = "name") %>%arrange(-Importance)
  names(fig1.n) <- c("name", "Importance", "label")
  
  return (fig1.n)
}


# Core wrapping function to get wrap the label text
text.wrap <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

chart <- function (data, key){
  label_wrapped <- text.wrap(data$label,50)
  if (key == "depression"){
    col <- "#0000CC"
    title <- "What aspects of exercise improve depression?"
  } else{
    col <- "#339900"
    title <- "What aspects of exercise decrease binge drinking?"
  }
  fig.varimp <- ggplot(data, aes(x=reorder(label_wrapped, Importance),y=data$Importance)) + 
    geom_bar(fill= col,stat="identity",width=.85,alpha=.5) +
    theme(axis.title = element_text(size=15, face="bold"),
          axis.text = element_text(size=10,color="black"),
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
    ggtitle(title) +
    coord_flip(ylim=c(1,100))
  return (fig.varimp)
}

download_file <- function (link, filename){
  path <- paste("~/R/", filename, sep ="")
  download.file(url = link, destfile = path, method = "wget")
  unzip(path, exdir = "~/R/")
  extract_name <- unzip(path,list =TRUE)[1,1]
  remove_code <- paste ("rm", "-rf", path, sep = " ")
  system(remove_code)
   #get the file name
  return (extract_name)
}
