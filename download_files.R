
source ("~/R/helpers.R")


#Download 2014 BRFSS data
link <- c("http://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.ZIP",
         "http://www.cdc.gov/brfss/annual_data/2013/files/LLCP2013XPT.ZIP",
         "http://www.cdc.gov/brfss/annual_data/2012/files/LLCP2012XPT.ZIP",
         "http://www.cdc.gov/brfss/annual_data/2011/files/LLCP2011XPT.ZIP")
filename <- c("BRFSS_data_2014.zip", "BRFSS_data_2013.zip",
              "BRFSS_data_2012.zip", "BRFSS_data_2011.zip")
mapply(download_file, link, filename)


############################################
####    Check existence of variables    ####
############################################

missing_var_elastic <- function (data){
  all_var <- colnames(data)
  missing <- elastic_var[which(!elastic_var %in% all_var)]
  return (missing)
}


brfss2014 <- read.xport("~/R/LLCP2014.XPT ")

dim (brfss2014)
# 464664 entries, 279 variables
missing_var_elastic (brfss2014)


brfss2013 <- read.xport("~/R/LLCP2013.XPT") #for some absurd reason, there is no space behind.XPT this time
dim(brfss2013)
missing_var_elastic(brfss2013)



brfss2012 <- read.xport("~/R/LLCP2012.XPT")
dim(brfss2012)
missing_var_elastic(brfss2012)



brfss2011 <- read.xport("~/R/LLCP2011.XPT")
dim(brfss2011)
missing <- missing_var_elastic(brfss2011)
length(missing)
# var_2011 does not have 17 columns, probably cannot merge with 2013
