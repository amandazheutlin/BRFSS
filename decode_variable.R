library (pdftools)

##########################
#get variable description#

text <- pdf_text("./R/codebook.pdf")

name <- vector(length =0)
meaning<- vector (length =0)


for (i in 2: length(text)){
  # get variable names from each page
  lines <- strsplit(text[i], "\n") %>% unlist # extract every lines
  name_str <- lines[which(grepl("SAS Variable Name",lines))] # search for lines of variables name
  variable_name <- lapply(name_str, function (k) unlist(strsplit(k, "SAS Variable Name: "))[2]) %>% unlist
  
  # get description name from each plage
  lines <- strsplit(text[i], "Weighted") %>% unlist
  description <- lapply (lines, function (k) unlist(strsplit(k, "Description: "))[2]) %>% unlist %>% na.omit
  
  name <- append(name, variable_name)
  meaning <- append(meaning, description)
}


name <- unlist(lapply (name, function (i) ifelse (substring(i,1,1) == "_", gsub("_", "X_",i),i)))
code_book <- data.frame(variable = name, description = meaning, stringsAsFactors = FALSE)

write.csv(code_book, "./R/variable.csv", row.names = FALSE)


