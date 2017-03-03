library (pdftools)

text <- pdf_text("./R/codebook.pdf")

name <- vector(length =0)
meaning<- vector (length =0)

# loop through every page
for (i in 2: length(text)){
  lines <- strsplit(text[i], "\n") %>% unlist # extract every lines
  name_str <- lines[which(grepl("SAS Variable Name",lines))] # search for lines of variables name
  variable_name <- lapply(name_str, function (k) unlist(strsplit(k, "SAS Variable Name: "))[2])
  description_str <- lines[which(grepl("Description",lines))] # search for lines of variable description
  description <- lapply (description_str, function (k) unlist(strsplit(k, "Description: "))[2])

  name <- append(name, unlist(variable_name))
  meaning <- append(meaning, unlist(description))
}


name <- unlist(lapply (name, function (i) ifelse (substring(i,1,1) == "_", gsub("_", "X_",i),i)))
code_book <- data.frame(name, meaning, stringsAsFactors = FALSE)


