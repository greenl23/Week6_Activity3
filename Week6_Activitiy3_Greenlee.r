library(jsonlite)
library(RCurl)
base<-'https://finds.org.uk/'

url <-"https://finds.org.uk/database/search/results/q/gold/broadperiod/BRONZE+AGE/thumbnail/1/format/json"

json <- fromJSON(url)

head(json)


total <- json$meta$totalResults

results <- json$meta$resultsPerPage

pagination <- ceiling(total/results)

keeps <- c("id", "objecttype", "old_findID", "broadperiod", "institution", "imagedir", "filename")
data <- json$results
data <- data[,(names(data) %in% keeps)]
head(data)

for (i in seq(from=2, to=pagination, by=1)){
  urlDownload <- paste(url, '/page/', i, sep='')
  pagedJson <- fromJSON(urlDownload)
  records <- pagedJson$results
  records <- records[,(names(records) %in% keeps)]
  data <-rbind(data,records)}
write.csv(data, file='data.csv',row.names=FALSE, na="")

failures <- "failures.log"
log_con <- file(failures)

# Download function with test for URL
download <- function(data){
  # This should be the object type taken from column 3
  object = data[3]
  # This should be the record old find ID taken from column 2
  record = data[2]
  
  # Check and create a folder for that object type if does not exist
  if (!file.exists(object)){
    dir.create(object)
  }
  
  # Create image url - image path is in column 7 and filename is column 6
  URL = paste0(base,data[7],data[6])
  
  # Test the file exists
  exist <- url.exists(URL) 
  
  # If it does, download. If not say 404
  if(exist == TRUE){
    download.file(URLencode(URL), destfile = paste(object,basename(URL), sep = '/'))
  } else {
    print("That file is a 404")
    # Log the errors for sending back to PAS to fix - probably better than csv as you 
    # can tail -f and watch the errors come in
    message <- paste0(record,"|",URL,"|","404 \n")
    # Write to error file
    cat(message, file = failures, append = TRUE)
  }
}
apply(data, 1, download)
