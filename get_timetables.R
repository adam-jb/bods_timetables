



## contains secret API key!!!


# can download XML files one service at a time from
# https://data.bus-data.dft.gov.uk/timetable/?q=&area=&organisation=&status=&is_pti_compliant=&start=&submitform=submit&ordering=-modified



# API docs
# https://data.bus-data.dft.gov.uk/guidance/requirements/?section=apireference




library(dplyr)
library(jsonlite)
library(purrr)

# source("process_timetables.R" )



# returns some tiny sample of timetables
url <- 'https://data.bus-data.dft.gov.uk/api/v1/dataset/?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27'
return <- jsonlite::fromJSON(url)





# select  dataset 101 
url <- 'https://data.bus-data.dft.gov.uk/api/v1/dataset/101/?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27'
return <- jsonlite::fromJSON(url)

# national operators included (their codes). Should include data outside suffolk where NOCs work outside Suffolk
return$noc

map(return, names)
map(return, length)
map(return, class)

return$url   # going here downloads a zipped xml file





### select by NOC
url <- 'https://data.bus-data.dft.gov.uk/api/v1/dataset/?noc=BNDY&api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27'
bndy <- jsonlite::fromJSON(url)
names(bndy$results)
bndy$results$url     # datasets for this NOC

bndy$results$adminAreas



# Suffolk? Not working
url <- 'https://data.bus-data.dft.gov.uk/api/v1/dataset/?search=suffolk?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27'
jsonlite::fromJSON(url)

url <- 'https://data.bus-data.dft.gov.uk/api/v1/dataset/?atco_code=390?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27'
jsonlite::fromJSON(url)


## End of experiements






# Harvesting data ---------------------------------------------------------


### Loop to get urls for dataset numbers
storage_list <- vector(mode='list', length=99000)
for (number in 1:length(storage_list)){

  url <- paste0('https://data.bus-data.dft.gov.uk/api/v1/dataset/', number, '/?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27')
  
  try({
    return <- jsonlite::fromJSON(url)
    storage_list[[number]] <- c(return$adminAreas,
                                return$url,
                                return$description)
  })
  
  print(url)
}


# add dataset number
for (i in 1:length(storage_list)){
  list_temp <- storage_list[[i]]
  list_temp$dataset_number <- i
  storage_list[[i]] <- list_temp
}

# storing list
save(storage_list, file = 'timetable_dataset_query_results.RData')




# getting all datasets which feature suffolk
successful_queries <- Filter(function(x) length(x) > 1, storage_list)

suffolk_successful_queries <- list()
counter <- 1
for (i in 1:length(successful_queries)){
  if ('Suffolk' %in% successful_queries[[i]][['name']]) {
    suffolk_successful_queries[[counter]] <- successful_queries[[i]]
    counter = counter + 1
  }
}



# Some datasets may replace old ones whichh appear to be live, eg: this
# message for 5593 in 4th element of it's list
# "Go East Anglia, including Konectbus, Norwich Park & Ride, Hedingham, and Chambers bus services. Replaces dataset 5427."
# Though this looks like the only one of it's kind for Suffolk. 
# Might be able to check they're running using AVL
lapply(suffolk_successful_queries, '[[', 4)



# get urls for zip files
download_zip_urls <- sapply(suffolk_successful_queries, '[[', 3)
dataset_ids <-  sapply(suffolk_successful_queries, '[[', 5)


# downloading zip file
for (i in 2:length(download_zip_urls)) {

  chosen_url <- download_zip_urls[i]
  dataset_id <- dataset_ids[i]
  
  downloader::download(chosen_url, dest="temp_timetables_download/dataset.zip", mode="wb") 
  unzip ("temp_timetables_download/dataset.zip", exdir = "./temp_timetables_download/xmls/")
  
  
  # extract all XML files to table
  tables_list <- extract_all_xml_files("temp_timetables_download/xmls", 'extracts_store', dataset_id)
  

  # delete downloaded and XML files
  unlink('temp_timetables_download/xmls/*')
  unlink("temp_timetables_download/dataset.zip")
  
  
  save(tables_list, file = paste0('extracts_store/dataset_', dataset_id, '.RData'))
  
}


  
# aggregate all 
  
  
  
  
# filter to routes in the Ipswich area




# For 8am, work out where buses will go over the next hour







## AVL
#url <- "https://data.bus-data.dft.gov.uk/api/v1/datafeed/?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27" 
#return <- jsonlite::fromJSON(url)











