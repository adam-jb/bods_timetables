

# can download XML files one service at a time from
# https://data.bus-data.dft.gov.uk/timetable/?q=&area=&organisation=&status=&is_pti_compliant=&start=&submitform=submit&ordering=-modified



# API docs
# https://data.bus-data.dft.gov.uk/guidance/requirements/?section=apireference




library(dplyr)
library(jsonlite)
library(purrr)




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





# Suffolk? Not working
url <- 'https://data.bus-data.dft.gov.uk/api/v1/dataset/?search=suffolk?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27'
jsonlite::fromJSON(url)






## AVL
#url <- "https://data.bus-data.dft.gov.uk/api/v1/datafeed/?api_key=cfc56d90ee6e0cc441c5b75668c14e6969b0ef27" 
#return <- jsonlite::fromJSON(url)





