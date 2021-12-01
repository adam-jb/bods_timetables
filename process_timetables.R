


##### Code to extract BODS timetable data from XML into list of data.frames

##### Source this and use extract_all_xml_files()


### Adam Bricknell, November 2021



# Load packages
library(xml2)
library(methods)
library(dplyr )
library(purrr)
library(data.table)
library(rjson)
library(docstring)



# Funcs -------------------------------------------------------------------

#### extracts some fields from XML, but not all
extract_list_of_lists_to_df <- function(x) {
  x <-  map(x, unlist)
  rbindlist(lapply(x, as.data.frame.list), fill=T) 
}








# Read in data ------------------------------------------------------------


# Define file path and identify XML files
setwd('/Users/dftdatascience/Desktop/bods_timetables')

xml_path <- 'xml_timetables_data/bus_timetable_xml/'

IoW_list <- list.files(xml_path, pattern = 'xml')

1_CA_PF_1_20200405





# Extract single XML file  -------------------------------------------------

x = IoW_list[2]
print(x)

xml_object <- read_xml(paste0(xml_path, '/', x)) %>% 
  xml_ns_strip()


## override
xml_object <- read_xml('temp_timetables_download/xmls/1_CA_PF_1_20200405.xml') %>% 
  xml_ns_strip()


l = as_list(xml_object)

names(l$TransXChange)   # 7 headings of data




## list to store 8 dataframes extracted from XML
list_timetables <- list()




# stop point codes and lat/longs
list_timetables[['stop_points']] <- l$TransXChange$StopPoints %>% extract_list_of_lists_to_df()





# route sections: linestrings (ie, links) for each section of the route
# looks like there is a set
# of linestrings for each of the route stored in 'routes'

# the 4 route_section_id values don't match the 4 route id's in 'routes' df below. Would
# have expected them to. Maybe can be linked: may have to read metadata

# don't bother with easting or northing here: only extract lat/long columns

all_route_sections_list  <- vector(length = length(l$TransXChange$RouteSections), mode = 'list')
for (iter in seq_along(1:length(all_route_sections_list))){
  
  section_id <- attributes(l$TransXChange$RouteSections[[iter]])$id

  routelinks <- lapply(map(l$TransXChange$RouteSections[[iter]], attributes), '[[', 2)  %>% unlist()
  store_links <- vector(length = length(routelinks), mode = 'list')
  for (i in seq_along(1:length(routelinks))){
    links <- l$TransXChange$RouteSections[[iter]][[i]][[5]][[1]]
    
    print(links)
    #IDs <- lapply(map(links, attributes), '[[', 2) %>% unlist()
    
    IDs <- try(lapply(map(links, attributes), '[[', 2) %>% unlist())
    if("try-error" %in% class(IDs)) {
      IDs <- rep('placeholder', length(links))
    }
    
    latitude <- lapply(links, '[[', '1') %>% lapply('[[', 'Latitude') %>% unlist()
    if (is.null(latitude)) {
      latitude <- lapply(links, '[[', 'Latitude') %>% unlist()
    }
    
    
    longitude <- lapply(links, '[[', 1) %>% lapply('[[', 'Longitude') %>% unlist()
    if (is.null(longitude)) {
      longitude <- lapply(links, '[[', 'Longitude') %>% unlist()
    }
    
    routelinks_repd <- rep(routelinks[i], length(latitude))
    
    store_links[[i]] <- data.frame(routelinks_repd, IDs, latitude, longitude)
  }
  
  route_section <- rbindlist(store_links) %>% rename(link_id = IDs)
  route_section$route_section_id <- section_id
  
  route_section$unique_routelink_section_id <- paste0(route_section$routelink, '_', route_section$route_section_id)
  
  all_route_sections_list[[iter]]  <- route_section
  
  
  
}
list_timetables[['all_route_sections']] <- rbindlist(all_route_sections_list)







# Routes: list of route IDs
routes <- l$TransXChange$Routes %>% map(attributes) %>% map(unlist)  %>% lapply(as.data.frame.list)
list_timetables[['routes']] <- routes %>% rbindlist(fill=T) %>% select(-starts_with('names'))






# journey pattern sections
# has where FROM, where TO, RouteLink reference, RunTime
# StopPointRef links to stop_points
# Would expect RunTime to be numeric but it isn't

# journey_id looks like it should link to JourneyPatternRef from vehicle_journeys_and_departure_times
#   BUT there is an extra 'S' in the codes here. Could be a bug
value_store <- vector(mode='list', length = length(l$TransXChange$JourneyPatternSections))
for (i in seq_along(1:length(value_store))) {

  first <- l$TransXChange$JourneyPatternSections[[i]]
  first <- map(first, unlist) %>% lapply(as.data.frame.list) %>% rbindlist(fill=T) 
  first$journey_id <- attributes(l$TransXChange$JourneyPatternSections[[i]])$id
  value_store[[i]] <- first

}
list_timetables[['journey_pattern_sections']] <- rbindlist(value_store, fill=T)






## Operators
list_timetables[['operators']] <- l$TransXChange$Operators %>% extract_list_of_lists_to_df()






### Services: inc various info about the service. Split into 2 tables
all_order_service_data <- l$TransXChange$Services$Service
all_order_service_data[['StandardService']] <- NULL
all_order_service_df <- unlist(all_order_service_data) %>% as.data.frame() %>% t()%>% as.data.frame()
names(all_order_service_df) <- colnames(all_order_service_df)

standard_service <- l$TransXChange$Services$Service$StandardService

# Vias arent always included
if (sum(names(standard_service) == 'Vias') > 0.5) {
  services_via <- expand.grid(standard_service$Origin, standard_service$Destination, unlist(standard_service$Vias))
  standard_service[['Vias']] <- NULL  # need to do this to make journey pattern table
} else {
  services_via <- expand.grid(standard_service$Origin, standard_service$Destination)
  services_via$vias <- ''
}

names(services_via) <- c('start', 'end', 'vias')

list_timetables[['service_details']] <- data.frame(all_order_service_df, services_via)



# making journey pattern table (separate to other elements of 'Service' as formatted differently - deeper table)
standard_service[['Origin']] <- NULL
standard_service[['Destination']] <- NULL
standard_service[['UseAllStopPoints']] <- NULL

# make line
patterns_store <- list()
for (i in 1:length(standard_service)){
  thing <- unlist(standard_service[[i]])
  ix <- (thing %>% names()) == "JourneyPatternSectionRefs"
  patterns <- data.frame(thing[!ix]) %>% t()
  patterns$JourneyPatternSectionRefs <- thing[ix]
  patterns <- data.frame(patterns)
  
  
  names(patterns) <-c( "PrivateCode" ,  "DestinationDisplay",  "Direction"  , "RouteRef" ,"JourneyPatternSectionRefs") 
  
  
  
  patterns_store[[i]] <- patterns
}
standard_service <- rbindlist(patterns_store, fill = T)


#standard_service <- rbindlist(standard_service, fill=T)     # combine and loop to ensure dataframe format is correct
setDF(standard_service)   # ensure isnt a data.table
for (i in 1:ncol(standard_service)){
  standard_service[, i] <- unlist(standard_service[, i])  # ensure all cols are just vectors: nothing more
}
list_timetables[['standard_service']] <- standard_service







# exporting to json which can make it easier to intuit
for_json <- l$TransXChange$Services
jsonData <- toJSON(for_json) %>% jsonlite::prettify()
write(jsonData, 'example_of_services.json')







# get vehicle_journeys_and_departure_times
## VehicleJourneyCode (unique) and JourneyPatternRef (not unique) look useful for looking things up 
## I think JourneyPatternRef might be the code for a particular service
# DepartureTime looks like the team a given bus departed to start that service 
list_timetables[['vehicle_journeys_and_departure_times']] <- l$TransXChange$VehicleJourneys %>% extract_list_of_lists_to_df()



print(lapply(list_timetables, dim))


### Could run process and stack tables for all XML files







# Same process as above,  wrapped into a function ------------------------
get_list_of_dfs_for_service <- function(x, xml_path) {
  
  print(xml_path)
  
  
  xml_object <- read_xml(paste0(xml_path, "/", x)) %>% 
    xml_ns_strip()
  
  l = as_list(xml_object)
  

  ## list to store 7 dataframes extracted from XML
  list_timetables <- list()
  
  
  # stop point codes and lat/longs
  list_timetables[['stop_points']] <- l$TransXChange$StopPoints %>% extract_list_of_lists_to_df()
  
  
  
  # route sections: linestrings (ie, links) for each section of the route
  # looks like there is a set
  # of linestrings for each of the route stored in 'routes'
  
  # the 4 route_section_id values don't match the 4 route id's in 'routes' df below. Would
  # have expected them to. Maybe can be linked: may have to read metadata
  
  # don't bother with easting or northing here: only extract lat/long columns
  
  all_route_sections_list  <- vector(length = length(l$TransXChange$RouteSections), mode = 'list')
  for (iter in seq_along(1:length(all_route_sections_list))){
    
    section_id <- attributes(l$TransXChange$RouteSections[[iter]])$id
    
    routelinks <- lapply(map(l$TransXChange$RouteSections[[iter]], attributes), '[[', 2)  %>% unlist()
    store_links <- vector(length = length(routelinks), mode = 'list')
    for (i in seq_along(1:length(routelinks))){
      links <- l$TransXChange$RouteSections[[iter]][[i]][[5]][[1]]
      
      #print(links)
      
      IDs <- try(lapply(map(links, attributes), '[[', 2) %>% unlist())
      if("try-error" %in% class(IDs)) {
        IDs <- rep('placeholder', length(links))
      }
      
      latitude <- lapply(links, '[[', '1') %>% lapply('[[', 'Latitude') %>% unlist()
      if (is.null(latitude)) {
        latitude <- lapply(links, '[[', 'Latitude') %>% unlist()
      }
      if (is.null(latitude)) {
        print('latitude still null')
        print(latitude)
        print('links:')
        print(links)
      }
      
      
      longitude <- lapply(links, '[[', 1) %>% lapply('[[', 'Longitude') %>% unlist()
      if (is.null(longitude)) {
        longitude <- lapply(links, '[[', 'Longitude') %>% unlist()
      }
      
      
      routelinks_repd <- rep(routelinks[i], length(latitude))
      
      store_links[[i]] <- data.frame(routelinks_repd, IDs, latitude, longitude)
    }
    
    route_section <- rbindlist(store_links) %>% rename(link_id = IDs)
    route_section$route_section_id <- section_id

    
    route_section$unique_routelink_section_id <- paste0(route_section$routelink, '_', route_section$route_section_id)
    
    all_route_sections_list[[iter]]  <- route_section
  }
  
  list_timetables[['all_route_sections_df']] <- rbindlist(all_route_sections_list)
  
  
  
  
  # Routes: list of route IDs
  routes <- l$TransXChange$Routes %>% map(attributes) %>% map(unlist)  %>% lapply(as.data.frame.list)
  list_timetables[['routes']] <- routes %>% rbindlist(fill=T) %>% select(-starts_with('names'))
  
  
  
  # journey pattern sections
  # has where FROM, where TO, RouteLink reference, RunTime
  # StopPointRef links to stop_points
  # Would expect RunTime to be numeric but it isn't
  
  # journey_id looks like it should link to JourneyPatternRef from vehicle_journeys_and_departure_times
  #   BUT there is an extra 'S' in the codes here. Could be a bug
  value_store <- vector(mode='list', length = length(l$TransXChange$JourneyPatternSections))
  for (i in seq_along(1:length(value_store))) {
    
    first <- l$TransXChange$JourneyPatternSections[[i]]
    first <- map(first, unlist) %>% lapply(as.data.frame.list) %>% rbindlist(fill=T) 
    first$journey_id <- attributes(l$TransXChange$JourneyPatternSections[[i]])$id
    value_store[[i]] <- first
    
  }
  list_timetables[['journey_pattern_sections']] <- rbindlist(value_store, fill=T)
  
  
  
  
  
  ### Services: inc various info about the service. Split into 2 tables
  all_order_service_data <- l$TransXChange$Services$Service
  all_order_service_data[['StandardService']] <- NULL
  all_order_service_df <- unlist(all_order_service_data) %>% as.data.frame() %>% t()%>% as.data.frame()
  names(all_order_service_df) <- colnames(all_order_service_df)
  
  standard_service <- l$TransXChange$Services$Service$StandardService
  
  # Vias arent always included
  if (sum(names(standard_service) == 'Vias') > 0.5) {
    services_via <- expand.grid(standard_service$Origin, standard_service$Destination, unlist(standard_service$Vias))
    standard_service[['Vias']] <- NULL    # need to do this to make journey pattern table
  } else {
    services_via <- expand.grid(standard_service$Origin, standard_service$Destination)
    services_via$vias <- ''
  }
  
  names(services_via) <- c('start', 'end', 'vias')
  list_timetables[['service_details']] <- data.frame(all_order_service_df, services_via)
  
  
  
  
  # making journey pattern table (separate to other elements of 'Service' as formatted differently - deeper table)
  standard_service[['Origin']] <- NULL
  standard_service[['Destination']] <- NULL
  standard_service[['UseAllStopPoints']] <- NULL
  
  # make line
  patterns_store <- list()
  for (i in 1:length(standard_service)){
    thing <- unlist(standard_service[[i]])
    ix <- (thing %>% names()) == "JourneyPatternSectionRefs"
    patterns <- data.frame(thing[!ix]) %>% t()
    patterns$JourneyPatternSectionRefs <- thing[ix]
    patterns <- data.frame(patterns)
    
    if (is.null(patterns$JourneyPatternSectionRefs)) {   # this is missing in some cases
      patterns$JourneyPatternSectionRefs <- rep('placeholder', nrow(patterns)) 
    }
    
    if (nrow(patterns) == 5){
      names(patterns) <-c( "PrivateCode" ,  "DestinationDisplay",  "Direction"  , "RouteRef" ,"JourneyPatternSectionRefs") 
    } else {
      print(patterns)
    }
    
    patterns_store[[i]] <- patterns
  }
  standard_service <- rbindlist(patterns_store, fill = T)
  
  
  #standard_service <- rbindlist(standard_service, fill=T)     # combine and loop to ensure dataframe format is correct
  setDF(standard_service)   # ensure isnt a data.table
  for (i in 1:ncol(standard_service)){
    standard_service[, i] <- unlist(standard_service[, i])  # ensure all cols are just vectors: nothing more
  }
  list_timetables[['standard_service']] <- standard_service
  
  
  
  
  
  
  ## Operators
  operators <- l$TransXChange$Operators %>% extract_list_of_lists_to_df()
  operators$ServiceCode <- list_timetables[['service_details']]$ServiceCode[1]  # adding service code
  list_timetables[['operators']] <- operators
  
  
  
  
  # get vehicle_journeys_and_departure_times
  list_timetables[['vehicle_journeys_and_departure_times']] <- l$TransXChange$VehicleJourneys %>% extract_list_of_lists_to_df()

  
  # adding file name source to all tables
  for (n in names(list_timetables)) {
    df <- list_timetables[[n]]
    df$file_source <- rep(x, nrow(df))
    list_timetables[[n]] <- df
  }
  
  
  
  list_timetables
}



### apply func to all services on Isle of Wight
all_services_list <- vector(mode='list', length=length(IoW_list))

for (i in seq_along(1:length(all_services_list))){
  x = IoW_list[i]
  all_services_list[[i]] <- get_list_of_dfs_for_service(x, xml_path)
  print(x)
}



# append to get 8 large dfs covering all services
full_isle_of_wight <- list()
table_names <- names(all_services_list[[1]])

for (i in seq_along(1:length(table_names))){
  table_name <- table_names[i]
  full_isle_of_wight[[table_name]] <- bind_rows(lapply(all_services_list, '[[', i))
  full_isle_of_wight[[table_name]] <- distinct(full_isle_of_wight[[table_name]])
  setDF(full_isle_of_wight[[table_name]])  # ensure it isn't a data.table
}





### adding service name to links table
service_code_lookup <- full_isle_of_wight$service_details %>% 
  select(ServiceCode, file_source) %>%
  distinct()

full_isle_of_wight$all_route_sections_df  <- full_isle_of_wight$all_route_sections_df %>% 
  merge(service_code_lookup, by='file_source')

# add an ID that's unique for that link and service. Useful when plotting the links
full_isle_of_wight$all_route_sections_df  <- full_isle_of_wight$all_route_sections_df %>%
  mutate(unique_service_routelink_id = paste0(ServiceCode, '_', routelink))





# saving results
save(full_isle_of_wight, file = "full_isle_of_wight.RData")






###### End of processing






# Wrapping above into single function -------------------------------------


extract_all_xml_files <- function(xml_path, output_list_folder, dataset_id) {
  #' Take all bus timetables in XML format from one folder. Return list of 8 tables
  #' 
  #' @param xml_path Folder pathway where XML files are
  #' 
  #' @param output_list_folder Pathway where output list will be put
  #' 
  #' @param dataset_id ID number of that dataset. Use to set output filename
  #' 
  
  ## In the below 'full_isle_of_wight' is the list of data. It could be the data for
  ## any dataset in practice
  
  #setwd('/Users/dftdatascience/Desktop/bods_timetables')
  #xml_path <- "xml_timetables_data/bus_timetable_xml/"
  
  IoW_list <- list.files(xml_path, pattern = "xml")
  
  
  
  ### apply func to all services on Isle of Wight
  all_services_list <- vector(mode='list', length=length(IoW_list))
  
  for (i in seq_along(1:length(all_services_list))){
    x = IoW_list[i]
    all_services_list[[i]] <- get_list_of_dfs_for_service(x, xml_path)
    save(all_services_list, file = 'all_services_list.RData')
    print(x)
  }
  
  
  print('append to get 8 large dfs covering all services')
  full_isle_of_wight <- list()
  table_names <- names(all_services_list[[1]])
  
  for (i in seq_along(1:length(table_names))){
    table_name <- table_names[i]
    full_isle_of_wight[[table_name]] <- bind_rows(lapply(all_services_list, '[[', i))
    full_isle_of_wight[[table_name]] <- distinct(full_isle_of_wight[[table_name]])
    setDF(full_isle_of_wight[[table_name]])  # ensure it isn't a data.table
  }
  save(full_isle_of_wight, file = 'all_services_list.RData')
  
  
  
  ## adding service name to links table
  service_code_lookup <- full_isle_of_wight$service_details %>% 
    select(ServiceCode, file_source) %>%
    distinct()
  
  full_isle_of_wight$all_route_sections_df  <- full_isle_of_wight$all_route_sections_df %>% 
    merge(service_code_lookup, by='file_source')
  
  # add an ID that's unique for that link and service. Useful when plotting the links
  full_isle_of_wight$all_route_sections_df  <- full_isle_of_wight$all_route_sections_df %>%
    mutate(unique_service_routelink_id = paste0(ServiceCode, '_', routelinks_repd))
  
  
  
  # saving results
  output_path <- paste0(output_list_folder, '/dataset', dataset_id, '.RData')
  save(full_isle_of_wight, file = output_path)
  
  
}











######## more metadata here:

# http://naptan.dft.gov.uk/transxchange/schema/2.4/doc/TransXChangeSchemaGuide-2.4-v-52.pdf
# https://www.pti.org.uk/system/files/files/TransXChange%20UK%20PTI%20Profile%20v1.1.pdf





















