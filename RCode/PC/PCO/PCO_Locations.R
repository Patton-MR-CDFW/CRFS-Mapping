# Script to clean up and compile the i8b (location data) for the PCO mode
# Michael Patton (michael.patton@wildlife.ca.gov)

#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below install.packages("packagename") so for example install.packages("data.table") #install.packages('tidyverse', repos='http://cran.rstudio.com'), need to be on VPN
library(data.table)
library(readxl)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf) 
library(leaflet)
library(units)
options(scipen = 999)



# create a function that takes the format of coordinates in the CRFS data and convert to decimal degrees. The function inputs are the CRFS coordinate values, if its longitude or latitude and the gformat
CRFStoDD <-function(value, type, gformat) {
  if (type == 'latitude'){
    if (gformat == 1) {
      degree = substr(value, start = 1, stop = 2)
      minute = substr(value, 3, nchar(value))
      minute = as.numeric(minute)/100
      decimal = minute/60
      dd = as.numeric(degree) + decimal
      
    } else {
      degree = substr(value, start = 1, stop = 2)
      minute = substr(value, 3, 4)
      seconds = substr(value, 5, nchar(value))
      
      dd = as.numeric(degree) + (as.numeric(minute)/60) + (as.numeric(seconds)/3600)

    }
  } else if (type == 'longitude') {
    if (gformat == 1) {
      degree = substr(value, start = 1, stop = 2)
      degree = paste0('1', degree)
      minute = substr(value, 3, nchar(value))
      minute = as.numeric(minute)/100
      decimal = minute/60
      dd = as.numeric(degree) + decimal
      dd = dd*-1
      
    } else {
      degree = substr(value, start = 1, stop = 2)
      degree = paste0('1', degree)
      minute = substr(value, 3, 4)
      seconds = substr(value, 5, nchar(value))
      
      dd = as.numeric(degree) + (as.numeric(minute)/60) + (as.numeric(seconds)/3600)
      dd = dd*-1
    }
  } else {
    break
  }
  
  return(dd)

}

# function to take the format of the start and end times CRFS data and convert to a machine readable time value
convert_to_time <- function(time_str) {
  # Ensure that the time string has 4 characters (e.g., "0942")
  padded_time_str <- sprintf("%04s", time_str)
  
  # Extract hours and minutes
  hours <- as.integer(substr(padded_time_str, 1, 2))
  minutes <- as.integer(substr(padded_time_str, 3, 4))
  
  
  # Create a lubridate time object
  time_obj <- make_datetime(hour = hours, min = minutes)
  
  return(time_obj)
}


# Read in data ------------------------------------------------------------
# 8a: Boat information (CPFV, total anglers, port, # of stops, etc.)
# this is needed to bring in the trip type associated with the i8b table

# data is split between data from 2012- 2024, and pre2012.
i8a = fread(file = here('RCode', 'PC', 'Dat12toPresent', 'Data', 'i8a_data_12to24.csv'), fill=TRUE) %>%
  mutate_all(as.character)
unique(i8a$year)

i8a_old = rbind(read_excel(here('RCode', 'PC', 'Dat04to16', 'PC_i8a_1999-2016_NorCal_2283r.xlsx'), guess_max = 100000),
                read_excel(here('RCode', 'PC', 'Dat04to16', 'PC_i8a_1999-2016_SoCal_8472r.xlsx'), guess_max = 100000)) %>%
  filter(YEAR >= 2004 & YEAR < 2012)

# change names for agreement so the datasets can be joined together
names(i8a_old) <- tolower(names(i8a_old))

i8a_old = i8a_old %>%
  rename(`OSP port` = osp_port,
         FIRST = first,
         `Ref #` = ref_num
         ) %>%
  select(-assnid_c, -wave) %>%
  mutate(DD = NA)

i8a_old[i8a_old=="."]<-NA

# bind old and new i8a datasets together
i8a = rbind(i8a, i8a_old)

### Read in Species Lookup table
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.character(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") %>%
  filter(!is.na(PSMFC_Code)) %>%
  mutate(PSMFC_Code = as.character(PSMFC_Code))

# Prim1 and Prim2 is blank on the i8 files in the pre2012 data. Can be brought in from the i1 table and joined when you shorten the ID_CODE by the last three digits. Drops the last digits on the ID_CODE on the i1 file to link to the ASSN on the i8b data.
i1_old = read_excel(here('RCode', 'PC', 'Dat04to16', 'PC_i1_2004-2015_177627r.xlsx'), guess_max = 10000000) %>%
  filter(YEAR >= 2004 & YEAR < 2012)

# an issue with the i1 data is that assn are replicated for each angler interviewed. Often (not sure why), each angler specifies a different primary species. 
# This gets the assn from each ID by removing the last three values, then joins the trip type for below logic
i1_old_sum = i1_old %>%
  mutate(assn_i1 = substr(ID_CODE,1,nchar(ID_CODE)-3)) %>%
  select(assn_i1, prim1, prim2) %>%
  unique() %>%
  mutate(prim1 = as.character(prim1)) %>%
  left_join(Sp %>% select(PSMFC_Code, TripType_Description), by = c("prim1" = "PSMFC_Code"))

# to avoid the duplication created by each ID having its own trip type, the count of each trip type is within the assn is taken. If there are multiple trip types within the assn, the 'Anything' specified trip type is removed from that assn. Then the trip type that the most anglers specified is used
top_trip_type = i1_old_sum %>%
  group_by(assn_i1, prim1, TripType_Description) %>%
  count() %>%
  group_by(assn_i1) %>%
  mutate(unique_types = n_distinct(TripType_Description)) %>%
  filter(!(TripType_Description == 'Anything' & unique_types > 1)) %>%
  slice(which.max(n)) %>%
  select(-unique_types, -TripType_Description)

test = top_trip_type %>%
  group_by(assn_i1) %>%
  count() %>%
  filter(n>1)

# add in the prim1 for pre2012 data
i8a = i8a %>%
  left_join(top_trip_type, by = c('assn' = 'assn_i1')) %>%
  mutate(prim1 = ifelse(is.na(prim1.x), prim1.y, prim1.x)) %>%
  select(-prim1.x, -prim1.y)
  
# add in prim1 vs prim2 logic where prim1 is only used unless prim1 == 'Invertebrates",
i8a <- i8a %>%
  left_join(Sp %>% select(PSMFC_Code, TripType_Description), by = c("prim1" = "PSMFC_Code")) %>%
  mutate(primary = ifelse(TripType_Description == "Invertebrates" & !is.na(TripType_Description) & !is.na(prim2), prim2, prim1)) %>%
  select(-TripType_Description)

# pull out data that is not used because it uses a bad sp_code
notused <- i8a %>%
  anti_join(Sp, by = c("primary" = "PSMFC_Code")) %>%
  mutate(Reason = "Primary Species not found in species lookup.")
notused %>% group_by(primary) %>% count()
byyear = notused %>% group_by(year) %>% count()
write.csv(notused, 'Outputs/PC/PCO/NotUsed/i8a/notused.csv', row.names = F, na = "")


# join species lookup to data
i8a <- i8a %>%
  inner_join(Sp, by = c("primary" = "PSMFC_Code"))

# create a new ID that will be used throughout the scrip with the assn (no locnum in i8a data)
i8a = i8a %>%
  mutate(ID_8a = assn) %>%
  select(-assnid, -assn, -`Ref #`, -month)

# remove data where the ID (assn) is used multiple times. This is from data error
notused2 = i8a %>%
  group_by(ID_8a) %>%
  count() %>%
  filter(n > 1) %>%
  left_join(i8a, by = 'ID_8a') %>%
  mutate(Reason = 'Duplicates in i8a table')
byyear = notused2 %>% group_by(year) %>% count()
write.csv(notused2, 'Outputs/PC/PCO/NotUsed/i8a/notused2.csv', row.names = F, na = "")

i8a = i8a %>%
  filter(!(ID_8a %in% notused2$ID_8a))

#summary of the different trip type descriptions found in the data
type_summary = i8a %>%
  group_by(Common_Name, TripType_Description) %>%
  count() %>%
  arrange(desc(n)) # some invertebrates still make it through

# read in i8b data --------------------------------------------------------

#8b: Stop locations (GPS coordinates)
# again data is split between 2012-2024 and pre2012
i8b = fread(file = here('RCode', 'PC', 'Dat12toPresent', 'Data', 'i8b_data_12to24.csv'), fill=TRUE) %>%
  mutate_all(as.character)

i8b_old = rbind(read_excel(here('RCode', 'PC', 'Dat04to16', 'PC_i8b_1999-2016_NorCal_18385r.xlsx'), guess_max = 100000),
                read_excel(here('RCode', 'PC', 'Dat04to16', 'PC_i8b_1999-2016_SoCal_53589r.xlsx'), guess_max = 100000)) %>%
  filter(YEAR >= 2004 & YEAR < 2012)

i8b_old[i8b_old=="."]<-NA

# get columns to agree so data can be binded together
names(i8b_old) = tolower(names(i8b_old))

i8b_old = i8b_old %>%
  rename(`Ref #` = ref_num) %>%
  select(-assnid_c, -delete, -etemp, -pinniped, -plbait, -plfish, -plgear, -pltime, -prmove, -sitename, -stemp, -wave) %>%
  mutate(ASSNID=NA) %>%
  mutate_all(as.character)

i8b = rbind(i8b, i8b_old)

# toss any data that does not have a gformat pr ftype. 
notused3 = i8b %>%
  filter(is.na(gformat) | is.na(ftype)) %>%
  mutate(Reason = 'No gformat or ftype provided')
byyear = notused3 %>% group_by(year) %>% count()
write.csv(notused3, 'Outputs/PC/PCO/NotUsed/i8b/notused3.csv', row.names = F, na = "")

i8b = i8b %>%
  filter(!is.na(gformat)) %>%
  filter(!is.na(ftype))

# toss data that does not have a coordinate inputted. 
# if ftype is anchor (3) or static (2), we could use the available coordinates since the boat wouldn’t move very much. If the ftype is drift (1) or troll (4), we should not use the coordinates since it only captures one end of the drift/troll and might not be representative of the fishing location.
notused4 = i8b %>%
  filter((grepl('99999', slat) | grepl('99999', slon)) | ((grepl('99999', elat) | grepl('99999', elon)) & ftype %in% c(1, 4))) %>%
  mutate(Reason = 'Bad coordinates')
byyear = notused4 %>% group_by(year) %>% count()
write.csv(notused4, 'Outputs/PC/PCO/NotUsed/i8b/notused4.csv', row.names = F, na = "")

i8b = i8b %>%
  filter(!(grepl('99999', slat) | grepl('99999', slon))) %>%
  filter(!((grepl('99999', elat) | grepl('99999', elon)) & ftype %in% c(1, 4)))

# use the CRFStoDD function to convert each coordinate column to the correct format
i8b = i8b %>%
  rowwise() %>%
  mutate(slat_dd = CRFStoDD(slat, 'latitude', gformat),
         slon_dd = CRFStoDD(slon, 'longitude', gformat),
         elat_dd = CRFStoDD(elat, 'latitude', gformat),
         elon_dd = CRFStoDD(elon, 'longitude', gformat)) %>%
  ungroup()

# toss any data that was not able to be converted to decimal degrees correctly
notused5 = i8b %>%
  filter(is.na(slat_dd) | is.na(slon_dd) | is.na(elat_dd) | is.na(elon_dd)) %>%
  mutate(Reason = 'Bad coordinates  -round 2')
byyear = notused5 %>% group_by(year) %>% count()
write.csv(notused5, 'Outputs/PC/PCO/NotUsed/i8b/notused5.csv', row.names = F, na = "")

i8b = i8b %>%
  filter(!is.na(slat_dd) & !is.na(slon_dd) & !is.na(elat_dd) & !is.na(elon_dd))

# Apply the time conversion function to the start and end time 
i8b$start_time <- sapply(i8b$stime, convert_to_time)
i8b$end_time <- sapply(i8b$etime, convert_to_time)

# calculate a duration field in hours. This will be used to compare the distance and time traveled of each trip
i8b = i8b %>%
  mutate(duration = end_time - start_time) %>%
  mutate(duration = duration/3600) 

# remove data where duration is negative or could not be converted correctly
notused6 = i8b %>%
  filter(duration <= 0 | (is.na(start_time) | is.na(end_time))) %>%
  mutate(Reason = 'Start and end times not entered correctly')
byyear = notused6 %>% group_by(year) %>% count()
write.csv(notused6, 'Outputs/PC/PCO/NotUsed/i8b/notused6.csv', row.names = F, na = "")

i8b = i8b %>%
  filter(duration > 0)


# create a new ID that will be used throughout the script with the assn and locnum. A version without the locn will be needed as well for joining the i8a data
i8b = i8b %>%
  mutate(ID = paste0(assn, locnum)) %>%
  mutate(ID_noloc = paste0(assn))

# remove data where the ID field is duplicated. This is from data error
notused7 = i8b %>%
  group_by(ID) %>%
  count() %>%
  filter(n > 1) %>%
  left_join(i8b, by = 'ID') %>%
  mutate(Reason = 'Duplicates in i8b data')
byyear = notused7 %>% group_by(year) %>% count()
write.csv(notused7, 'Outputs/PC/PCO/NotUsed/i8b/notused7.csv', row.names = F, na = "")

i8b = i8b %>%
  filter(!(ID %in%  notused7$ID))

# join i8a to i8b join by the ID (without locnum), toss any data that cannot be joined
notused8 = i8b %>%
  anti_join(i8a, by = c('ID_noloc' = 'ID_8a')) %>%
  mutate(Reason = 'no i8a data')
byyear = notused8 %>% group_by(year) %>% count()
write.csv(notused8, 'Outputs/PC/PCO/NotUsed/i8b/notused8.csv', row.names = F, na = "")

i8b_withi8a = i8b %>%
  select(-year) %>%
  inner_join(i8a, by = c('ID_noloc' = 'ID_8a'))

# Start of spatial processing ---------------------------------------------
# A '.SP' is added to the end of all variables representing a spatial sf feature. This is to save you from accidentally trying a computationally time consuming function for spatial features that are simple for dataframes (ie group by)

# bring in the block shapefile from lookup folder
blocks.SP <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013.shp"))

st_crs(blocks.SP) # provide the projection information about the crfs microblocks file

blocks.SP = st_transform(blocks.SP, crs = 4326) # change to WGS84

# test blocks shapefile for validity and simpleness, need to go back and fix issues in the source data. They will cause errors during processing but can fix in the script temporarily
all(st_is_valid(blocks.SP)) # test for valid, ie dont intersect, have missing connections as a polygon etc

test = cbind(blocks.SP, valid = st_is_valid((blocks.SP))) %>%
  filter(valid == FALSE)

plot(st_geometry(test[2,])) # see whats going on 
test2 = st_make_valid(test) # make valid
plot(st_geometry(test2[2,]))

# do with whole feature temporarily, will fix in source data
blocks.SP = st_make_valid(blocks.SP)

blocks.SP = select(blocks.SP, NM_INDEX)


# Point processing (where coordinates are the same) -----------------------
# pull out points 
points_i8b = i8b_withi8a %>%
  filter(((slat_dd == elat_dd) & (slon_dd == elon_dd)) | (grepl('99999', elat) & grepl('99999', elon)))


# create an sf feature using the coordinates and assign the WGS84 CRFS
points_i8b.SP = st_as_sf(points_i8b, coords=c('slon_dd', 'slat_dd'), crs = 4326, remove = FALSE)

# temporary plotting to see what the points look like
plot(st_geometry(points_i8b.SP))
plot(blocks.SP %>% group_by() %>% summarise(), add=T, col='red') # similiar to dissolve to get outline of blocks

# spatial join of points with blocks
points_wblocks.SP = points_i8b.SP %>%
  st_join(blocks.SP) #left join unless otherwise specificed

# remove data where points do not intersect with blocks
notused_points.SP = filter(points_wblocks.SP, is.na(NM_INDEX))
plot(st_geometry(notused_points.SP), add=TRUE, col = 'red')

notused9 = notused_points.SP%>%
  mutate(Reason = 'Coordinates do not intersect a block') %>%
  st_drop_geometry()
byyear = notused9 %>% group_by(year) %>% count()
write.csv(notused9, 'Outputs/PC/PCO/NotUsed/i8b/notused9.csv', row.names = F, na = "")

points_wblocks.SP = filter(points_wblocks.SP, !is.na(NM_INDEX))

# since spatial processing takes a long time, write a file for record keeping if any additional processing is needed
st_write(points_wblocks.SP, here('Outputs', 'PC', 'PCO', 'Shapefiles', 'pointswblocks.shp'), append=FALSE)

# keep only points that intersect with block
points_wblocks = points_wblocks.SP %>% 
  st_drop_geometry() # drop geometry column. No longer needed, go back to only dataframe


# Line processing of i8b data ---------------------------------------------
lines_i8b = i8b_withi8a %>%
  filter(!(ID %in% points_i8b$ID))
nrow(points_i8b) + nrow(lines_i8b) == nrow(i8b_withi8a)

# create point features of both starting and ending coordinates  
starting.SP <- st_as_sf(lines_i8b, coords = c("slon_dd", "slat_dd"), crs = 4326, remove = FALSE)
ending.SP <- st_as_sf(lines_i8b, coords = c("elon_dd", "elat_dd"), crs = 4326, remove = FALSE) 

# bind these together, and recast as a line using the unique ID for each row of data
lines.SP = rbind(starting.SP, ending.SP) %>%
  group_by(across(c(-geometry))) %>%
  summarise(do_union=FALSE) %>% # if not set to false, will combine points by averaging instead of a multipoint feature
  st_cast("LINESTRING") 

# QA check to make sure start and end points are on a block
notused_lines = rbind(starting.SP, ending.SP) %>%
  st_join(blocks.SP) %>% #left join unless otherwise specificed
  st_drop_geometry() %>% # drop geometry for speed
  group_by(ID) %>%
  summarise(blocks_intersected = sum(!is.na(NM_INDEX))) %>%
  filter(blocks_intersected != 2)

# toss data where the start or end point do not start on blocks
notused10 = lines_i8b %>%
  filter(ID %in% notused_lines$ID) %>%
  mutate(Reason = 'Start or end coordiante does not fall within block')
byyear = notused10 %>% group_by(year) %>% count()
write.csv(notused10, 'Outputs/PC/PCO/NotUsed/i8b/notused10.csv', row.names = F, na = "")

# keep lines that fall within blocks
lines.SP = lines.SP %>%
  filter(!(ID %in% notused_lines$ID))


# QA check on drifts ------------------------------------------------------
### this section applies filters that we developed to toss out erroneous data such as way too long drift given the trip type and duration

# calculate distance of the straight line between start and end coordinates
lines.SP = lines.SP %>%
  mutate(straight_line_distance = st_length(geometry))

# convert to miles and make numeric
units(lines.SP$straight_line_distance) = 'miles'
lines.SP$straight_line_distance = as.numeric(lines.SP$straight_line_distance)

# following section applies filters based on the duration, distance, and ftype of the data
# ftype: 1= Free drift, 2=Stationed, 3=Anchored 4=Troll

# calculate mph of each trip by dividing the distance by duration
linesQA.SP = lines.SP %>%
  mutate(mph_of_trip = straight_line_distance/duration)

# remove any data where the mph was greater than 10, this never happens when fishing. 
notused11 = linesQA.SP %>%
  st_drop_geometry() %>%
  filter(mph_of_trip > 10) %>%
  mutate(Reason = 'mph of trip was greater than 10')
byyear = notused11 %>% group_by(year) %>% count()
write.csv(notused11, 'Outputs/PC/PCO/NotUsed/i8b/notused11.csv', row.names = F, na = "")

linesQA.SP = linesQA.SP %>%
  filter(mph_of_trip < 10)

# remove data where trip type is bottomfish and the total distance travelled was greater than a mile
# also remove data where the ftype == 1 and the total distance travelled was greater than 3 miles
notused12 = linesQA.SP %>%
  st_drop_geometry() %>%
  filter((TripType_Description == "Bottomfish" & straight_line_distance > 1) | (ftype == 1 &  straight_line_distance > 3)) %>%
  mutate(Reason = 'bottomfish or ftype of 1 trip with too long of distance traveled')
byyear = notused12 %>% group_by(year) %>% count()
write.csv(notused12, 'Outputs/PC/PCO/NotUsed/i8b/notused12.csv', row.names = F, na = "")


# remove any data where ftype was 2 or 3 and the total distance was greater than .5 miles
notused13 = linesQA.SP %>%
  st_drop_geometry() %>%
  filter(ftype %in% c(2, 3) & straight_line_distance > .5) %>%
  mutate(Reason = 'stationed or anchored trip with too long of a distance traveled')
byyear = notused13 %>% group_by(year) %>% count()
write.csv(notused13, 'Outputs/PC/PCO/NotUsed/i8b/notused13.csv', row.names = F, na = "")

# apply filters to main dataset
linesQA.SP = linesQA.SP %>%
  filter(!(TripType_Description == 'Bottomfish' & straight_line_distance > 1))  %>%
  filter(!(ftype == 1 & straight_line_distance > 3)) %>%
  filter(!(ftype == 1 & mph_of_trip > 5)) %>%
  filter(!(ftype %in% c(2,3) & straight_line_distance > .5))


# Join the blocks to the lines --------------------------------------------
# function that speeds up the intersection of the lines with the points
st_intersection_faster <- function(x,y,...){
  #faster replacement for st_intersection(x, y,...)
  
  y_subset <-
    st_intersects(x, y) %>%
    unlist() %>%
    unique() %>%
    sort() %>%
    {y[.,]}
  
  st_intersection(x, y_subset,...)
}

# continue with spatial processing, intersect lines with blocks. That creates a new line that is inside of each block. . This pushes the limit of what R can do based on the RAM of your computer so broken down by year. 
rm(ending.SP, lines.SP, starting.SP, points_i8b.SP)
gc()

for (i_year in unique(linesQA.SP$year)){
  print(i_year)
  oneyear_data = filter(linesQA.SP, year == i_year)
  
  i_lines_wblocks.SP = oneyear_data %>%
    st_intersection_faster(blocks.SP) 
  
  if (exists('lines_wblocks.SP')){
    lines_wblocks.SP = rbind(lines_wblocks.SP, i_lines_wblocks.SP)
  } else {
    lines_wblocks.SP = i_lines_wblocks.SP
  }
  rm(oneyear_data, i_lines_wblocks.SP)
  gc()
}

# write a backup copy of the intersected lines so dont have to rerun the processing
st_write(lines_wblocks.SP, here('Outputs', 'PC', 'PCO', 'Shapefiles', 'lines_i8b.shp'), append=FALSE)


# calculate the distance of the line within each block
lines_wblocks.SP = lines_wblocks.SP %>%
  mutate(partial_line_distance = st_length(geometry))

units(lines_wblocks.SP$partial_line_distance) = 'miles'
lines_wblocks.SP$partial_line_distance = as.numeric(lines_wblocks.SP$partial_line_distance)

# QA check to see if any line sections are not assigned to a block
line_sum = lines_wblocks.SP %>%
  st_drop_geometry() %>%
  group_by(ID) %>%
  summarise(partial_sum = sum(as.numeric(partial_line_distance)))

# compare the summed partial lines with the total line
lines_wblocks.SP = lines_wblocks.SP %>%
  left_join(line_sum, by = 'ID') %>%
  mutate(difference = as.numeric(straight_line_distance) - partial_sum) 

# test for any lines that are missing coverage by a block
missing_distance = lines_wblocks.SP %>%
  filter(difference > 100)

# calculate the weighting for each block by dividing the partial line distance within the block by the total distance of the line
lines_wblocks.SP = lines_wblocks.SP %>%
  mutate(weighting = as.numeric(partial_line_distance)/as.numeric(straight_line_distance))


# clean up final line with block data
lines_locations_clean = lines_wblocks.SP %>%
  st_drop_geometry() %>%
  rename(Ref_Number = `Ref..`, OSP_Port = OSP.port) %>%
  select(-difference) %>%
  mutate(spatial = 'line')

# clean up final point with block data
# a weighting of 1 is automatically assigned to all point data
points_locations_clean = points_wblocks %>%
  rename(Ref_Number = `Ref #`, OSP_Port = `OSP port`) %>%
  mutate(weighting = 1) %>%
  mutate(spatial = 'point')

# bind together to get all locations
locations_clean = bind_rows(lines_locations_clean, points_locations_clean)


###normalize effort to angler day, without normalization every drops anglers will be summed on the effort side (ie 3 anglers do 3 drops in the same block, itll be 9 anglers total) Want to align with PR where we just get the anglers in that block for that day (3 in the above example)
names(locations_clean)
angs_by_block = locations_clean %>%
  group_by(assn, NM_INDEX) %>%
  summarise(n_drops = n(),
            unique_angs = paste(unique(obsang), collapse = ', '))

# calculate the total number of blocks visited for tracking
total_blocks = locations_clean %>%
  group_by(ID) %>%
  summarise(total_blocks = n_distinct(NM_INDEX)) 

locations_clean = locations_clean %>%
  left_join(total_blocks, by = 'ID') %>%
  arrange(ID) %>%
  select(-intvuer, -OSP_Port, -boatnum, -boatname, -landing, -captain,-cnty, -intsite,-numsp,-st, -FIRST, -prim1, -prim2, -survey, -DD, -partial_line_distance, -partial_sum, -mph_of_trip) %>%
  rename(Block = NM_INDEX) %>%
  left_join(angs_by_block, by = c('assn' ,'Block' = 'NM_INDEX'))

locations_clean = locations_clean %>%
  mutate(ang_norm = as.numeric(obsang)/n_drops)


# write to .csv
write.csv(locations_clean,  "Outputs/PC/PCO/PCO_locations.csv", na = "", row.names = F)


# final check for locations_clean
final_check = locations_clean %>%
  group_by(ID, Block, Common_Name) %>% 
  count() %>%
  filter(n > 1)

# not used summary
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason), unique(notused5$Reason), unique(notused6$Reason), unique(notused7$Reason), unique(notused8$Reason), unique(notused9$Reason), unique(notused10$Reason), unique(notused11$Reason), unique(notused12$Reason), unique(notused13$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4), nrow(notused5), nrow(notused6), nrow(notused7), nrow(notused8), nrow(notused9), nrow(notused10), nrow(notused11), nrow(notused12), nrow(notused13))
                             )
names(notused_summary) = c("Reason", "Count")
write.csv(notused_summary, "Outputs/PC/PCO/PCO_location_notusedsummary.csv", na = "", row.names = F)


gc()




