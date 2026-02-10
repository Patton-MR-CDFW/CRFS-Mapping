# Script to clean up and compile the i8 (location data) for the PC mode
# Michael Patton (michael.patton@wildlife.ca.gov)

#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below install.packages("packagename") so for example install.packages("data.table")
library(data.table)
library(readxl)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(sf)
library(leaflet)
options(scipen = 999)

# function takes block column and microblock column and standardizes if populated
format_box = function(block_column, microblock_column) {
  combined = paste(block_column, microblock_column, sep="-")
  combined_noNA = gsub("NA", "", combined)
  combined_noblanks = ifelse(nchar(combined_noNA) < 5, "", combined_noNA)
  return(combined_noblanks)
}

#PCD
i8 = fread(file = here('RCode', 'PC', 'Dat12toPresent', 'Data', 'i8_data_12to24.csv'), fill=TRUE) %>%
  rename(ID_CODE = id_code) %>%
  mutate_all(as.character)


# Read in older location data, or the i8 data table for northern california. Uses same file as the PR data
NLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_NorCal_235374r.csv"), fill = TRUE, na.string = c("",".", "NA")) 
# read in older i8 location data for southern california
SLocList <- fread(file = here("RCode", "PR", "Dat04to15", "Data", "i8_1999-2016_SoCal_204029r.csv"), fill = TRUE, na.string = c("",".", "NA")) 

#combine north and south locaiton data together
old_loc = rbind(NLocList, SLocList) %>%
  mutate_all(as.character) %>%
  filter(YEAR < 2012 & YEAR >= 2004) %>%
  filter(MODE_FX == '6')


# combine all years of data
i8 = bind_rows(i8, old_loc)

# remove columns that are just NAs
i8 <- select(i8, where(function(x) !all(is.na(x))))

# no locn is needed for PCD
PCD_loc <- i8 %>% 
  mutate(
         block1 = ifelse(block1 == 9999999, NA, block1),
         Bk1Bx1a = format_box(block1, box1a),  
         Bk1Bx1b = format_box(block1, box1b), 
         Bk1Bx1c = format_box(block1, box1c), 
         Bk2Bx2a = format_box(block2, box2a), 
         Bk2Bx2b = format_box(block2, box2b), 
         Bk2Bx2c = format_box(block2, box2c)) %>%
  select(year=YEAR, ID_CODE, MODE_FX, survey, ddlat, ddlong, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, HGSIZE, hgsize2)


unique(PCD_loc$MODE_FX)
PCD_loc[PCD_loc==""]<-NA

before = nrow(PCD_loc)
# remove data that does not have block or coordinates reported
notused = PCD_loc %>% #AROUND 50% of the data does NOT HAVE BLOCK BOX. That seems really high. 
  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a) & is.na(Bk2Bx2b) & is.na(ddlat)) %>%
  mutate(Reason = "Location data does not include any block data or a box is not provided")
byyear = notused %>% group_by(year) %>% count()
write.csv(notused, 'Outputs/PC/PCD/NotUsed/i8/notused.csv', row.names = F, na = "")
removed = nrow(notused)

PCD_loc = PCD_loc %>%
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)| !is.na(ddlat))
after = nrow(PCD_loc)
before - removed == after


# remove any blocks that have an hgsize greater than 1 reported, this field is where the survey gives the "range" of blocks visited and a decision was made to include no range or a small range of 1
hgsize_summary = PCD_loc %>% group_by(HGSIZE, hgsize2) %>% count()

notused2 = filter(PCD_loc, HGSIZE > 1 | hgsize2 > 1) %>%
  mutate(Reason = "HGSize or HGSize2 reported ")
byyear = notused2 %>% group_by(year) %>% count()
write.csv(notused2, 'Outputs/PC/PCD/NotUsed/i8/notused2.csv', row.names = F, na = "")

# filter to only use data that does not have an HGSIZE or hgsize2 or only has 1 reported
PCD_loc = PCD_loc %>%
  filter((is.na(HGSIZE) | HGSIZE <=1) & (is.na(hgsize2) | hgsize2 <=1))

# QA
nrow(PCD_loc) + nrow(notused2) == after

#pull out the data that has blocks report
BlkFilter <- PCD_loc  %>% 
  filter(!is.na(Bk1Bx1a) | !is.na(Bk1Bx1b) | !is.na(Bk1Bx1c) | !is.na(Bk2Bx2a) | !is.na(Bk2Bx2b)  | !is.na(Bk2Bx2c)) 

#pull out the data has has lat longs reported INSTEAD of blocks
LatFilter  <- PCD_loc %>%  filter(is.na(Bk1Bx1a) & is.na(Bk1Bx1b) & is.na(Bk1Bx1c) & is.na(Bk2Bx2a)  & is.na(Bk2Bx2b) & is.na(Bk2Bx2c) & !is.na(ddlat)) %>%
  mutate(ddlong = as.numeric(ddlong)*-1) %>%
  mutate(ddlat = as.numeric(ddlat))

# qa the lat long filter, there is alot of bad coordinates
notused3 = LatFilter %>%
  filter(ddlat > 50 | ddlong < -150  | is.na(ddlong) ) %>%
  mutate(Reason = "Bad COORDS REPORTED")
write.csv(notused3, 'Outputs/PC/PCD/NotUsed/i8/notused3.csv', row.names = F, na = "")
byyear = notused3 %>% group_by(year) %>% count()

LatFilter = LatFilter %>%
  filter(ddlat < 50 & ddlong > -150)


# convert data with coordinates reported to spatial dataframe, plotted by the coordinates
coords.SP <- st_as_sf(LatFilter, coords = c("ddlong", "ddlat"), crs = 4326, remove = FALSE)

# bring in the block shapefile from lookup folder
blocks.SP <- st_read(here("Lookups", "MAN_CA_CRFS_microblocks2013.shp"))
# change projection of blocks to WGS84 (what the coordinates are)
blocks.SP = st_transform(blocks.SP, crs = st_crs(coords.SP)) 
blocks.SP = select(blocks.SP, NM_INDEX)

# block feature has some overlap so need to remove sphereical geometry requirements (arc does this on the fly when loaded into PRO)
sf_use_s2(FALSE)

# spatial join to get corresponding block data
coords_wblocks.SP = st_join(coords.SP, blocks.SP, left = TRUE)

# convert back to dataframe
coords_wblocks = coords_wblocks.SP %>%
  st_drop_geometry() %>%
  mutate(Bk1Bx1a = NM_INDEX) %>%
  select(colnames(BlkFilter))

# remove any data that did not get blocks after join
notused4 = filter(coords_wblocks, is.na(Bk1Bx1a)) %>%
  mutate(Reason = "No fishing block overlapped with coordinate.")
write.csv(notused4, 'Outputs/PC/PCD/NotUsed/i8/notused4.csv', row.names = F, na = "")

#only data that had a block joined
coords_wblocks = filter(coords_wblocks, !is.na(Bk1Bx1a))

# recombine the data that had a block reported to the data that had coordinates (and now a joined block)
all_locations = rbind(BlkFilter, coords_wblocks)

# sum up the total blocks reported
all_locations <- cbind(PCD_loc, total_blocks = apply(PCD_loc[,Bk1Bx1a:Bk2Bx2a], 1, function(x)length(unique(x[!is.na(x)]))))


# final check for duplicates that will lead to double counting, these are caused by IDs reported twice. These are an error and should be removed
duplicates = all_locations %>% #NEED TO REVIEW
  group_by(ID_CODE) %>%
  count() %>%
  filter(n > 1) %>%
  left_join(all_locations)

notused5 = all_locations %>%
  filter((ID_CODE %in% duplicates$ID_CODE)) %>%
  mutate(Reason = 'Duplicate IDs caused by Assignment IDs')
write.csv(notused3, 'Outputs/PC/PCD/NotUsed/i8/notused5.csv', row.names = F, na = "")

all_PCD_locations = all_locations %>%
  filter(!(ID_CODE %in% duplicates$ID_CODE)) %>%
  mutate(ID_CODE = paste0('ID', ID_CODE)) %>% # fix for annoying loss of precision in csvs, force ids to be strings
  select(-ddlat, -ddlong, -HGSIZE, -hgsize2)
  
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason), unique(notused5$Reason)),
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4), nrow(notused5)))
names(notused_summary) = c("Reason", "Count")

write.csv(all_PCD_locations, "Outputs/PC/PCD/PCD_location_cleaned.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/PC/PCD/PCD_location_notusedsummary.csv", na = "", row.names = F)

