# PCD Observed Catch ---------------------------------------------

# Michael Patton's (michael.patton@wildlife.ca.gov) R script to clean and aggregate the CRFS observed catch data (i3 table)

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
options(scipen = 999)

# reads in the .csv exported from the locations script.
all_locations = read.csv("Outputs/PC/PCD/PCD_location_cleaned.csv", na.strings = "") %>%
  mutate_all(as.character) %>%
  mutate(total_blocks = as.numeric(total_blocks))

#  read in i3 table: sampler observed catch data. the here function provides the relative path to where the data is saved. 
oc <- fread(file = here("RCode", "PC", "Dat12toPresent", "Data", "i3_data_12to24.csv"), fill = TRUE, na.string = c("",".")) %>%
  mutate_all(as.character) 

oc_old = fread(here('RCode', 'PC', 'Dat04to16', 'PC_i3_2004-2015_616959r.csv'), fill=TRUE,na.string = c("",".")) 

# see differences between old and new data in data schema. No changes needed
setdiff(names(oc_old), names(oc))
setdiff(names(oc), names(oc_old))

oc_old = oc_old %>%
  rename(assnid = ASSNID, 
         `Ref #` = REF_NUM) %>%
  select(any_of(names(oc))) %>%
  mutate_all(as.character)

# bind together old and new data
oc = oc %>%
  bind_rows(oc_old) %>%
  rename(year = YEAR)


# remove any data that does not have a valid SP_CODE, these "notused" variables are later summarized in a separate output
notused = oc %>%
  filter(is.na(as.numeric(SP_CODE)))  %>%
  mutate(Reason = "SP_CODE is not valid.")
unique(notused$SP_CODE)
byyear = notused %>% group_by(year) %>% count()
write.csv(notused, 'Outputs/PC/PCD/NotUsed/i3/notused.csv', row.names = F, na = "")

#  Extract the year, month and date. Select only the required columns. The NAs introduced by coercion warning message is for the SP Codes that are included in the not used object below.
oc <- oc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE))

#Create species table by extracting data from SpeciesList.csv
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") #SP_CODE assocated with bivalve class is duplicated in lookup table. Should fix in source table eventually. 

#join together catch and species
oc_species <- oc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code"))  %>%
  mutate(ID_CODE = paste0('ID', ID_CODE))

# this will later be used to test for duplication from a bad join
test_id = oc_species %>%
  group_by(ID_CODE) %>%
  count()

library(ggplot2)
test = oc_species %>%
  mutate(type = ifelse(grepl('PCD', `Ref #`), 'PCD', NA)) %>%
  mutate(type = ifelse(grepl('PCO', `Ref #`), 'PCO', type)) %>%
  filter(!is.na(type)) %>%
  filter(TripType_Description != '') %>%
  select(district, type, TripType_Description, `Ref #`) %>%
  unique() %>%
  group_by(district, type,TripType_Description) %>%
  count() %>%
  mutate(district = paste0('District ', district )) %>%
  rename(`Trip Type` = TripType_Description)

ggplot(test, aes(fill=`Trip Type`, y=n, x=type)) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(~ district) +
  theme_bw() +
  ylab('Number of IDs') +
  xlab('Onboard vs Dockside')


# remove PCO data, there is still PCO data present for the older years where PCO is not used. 
# The ref # lists the mode. i2 and i3 will have both PCO and PCD, whereas i8 will only have PCD. 
# drop catch data if the i2/i3 ID_CODE does not have a match on the i8 file. 
oc_species = oc_species %>%
  filter(!grepl('PCO', `Ref #`))



# Merge together catch and location data ----------------------------------
# joining variable is the id, select required columns
oc_species_loc <- oc_species %>%
  inner_join(all_locations, by = c("ID_CODE")) %>% 
  select(ID_CODE, year = year.x, SP_CODE, Common_Name, TripType_Description, DISP3, WGT, FSHINSP, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, total_blocks)

# this will be very high because we are removing PCO data
notused2 = oc_species %>%
  anti_join(all_locations, by = c("ID_CODE")) %>%
  mutate(Reason = 'PCO data or location data not found for PCD data.')
byyear = notused2 %>% group_by(year) %>% count()
nrow(notused2)/nrow(oc_species)
write.csv(notused, 'Outputs/PC/PCD/NotUsed/i3/notused2.csv', row.names = F, na = "")


# clean up fish so NAs are 0s and WGT values in weight are NAs (inspected but not weighed)
oc_joined <- oc_species_loc %>% 
  mutate(FSHINSP = ifelse(!is.na(FSHINSP), as.numeric(FSHINSP), 0), 
         WGT = as.numeric(ifelse(WGT == "WGT", NA, WGT)))

# normalize the fish count to the number of blocks visited
oc_joined <- oc_joined %>% 
  mutate(FishPerBlock = FSHINSP/total_blocks)

# Create tally for groups of identical records for id_n, SP_CODE. Data has a unique row for each weight data entered. Need to account for this in total fish count. 
oc_joined  <- oc_joined %>% mutate(unique_id = paste(ID_CODE, SP_CODE, sep="_"))
fishinspected_count = oc_joined %>% group_by(unique_id) %>% summarise(Ob_Weighed_Fish = n())

# join the freq_id counter to table so that total fish can be divided
oc_joined  <- oc_joined %>%
  inner_join(fishinspected_count, by= "unique_id")

# normalize the fish per block to remove the repeated data due to reporting each fish weight
oc_normalized <- oc_joined %>% 
  mutate(FishPerBlock = ifelse(!is.na(FishPerBlock), FishPerBlock/Ob_Weighed_Fish, 0))

# Leader follower PR2 angler form fix -------------------------------------
# read in i1 table to identify the leaders for pre2012 PC data
i1 = fread(file=here("RCode", "PC", "Dat04to16", "PC_i1_2004-2015_177627r.csv"), fill = T, na.string = c("","."))%>%
  filter(YEAR <= 2012 & is.na(survey)) %>%
  mutate_all(as.character) %>%
  select(ID_CODE, LEADER, PRT_CODE) %>%
  mutate(LEADER_ID = ifelse(!is.na(LEADER), LEADER, PRT_CODE)) %>%
  mutate(LEADER_ID = ifelse(is.na(LEADER_ID), ID_CODE, LEADER_ID)) %>%
  select(ID_CODE, LEADER_ID) %>%
  mutate(ID_CODE = paste0('ID', ID_CODE),
         LEADER_ID = paste0('ID', LEADER_ID))

oc_normalized = oc_normalized %>%
  left_join(i1, by = 'ID_CODE') %>%
  mutate(ID_CODE = ifelse(!is.na(LEADER_ID), LEADER_ID, ID_CODE)) # no location with PCD data


oc_normalized = oc_normalized %>%
  mutate(row = row_number()) # create index of rows to not get rid of identical weighed fish in the unique logic used below

# pivot the data so each block reported for a single id has its own row. Counts are already normalized by blocks visited so this will not double count anything but greatly simplifies the logic that WINN uses to generate summary statistics
by_block = oc_normalized %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block)) %>%
  select(-col) %>%
  unique() # there are situations where the same block is entered mutlple times for the same id in the data. This is accounted for in the total blocks calculation but only a single instance of this block needs to be aggregated to avoid double counting. 

# aggregates to the id-block-species level the total number of fish caught and the average weight, this output is later used in another script to calculate different metrics but I thought there would be some utility in keeping things at the ID level (easily aggregate to a variety of temporal or sample level metrics)
oc_by_id_agg = by_block %>%
  group_by(ID_CODE, year, Block, Common_Name) %>%
  summarise(Total_Obs_Fish_Caught  = sum(FishPerBlock, na.rm = T), 
            Ob_AvKWgt = mean(WGT, na.rm=TRUE),
            Ob_Weighed_Fish = sum(!is.na(WGT)))


# data validation to identify outlier data
outliers = oc_by_id_agg %>%
  mutate(Total_Obs_Fish_Caught = as.numeric(Total_Obs_Fish_Caught),
         Ob_AvKWgt = as.numeric(Ob_AvKWgt)) %>%
  group_by(Common_Name) %>%
  mutate(z_score_caught = scale(Total_Obs_Fish_Caught),
         z_score_weight = scale(Ob_AvKWgt))


weight_outliers = filter(outliers, z_score_weight > 5) %>%
  select(-z_score_caught) %>%
  arrange(desc(z_score_weight))
write.csv(weight_outliers, 'Outputs/PC/PCD/PotentialOutliers/OC_weight.csv', na = "")

# final check for duplicates
final_check = oc_by_id_agg %>%
  group_by(ID_CODE, Block, Common_Name) %>% 
  count() %>%
  filter(n > 1)

# export not used data
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason)), 
                             c(nrow(notused), nrow(notused2)))
names(notused_summary) = c("Reason", "Count")

write.csv(oc_by_id_agg, "Outputs/PC/PCD/PCD_ObservedCatch.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/PC/PCD/PCD_ObservedCatch_notusedsummary.csv", na = "", row.names = F)

gc()







