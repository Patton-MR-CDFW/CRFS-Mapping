# PCD Reported Catch ---------------------------------------------

# Michael Patton (michael.patton@wildlife.ca.gov)

# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below: install.packages("packagename") so for example install.packages("data.table")
library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
options(scipen = 999)

# reads in the .csv exported from the locations script.
all_locations = read.csv("Outputs/PC/PCD/PCD_location_cleaned.csv", na.strings = "") %>%
  mutate_all(as.character) %>%
  mutate(total_blocks = as.numeric(total_blocks))

# read in 2016-2022 data. New years of data can be added in the PC_readdata_16_present.R
rc = fread(here("RCode", "PC", "Dat12toPresent", "Data", "i2_data_12to24.csv"), fill = T, na.string = c("",".") )  %>%
  mutate_all(as.character) 

# read in the i2 table for reported catch for 2004-2015
rc_old <- fread(here('RCode', 'PC', 'Dat04to16', 'PC_i2_2004-2015_161897r.csv'), fill = TRUE,na.string = c("",".")) %>%
  mutate_all(as.character) %>%
  filter(!(YEAR %in% c('2012', '2013', '2014', '2015'))) # overlap in years covered. use new data

# see difference in column names between new and old data. No changes needed
setdiff(names(rc), names(rc_old))
#[1] "assnid" "Ref #" 
setdiff(names(rc_old), names(rc))

# combine all years of data
rc = rc %>%
  bind_rows(rc_old) %>%
  unique() # for whatever reason there are some duplicated rows

# Not used data will be combined as a separate output for review. Reasons are provided in new column. 
notused = rc %>%
  filter(is.na(as.numeric(SP_CODE)))  %>% # forces invalid SP CODES (not numbers) to be NAs and are therefore removed, responsible for the warning you recieve
  mutate(Reason = "SP_CODE is not valid.")
unique(notused$SP_CODE)
write.csv(notused, 'Outputs/PC/PCD/NotUsed/i2/notused.csv', row.names = F, na = "")

# remove invalid SP CODEs, create new id from ID_CODE and location number (locn). Select only needed columns. 
rc <- rc %>% 
  mutate(SP_CODE = as.numeric(SP_CODE)) %>%
  filter(!is.na(SP_CODE))

#Create species table by extracting data from SpeciesList.csv
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description, ALPHA5) %>% 
  mutate(PSMFC_Code = as.numeric(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") 

# join catch data with species
rc_species <- rc %>%
  inner_join(Sp, by = c("SP_CODE" = "PSMFC_Code"))

library(ggplot2)
test = rc_species %>%
  mutate(type = ifelse(grepl('PCD', `Ref #`), 'PCD', NA)) %>%
  mutate(type = ifelse(grepl('PCO', `Ref #`), 'PCO', type)) %>%
  group_by(district, type, TripType_Description) %>%
  summarise(n = n_distinct(`Ref #`)) %>%
  filter(!is.na(type)) %>%
  filter(TripType_Description != '') %>%
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
rc_species = rc_species %>%
  filter(!grepl('PCO', `Ref #`))

# Merge together catch and location data ----------------------------------
rc_species_loc <- rc_species  %>%
  mutate(ID_CODE = paste0('ID', ID_CODE)) %>%
  inner_join(all_locations, by = 'ID_CODE')  %>% 
  select(ID_CODE, locn, year, SP_CODE, Common_Name, TripType_Description, DISPO, NUM_FISH, Bk1Bx1a, Bk1Bx1b, Bk1Bx1c, Bk2Bx2a, Bk2Bx2b, Bk2Bx2c, total_blocks)

# export data that does not have corresponding location data. Will be alot since PCO data is included
notused2 <- rc_species %>%
  mutate(ID_CODE = paste0('ID', ID_CODE)) %>%
  anti_join(all_locations, by = c("ID_CODE")) %>%
  mutate(Reason = "Does not have corresponding location data by ID or is PCO data")
write.csv(notused2, 'Outputs/PC/PCD/NotUsed/i2/notused2.csv', row.names = F, na = "")

#normalize the reported fish caught by the number of blocks visited
rc_joined <- rc_species_loc %>% 
  mutate(FishPerBlock = ifelse(!is.na(NUM_FISH), as.numeric(NUM_FISH)/total_blocks, 0))

unique(rc_joined$DISPO)
# sort fish in Released or kept by DISPO code
rc_joined = rc_joined %>% 
  mutate(RepReleasedAlive  = ifelse(DISPO %in% c(1,2), FishPerBlock, 0), 
         RepReleasedDead = ifelse(DISPO %in% c(6), FishPerBlock, 0),
         RepKept = ifelse(DISPO %in% c(3,4,5,7), FishPerBlock, 0))

# small number of remaining duplicates from bad data entry
duplicates = rc_joined %>%
  group_by(ID_CODE, SP_CODE, DISPO) %>%
  count()

# join back to identify duplicates
rc_joined = rc_joined %>%
  left_join(duplicates)

# filter out data that had duplicate fishing counts, we may want to look into salvaging this data since contrbtrs is not important for catch
notused3 = rc_joined %>%
  filter(n > 1) %>%
  mutate(Reason = "Duplicate IDs caused by data entry")
byyear = notused3 %>% group_by(year) %>% count()
write.csv(notused3, 'Outputs/PC/PCD/NotUsed/i2/notused3.csv', row.names = F, na = "")

rc_joined = rc_joined %>%
  filter(n == 1) %>%
  mutate(row = row_number())

# pivot so each block-id has its own row. Makes summaries easier. Already normalized to fish per block so no double counting will occur
by_block = rc_joined %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block)) %>%
  select(-col) %>%
  unique() # there are situations where the same block is entered mutlple times for the same id in the data. This is accounted for in the total blocks calculation but only a single instance of this block needs to be aggregated to avoid double counting. 

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

by_block_wleader = by_block %>%
  left_join(i1, by = 'ID_CODE') %>%
  mutate(ID_CODE = ifelse(!is.na(LEADER_ID), LEADER_ID, ID_CODE))

# final aggregation by ID - block - species
rc_by_id_agg = by_block_wleader %>%
  group_by(ID_CODE, year, Block,  SP_CODE, Common_Name) %>%
  summarise(Rep_Released_Alive = sum(RepReleasedAlive, na.rm = T), 
            Rep_Released_Dead = sum(RepReleasedDead, na.rm = T), 
            Rep_Kept  = sum(RepKept, na.rm = T)) %>%
  mutate(Total_Rep_Fish_Caught = Rep_Released_Alive + Rep_Kept + Rep_Released_Dead)  %>%
  mutate(Total_Released_Fish_Caught = Rep_Released_Alive + Rep_Released_Dead)




# final check for duplicates
finalcheck = rc_by_id_agg %>%
  group_by(ID_CODE, Block, Common_Name) %>%
  count() %>%
  filter(n > 1)

# create summary of data that is not used and the provided reason, manually write in notused2 reason as a temp fix because it went down to 0 rows
notused_summary = data.frame(c('SP_CODE is not valid.', unique(notused2$Reason), unique(notused3$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3)))
names(notused_summary) = c("Reason", "Count")

write.csv(rc_by_id_agg, "Outputs/PC/PCD/PCD_ReportedCatch.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/PC/PCD/PCD_ReportedCatch_notused_summary.csv", na = "", row.names = F)


gc()
