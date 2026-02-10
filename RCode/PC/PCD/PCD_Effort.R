# PCD Effort  ---------------------------------------------

# Michael Patton (michael.patton@wildlife.ca.gov)
# script that processes the effort i1 table to provide a cleaned up and aggregated effort for each ID for PCD data

#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below install.packages("packagename") so for example install.packages("data.table")
library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
options(scipen = 999)

# reads in the .csv exported from the locations script.
all_locations_effort = read.csv("Outputs/PC/PCD/PCD_location_cleaned.csv", na.strings = "") %>%
  mutate_all(as.character) %>%
  mutate(total_blocks = as.numeric(total_blocks)) %>%
  select(-year)



#read in the i1 table for 2012 to present
oe = fread(file=here("RCode", "PC", "Dat12toPresent", "Data", "i1_data_12to24.csv"), fill = T, na.string = c("",".") )  %>%
  mutate_all(as.character)

# read in old i1
oe_old = fread(file=here("RCode", "PC", "Dat04to16", "PC_i1_2004-2015_177627r.csv"), fill = T, na.string = c("",".")) %>% 
  filter(!(YEAR %in% c('2012', '2013', '2014', '2015'))) %>%
  mutate_all(as.character)

oe_old = oe_old %>%
  rename(daysf = DAYSF,
         geara = GEARA,
         gearB = GEARB,
         port = PORT,
         assnid = ASSNID,
         island = ISLAND,
         mode_f = MODE_F,
         DD = dd
         )

setdiff(names(oe), names(oe_old))
setdiff(names(oe_old), names(oe))


oe = oe %>%
  bind_rows(oe_old)

#remove all columns that are only NAs
not_all_na <- function(x) any(!is.na(x))
oe = oe %>% select(where(not_all_na))


### Read in Species Lookup table
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  select(PSMFC_Code, Common_Name, TripType_Description) %>% 
  mutate(PSMFC_Code = as.character(PSMFC_Code)) %>%
  filter(Common_Name != "bivalve class") %>%
  filter(!is.na(PSMFC_Code)) %>%
  mutate(PSMFC_Code = as.character(PSMFC_Code))


# add in prim1 vs prim2 logic where prim1 is only used unless prim1 == 'Invertebrates"
#Selects only relevant columns 
oe <- oe %>%
  select(ID_CODE, year=YEAR, prim1, prim2, CNTRBTRS, daysf, LEADER, PRT_CODE, FIRST, survey, `Ref #`, district) %>%
  left_join(Sp %>% select(PSMFC_Code, TripType_Description), by = c("prim1" = "PSMFC_Code")) %>%
  mutate(primary = ifelse(TripType_Description == "Invertebrates" & !is.na(TripType_Description) & !is.na(prim2), prim2, prim1)) %>%
  select(-TripType_Description)

# may want to convert these to anythings in the future so the data is still used
na_triptypes = oe %>%
  filter(is.na(primary))

#clean up DAYSF and CNTRBTRS fields so NAs are 0, if days are reported as 0 in the data but CNTRBTRS were reported, change the days fished to 1
oe <- oe %>% 
  mutate(daysf = as.numeric(ifelse(is.na(daysf), 0, daysf)), 
         CNTRBTRS = as.numeric(ifelse(is.na(CNTRBTRS), 0 , CNTRBTRS))) %>%
  mutate(daysf = ifelse(daysf == 0 & CNTRBTRS > 0, 1, daysf))


# Pre-2012 Leader Follower fix for Angler Form Data -----------------------
# filter out angler form data (pre 2012 for PC data)
anglerform = oe %>%
  filter(year <= 2012 & is.na(survey))

# keep non-angler form data so fixed angler form data can be added back in
oe_noangler = oe %>%
  anti_join(anglerform)

# filter out and flag LEADER data
leader = anglerform %>%
  filter(is.na(LEADER) & is.na(PRT_CODE) & FIRST == 1) %>% 
  mutate(LEADER_ID = ID_CODE)

# export the data that is not being used due to issues with the angler form data
notused = anglerform %>%
  filter(is.na(LEADER) & is.na(PRT_CODE) & (FIRST != 1 | is.na(FIRST))) %>%
  mutate(Reason = 'Angler form data that could not be flagged as leader or follower')
write.csv(notused, 'Outputs/PC/PCD/NotUsed/i1/notused.csv', row.names = F, na = "")

# filter out and flag FOLLOWER data
follower = anglerform %>%
  filter((!is.na(LEADER) | !is.na(PRT_CODE))) %>%
  mutate(LEADER_ID = ifelse(!is.na(LEADER), LEADER, PRT_CODE))

# QA check for Angler form data that is not flagged as leader or follower
missing = anglerform %>%
  anti_join(leader) %>%
  anti_join(follower) %>%
  anti_join(notused)

# in the follower data, sum up the number of contributors for each LEADER_ID
follower_sum = follower %>%
  group_by(LEADER_ID)%>%
  summarise(CNTRBTRS = sum(CNTRBTRS, na.rm = T))

# join together the leader and summed follower data, and sum the total number of contributors
pr2_total <- left_join(leader, follower_sum, by = c('LEADER_ID')) %>%
  rowwise() %>%
  mutate(CNTRBTRS = sum(CNTRBTRS.x, CNTRBTRS.y, na.rm = T)) %>%
  select(-CNTRBTRS.x, -CNTRBTRS.y, -LEADER_ID)

# join the fixed angler form data back with the i1 dataset
oe_anglerform_fixed = rbind(oe_noangler, pr2_total)

# pull out data that is not used because it uses a bad sp_code
notused2 <- oe_anglerform_fixed %>%
  anti_join(Sp, by = c("primary" = "PSMFC_Code")) %>%
  mutate(Reason = "Primary Species not found in species lookup.")
unique(notused2$primary) # looks like a lot of sp codes re reported as the alpha code need to clean this up
# NFOTH and others are used to indicate a non fishing vessel so are rightfully excluded
byyear = notused2 %>% group_by(year, primary) %>% count()
write.csv(notused2, 'Outputs/PC/PCD/NotUsed/i1/notused2.csv', row.names = F, na = "")


# join species lookup to data
oe_species <- oe_anglerform_fixed %>%
  inner_join(Sp, by = c("primary" = "PSMFC_Code"))


library(ggplot2)
test = oe_species %>%
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

#summary of the different trip type descriptions found in the data
type_summary = oe_species %>%
  group_by(TripType_Description) %>%
  count() %>%
  arrange(desc(n)) # some invertebrates still make it through

# check to make sure no duplicates were created or rows were lost
nrow(oe_anglerform_fixed) == (nrow(oe_species) + nrow(notused2))

# remove PCO data, there is still PCO data present for the older years where PCO is not used. 
# The ref # lists the mode. i2 and i3 will have both PCO and PCD, whereas i8 will only have PCD. 
# drop catch data if the i2/i3 ID_CODE does not have a match on the i8 file. 
oe_species = oe_species %>%
  filter(!grepl('PCO', `Ref #`))

# Merge together effort and location data ----------------------------------
# paste 'ID' to the ID column to force from integer to string in the script and during exports. Makes life easier when opening in excel since excel loses resolution of large numbers
# join location data by ID
oe_species_loc <- oe_species  %>%
  mutate(ID_CODE = paste0('ID',ID_CODE)) %>%
  inner_join(all_locations_effort, by = "ID_CODE") 

# export data that does not have corresponding locaiton data
notused3 <- oe_species  %>%
  mutate(ID_CODE = paste0('ID', ID_CODE)) %>%
  anti_join(all_locations_effort, by = "ID_CODE") %>%
  mutate(Reason = "Does not have corresponding location data by ID or is older PCO")

byyear = notused3 %>% group_by(year) %>% count()
write.csv(notused3, 'Outputs/PR/NotUsed/i1/notused3.csv', row.names = F, na = "")


# Rename columns for clarity. There is no weighting of effort by block.
oe_species_loc <- oe_species_loc %>% 
  mutate(DaysPerBlock = daysf, 
         CntrbPerBlock = CNTRBTRS, 
         VesselPerBlock = 1)



# pivot the data so each block reported for a single id has its own row. 
by_block = oe_species_loc %>%
  pivot_longer(Bk1Bx1a:Bk2Bx2c, names_to = "col", values_to = 'Block') %>%
  filter(!is.na(Block)) %>%
  select(-col, -total_blocks) %>%
  unique() # remove situations where the same block is entered into different block columns

# check for duplicates created by bad data entry that will effect the summing below
# notes from FAP: these are caused by when a sampler completes two different PCD assignments in the same day and an associated issue with data entry. Will remove for now
duplicates = by_block %>%
  group_by(ID_CODE, Block, TripType_Description, year) %>%
  count() %>%
  filter(n > 1) %>%
  left_join(by_block)

# remove the duplicates identied above
by_block = by_block %>%
  anti_join(duplicates)

# aggregate effort to the id-block-trip type level.
oe_by_id_agg = by_block %>%
  group_by(ID_CODE, year, Block, Common_Name, TripType_Description) %>%
  summarise(Days = sum(DaysPerBlock, na.rm = T), 
            Cntrbs = sum(CntrbPerBlock, na.rm = T), 
            Vessels = sum(VesselPerBlock, na.rm = T)) %>%
  mutate(AnglerDays = Cntrbs*Days) %>%
  arrange(ID_CODE) %>%
  filter(!is.na(Block))

#check for no change
nrow(by_block) == nrow(oe_by_id_agg)

# QA check for duplicate combinations that will lead to double counting
finalcheck = oe_by_id_agg %>%
  group_by(ID_CODE, Block, TripType_Description, year) %>%
  count() %>%
  filter(n > 1)

# create summary of data that is not used and the provided reason
notused_summary = data.frame(c(unique(notused$Reason), "Primary Species not found in species lookup.", unique(notused3$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3)))
names(notused_summary) = c("Reason", "Count")


# export cleaned up effort data and the summary of not used data. 
write.csv(oe_by_id_agg, "Outputs/PC/PCD/PCD_effort.csv", na = "", row.names = F)
write.csv(notused_summary, "Outputs/PC/PCD/PCD_effort_notused_summary.csv", na = "", row.names = F)

gc()

