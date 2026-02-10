# Script to clean up and compile the i8c (catch data) for the PCO mode
# Michael Patton (michael.patton@wildlife.ca.gov)

#USER INPUT:
# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)

#load in required packages, if R says the package is not installed then run the following code in the console below install.packages("packagename") so for example install.packages("data.table") #install.packages('tidyverse', repos='http://cran.rstudio.com'), need to be on VPN
library(data.table)
library(stringi)
library(tidyverse)
library(lubridate)
library(here)
library(units)
library(readxl)
options(scipen = 999)


# reads in the .csv exported from the locations script. This replaced the previous sourcing logic for speed. IF CHANGES HAVE BEEN MADE TO THE PCO_LOCATION.R SCRIPT THEN THAT SCRIPT NEEDS TO BE RERAN TO HAVE THE MOST UP TO DATE DATA
PCO_locations = read.csv("Outputs/PC/PCO/PCO_locations.csv", na.strings = "") %>%
  mutate(ID = paste0(assn, locnum))

#Create species table by extracting data from SpeciesList.csv
Sp<- fread(here("Lookups", "SpeciesList05102023.csv" )) 
Sp<- Sp %>% 
  filter(OBJECTID != 428) %>% # fix duplication of SQUID in source
  select(ALPHA5, PSMFC_Code, Common_Name, TripType_Description) %>% 
  filter(Common_Name != "bivalve class") %>% #SP_CODE assocated with bivalve class is duplicated in lookup table. Should fix in source table eventually. 
  mutate(PSMFC_Code = as.character(PSMFC_Code))
# read in i8c data --------------------------------------------------------
#i8c: Stop catch data can be directly linked to each i8b stop location. 
# data is split between 2012-2022 and older data. 
i8c =fread(file = here('RCode', 'PC', 'Dat12toPresent', 'Data', 'i8c_data_12to24.csv'), fill=TRUE) %>%
  mutate_all(as.character) %>%
  mutate(year = substr(assn, 6,9) )

# bring in old data, its split between north and south
i8c_old = rbind(read_excel(here('RCode', 'PC', 'Dat04to16', 'PC_i8c_1999-2014_NorCal_119176r.xlsx'), guess_max = 1000000),
                read_excel(here('RCode', 'PC', 'Dat04to16', 'PC_i8c_1999-2014_SoCal_327163r.xlsx'), guess_max = 1000000)) %>%
  mutate(year = substring(ASSN, 6, 9)) %>%
  filter(year >= 2004 & year < 2012)

# old data has periods instead of NA
i8c_old[i8c_old=="."]<-NA

# change names for agreement so the datasets can be joined together
names(i8c_old) <- tolower(names(i8c_old))

# clean up columns so they can bind
# based on the 2004 sampler manual retd are counts of all released fish, and they didn’t split up released alive and released dead that year. We dont parse between retd alive/dead so just evenly split retd between the two
i8c_old = i8c_old %>%
  rename(`Ref #` = ref_num,
         ) %>%
  mutate(ASSNID = NA,
         Rels_DD = NA) %>%
  mutate(retd = as.numeric(retd)) %>%
  mutate(retdaliv = ifelse(!is.na(retd), retd/2, retdaliv),
         retddead =ifelse(!is.na(retd), retd/2, retddead))


i8c_old = i8c_old %>%
  select(-assnid_c, -counter, -drop, -month, -retd)
  
# bind rows together
i8c = bind_rows(i8c, i8c_old)

# create unique ID
i8c = i8c %>%
  mutate(ID = paste0(assn, locnum))

# 2004 data only provides an SP code and not an alpha code. Uses species lookup to assign alpha for these data
sp_2004 = Sp %>%
  select(PSMFC_Code, ALPHA5) %>%
  filter(!is.na(PSMFC_Code))

i8c = i8c %>%
  left_join(sp_2004, by = c('sp_code' = 'PSMFC_Code')) %>%
  mutate(alpha5 = ifelse(is.na(alpha5) & year == 2004, ALPHA5, alpha5)) %>%
  select(-ALPHA5)



# extract any catch data that uses a SP code that is not in the lookup table
notused <- i8c %>%
  anti_join(Sp, by = c("alpha5" = "ALPHA5")) %>%
  mutate(Reason = "Species not found in Lookup.")
unique(notused$SP_CODE)
byyear = notused %>% group_by(year) %>% count() # 2004 doesnt have alpha5
write.csv(notused, 'Outputs/PC/PCO/NotUsed/i8c/notused.csv', row.names = F, na = "")



#join together catch and species
i8c_species <- i8c %>%
  inner_join(Sp, by = c("alpha5" = "ALPHA5")) 

# clean up catch columns to numeric and 0s
i8c_species = i8c_species %>%
  mutate(kept = ifelse(!is.na(kept), as.numeric(kept), 0),
         retdaliv = ifelse(!is.na(retdaliv), as.numeric(retdaliv), 0),
         retddead = ifelse(!is.na(retddead), as.numeric(retddead), 0),
         Rels_DD = ifelse(!is.na(Rels_DD), as.numeric(Rels_DD), 0))

# 999 in kept, retdaliv, and retddead should not be used. This data seems to be automatically added by the CRFS Data Portal when the sampler observes 0 anglers during a stop (this usually occurs near the end of the trip when they are attempting to measure bags of fish and cannot observe anglers while fishing). 
notused3 = i8c_species %>%
  filter(kept == 999 | retdaliv == 999 | retddead == 999) %>%
  mutate(Reason = '999 used in the kept and released fields')
byyear = notused3 %>% group_by(year) %>% count()
write.csv(notused3,  'Outputs/PC/PCO/NotUsed/i8c/notused3.csv', row.names = F, na = "")

# this na logic is needed so you keep the location data that has no catch, where NAs in the fields due to the lack of a join
i8c_species = i8c_species %>%
  filter((kept != 999 | is.na(kept)) & (retdaliv != 999 | is.na(retdaliv)) & (retddead != 999| is.na(retddead)))

# join in location data, needs to be a right join so you get any trips that didnt catch fish. Will 
i8c_species_loc = i8c_species %>%
  select(-year) %>%
  right_join(PCO_locations, by = 'ID', suffix=c('_catch', '_effort'), relationship = "many-to-many")



# toss data that could not be joined
# reviewing this data, is appears as if most is tossed because of 99999 used in the location data in i8b
notused2 = i8c_species %>% 
  anti_join(PCO_locations, by = 'ID') %>%
  mutate(Reason = 'Location data not found')
byyear = notused2 %>% group_by(year) %>% count()
write.csv(notused2,  'Outputs/PC/PCO/NotUsed/i8c/notused2.csv', row.names = F, na = "")

# after the join, each block that the drift crossed over has its own row in the data. To get the catch specific to that block, the weighting needs to be applied
i8c_species_loc = i8c_species_loc %>%
  mutate(kept_weighted = kept*weighting,
         returned_alive_weighted = retdaliv*weighting,
         returned_dead_weighted = retddead*weighting)




# There were various reasons for the duplication: 1. duplicates were valid due to the sampler recording counts on separate rows of the datasheets. 2. duplicates were invalid due to data entry/recording errors of species codes. 3. Duplicates were invalid due to a data portal glitch occurring in 2013. 
# removing all the data from these specific stops, since it could affect CPUE calculations when a portion of the catch at a stop is tossed
dup_check = i8c_species_loc %>%
  group_by(ID, Common_Name_catch, Block) %>%
  count() %>%
  filter(n > 1)

notused4 = i8c_species_loc %>%
  inner_join(dup_check, by = c("ID"))%>%
  mutate(Reason = 'Duplicates in ID-species in i8c')
byyear = notused4 %>% group_by(year) %>% count()
write.csv(notused4,  'Outputs/PC/PCO/NotUsed/i8c/notused4.csv', row.names = F, na = "")

# clean up dataset and remove duplicates
i8c_species_loc = i8c_species_loc %>%
  anti_join(dup_check, by = c("ID"))%>%
  select(ID, year, month,  Block, Common_Name = Common_Name_catch, TripType_Description_effort, anglers, obsang, ang_norm, ftype, weighting, spatial, kept_weighted, returned_alive_weighted, returned_dead_weighted) %>%
  arrange(ID) %>%
  mutate(ID = paste0('ID', ID))

i8c_species_loc = i8c_species_loc %>%
  mutate(district = substr(ID, 5,5))

write.csv(i8c_species_loc, "Outputs/PC/PCO/PCO_wblocks.csv", na = "", row.names = F)


# not used summary
notused_summary = data.frame(c(unique(notused$Reason), unique(notused2$Reason), unique(notused3$Reason), unique(notused4$Reason)), 
                             c(nrow(notused), nrow(notused2), nrow(notused3), nrow(notused4))
)
names(notused_summary) = c("Reason", "Count")
write.csv(notused_summary, "Outputs/PC/PCO/PCO_catch_notusedsummary.csv", na = "", row.names = F)











  
  
  





