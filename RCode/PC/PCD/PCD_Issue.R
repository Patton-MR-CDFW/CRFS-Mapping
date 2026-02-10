library(tidyverse)
library(data.table)
library(here)


### RAW FILE

i3_raw <- read_excel("U:\\Shared\\Common\\CRFS\\Estimates and Data (by year)\\2023 Estimates and Data\\2023 Estimates and Data Files Full Year\\PC_i3_2023_(5_16_2024).xlsx") %>%
  mutate_all(as.character)

PCD = filter(i3_raw)

i8_raw <- read_excel("U:\\Shared\\Common\\CRFS\\Estimates and Data (by year)\\2023 Estimates and Data\\2023 Estimates and Data Files Full Year\\PC_i8_2023_(5_16_2024).xlsx", guess_max = 10000000) %>%
  mutate_all(as.character) %>%
  rename(ID_CODE = id_code)

notinlocation = i3_raw %>% 
  filter(!(ID_CODE %in% i8_raw$ID_CODE))
nrow(notinlocation)/nrow(i3_raw) # 90%


# copy path to where you downloaded the shared CRFS_Mapping folder between the ()
working_directory = r"(C:\Users\MPatton\OneDrive - California Department of Fish and Wildlife\CRFS_Mapping)"
setwd(working_directory)


# aggregate data created from PC_ReadData_2012toPresent
# uses data downloaded from U:\Shared\Common\CRFS\Estimates and Data (by year)
i3 <- fread(file = here("RCode", "PC", "Dat12toPresent", "Data", "i3_data_12to23.csv"), fill = TRUE, na.string = c("",".")) %>%
  mutate_all(as.character) 

i8 = fread(file = here('RCode', 'PC', 'Dat12toPresent', 'Data', 'i8_data_12to23.csv'), fill=TRUE) %>%
  rename(ID_CODE = id_code) %>%
  mutate_all(as.character)

i3_PCD = i3 %>%
  filter(grepl('PCD', `Ref #`))

notinlocation = i3_PCD %>% 
  filter(!(ID_CODE %in% i8$ID_CODE))
nrow(notinlocation)/nrow(i3) # 90%

notinlocation2 = i3 %>% 
  filter(!(assnid %in% i8$assnid))
nrow(notinlocation2)/nrow(i3) # 72%

notinlocation3 = i3 %>% 
  filter(!(`Ref #` %in% i8$`Ref #`))
nrow(notinlocation3)/nrow(i3) # 90%

notincatch = i8 %>%
  filter(!(ID_CODE %in% i3$ID_CODE))
nrow(notincatch)/nrow(i8) # 50%

# Kristen suggested looking at i2
i2 <- fread(file = here("RCode", "PC", "Dat12toPresent", "Data", "i2_data_12to23.csv"), fill = TRUE, na.string = c("",".")) %>%
  mutate_all(as.character) 

i2_notinlocation = i2 %>% 
  filter(!(ID_CODE %in% i8$ID_CODE))
nrow(i2_notinlocation)/nrow(i2) # 98%






