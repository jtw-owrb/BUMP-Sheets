#-----
library(readxl)
library(lubridate)
library(tidyverse)
library(dplyr)

#---- 
# Original import
# Import dowloaded dataset from AWQMS
# master <- read_excel("C:/Users/308331/OneDrive - State of Oklahoma/Data Management/R/R Shiny Development/Training Resources/BUMP Sheets/raw.xlsx", 
#                  col_types = c("text", "text", "text", 
#                                      "text", "text", "numeric", "numeric", 
#                                      "text", "text", "date", "skip", "skip", 
#                                      "skip", "skip", "skip", "skip", "skip", 
#                                      "skip", "skip", "skip", "skip", "skip", 
#                                      "skip", "skip", "skip", "skip", "skip", 
#                                      "skip", "text", "numeric", "text", 
#                                      "text", "text", "text", "text", "text", 
#                                      "text", "text", "skip", "skip", "skip", 
#                                      "skip", "skip", "text", "text", "numeric", 
#                                      "text", "skip", "skip", "skip", "text", 
#                                      "numeric", "numeric", "numeric", 
#                                      "numeric", "numeric", "numeric", 
#                                      "text", "text", "text", "numeric", 
#                                      "numeric"), skip = 1)
#-----
#RDS conversion
#saveRDS(master,"C:/Users/308331/OneDrive - State of Oklahoma/Data Management/R/R Shiny Development/Training Resources/BUMP Sheets/master.rds")

#-----
# Read RDS
master <- readRDS("C:/Users/308331/OneDrive - State of Oklahoma/Data Management/R/R Shiny Development/Training Resources/BUMP Sheets/master.rds")

#-----

# Convert non-detects to 1/2 the detection limit, and copy over detection limit maximums
master$`Result Value` <- as.numeric(master$`Result Value`)
master$`Detection Limit Value1` <- as.numeric(master$`Detection Limit Value1`)
master<- master %>%
  mutate(`Result Value` = case_when((is.na(`Result Value`) & `Detection Condition` == "Present Below Quantification Limit" ~ `Detection Limit Value1`/2),
                                     is.na(`Result Value`) & `Detection Condition` == "Present Above Quantification Limit" ~ `Detection Limit Value1`,
                            TRUE ~ `Result Value`))

#TDS has both calculated values and multiprobe values; remove multiprobe values
TDS_subset <- subset(master, master$`Characteristic Name` == "Total dissolved solids")
TDS_subset <- subset(TDS_subset, TDS_subset$`Result Value Type` == "Actual")
raw <- rbind(TDS_subset, subset(master, master$`Characteristic Name` != "Total dissolved solids"))


# Add site name as a new column, removing everything but the town name
# Rename columns to remove spaces, prepare for new dataset
raw$Site_Name <- gsub(".*\\, ","",raw$`Monitoring Location Name`)
raw$Date <- raw$`Activity Start Date`
raw$ID <- raw$`Monitoring Location ID`
raw$Latitude <- raw$`Monitoring Location Latitude`
raw$Longitude <- raw$`Monitoring Location Longitude`
raw$Constituent <- raw$`Characteristic Name`
raw$Value <- raw$`Result Value`
raw$Unit <- raw$`Result Unit`
raw$Detection <- raw$`Detection Condition`
raw$Detection_Value <- raw$`Detection Limit Value1`

# Reorder columns
raw_working <- raw[,c(37,38,39,40,41,42,43,44,45,46)]

# Fix Names
raw_working <- raw_working %>%
  mutate(Site_Name = case_when(
         Site_Name == "near Lovell" ~ "Lovell",
         Site_Name == "Lenepah" ~ "Lenapah",
         Site_Name == "near Watonga" ~ "Watonga",
         Site_Name == "near Alex" ~ "Alex",
         Site_Name == "near Short" ~ "Short",
         Site_Name == "near Ames" ~ "Ames",
         Site_Name == "near Holly Creek" ~ "Holly Creek",
         Site_Name == "near Mclure" ~ "McClure",
         Site_Name == "near Waynoka" ~ "Waynoka",
         Site_Name == "near Granite" ~ "Granite",
         Site_Name == "near Roosevelt" ~ "Roosevelt",
         Site_Name == "near Carl" ~ "Carl",
         Site_Name == "near Nicut" ~ "Nicut",
         Site_Name == "Little River near Tecumseh" ~ "Tecumseh",
         Site_Name == "Island Bayou near Albany" ~ "Albany",
         Site_Name == "Walnut Bayou near Burneyville" ~ "Burneyville",
         Site_Name == "Beaver Creek near Ryan" ~ "Ryan",
         Site_Name == "Red River Near Burkburnett" ~ "Burkburnett",
         Site_Name == "Deep Red Creek near Randlett" ~ "Randlett",
         Site_Name == "North Fork of The Red River near Tipton" ~ "Tipton",
         Site_Name == " near Tonkawa" ~ "Tonkawa",
         Site_Name == "near Avant" ~ "Avant",
         Site_Name == "Walnut Creek near Purcell" ~ "Walnut Creek", 
         TRUE ~ Site_Name
  )) %>%
# Rename constituents to add units
# raw_working <- raw_working %>%
  mutate(Constituent = case_when(Constituent== "pH" ~ "pH",
                                 Constituent== "Total Nitrogen, mixed forms" ~ "TN_mgL",
                                 Constituent== "Chloride" ~ "Cl_mgL",
                                 Constituent== "Total hardness" ~ "Hardness_mgL",
                                 Constituent== "Phosphorus" ~ "TP_mgL",
                                 Constituent== "Total dissolved solids" ~ "TDS_mgL",
                                 Constituent== "Sulfate" ~ "SO4_mgL",
                                 Constituent== "Turbidity" ~ "Turbidity_NTU",
                                 Constituent== "Dissolved oxygen (DO)" ~ "DO_mgL",
                                 Constituent== "Specific conductance" ~ "SpC_uScm",
                                 Constituent== "Temperature, water" ~ "Temp_C",
                                 Constituent== "Chlorophyll a, corrected for pheophytin" ~ "Chlorophyll_mgm3",
                                 Constituent== "Inorganic nitrogen (nitrate and nitrite)" ~ "InorganicN",
                                 TRUE ~ Constituent
)) %>%
# Select columns of interest
# raw_working <- raw_working %>%
  select(Site_Name, Date, Constituent, Value) %>%
# There are a few issues with turbidity; let's force those to a number that makes sense (either 1 or 1000, nothing outside)
# raw_working <- raw_working %>%
  mutate(Value = case_when(Value < 1 & Constituent == "Turbidity_NTU" ~ 1,
                         Value > 1000 & Constituent == "Turbidity_NTU" ~ 1000,
                         TRUE ~ Value)) %>%
  
# Group data for summarise function
# raw_working <- raw_working %>%
  group_by(Site_Name, Date, Constituent, Value) %>%
  summarise(Value = mean(Value, na.rm = TRUE))


# Check for which rows are equal for site, date, and constituent so we don't have errors in the wide set
raw_working$fixer <- (duplicated(raw_working[,1:3]))
raw_working <- subset(raw_working, fixer != TRUE)

raw_wide <- raw_working %>%
  pivot_wider(names_from = "Constituent", values_from = "Value")

# Add NO2 and NO3 together
raw_wide$NO2NO3_mgL <- rowSums(raw_wide[ ,c("InorganicN", "Nitrate", "Nitrite")], na.rm = TRUE)

#Convert 0s to NAs in NO2NO3
raw_wide$NO2NO3_mgL[raw_wide$NO2NO3_mgL == 0] <- NA

# Drop needless columns before the split
raw_wide <- raw_wide %>%
  select(Site_Name, Date, Temp_C, 
         Turbidity_NTU, pH, 
         DO_mgL, Hardness_mgL, TDS_mgL,
         SpC_uScm, Cl_mgL, SO4_mgL, TP_mgL, 
         TN_mgL, NO2NO3_mgL, Chlorophyll_mgm3)

saveRDS(raw_wide, "C:/Users/308331/OneDrive - State of Oklahoma/Data Management/R/R Shiny Development/Training Resources/BUMP Sheets/BUMPPOR.rds")
