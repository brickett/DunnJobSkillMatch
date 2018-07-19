# Import Packages
library(readxl) #reads excel files
library(haven) #reads sas7bdat files
library(plyr)
library(dplyr)
library(reshape2)


# set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnJobSkillMatch")

# Import Data
EdAtt <- read_sas("IA_IL_EdAtt.sas7bdat")
Zone <- read_excel("Job_Zones.xlsx")
Empdata <- read_excel("ltprojections.xlsx")
LoD <- read_excel("state_M2017_dl.xlsx")

# Rename columns to remove spaces
colnames(Empdata) <- c("StateFIPS", "AreaName", "OccCode", "OccName", "BaseYear", "Base", "ProjYear", "Proj", "Change", "PctChange", "AvgAnnOpen")

# Subset to IN and IL
Empdata <- subset(Empdata, StateFIPS == "17" | StateFIPS == "19")

## Workforce Information
# Recode Education to Job Zone
EdAtt$JobZone <- EdAtt$EDUCD

EdAtt$JobZone[EdAtt$JobZone<=61] <- 1 #less than high school diploma
EdAtt$JobZone[EdAtt$JobZone == 63 | EdAtt$JobZone == 64] <- 2 #HS diploma (63) or GED (64)
EdAtt$JobZone[EdAtt$JobZone == 65] <- 2 #less than 1 year of college -> equivalent to just diploma
EdAtt$JobZone[EdAtt$JobZone == 71] <- 3 #more than 1 year of college -> estimate that they gained credential or equivalent skills to associate
EdAtt$JobZone[EdAtt$JobZone == 81] <- 3 #associate degree
EdAtt$JobZone[EdAtt$JobZone == 101] <- 4 #Bachelor degree
EdAtt$JobZone[EdAtt$JobZone == 114 | EdAtt$JobZone == 115 |EdAtt$JobZone == 116] <- 5 #Master (114), Professional (115), or Doctoral (116) degree

# Split by state
EdAtt_IL <- subset(EdAtt, STATEFIP == 17)
EdAtt_IA <- subset(EdAtt, STATEFIP == 19)

# Count with Person Weight
Zone_IL <- plyr::count(EdAtt_IL, 'JobZone', wt_var = 'PERWT')
Zone_IA <- plyr::count(EdAtt_IA, 'JobZone', wt_var = 'PERWT')

# Find the total number of workers
Tot_Zone_IL <- sum(Zone_IL$freq)
Tot_Zone_IA <- sum(Zone_IA$freq)

# Create a new column
Zone_IL$Perc <- Zone_IL$freq
Zone_IA$Perc <- Zone_IA$freq

# Convert frequency to a percentage
Zone_IL$Perc <- Zone_IL$Perc/Tot_Zone_IL*100
Zone_IA$Perc <- Zone_IA$Perc/Tot_Zone_IA*100

## Match Occupation Data with Job Zone
# Get unique set of codes and groups to take only the detailed codes
LoD <- LoD[c("OCC_CODE", "OCC_GROUP")] #select useful columns
colnames(LoD) <- c("OccCode", "OccGroup") #rename column for join
LoD_unique <- unique(LoD) #take only 1 copy of each job code

# Join datasets
Empdata_Codes <- merge(Empdata, LoD_unique, by="OccCode", all=TRUE) #outer join
Empdata_Codes <- subset(Empdata_Codes, OccGroup != "total") #remove the statewide totals
Empdata_Codes <- subset(Empdata_Codes, OccGroup != "major") #remove the major categories, leaving just detailed codes, which can be matched with job zones
Empdata_Codes <- subset(Empdata_Codes, AreaName != is.na(TRUE)) #remove those that have a code from the Level of Detail dataset, but no corresponding job in the state

# Standardize job zone codes up from sub-detail to detail level
# split apart the job code string
hold1 <- substr(Zone$`O*NET-SOC Code`, 1,2)
hold2 <- substr(Zone$`O*NET-SOC Code`, 4,10)

# rejoin the job code string without the dash
options(digits = 8)
Zone$NumCode <- as.double(paste(hold1,hold2, sep = ""))

# take the job code to its floor, reducing it to just the detail level
Zone$NumCode <- floor(Zone$NumCode)
colnames(Zone) <- c("OccCode","Title", "JobZone", "Date", "Source", "NumCode" )

# melt and cast to get average job code for those with multiple sub-details
Zone_melt <- melt(Zone, id = c( "NumCode", "OccCode", "Title", "Date", "Source"))
Zone_melt <- Zone_melt[c("NumCode", "variable", "value")]
Zone_cast <- dcast(Zone_melt, NumCode ~ ., mean)
colnames(Zone_cast) <- c("NumCode", "NewJobZone")
Zone_cast$NewJobZone <- round(Zone_cast$NewJobZone)

# convert NumCode back to OccCode
hold1 <- substr(Zone_cast$NumCode, 1,2)
hold2 <- substr(Zone_cast$NumCode, 3,6)
Zone_cast$OccCode <- paste(hold1, hold2, sep = "-")

# attach Job Code to employment data
Empdata_Codes <- merge(Empdata_Codes, Zone_cast, by = "OccCode")

# Subset
Empdata_Codes_IL <- subset(Empdata_Codes, StateFIPS == 17)
Empdata_Codes_IA <- subset(Empdata_Codes, StateFIPS == 19)

## Count Jobs for each level - current
Jobs_IL <- plyr::count(Empdata_Codes_IL, 'NewJobZone', wt_var = "Base")
Jobs_IA <- plyr::count(Empdata_Codes_IA, 'NewJobZone', wt_var = "Base")

# Find the total number of jobs
Tot_Jobs_IL <- sum(Jobs_IL$freq)
Tot_Jobs_IA <- sum(Jobs_IA$freq)

# Create a new column
Jobs_IL$Perc <- Jobs_IL$freq
Jobs_IA$Perc <- Jobs_IA$freq

# Convert frequency to a percentage
Jobs_IL$Perc <- Jobs_IL$Perc/Tot_Jobs_IL*100
Jobs_IA$Perc <- Jobs_IA$Perc/Tot_Jobs_IA*100

## Count Jobs for each level - future
Fut_Jobs_IL <- plyr::count(Empdata_Codes_IL, 'NewJobZone', wt_var = "Proj")
Fut_Jobs_IA <- plyr::count(Empdata_Codes_IA, 'NewJobZone', wt_var = "Proj")

# Find the total number of jobs
Tot_Fut_Jobs_IL <- sum(Fut_Jobs_IL$freq)
Tot_Fut_Jobs_IA <- sum(Fut_Jobs_IA$freq)

# Create a new column
Fut_Jobs_IL$Perc <- Fut_Jobs_IL$freq
Fut_Jobs_IA$Perc <- Fut_Jobs_IA$freq

# Convert frequency to a percentage
Fut_Jobs_IL$Perc <- Fut_Jobs_IL$Perc/Tot_Fut_Jobs_IL*100
Fut_Jobs_IA$Perc <- Fut_Jobs_IA$Perc/Tot_Fut_Jobs_IA*100

# export tables
write.csv(Jobs_IA, "Jobs_IA.csv")
write.csv(Zone_IA, "Educ_IA.csv")
