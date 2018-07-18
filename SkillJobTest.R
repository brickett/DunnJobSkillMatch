# Import Packages
library(readxl)
library(haven)
library(plyr)

# set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnJobSkillMatch")

# Import Data
EdAtt <- read_sas("IN_IL_EdAtt.sas7bdat")
Zone <- read_excel("Job_Zones.xlsx")
Empdata <- read_excel("state_M2017_dl.xlsx")

# Subset to IN and IL
Empdata <- subset(Empdata, ST == "IN" | ST == "IL")

## Workforce Information
# Recode Education to Job Zone
EdAtt$JobZone <- EdAtt$EDUCD

EdAtt$JobZone[EdAtt$JobZone<=61] <- 1
EdAtt$JobZone[EdAtt$JobZone == 63 | EdAtt$JobZone == 64] <- 2
EdAtt$JobZone[EdAtt$JobZone == 65] <- 2
EdAtt$JobZone[EdAtt$JobZone == 71] <- 3
EdAtt$JobZone[EdAtt$JobZone == 81] <- 3
EdAtt$JobZone[EdAtt$JobZone == 101] <- 4
EdAtt$JobZone[EdAtt$JobZone == 114 | EdAtt$JobZone == 115 |EdAtt$JobZone == 116] <- 5

# Split by state
EdAtt_IL <- subset(EdAtt, STATEFIP == 17)
EdAtt_IN <- subset(EdAtt, STATEFIP == 18)

# Count with Person Weight
Zone_IL <- count(EdAtt_IL, 'JobZone', wt_var = 'PERWT')
Zone_IN <- count(EdAtt_IN, 'JobZone', wt_var = 'PERWT')

Tot_Zone_IL <- sum(Zone_IL$freq)
Tot_Zone_IN <- sum(Zone_IN$freq)

Zone_IL$Perc <- Zone_IL$freq
Zone_IN$Perc <- Zone_IN$freq

Zone_IL$Perc <- Zone_IL$Perc/Tot_Zone_IL*100
Zone_IN$Perc <- Zone_IN$Perc/Tot_Zone_IN*100
