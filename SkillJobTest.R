# Import Packages
library(readxl)
library(haven)

# set directory to GitHub repo
setwd("C:/Users/bjr21/Documents/GitHub/DunnJobSkillMatch")

# Import Data
EdAtt <- read_sas("IN_IL_EdAtt.sas7bdat")
Zone <- read_excel("Job_Zones.xlsx")
Empdata <- read_excel("state_M2017_dl.xlsx")

# Subset to IN and IL
Empdata <- subset(Empdata, ST == "IN" | ST == "IL")
