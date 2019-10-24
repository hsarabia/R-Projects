#R-Script by Hiram Sarabia-Ramirez, M.S.
#Environmental Scientist
#San Diego Water Board
#October, 2019
#GITHUB version

#PROBLEM: State agency is evaluating existing water quality data to determine whether
#waterbodies are impaired, however, the tables of standards for maximum contaminant 
#levels used to identify exceedances have not been updated with the latest data 
#published online by USEPA. 
#Goal: use R to create a new table with a side-by-side comparison of existing standards for the 
#State agency and the latest guidelines from USEPA.
#Approach: Use rvest web scrape to table data from USEPA web site and dplyr to update tables 
#for three beneficial uses (i.e., MUN, COM, and SHELL)
#Required libraries:
#rvest
#dplyr
#Required Files:
#R9_Analytes_DRAFT_wOBJref_R1_R2.csv - Existing State of California MCL table

############################
### Environment Settings ###
############################
#Set working directory 
setwd('C:/R/IR')
#Turn off scientific notation to facilitate comparison
options(scipen = 999)
#Load required packages if not already installed
#if(!require(dplr)) install.packages("dplyr")
#if(!require(dplr)) install.packages("rvest")

##################
### Web Scrape ###
##################
#USEPA National Recommended Water Quality Criteria 
#Human Health Criteria for drinking water and organism
library("rvest")
url <- "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-human-health-criteria-table"
table_list <- url %>%
  read_html() %>%
  html_nodes(xpath='/html/body/section/div[2]/div[1]/div/div/table') %>%
  html_table() 
NRWQC_HHC <- table_list[[1]]

#########################################
#### MUN Beneficial Use Table Update ####
#########################################

#Load State agency analyte table from csv file
dfCA <- read.csv('R9_Analytes_DRAFT_wOBJref_R1_R2.csv', stringsAsFactors = FALSE)
dfCA.MU <- subset(dfCA, BUCODE == 'MU')
v <- c('mg/l','ug/l')
dfCA.MU <- dfCA.MU[dfCA.MU$H2O_Units %in% v,]
dfCA.MU <- dfCA.MU[,c(1,2,4,5,3,6,7,8,9,10,11)]

#Remove unnecessary columns
dfNRWQC_HHC_MU <- NRWQC_HHC[,-4]
#Add necessary columns and format to facilitate comparison
dfNRWQC_HHC_MU$W_and_O_Units <- "ug/l"
dfNRWQC_HHC_MU <- dfNRWQC_HHC_MU[,c(1,2,3,6,4,5)]
names(dfNRWQC_HHC_MU)[1] <- "Analyte"
#Clean data
#REFERENCE: https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
#Remove special characters, letters, and/or and spaces to synchronize join columns
dfCA.MU$Analyte <- gsub("[^[:alnum:]///' ]",'', dfCA.MU$Analyte)
dfCA.MU$Analyte <- gsub('\\s+', '', dfCA.MU$Analyte)
dfNRWQC_HHC_MU$Analyte <- gsub('\\s+', '', dfNRWQC_HHC_MU$Analyte)
dfNRWQC_HHC_MU$Analyte <- gsub('P', '', dfNRWQC_HHC_MU$Analyte)
dfNRWQC_HHC_MU$Analyte <- gsub("[^[:alnum:]///' ]",'', dfNRWQC_HHC_MU$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_MU_Table <- full_join(dfCA.MU, dfNRWQC_HHC_MU)
dfNew_MU_Table <- dfNew_MU_Table[,c(1,2,12,3,4,5,13,14,6,7,8,9,10,11,15,16)]
write.csv(dfNew_MU_Table, file = "Updated_MU_table.csv")

############################
#### CM BENEFICIAL USE #####
############################
dfCA.CM <- subset(dfCA, BUCODE == 'CM')
v <- c('mg/l','ug/l')
dfCA.CM <- dfCA.CM[dfCA.CM$H2O_Units %in% v,]
dfCA.CM <- dfCA.CM[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Alternate solution to subset
#dfCA.CM <- subset(dfCA.CM, dfCA.CM[[3]] %in% c("mg/l", "ug/l"), drop = T) 
dfCA.CM$Analyte <- gsub("[^[:alnum:]///' ]",'', dfCA.CM$Analyte)
dfCA.CM$Analyte <- gsub('\\s+', '', dfCA.CM$Analyte)
#Human Health Criteria for Organism Only
#load data and format
dfNRWQC_HHC_CM <- NRWQC_HHC[,-3]
dfNRWQC_HHC_CM$W_and_O_Units <- "ug/l"
dfNRWQC_HHC_CM <- dfNRWQC_HHC_CM[,c(1,2,3,6,4,5)]
names(dfNRWQC_HHC_CM)[1] <- "Analyte"
names(dfNRWQC_HHC_CM)[3] <- "USEPA_HHC_Org_Only"
names(dfNRWQC_HHC_CM)[4] <- "USEPA_HHC_Org_Only_Units"
#Clean data
dfNRWQC_HHC_CM$Analyte <- gsub('\\s+', '', dfNRWQC_HHC_CM$Analyte)
dfNRWQC_HHC_CM$Analyte <- gsub('P', '', dfNRWQC_HHC_CM$Analyte)
dfNRWQC_HHC_CM$Analyte <- gsub("[^[:alnum:]///' ]",'', dfNRWQC_HHC_CM$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_CM_Table <- full_join(dfCA.CM, dfNRWQC_HHC_CM)
dfNew_CM_Table <- dfNew_CM_Table[,c(1,2,12,3,4,5,13,14,6,7,8,9,10,11,15,16)]
write.csv(dfNew_CM_Table, file = "Updated_CM_table.csv")

############################
#### SHELL BENEFICIAL USE###
############################
dfCA.SH <- subset(dfCA, BUCODE == 'SH')
v <- c('mg/l','ug/l')
dfCA.SH <- dfCA.SH[dfCA.SH$H2O_Units %in% v,]
dfCA.SH <- dfCA.SH[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Alternate subset solution
#dfCA.SH <- subset(dfCA.SH, dfCA.SH[[3]] %in% c("mg/l", "ug/l"), drop =T) 
dfCA.SH$Analyte <- gsub("[^[:alnum:]///' ]",'', dfCA.SH$Analyte)
dfCA.SH$Analyte <- gsub('\\s+', '', dfCA.SH$Analyte)
#Human Health Criteria for Organism Only
dfNRWQC_HHC_SH <- NRWQC_HHC[,-3]
dfNRWQC_HHC_SH$W_and_O_Units <- "ug/l"
names(dfNRWQC_HHC_SH)[1] <- "Analyte"
names(dfNRWQC_HHC_SH)[3] <- "USEPA_HHC_Org_Only"
names(dfNRWQC_HHC_SH)[6] <- "USEPA_HHC_Org_Only_Units"
dfNRWQC_HHC_SH <- dfNRWQC_HHC_SH[,c(1,2,3,6,4,5)]
#clean data
dfNRWQC_HHC_SH$Analyte <- gsub('\\s+', '', dfNRWQC_HHC_SH$Analyte)
dfNRWQC_HHC_SH$Analyte <- gsub('P', '', dfNRWQC_HHC_SH$Analyte)
dfNRWQC_HHC_SH$Analyte <- gsub("[^[:alnum:]///' ]",'', dfNRWQC_HHC_SH$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_SH_Table <- full_join(dfCA.SH, dfNRWQC_HHC_SH)
dfNew_SH_Table <- dfNew_SH_Table[,c(1,2,12,3,4,5,13,14,6,7,8,9,10,11,15,16)]
write.csv(dfNew_SH_Table, file = "Updated_SH_table.csv")

