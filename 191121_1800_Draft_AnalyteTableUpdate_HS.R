#R-Script by Hiram Sarabia-Ramirez, M.S.
#Environmental Scientist
#San Diego Water Board
#Version November, 2019
#Changes: Corrected gsub error and expanded process to include: MU, CM, SH, CO, ES, ES, and MA

#PROBLEM: State agency is evaluating existing water quality data to determine whether
#waterbodies are impaired, however, the tables of standards for maximum contaminant 
#levels used to identify exceedances have not been updated with the latest data from USEPA. 
#The latest standards are available on a table located on a USEPA web site.
#Goal: use R to create a new table with a side-by-side comparison of existing standards for the 
#State agency and the latest guidelines from USEPA
#Approach: Use rvest web scrape to extract data from web site and dplyr to update tables for three beneficial uses (i.e., MUN, COM, and SHELL)
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
#Load required packages if already installed
#if(!require(dplr)) install.packages("dplyr")
#if(!require(dplr)) install.packages("rvest")
#if(!require(dplr)) install.packages("xml2")

########################
### Web Scrape NRWQC ###
########################
#Human Health Criteria for drinking ESter and organism
library("rvest")
library("xml2")
url <- "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-human-health-criteria-table"
table_list <- url %>%
  read_html() %>%
  # previously issue ESs missing xpath 
  # actual xpath from chrome = //*[@id="main-content"]/div[2]/div[1]/div/div/table
  # https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/
  html_nodes(xpath='/html/body/section/div[2]/div[1]/div/div/table') %>%
  html_table() 
NRWQC_HHC <- table_list[[1]]

#################################################
#### MUN Beneficial Use Anlayte Table Update ####
#################################################

#LOAD ANALYTE TABLE
dfCA <- read.csv('R9_Analytes_DRAFT_wOBJref_R1_R2.csv', stringsAsFactors = FALSE)
dfCA.MU <- subset(dfCA, BUCODE == 'MU')
v <- c('mg/l','ug/l')
dfCA.MU <- dfCA.MU[dfCA.MU$H2O_Units %in% v,]
dfCA.MU <- dfCA.MU[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Remove unnecessary columns
dfNRWQC_HHC_MU <- NRWQC_HHC[,-4]
#Add necessary columns and format to facilitate comparison
dfNRWQC_HHC_MU$USEPA_Units <- "ug/l"
dfNRWQC_HHC_MU <- dfNRWQC_HHC_MU[,c(1,2,3,6,4,5)]
names(dfNRWQC_HHC_MU)[1] <- "Analyte"
names(dfNRWQC_HHC_MU)[5] <- "USEPA Publication Year"
names(dfNRWQC_HHC_MU)[6] <- "USEPA Notes"
#Clean data
#REFERENCE: https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
#Remove special characters, letters, and/or and spaces to synchronize join columns
dfNRWQC_HHC_MU$Analyte <- gsub("[[:punct:]]", " ", dfNRWQC_HHC_MU$Analyte)
dfNRWQC_HHC_MU$Analyte <- gsub(" P ", " ", dfNRWQC_HHC_MU$Analyte)
dfNRWQC_HHC_MU$Analyte <- gsub('\\s+', '', dfNRWQC_HHC_MU$Analyte)
dfCA.MU$Analyte <- gsub("[[:punct:]]", " ", dfCA.MU$Analyte)
dfCA.MU$Analyte <- gsub('\\s+', '', dfCA.MU$Analyte)
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
#Human Health Criteria for Organism Only
#load data and format
dfNRWQC_HHC_CM <- NRWQC_HHC[,-3]
dfNRWQC_HHC_CM$USEPA_Units <- "ug/l"
dfNRWQC_HHC_CM <- dfNRWQC_HHC_CM[,c(1,2,3,6,4,5)]
names(dfNRWQC_HHC_CM)[1] <- "Analyte"
names(dfNRWQC_HHC_CM)[5] <- "USEPA Publication Year"
names(dfNRWQC_HHC_CM)[6] <- "USEPA Notes"

#Clean data
dfNRWQC_HHC_CM$Analyte <- gsub("[[:punct:]]", " ", dfNRWQC_HHC_CM$Analyte)
dfNRWQC_HHC_CM$Analyte <- gsub(" P ", " ", dfNRWQC_HHC_CM$Analyte)
dfNRWQC_HHC_CM$Analyte <- gsub('\\s+', '', dfNRWQC_HHC_CM$Analyte)
dfCA.CM$Analyte <- gsub("[[:punct:]]", " ", dfCA.CM$Analyte)
dfCA.CM$Analyte <- gsub('\\s+', '', dfCA.CM$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_CM_Table <- full_join(dfCA.CM, dfNRWQC_HHC_CM)
dfNew_CM_Table <- dfNew_CM_Table[,c(1,2,12,3,4,5,13,14,6,7,8,9,10,11,15,16)]
write.csv(dfNew_CM_Table, file = "Updated_CM_table.csv")


##########################
#### SH BENEFICIAL USE ###
##########################
dfCA.SH <- subset(dfCA, BUCODE == 'SH')
v <- c('mg/l','ug/l')
dfCA.SH <- dfCA.SH[dfCA.SH$H2O_Units %in% v,]
dfCA.SH <- dfCA.SH[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Human Health Criteria for Organism Only
#load data and format
dfNRWQC_HHC_SH <- NRWQC_HHC[,-3]
dfNRWQC_HHC_SH$USEPA_Units <- "ug/l"
dfNRWQC_HHC_SH <- dfNRWQC_HHC_SH[,c(1,2,3,6,4,5)]
names(dfNRWQC_HHC_SH)[1] <- "Analyte"
names(dfNRWQC_HHC_SH)[5] <- "USEPA Publication Year"
names(dfNRWQC_HHC_SH)[6] <- "USEPA Notes"

#Clean data
dfNRWQC_HHC_SH$Analyte <- gsub("[[:punct:]]", " ", dfNRWQC_HHC_SH$Analyte)
dfNRWQC_HHC_SH$Analyte <- gsub(" P ", " ", dfNRWQC_HHC_SH$Analyte)
dfNRWQC_HHC_SH$Analyte <- gsub('\\s+', '', dfNRWQC_HHC_SH$Analyte)
dfCA.SH$Analyte <- gsub("[[:punct:]]", " ", dfCA.SH$Analyte)
dfCA.SH$Analyte <- gsub('\\s+', '', dfCA.SH$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_SH_Table <- full_join(dfCA.SH, dfNRWQC_HHC_SH)
dfNew_SH_Table <- dfNew_SH_Table[,c(1,2,12,3,4,5,13,14,6,7,8,9,10,11,15,16)]
write.csv(dfNew_SH_Table, file = "Updated_SH_table.csv")

############################
### Web Scrape NRWQC ALC ###
############################
library("rvest")
url <- "https://www.epa.gov/wqc/national-recommended-water-quality-criteria-aquatic-life-criteria-table"
NRWQC_ALC <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="main-content"]/div[2]/div[1]/div/div/table[1]') %>%
  html_table()
NRWQC_ALC <- NRWQC_ALC[[1]]

###########################
#### CO Beneficial Use ####
###########################

#Load and format analyte table
dfCA.CO <- subset(dfCA, BUCODE == 'CO')
v <- c('mg/l','ug/l')
dfCA.CO <- dfCA.CO[dfCA.CO$H2O_Units %in% v,]
dfCA.CO <- dfCA.CO[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Load and format web scrape table
dfNRWQC_ALC_CO <- NRWQC_ALC
#Add necessary columns and format to facilitate comparison
names(dfNRWQC_ALC_CO)[1] <- "Analyte"
#Clean data
#REFERENCE: https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
#Remove special characters, letters, and/or and spaces to synchronize join columns
dfNRWQC_ALC_CO$Analyte <- gsub("[[:punct:]]", " ", dfNRWQC_ALC_CO$Analyte)
dfNRWQC_ALC_CO$Analyte <- gsub(" P ", " ", dfNRWQC_ALC_CO$Analyte)
dfNRWQC_ALC_CO$Analyte <- gsub('\\s+', '', dfNRWQC_ALC_CO$Analyte)
dfCA.CO$Analyte <- gsub("[[:punct:]]", " ", dfCA.CO$Analyte)
dfCA.CO$Analyte <- gsub('\\s+', '', dfCA.CO$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_CO_Table <- full_join(dfCA.CO, dfNRWQC_ALC_CO)
dfNew_CO_Table <- dfNew_CO_Table[,c(1,2,12,3,4,5,13,14,15,16,6,7,8,9,10,11)]
write.csv(dfNew_CO_Table, file = "Updated_CO_table.csv")

###########################
#### WA Beneficial Use ####
###########################

#Load and format analyte table
dfCA.WA <- subset(dfCA, BUCODE == 'WA')
v <- c('mg/l','ug/l')
dfCA.WA <- dfCA.WA[dfCA.WA$H2O_Units %in% v,]
dfCA.WA <- dfCA.WA[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Load and format web scrape table
dfNRWQC_ALC_WA <- NRWQC_ALC
#Add necWAsary columns and format to facilitate comparison
names(dfNRWQC_ALC_WA)[1] <- "Analyte"
#Clean data
#REFERENCE: https://stackoverflow.WAm/quWAtions/10294284/remove-all-special-characters-from-a-string-in-r
#Remove special characters, letters, and/or and spacWA to synchronize join WAlumns
dfNRWQC_ALC_WA$Analyte <- gsub("[[:punct:]]", " ", dfNRWQC_ALC_WA$Analyte)
dfNRWQC_ALC_WA$Analyte <- gsub(" P ", " ", dfNRWQC_ALC_WA$Analyte)
dfNRWQC_ALC_WA$Analyte <- gsub('\\s+', '', dfNRWQC_ALC_WA$Analyte)
dfCA.WA$Analyte <- gsub("[[:punct:]]", " ", dfCA.WA$Analyte)
dfCA.WA$Analyte <- gsub('\\s+', '', dfCA.WA$Analyte)
#Join data framWA, create new table, and save as csv
library(dplyr)
dfNew_WA_Table <- full_join(dfCA.WA, dfNRWQC_ALC_WA)
dfNew_WA_Table <- dfNew_WA_Table[,c(1,2,12,3,4,5,13,14,15,16,6,7,8,9,10,11)]
write.csv(dfNew_WA_Table, file = "Updated_WA_table.csv")

###########################
#### ES Beneficial Use ####
###########################

#Load and format analyte table
dfCA.ES <- subset(dfCA, BUCODE == 'ES')
v <- c('mg/l','ug/l')
dfCA.ES <- dfCA.ES[dfCA.ES$H2O_Units %in% v,]
dfCA.ES <- dfCA.ES[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Load and format web scrape table
dfNRWQC_ALC_ES <- NRWQC_ALC
#Add necessary columns and format to facilitate comparison
names(dfNRWQC_ALC_ES)[1] <- "Analyte"
#Clean data
#REFERENCE: https://stackoverflow.ESm/questions/10294284/remove-all-special-characters-from-a-string-in-r
#Remove special characters, letters, and/or and spaces to synchronize join ESlumns
dfNRWQC_ALC_ES$Analyte <- gsub("[[:punct:]]", " ", dfNRWQC_ALC_ES$Analyte)
dfNRWQC_ALC_ES$Analyte <- gsub(" P ", " ", dfNRWQC_ALC_ES$Analyte)
dfNRWQC_ALC_ES$Analyte <- gsub('\\s+', '', dfNRWQC_ALC_ES$Analyte)
dfCA.ES$Analyte <- gsub("[[:punct:]]", " ", dfCA.ES$Analyte)
dfCA.ES$Analyte <- gsub('\\s+', '', dfCA.ES$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_ES_Table <- full_join(dfCA.ES, dfNRWQC_ALC_ES)
dfNew_ES_Table <- dfNew_ES_Table[,c(1,2,12,3,4,5,13,14,15,16,6,7,8,9,10,11)]
write.csv(dfNew_ES_Table, file = "Updated_ES_table.csv")

###########################
#### MA Beneficial Use ####
###########################

#Load and format analyte table
dfCA.MA <- subset(dfCA, BUCODE == 'MA')
v <- c('mg/l','ug/l')
dfCA.MA <- dfCA.MA[dfCA.MA$H2O_Units %in% v,]
dfCA.MA <- dfCA.MA[,c(1,2,4,5,3,6,7,8,9,10,11)]
#Load and format web scrape table
dfNRWQC_ALC_MA <- NRWQC_ALC
#Add necessary columns and format to facilitate comparison
names(dfNRWQC_ALC_MA)[1] <- "Analyte"
#Clean data
#REFERENCE: https://stackoverflow.ESm/questions/10294284/remove-all-special-characters-from-a-string-in-r
#Remove special characters, letters, and/or and spaces to synchronize join ESlumns
dfNRWQC_ALC_MA$Analyte <- gsub("[[:punct:]]", " ", dfNRWQC_ALC_MA$Analyte)
dfNRWQC_ALC_MA$Analyte <- gsub(" P ", " ", dfNRWQC_ALC_MA$Analyte)
dfNRWQC_ALC_MA$Analyte <- gsub('\\s+', '', dfNRWQC_ALC_MA$Analyte)
dfCA.MA$Analyte <- gsub("[[:punct:]]", " ", dfCA.MA$Analyte)
dfCA.MA$Analyte <- gsub('\\s+', '', dfCA.MA$Analyte)
#Join data frames, create new table, and save as csv
library(dplyr)
dfNew_MA_Table <- full_join(dfCA.MA, dfNRWQC_ALC_MA)
dfNew_MA_Table <- dfNew_MA_Table[,c(1,2,12,3,4,5,13,14,15,16,6,7,8,9,10,11)]
write.csv(dfNew_MA_Table, file = "Updated_MA_table.csv")