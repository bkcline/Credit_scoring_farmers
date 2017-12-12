# Author: Michael Barber
# Date: 2017-06-01
# Modified: 2017-10-02
# Description: Master Script to call other scripts locally
# Packages Used: "plyr", "dplyr","mice",  "reshape","caTools","ROCR","corrplot","car", "plotly","randomForest", "ggplot2","sp","cowplot","car","forcats","readr
# Blog Reference: Not published


## libs ----


require(devtools)


# @InputDataSet: input data frame, result of SQL query execution
# @OutputDataSet: data frame to pass back to SQL
# SQL server R stored procedure
# Test code

rm(datout)
rm(RF_objs)
rm(InputDataSet)
rm(predsouty)
library(RODBC)

dbConnection <- 'Driver={SQL Server};Server=***;Database=***;Uid=***;Pwd=***'

#connection info from settings file
channel <- odbcDriverConnect(dbConnection)
InputDataSet <- sqlQuery(channel, paste("Select * from SCVR where DistrictID%1000 = 834"))
unique(InputDataSet$SeasonName)
#InputDataSet <- sqlQuery(channel, iconv(paste(readLines('c:/users/micha/onedrive/oaf/mb-onedrive/projects/repayment_v2/repayment_v2/repayment_v2/sqlsproc.query.sql', encoding = 'UTF-8', warn = FALSE), collapse = '\n'), from = 'UTF-8', to = 'ASCII', sub = ''))

odbcClose(channel)
odbcCloseAll()
dim(InputDataSet)


###### Code 1 - clean data -------------------
#source_url("https://raw.githubusercontent.com/Michael-Bar/OAF-predicting-defaults-prototype/SQL-version/Data_cleaning.R")

source("C:/R_default/Data_cleaning.R")
datout <- process_data(InputDataSet, fakedate="2017-06-06")


d2018 <- datout[[as.integer(format(Sys.Date(), "%Y")) + 1]]
datout[[as.integer(format(Sys.Date(), "%Y")) + 1]] <- NULL


head(d2018)

### code 2 ---- train model -----------
source("C:/R_default/train_model.R")
# source_url("https://raw.githubusercontent.com/Michael-Bar/OAF-predicting-defaults-prototype/master/.R")
rm(RF_objs)
RF_objs <- train_RF_on_data("./", datout, full_mode = FALSE, 4)
length(RF_objs)
### code 3 - predictions ------
source("C:/R_default/predict_default.R")
# source_url("https://raw.githubusercontent.com/Michael-Bar/OAF-predicting-defaults-prototype/master/.R")

predsouty <- RF_predictions(d2018, RF_objs[[3]], RF_objs, "./") 