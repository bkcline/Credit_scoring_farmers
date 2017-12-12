# Author: Michael Barber
# Date: 2017-06-01
# Modified: 2017-10-02
# Description: These functions will take the model produced in step 2, and the clean data produced in step 1 to produce new predicions
# Packages Used: "plyr", "dplyr","mice",  "reshape","caTools","ROCR","corrplot","car", "plotly","randomForest", "ggplot2","sp","cowplot","car","forcats","readr
# Blog Reference: Not published

#script 3 of 3
 

RF_predictions <- function(newdata, oldtrain, RFs_in, data_out_path) {

   
    require(plyr) #data manipulation
    require(dplyr)
    #require(reshape, quietly = T)
    #require(ggplot2, quietly = T)
    #require(cowplot)
    require(mice)
    #require(readr)
    require(randomForest)
    #require(ROCR)
    #require(car)
    #require(forcats, quietly = T) #data manipulation
 # print(1)
  #read newseason data    
    grpdat <- newdata
  #print(str(grpdat))
  #### Read in RF object made by script 2 (training.R)--------

    rf.tuned <- RFs_in[[1]]
    rf.newD <- RFs_in[[2]]
    

  #find districts added since last season for use in ditrict-free model
    grpdat$District = as.factor(grpdat$District)
    
    ot = oldtrain

    oldtrain <- ot$District

  #make default flag
    grpdat$default = grpdat$Percent_todate
  grpdat$default[grpdat$default < 100] = 1
  grpdat$default[grpdat$default >= 100] = 0
  
  grpdat <- clean_data(grpdat)
  
  #split names to get district, site, group
  temp_name <- data.frame(do.call('rbind', strsplit(as.character(grpdat$group_uniq),'-',fixed=TRUE)))
  grpdat$Site <- temp_name$X2
  grpdat$Group <- temp_name$X3
  
  nanvars <- c()
  for(i in colnames(grpdat)){
    if(sum(is.na(grpdat[i])) > 0){
      print(paste("found", sum(is.na(grpdat[i])),  "NANs in...",i))
      nanvars <- c(nanvars, i)}}
  
  
  
  
  print(paste(round(sum(is.na(grpdat))/dim(grpdat)[1]*100,5), "% missing data before attempted fix"))
  Sys.sleep(3)
  
  #clean up any vars missing due to math (e.g. division by 0)

  
  
  outp <- paste(data_out_path,"/predictions",sep="")
  dir.create(outp, showWarnings = FALSE, recursive = TRUE)
  
  if(sum(is.na(grpdat)) > dim(grpdat)[1]*0.05){
    print("attempting large missing data imputation, prediction quality may suffer.")
    Sys.sleep(5)
    #library(VIM)
    #png(paste(outp,"MICE_plots.png",sep=""), width=1080, height=1080, res=180)
    #aggr_plot <- aggr(grpdat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
    #dev.off()
    subdat <- grpdat[c(nanvars, rownames(as.data.frame( sort(rf.tuned$importance[,3],decreasing = TRUE)  ))[1:4])]
    tempData <- mice(subdat,m=5,maxit=5,method="fastpmm",printFlag = TRUE)
    grpdat <- complete(tempData,1)
    
  }

  if(sum(is.na(grpdat)) <= dim(grpdat)[1]*0.05 & sum(is.na(grpdat)) >0){
    print("attempting missing data imputation...")
    Sys.sleep(10)
    #library(VIM)
    #png(paste(outp,"MICE_plots.png",sep=""), width=1080, height=1080, res=180)
    #aggr_plot <- aggr(grpdat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
    #dev.off()
    
    subdat <- grpdat[c(nanvars, rownames(as.data.frame( sort(rf.tuned$importance[,3],decreasing = TRUE)  ))[1:7])]
    tempData <- mice(subdat,m=50,maxit=20,method="pmm",printFlag = FALSE)
    grpdat <- complete(tempData,1)
  }
  
    
  print(paste(round(sum(is.na(grpdat))/dim(grpdat)[1]*100,5), "% missing data after attempted fix"))
  Sys.sleep(5)  
  
  #rename
  latest_data_in <-  grpdat[complete.cases(grpdat),]
  

  #check for missing cols
  if(length(setdiff(rownames(rf.tuned$importance),colnames(latest_data_in) ) ) > 0){
    print("Error in dataframes, missing columns. STOPPING")
    stop()
  }

    #print("Scaling data...")

    #  bb <- latest_data_in
    #   latest_data_in <- bb

  latest_data_in$sum_rep_cut <- as.numeric( scale(latest_data_in$sum_rep_cut, center=TRUE, scale=FALSE) )
    latest_data_in$average_amount <- as.numeric(scale(latest_data_in$average_amount, center = TRUE, scale = TRUE))
    latest_data_in$sd_amount <- as.numeric(scale(latest_data_in$sd_amount, center = TRUE, scale = TRUE))
    if (length(unique(latest_data_in$TotalCredit)) <= 1) {
        latest_data_in$TotalCredit <- as.numeric(scale(latest_data_in$TotalCredit, center = TRUE, scale = FALSE))
    }

    if (length(unique(latest_data_in$TotalCredit)) > 1) {
        latest_data_in$TotalCredit <- as.numeric(scale(latest_data_in$TotalCredit, center = TRUE, scale = FALSE))
    }

  
  if(length(find_name(colnames(latest_data_in), "cyclecred")) >1  ){
    print("Found cycle credit metrics...")
    latest_data_in$X20XXA_CycleCredit <- scale(latest_data_in$X20XXA_CycleCredit, center=TRUE, scale=TRUE)
    latest_data_in$X20XXB_CycleCredit <- scale(latest_data_in$X20XXB_CycleCredit, center=TRUE, scale=TRUE)
    latest_data_in$changeAB <- latest_data_in$X20XXA_CycleCredit / latest_data_in$X20XXB_CycleCredit
    latest_data_in$changeAB[is.na(latest_data_in$changeAB)] = 0
    
  }
  


  #NEWLY ADDED
  ## split data into new and old districts, each to have its own model runon them
  #subset data
  #table(latest_data_in$District)
  #newdis <- latest_data_in[!latest_data_in$District %in% xdiff,]
  
 

  print("split data")
    latest = latest_data_in[latest_data_in$District %in% oldtrain,]
   
    newdis = latest_data_in[!latest_data_in$District %in% oldtrain,]





  
  latest$District = as.character(latest$District)
    latest$District = as.factor(latest$District)

  
  newdis$District = as.character(newdis$District)
    newdis$District = as.factor(newdis$District)



    levels(latest$District) <- c(levels(latest$District), setdiff(levels(oldtrain), levels(latest$District)))


   
    #for (i in names(which(lapply(latest, function(x) all(duplicated(x)[-1L])) == TRUE))) {
        #latest[i] <- as.factor(unlist(latest[i]))
    #}




    # d <- RF_objs[[3]]
    
    #latest <- latest[, colnames(ot)]

    latest$default = as.character(latest$default)
    latest$default[is.nan(latest$default)] = "0"
    latest$default = as.factor( as.character(latest$default))

    dx = latest[grep("factor", lapply(latest, class))]
    dxot = ot[grep("factor", lapply(ot, class))]

    levels(latest$default) = c(0,1)

    print(str(latest[colnames(ot)]))
    print("----")
    print(str(ot))



    latest$default = NULL
  
 
 
    pred_prob = predict(rf.tuned, latest, type = "prob") #probability of default

  pred_resp = predict(rf.tuned, latest, type = "response") # predicted class (1= default) 
  pred = data.frame("P defaulting" = pred_prob[,2], "will_default"=pred_resp)  # store as DF

  predictions_out <- cbind(latest[,c("group_uniq","District","Site","GroupName","Percent_todate")], pred)
  

  #order by site and prob. 
  #predictions_out <- predictions_out[ with(predictions_out, order(Site, -P.defaulting)),]
  #write out
  
  #write.csv(predictions_out, file=paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep=""),row.names = FALSE)
  #print(paste("Wrote CSV at",paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep="")))
  
  print("3")
  
  #calculate predictions for NEW districts
  
  
  if( dim(newdis)[1] >0 ){
    print("new districts found")
  pred_prob = predict(rf.newD, newdis, type = "prob") #probability of default
  pred_resp = predict(rf.newD, newdis, type = "response") # predicted class (1= default) 
  pred = data.frame("P defaulting" = pred_prob[,2], "will_default"=pred_resp)  # store as DF
  #newdis$District
  predictions_outnd <- cbind(newdis[c("group_uniq","District", "Site","GroupName","Percent_todate")], pred)
  
  #predictions_outnd$District <- data.frame(do.call('rbind', strsplit(as.character(predictions_outnd$group_uniq),'-',fixed=TRUE)))[1]
  #class(predictions_outnd$District)
  #predictions_outnd["District"] <- as.character(predictions_outnd$District) 

 print(4)
  #predictions_out$District  <- "New district"
  #predictions_out$District <- as.character(predictions_out$District) 
  table(predictions_outnd$District)
  }
  
  if( dim(newdis)[1] >0 ){
    pred_out <- rbind(predictions_out, predictions_outnd)
  }
  
  if( dim(newdis)[1] ==0 ){
    pred_out <- predictions_out
  }
  
  
  #print(5)
  
  #pred_out <- pred_out[ with(pred_out, order(Site, -P.defaulting)),]
  #write out
    #write_csv(pred_out, path=paste(outp,"/Enhanced_PREDICTIONS_", Sys.Date(),".csv",sep=""))


    #force set any group with 99% +  repayment to be unflagged as a sanity check
    pred_out$will_default[pred_out$Percent_todate > 99.5] <- 0
    pred_out$P.defaulting[pred_out$Percent_todate > 99.5] <- pred_out$P.defaulting[pred_out$Percent_todate > 99.5] * 0.5

  #write.csv(pred_out, file=paste(outp,"/Enhanced_PREDICTIONS_", Sys.Date(),".csv",sep=""),row.names = FALSE)
 # print(6)

   


  if( dim(newdis)[1] >0 ){
    #print(paste("Wrote CSV at",paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep="")))
    print(paste("Fraction of groups expected to default in new districts",  round(mean(as.numeric(predictions_outnd$will_default)-1, na.rm=TRUE),3 ) ))
    print(paste("Fraction of groups expected to default in established districts",   round(mean(as.numeric(predictions_out$will_default)-1, na.rm=TRUE),3) ))
    #write.csv(pred_out, file=paste(outp,"/Enhanced_PREDICTIONS_", Sys.Date(),".csv",sep=""),row.names = FALSE)
  }
  
  if( dim(newdis)[1] ==0 ){
    #print(paste("Wrote CSV at",paste(outp,"/PREDICTIONS_", Sys.Date(),".csv",sep="")))
    #print(paste("Fraction of groups expected to default in new districts",  round(mean(as.numeric(predictions_outnd$will_default)-1, na.rm=TRUE),3 ) ))
    print(paste("Fraction of groups expected to default in established districts",   round(mean(as.numeric(predictions_out$will_default)-1, na.rm=TRUE),3) ))
    #write.csv(pred_out, file=paste(outp,"/Enhanced_PREDICTIONS_", Sys.Date(),".csv",sep=""),row.names = FALSE)
  }

   
    print("-----------------------------------------Finished predictions-------------------------------")
  

    return(pred_out)
  
}

#end