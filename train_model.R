# Author: Michael Barber
# Date: 2017-06-01
# Modified: 2017-10-02
# Description: Trains and tunes a randomforest on loan default data
# Packages Used: "plyr", "dplyr","mice",  "reshape","caTools","ROCR","corrplot","car", "plotly","randomForest", "ggplot2","sp","cowplot","car","forcats","readr
# Blog Reference: Not published

#script 2 of 3

#### define helpful functions -----------

tuning_RF = function(trainingdata,yvar, num){
  #tune RF via half grid search half basic gradient descent
  #larger num for better tuning (probably diminishing returns after 5-10)
    #tune params via small gridsearch


    master_tuned = list()
    target = unlist(trainingdata[yvar] )
    tr =  trainingdata
    tr[yvar] = NULL
    tr$Final_repaid = NULL
    tr$year=NULL

    end = round(dim(tr)[1]*0.01)
    strt = round(dim(tr)[1]*0.0001)+1
    
    nodes = c(1, round(seq(strt, end, length.out = num)))
 

    #repeat 3 times
    for(x in c(1,2,3)){
      tuned = c()
        for (i in seq(1:length(nodes)) ) {
  
        #setTxtProgressBar(pbx, length(nodes) - (x*length(nodes)) + i ) 
            nd = nodes[i]
 
        tunex = as.data.frame( tuneRF(x=tr, y=as.factor(target) , mtryStart = 1, stepFactor=1.5, nodesize=nd, ntreeTry = 100, doBest = FALSE , trace=FALSE, improve=1e-5, plot=FALSE) )
        tunex$node = nd
        tuned = rbind(tuned,tunex)
       }
      master_tuned[[x]] = tuned      } 

    txerb = rbind(master_tuned[[1]] , master_tuned[[2]] , master_tuned[[3]])
    txers = ddply(txerb, c("mtry","node"), summarise,  avOOB = mean(OOBError), sdOOB = sd(OOBError))
    
    #remove anything without an SD (indicates it was only found once, therefore likely terrible)
    txers = txers[complete.cases(txers),]
    txers$score = txers$avOOB + txers$sdOOB
    
    a = as.numeric( txers$mtry[which(txers$score == min(txers$score) )] )[1]
    b = as.numeric( txers$node[which(txers$score == min(txers$score)  )] )[1]
    tuned_m = data.frame( mtry_opt =  a ) # min OOB
    tuned_n = data.frame( nodesize_opt = b )  # min OOB
    
    
    
    ret_out = list(txers,tuned_m,tuned_n)
    
    return(ret_out)   }


clear_NA <- function(d, threshhold){
  for(i in colnames(d)){
    xxx = sum(is.na(unlist(d[i])))
    if(xxx/dim(d)[1] >threshhold){
      d[i] = NULL
    }   }
  return(d) }


clear_zero <- function(d, threshhold){
  d$temp = rownames(d)
  d$temp = as.character(d$temp)
  
  dx= d[grep("factor", lapply(d,class) )]
  d= d[-grep("factor", lapply(d,class) )]
  dx$temp = d$temp
  for(i in colnames(d)){
    xxx = sum(unlist(d[i])==0)
    if(xxx/dim(d)[1] >threshhold){
      d[i] = NULL
      print(paste("Removed",i,"due to low variance"))
    }   }

  d = merge(d,dx, by="temp")
  d$temp = NULL
  return(d) }

pdp_plotting =function(objp,indata){
  

  varm =as.data.frame( sort(objp$importance[,3],decreasing = TRUE)  )
  feat_names =rownames(varm)

  png(filename=paste(master_path,"PDplots_1f.png"), width=1080, height=1080 ,  res=180)
  par(mfrow=c(2, 4))
  
  partialPlot(objp, indata, x.var= rownames(varm)[1], which.class = 1, xlab=rownames(varm)[1], ylab="logit p (--> more likely to default)", add=FALSE, main="",plot=TRUE)
  partialPlot(objp, indata, x.var= rownames(varm)[2], which.class = 1, xlab=rownames(varm)[2], ylab="logit probability", add=FALSE, main="", plot=TRUE)
  
  partialPlot(objp, indata, x.var= rownames(varm)[3], which.class = 1, xlab=rownames(varm)[3], ylab="logit probability", add=FALSE, main="",plot=TRUE)
  
  partialPlot(objp, indata, x.var= rownames(varm)[4], which.class = 1, xlab=rownames(varm)[4], ylab="logit probability", add=FALSE, main="",plot=TRUE)
  
  partialPlot(objp, indata, x.var= feat_names[5], which.class = 1, xlab=feat_names[5], ylab="logit p (--> more likely to default)", add=FALSE, main="",plot=TRUE)
  
  partialPlot(objp, indata, x.var= feat_names[6], which.class = 1, xlab=feat_names[6], ylab="logit probability", add=FALSE, main="",plot=TRUE)
  
  partialPlot(objp, indata, x.var= feat_names[7], which.class = 1, xlab=feat_names[7], ylab="logit probability", add=FALSE, main="",plot=TRUE)
  
  partialPlot(objp, indata, x.var= feat_names[8], which.class = 1, xlab=feat_names[8], ylab="logit probability", add=FALSE, main="",plot=TRUE)

  try(dev.off())
  try(dev.off())
  try(dev.off())
  try(dev.off())

  
  }


calibrate_fpr = function(target, var, dat, def ) {
#look at variations of AUC score  
  tpr_s = c()
  tnr_s = c()
  
  pb = txtProgressBar(min = min(def), max = max(def), style = 3)
  
  for(iz in def){
    setTxtProgressBar(pb, iz) 
    
    # iz = 88
    # dat = temp 
    # target = "default"
    # var = "Final_repaid"
    # def = def
    
    tem = dat
    
    #make default flag
    
    tem[,target] = tem[,var]
    tem[,var] = NULL
    tem[target][tem[target] <100] = 1
    tem[target][tem[target] >= 100] = 0
    
    
    tem[,target] = as.factor(tem[,target])
    
    
    completeind = complete.cases(tem)
    tem = tem[completeind,]
    
    
    loc = which(colnames(tem)==target)
    xn = names(tem)[-loc]
    xnames = paste(names(tem)[-loc], collapse=" + ")
    yj = paste(target, " ~ ", sep="")
    Xjsss = as.formula(paste(yj,xnames,sep = ""))
    
    
    
    set.seed(123123)
    sample = sample.split(tem, SplitRatio = 0.75) #sample contains only boolean indecies
    tra = subset(tem, sample == TRUE)
    ho = subset(tem, sample == FALSE)
    tra_y = tra[,target]
    ho_y = ho[,target]
    
    
    #run classy RF------
    
    iy = 1- iz
    
    
    rf.f = randomForest(Xjsss , ntree=150, replace=TRUE, importance = FALSE, norm.votes = TRUE, data=tra,cutoff=c(iy,iz))
    
    
    #as.data.frame( sort(rf.f$importance,decreasing = TRUE)  )
    #rownames(rf.f$importance)
    
    pred = predict(rf.f, newdata = ho , type="response", norm.votes=TRUE) 
    table(observed = ho[, target], predicted = pred)
    tpr = table(observed = ho[, target], predicted = pred)[4] / (table(observed = ho[, target], predicted = pred)[4]   +  table(observed = ho[, target], predicted = pred)[2]  )
    tnr = table(observed = ho[, target], predicted = pred)[1] / (table(observed = ho[, target], predicted = pred)[1] + table(observed = ho[, target], predicted = pred)[3]   )
    
    #print(paste(tpr, " ---", tnr))
    
    tpr_s = c(tpr_s,tpr)
    tnr_s = c(tnr_s, tnr) 
    
  }
  
  fpr_s = 1- tnr_s
  
  tpr_s = tpr_s *100 
  tnr_s = tnr_s * 100
  fnr_s = NULL
  
  for(i in tpr_s){
    xc = 100 - i
    fnr_s = c(fnr_s,xc)
  }
  
  fpr_s = fpr_s * 100
  
  r1 = data.frame("X" = def, "FPR" = fpr_s, "TPR" = tpr_s, "FNR" = fnr_s  )
  ggplot(data =r1) + geom_line(aes(x=X, y=TPR, colour="True positive rate") ,size=2) + geom_line(aes(x=X, y=FPR, colour="False positive rate"), size=2) +geom_line(aes(x=X, y=FNR, colour="False negative rate"), size=2) + ylab("Percentage")
  
  
  return(r1)
}

pROC = function(pred, fpr.stop){
  #limit FPR
  pe = performance(pred,"tpr","fpr")
  for (iperf in seq_along(pe@x.values)){
    ind = which(pe@x.values[[iperf]] <= fpr.stop)
    pe@y.values[[iperf]] = pe@y.values[[iperf]][ind]
    pe@x.values[[iperf]] = pe@x.values[[iperf]][ind]
  }
  return(pe)
}


#optimise cutoff
optimalcut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}





recursive_RF = function(dataf, targetname, CVdata, cv_y,type){
  #this will recursively eliminate features from our RF by cross val
  corr_pred = c()
  featnam = c()
  if(type=="Classification"){
    xl = dim(dataf)[2]
    newtrain = dataf

    for(i in seq(1:xl)) {
      cl = colnames(newtrain)
      
      ix = xl-i
      if(ix > 1){
        loc = which(colnames(newtrain)==targetname)
        xn = names(newtrain)[-loc]
        xnames = paste(names(newtrain)[-loc], collapse=" + ")
        Xj = as.formula(paste("default ~ ",xnames,sep = ""))
        rf.rec = randomForest(Xj , ntree=50, replace=TRUE, importance = TRUE, norm.votes = TRUE, data=train,cutoff=c(0.8,0.2))
        varimp = as.data.frame( sort(rf.rec$importance[,3],decreasing = TRUE)  )
        pred = predict(rf.rec, newdata = CVdata , type="response", norm.votes=TRUE) 
        x = as.numeric(pred) -1
        cv = x + (as.numeric(cv_y)-1)
        correct_predictions = (sum(cv==0) + sum(cv==2)) /  length(cv) *100
        #print(paste("Total correct prediction % ==", round(correct_predictions,2 )))
        newtrain = newtrain[,1:ix]
        newtrain$default = dataf$default
        corr_pred = c(corr_pred,correct_predictions )
        cl2 = colnames(newtrain)
        xcd = setdiff(cl,cl2)
        featnam = c(featnam, xcd)
      }}
    
    if(type!= "Classification"){
      print("Currently only rigged for classification")
    }
    featnam = c("NONE",featnam)
    xd = data.frame("exc"=corr_pred, "feats"=featnam)
    
    return(xd)
  } }


get_recursive_RF = function(datf, targetname, CVdata, cv_y,type,SD=FALSE){
  pb = txtProgressBar(min = 0, max = 5, style = 3)
  if(SD==TRUE){
    
    setTxtProgressBar(pb, 0.2) 
    yf1 = recursive_RF(datf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 2) 
    yf2 = recursive_RF(datf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 3) 
    yf3 = recursive_RF(datf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 4) 
    
    
    yfeat = (yf1$exc + yf2$exc + yf3$exc) / 3
    yfeatsd = c()
    for(i in seq(1:length(yf1$exc))){
      s = c(yf1$exc[i],yf2$exc[i],yf3$exc[i])
      xc = sd(s)
      yfeatsd = c(yfeatsd, xc)
      
      
    }
    setTxtProgressBar(pb, 5) 
    
    xdf = data.frame("average_CVscore" = yfeat, "SD_CVscore" = yfeatsd)
    x = seq(1:dim(xdf)[1])/ dim(xdf)[1] *100
    xdf$x = x
    xdf$no = seq(1:dim(xdf)[1])
    xdf$feats = yf1$feats
    
    return(xdf)}
  
  
  
  if(SD==FALSE){
    setTxtProgressBar(pb, 3) 
    cp = recursive_RF(dataf, targetname, CVdata, cv_y, type)
    setTxtProgressBar(pb, 5) 
    return(cp)
    
    
  }
}


#pretty plots on same page
multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots = c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout = matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx = as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

clean_data <- function(temp) {
    #remove useless cols
    temp$Deceased <- NULL
    temp$Dropped <- NULL
    temp$ClientID <- NULL
    temp$ClientPhoneNumber <- NULL
    temp$Final.repaid <- NULL

    #clean up others
    #temp$Transfer[is.na(temp$Transfer)] <- 0
    temp$first.last.date <- as.numeric(temp$first.last.date)
    temp$repcut.rate[is.infinite(temp$repcut.rate)] <- 0
    temp$repcut.rate[is.na(temp$repcut.rate)] <- 0

    temp$sd_amount[is.na(temp$sd_amount)] <- 0 # anyone with <= 1 repayment will have NA SD
    temp$average_amount[is.na(temp$average_amount)] <- 0 # likewise with average repayment
    temp$Percent_todate[is.na(temp$Percent_todate)] <- 0 # total of NA is NA

    temp$sum_rep_cut[is.na(temp$sum_rep_cut)] <- 0 # total of NA is NA

    #clean names
    names(temp) = gsub('\\.', "_", names(temp))
   

    #make default flag per group
    #temp$default = temp$Final_repaid
    #temp$default[temp$default < 100] = 1
    #temp$default[temp$default >= 100] = 0
    temp$default <- as.factor(temp$default)



    datenames = which(lapply(temp, class) == "Date")
    dnames = which(lapply(temp, class) == "difftime")
    datenames = c(datenames, dnames)
    charnames = sapply(temp, is.character)


    datenames = colnames(temp[, datenames])
    charnames = colnames(temp[, charnames])

    #grpdat[intnames] = sapply(grpdat[intnames],as.numeric)
    #recode data for RF
    for (i in charnames) {
        temp[[i]] = as.factor(temp[[i]])
    }

    for (i in datenames) {
        temp[[i]] = as.numeric(temp[[i]])
    }

    #remove any partial rows - could fill in missing data here with MICE, but we are not short on data so lets just drop them to be safe
    #temp = Filter(function(x)!all(is.na(x)), temp)
    completeind = complete.cases(temp)
    temp = temp[completeind,]
    return(temp)
}

#### START -----

train_RF_on_data = function(rfoutpa = "./", datainw, full_mode = FALSE, tune_num) {
    #full year data
    print("Starting training")
    require(plyr, quietly = T) #data manipulation
    require(dplyr, quietly = T)
    #require(reshape, quietly = T)
    
    #require(caTools, quietly = T)
    #require(cowplot)
    require(ROCR, quietly = T)
 
    #require(car, quietly = T)

   
    require(randomForest, quietly = T)

    if (require("caret")) {
        print("caret is loaded correctly")
    } else {
        print("caret not loaded... error")
    }


    if (full_mode == TRUE) {
        require(corrplot, quietly = T)
require(ggplot2, quietly = T)
        require(plotly, quietly = T) #interactive plots


    }


    #strip null elements from list 

    #strip this years data if found


    datan = datainw[-which(sapply(datainw, is.null))]
    combined <- c()
    # i <- 1
    #do some cleaning and feature eng
    for (i in seq_along(datan)) {
        tf <- as.data.frame(datan[[i]])

        tf <- clean_data(tf)
        print(dim(tf))

        #newd[[i]] <- as.data.frame(temp)
        combined <- rbind(tf, combined)


    }


    #bind together
    #combined <- do.call("rbind", datan)
    combined = Filter(function(x)!all(is.na(x)), combined)

    combined$default = as.integer(as.character(combined$default))
    combined$default[is.na(combined$default)] <- 1
    #print(sum(is.na(combined$default)))

  
    
    combined$default = as.factor(combined$default)
    print(paste("total default rate", mean(as.numeric(combined$default)) - 1))
    print(length(combined$default[is.nan(combined$default)]))


    for (i in unique(combined$SeasonName)) {
        t = subset(combined, SeasonName == i)
        #print(table(t$default))
        #print(class(t$default))
        print(paste("mean default rate", round(mean(as.numeric(t$default))-1,4), "for season", i))

    }
 
    names(combined) = gsub('\\.', "_", names(combined))
    Sys.sleep(5)
    combined$best_date_in_group = NULL
    combined$worst_date_in_group = NULL
    combined$GL_Name = NULL
    combined$phone_number = NULL



    #ensure no NAs in predictors
    #clean up infs
    combined[mapply(is.infinite, combined)] = NA
    #replace any NA with estimate of repayment
    #combined[which(is.na(combined$sum_rep_cut)),]$sum_rep_cut = combined[which(is.na(combined$sum_rep_cut)),]$N_repayments * combined[which(is.na(combined$sum_rep_cut)),]$average_amount


    x = combined[complete.cases(combined),]
    if (dim(x)[1] < 0.8 * dim(combined)[1]) {
        print(paste("WARNING. lost", 100 - (dim(x)[1] / dim(combined)[1] * 100), "% of cases"))
        Sys.sleep(10)
    }




    print(paste("Identified", dim(combined)[1], "Groups total"))

    print(Sys.sleep(5))


    #add in "NEW" district to prevent throwing an error when seeing new district in predictions
    #not this means totally new districts will have lower accuracy as the algo has not seen them and will use other factors instead
    #need to improve new district prediction with seperate model that ignores district effects

    combined$group_uniq = NULL
    ###






    print("Now scaling variables")
    combined$sum_rep_cut <- scale(combined$sum_rep_cut, center = TRUE, scale = FALSE)
    combined$average_amount <- scale(combined$average_amount, center = TRUE, scale = TRUE)
    combined$sd_amount <- scale(combined$sd_amount, center = TRUE, scale = TRUE)
    combined$TotalCredit <- scale(combined$TotalCredit, center = TRUE, scale = TRUE)
    #combined$TotalCredit <- scale(combined$TotalCredit, center=TRUE, scale=TRUE) #check

    if (length(find_name(colnames(combined), "cyclecred")) > 1) {
        print("Found cycle credit metrics...")
        combined$X20XXA_CycleCredit <- scale(combined$X20XXA_CycleCredit, center = TRUE, scale = TRUE)
        combined$X20XXB_CycleCredit <- scale(combined$X20XXB_CycleCredit, center = TRUE, scale = TRUE)
        combined$changeAB <- combined$X20XXA_CycleCredit / combined$X20XXB_CycleCredit
        combined$changeAB[is.na(combined$changeAB)] = 0
        combined$changeAB[combined$changeAB > 10] = 10
        combined$changeAB[combined$changeAB < -10] = -10
    }


    #combined$year = NULL
    combined$sum_rep_cut[combined$sum_rep_cut <= 0] = 0
    combined$Percent_todate[combined$Percent_todate <= 0] = 0
    combined$Percent_todate[is.na(combined$Percent_todate)] = 0


    ## RANDOMLY ADD defaults for testcase; turn off when running properly
    #if (1 == 0) {
    #x <- rnorm(length(combined$default))
    #combined$default <- NULL
    #combined$default[x < 0.0] <- 1
    #combined$default[x >= 0.0] <- 0
    #combined$default <- as.factor(combined$default)
    #print(summary(combined$default))
    #print("TURN OFF RANDOM DEFAULT FLAGS!")

    #}



    if (1 == 0) {
        #devmode
        xkeep <- subset(combined, year == 2014)
        combined <- subset(combined, year != 2014)
        table(combined$year)
        table(xkeep$year)
        table(combined$default)
        table(xkeep$default)
    }

    combined$year = NULL
    combined$SeasonID = NULL
    combined$TotalRepaid = NULL
    combined$TotalRepayment = NULL
    combined$DistrictID = NULL
    combined$SeasonName = NULL
    combined$Site = NULL
    combined$SiteName = NULL
    combined$EnrollmentDate = NULL
    combined$default_av = NULL

    combined$default <- as.factor(as.character(combined$default))



    combined$District = as.factor(combined$District)


    #set.seed(123)
    require(caTools)
    sample = sample.split(combined, SplitRatio = 0.75)
    train = subset(combined, sample == TRUE)
    hold = subset(combined, sample == FALSE)
    train_y = train$default
    hold_y = hold$default


    if (full_mode == TRUE) {
        master_path = paste0(rfoutpa, "RANDOMFOREST/")
        dir.create(master_path, recursive = TRUE, showWarnings = FALSE)
    }

    #clear anything 95%+ zeros
    train = clear_zero(train, 0.95)

    #convert anything with zero var to factor    
    for (i in names(which(lapply(train, function(x) all(duplicated(x)[-1L])) == TRUE))) {
        train[i] <- as.factor(unlist(train[i]))
    }

    print(111)

    gl = grep("factor", lapply(train, class))

    if (full_mode == TRUE) {
        #autocorr plot
        png(paste(master_path, "correlation_plots_full.png", sep = ""), width = 1900, height = 1900)
        corrplot(cor(train[-grep("factor", lapply(train, class))]), tl.cex = 0.5, tl.col = 'blue', tl.pos = 'lower')
    }



  
    if (1 == 0) {
        highlyCor = caret::findCorrelation(cor(train[-gl]), 0.95)
        hico = colnames(train[highlyCor])

        print(paste("Removed due to autocorr:", hico))
        train = train[, - highlyCor]
    }



    #district effects included in the model
    xn = names(train)
    xn = xn[-c(which(xn=="default"),which(xn=="Final_repaid"))]
    xnames = paste(xn, collapse=" + ")
    Xj = as.formula(paste("default ~ ",xnames,sep = ""))


    #new district model - no district, TES or NM effects (as these aren't relevant to new districts)
    xn1 = xn[-c(which(xn=="District"),which(xn=="TotalEnrolledSeasons"),which(xn=="NewMember"))]
    xnames1 = paste(xn1, collapse=" + ")
    Xjj = as.formula(paste("default ~ ", xnames1, sep = ""))

    print(Xj)
    print(Xjj)
  
  
    #autocorr plot
    #png(paste(master_path,"correlation_plots.png",sep=""), width=1900, height=1900)
    #corrplot(cor(train[- grep("factor", lapply(train,class) )]),tl.cex=0.5, tl.col='blue', tl.pos='lower'  ) 
    #dev.off()



    #calibrate the false positive rate using home made function
    #calc cutoffs

    ###recursive RF - this is long, reccommend running once and saving outputs
    if(full_mode == TRUE){
      print("Recursive feature analysis")
      xdf = get_recursive_RF(train, "default", hold, hold_y, "Classification", SD=TRUE)
  
      ggp1 =ggplot(xdf) + geom_point(aes(x=no,y=average_CVscore)) +geom_line(aes(x=no,y=average_CVscore)) +geom_errorbar(aes(x=no,ymin=average_CVscore-SD_CVscore, ymax=average_CVscore+SD_CVscore)) +xlab("Excluded features") + ylab("Total CV accuracy (%)")
      try(ggsave(paste(master_path, "feature_extraction.png",sep="/") , plot=ggp1) )
      try(write.csv(xdf, file=paste(master_path, "extracted_features.csv"))   )
      try(write.table(xdf, file=paste(master_path, "extracted_features.csv"),sep="\t" )   ) 
    }



    if(full_mode == TRUE){
      print("FPR calibration")
      def = seq(1:20) # i.e. how finely sampled
      def = (def/20)[1:19]
      rone = calibrate_fpr("default", "Final_repaid", combined, def=def )
      png(paste(master_path,"calibrate_.png",sep=""),  res=180)
      ggplot(data =rone) + geom_line(aes(x=X, y=TPR, colour="True positive rate") ,size=2) + geom_line(aes(x=X, y=FPR, colour="False positive rate"), size=2) +geom_line(aes(x=X, y=FNR, colour="False negative rate"), size=2) + ylab("Percentage") #+ geom_abline()
      try(dev.off())
      }



    #kill this for prediction, but leave name in to conserve names
    #train$Final_repaid =NULL

    #run classy RF------


    #remove old vars

    rm(combined)

       


    
    #get some tuned paramaters by half grid search half descent (non-efficient, needs full gradient descent solution in v2)
    #training data, yvar to test against, and number of tests (higher is better and slower)

    #if data is large, the take a random subset and continue
    if (dim(train)[1] > 300000) {
        print("subsetting for size")
        train = sample_n(train, size=30000, replace=FALSE)
    }

    if (dim(train)[1] <= 300000) {
        print("using full data")
        subtrain = train
    }
print("TUNING")
    #subtrain=train #check


    for (i in colnames(train)) {
        if (sum(is.na(unlist(train[i]))) > 0) { print(paste(i, "na", sum(is.na(unlist(train[i]))))) }
        if (sum(is.nan(unlist(train[i]))) > 0) { print(paste(i, "nan", sum(is.nan(unlist(train[i]))))) }
        if (sum(is.na(unlist(train[i]))) > 0) { print(paste(i, "inf", sum(is.infinite(unlist(train[i]))))) }
        }





    #print(paste("XXX",colnames(subtrain)))

    tuned_vals = NULL
    newD_vals = NULL

    if (tune_num > 0) {
  
      tuned_vals = tuning_RF(subtrain[, !names(subtrain) %in% c("Final_repaid")],"default", tune_num) #change number up for real run, 5-15 should be fine
     
        newD_vals = tuning_RF(subtrain[, !names(subtrain) %in% c("District","Final_repaid", "TotalEnrolledSeasons", "NewMember")],"default", tune_num) #change number up for real run, 5-15 should be fine

  
  
        #plot tuning 
        tunx = tuned_vals[[1]]
        tun = subset(tunx, node == tuned_vals[[3]]$nodesize_opt)

        if (full_mode == TRUE) {
            png(paste(master_path, "Tuning_2d.png", sep = ""))

            wireframe(score ~ mtry * node, data = tunx, zlab = "OOB error", xlab = "mtry", ylab = "nodesize")
            dev.off()


            png(filename = paste(master_path, "Tuning_3d.png"))
            p = wireframe(score ~ mtry * node, data = tunx, zlab = "OOB error", xlab = "mtry", ylab = "nodesize")
            npanel = c(2, 2)
            rotx = c(-50, -80)
            rotz = seq(60, 270, length = npanel[1] + 1)
            update(p[rep(1, prod(npanel))], layout = npanel,
               panel = function(..., screen) {
                   panel.wireframe(..., screen = list(z = rotz[current.column()],
                                                    x = rotx[current.row()]))
               })
            dev.off()
        }
        #### first RF --------------
        #run first RF, tuned and feature extracted, now we need to get optimal cut off for FPR/TPR
        rf.first = randomForest(Xj , ntree=200, replace=TRUE, mtry = tuned_vals[[2]]$mtry_opt, nodesize = tuned_vals[[3]]$nodesize_opt , importance = TRUE, norm.votes = TRUE, data=subtrain,cutoff=c(0.5,0.5), keep.forest=TRUE)
        rf.nd = randomForest(Xjj , ntree=200, replace=TRUE, mtry = newD_vals[[2]]$mtry_opt, nodesize = newD_vals[[3]]$nodesize_opt , importance = TRUE, norm.votes = TRUE, data=subtrain,cutoff=c(0.5,0.5), keep.forest=TRUE)
        # print(paste("number", ixx))
       
        }

    if(tune_num==0){
      print("WARNING: RF IS UNTUNED - PLEASE SET TUNING ")
  
      #### first RF --------------
      #run first RF, tuned and feature extracted, now we need to get optimal cut off for FPR/TPR
      rf.first = randomForest(Xj , ntree=200, replace=TRUE,importance = TRUE, norm.votes = TRUE, data=subtrain,cutoff=c(0.5,0.5), keep.forest=TRUE)
      rf.nd = randomForest(Xjj , ntree=200, replace=TRUE,  importance = TRUE, norm.votes = TRUE, data=subtrain,cutoff=c(0.5,0.5), keep.forest=TRUE)
      # print(paste("number", ixx))
  
  
    }
  
  

      # a1[[iz]] = c(tuned_vals[[3]]$nodesize_opt, tuned_vals[[2]]$mtry_opt )
      # print("OOB matrix")
      # c1[[iz]] = rf.first$confusion
      # oob = mean(rf.first$err.rate[,1])
      # a2[[iz]] = oob
      # print("########")
  
  



    #get ROC curve
    vec = predict(rf.first, hold, type = "prob")
    resp = predict(rf.first, hold, type = "response")
    vec1 = data.frame("defaulting" = vec[,2])
    cv.first = prediction(vec1, hold$default)



    v1 = predict(rf.nd, hold, type = "prob")

    newd.first = prediction(data.frame("defaulting" = v1[, 2]), hold$default)



    ##limit FPR 
    
#print(11)
    if (full_mode == TRUE) {
        proc.perf = pROC(cv.first, fpr.stop = 0.1)
        png(filename = paste(master_path, "Desired_ROC.png"))
        # master_path
        plot(proc.perf)
        abline(a = 0, b = 1)
        #print(11)
        dev.off()
    }

    

    rocp = performance(cv.first, measure = "tpr", x.measure = "fpr")
    #get optimal cutoff
    opt =as.data.frame(  optimalcut(rocp, cv.first) )
    print("Optimal cutoff 1")
    print(opt)
    cut_1 = c(opt[3,],1-opt[3,])


    #new districts
    rocp = performance(newd.first, measure = "tpr", x.measure = "fpr")
    #get optimal cutoff
    opt =as.data.frame(  optimalcut(rocp, newd.first) )
    print("Optimal ND cutoff 1")
    print(opt)
    cut_1nd = c(opt[3,], 1 - opt[3,])

 

    ##


    xthresh <- mean(as.numeric(train$default),na.rm=TRUE)-1
    print(paste("calculated threshhold...", xthresh))
    if(xthresh <= 0.1){
      #get optimal cutoff if we weight false Neg more than false pos
      print("aggressive model")
      cost.perf = performance(cv.first, "cost", cost.fp = 1, cost.fn = 3)
      cut_2 = cv.first@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
  
      #new district cut
      cost.p = performance(newd.first, "cost", cost.fp = 1.0, cost.fn = 3)
      cut_d = newd.first@cutoffs[[1]][which.min(cost.p@y.values[[1]])]
    }

    if(xthresh > 0.1 & xthresh < 0.3){
      #get optimal cutoff if we weight false Neg more than false pos
      print("balanced model")
      cost.perf = performance(cv.first, "cost", cost.fp = 1, cost.fn = 1.5)
      cut_2 = cv.first@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
  
      #new district cut
      cost.p = performance(newd.first, "cost", cost.fp = 1.0, cost.fn = 1.5)
      cut_d = newd.first@cutoffs[[1]][which.min(cost.p@y.values[[1]])]
    }

    if(xthresh >=0.3 ){
      print("passive model")
      #get optimal cutoff if we weight false Neg more than false pos
      cost.perf = performance(cv.first, "cost", cost.fp = 1, cost.fn = 1)
      cut_2 = cv.first@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
  
      #new district cut
      cost.p = performance(newd.first, "cost", cost.fp = 1.0, cost.fn = 1)
      cut_d = newd.first@cutoffs[[1]][which.min(cost.p@y.values[[1]])]
    }

    if(xthresh >=0.5 ){
      print("passive model")
      #get optimal cutoff if we weight false Neg more than false pos
      cost.perf = performance(cv.first, "cost", cost.fp = 3, cost.fn = 1)
      cut_2 = cv.first@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
  
      #new district cut
      cost.p = performance(newd.first, "cost", cost.fp = 3, cost.fn = 1)
      cut_d = newd.first@cutoffs[[1]][which.min(cost.p@y.values[[1]])]
    }

    #now get the cutoff which gives the overall best acc (simple mean of above methods)
    acc.first = performance(cv.first, measure = "acc")
    ind = which.max( slot(acc.first, "y.values")[[1]] )
    acc = slot(acc.first, "y.values")[[1]][ind]
    cut_3 = slot(acc.first, "x.values")[[1]][ind]
    ind = which( slot(acc.first, "x.values")[[1]] == cut_1)
    acc_fn = slot(acc.first, "y.values")[[1]][ind]


    print(paste("best accuracy (sens vs spec)", "cut 1 = ",cut_1[1]))
    print(paste("best accuracy vs FNR", acc_fn, "cut_2=", cut_2))
    print(paste("best accuracy overall", acc, "cut_3 = ",cut_3))



    #cutbal = sum(cut_1,cut_2,cut_3)/3


    #Cut3 looks to be the overall best on totally new data
    cutbal = cut_2
    #cutbal = mean(c(cut_2,cut_1[1]))
    #cutbal = cut_2
    #cutbal = sum(as.numeric(cut_1[1]),as.numeric(cut_2))/2

    print(paste("Balanced cutoff used  ==", cutbal))
    print(paste("Balanced cutoff used for new districts ==", cut_d))


    if (is.infinite(cutbal) | is.na(cutbal)) {
        cutbal = 0.4
        print(paste("Balanced cutoff used for est. districts, renewed ==", cutbal))
    }

    if(is.infinite(cut_d) | is.na(cut_d)){
      cut_d = cutbal 
      print(paste("Balanced cutoff used for new districts, renewed ==", cut_d))
      }


    if (full_mode == TRUE) {
        png(filename = paste(master_path, "Full_ROC.png"))
        plot(rocp, col = "blue", lty = 3, lwd = 3, colorize = TRUE, print.cutoffs.at = c(cut_1[1], cut_2, cut_3, cutbal))
        abline(a = 0, b = 1)
        dev.off()
    }



    trees_total = 200 + (tune_num*100)

    #lets not get too craazy..
    if(trees_total > 800){
      trees_total = 800
    }

    if(dim(train)[1] >100000){
      trees_total=500
    }

    ##### tuned RF ------------
    #run the tuned RF - mtry, nodesize and cutoff are now happy
    a = Sys.time()
    if(tune_num>0){
      rf.tuned= randomForest(Xj , mtry = tuned_vals[[2]]$mtry_opt, nodesize = tuned_vals[[3]]$nodesize_opt ,ntree=trees_total, replace=TRUE, importance = TRUE, norm.votes = TRUE, data=train,cutoff=c(1-cutbal,cutbal), keep.forest=TRUE)
      #save(rf.tuned,file = paste(master_path,"SUPERFOREST.RData", sep=""))
      #rm(rf.tuned)
  
      #run an RF for new districts - tuning is likely to be a bit off but OOB looks good
      rf.newD = randomForest(Xjj , ntree=trees_total, replace=TRUE, mtry = newD_vals[[2]]$mtry_opt, nodesize = newD_vals[[3]]$nodesize_opt , importance = FALSE, norm.votes = TRUE, data=train,cutoff=c(1-cut_d,cut_d), keep.forest=TRUE)
      #save(rf.newD,file = paste(master_path,"NEW_DISTRICTFOREST.RData", sep=""))
      #rm(rf.newD)
      #print("training complete")
      #load(paste(master_path,"SUPERFOREST.RData", sep="")) 
      #load(paste(master_path,"NEW_DISTRICTFOREST.RData", sep="")) 

    }

    if(tune_num==0){
      rf.tuned= randomForest(Xj , ntree=trees_total, replace=TRUE, importance = TRUE, norm.votes = TRUE, data=train,cutoff=c(1-cutbal,cutbal), keep.forest=TRUE)
      #save(rf.tuned,file = paste(master_path,"SUPERFOREST.RData", sep=""))
      #rm(rf.tuned)
  
      #run an RF for new districts - tuning is likely to be a bit off but OOB looks good
      rf.newD = randomForest(Xjj , ntree=trees_total, replace=TRUE, importance = FALSE, norm.votes = TRUE, data=train,cutoff=c(1-cut_d,cut_d), keep.forest=TRUE)
      #save(rf.newD,file = paste(master_path,"NEW_DISTRICTFOREST.RData", sep=""))
      #rm(rf.newD)
      print("training complete")
      #load(paste(master_path,"SUPERFOREST.RData", sep="")) 
      #load(paste(master_path,"NEW_DISTRICTFOREST.RData", sep="")) 
  
    }



    print(paste("model training took", round(Sys.time()-a) ) )

    #get AUC of new districts
    ve = predict(rf.newD, hold, type = "prob")
    re = predict(rf.newD, hold, type = "response")
    vex = data.frame("defaulting" = ve[,2])

    cv.newd = prediction(vex, hold$default)
    auc.newd = performance(cv.newd, measure = "auc")
    tuned_AUC_newd = as.numeric(auc.newd@y.values)


    #get AUC of old districts
    # hold <- xkeep
    print(1)

    #hold$default = 0
    #table(xkeep$default)
    #table(hold$default)
    vecx = predict(rf.tuned, hold, type = "prob")
    respx = predict(rf.tuned, hold, type = "response")
    vec1x = data.frame("defaulting" = vecx[,2])

    cv.tuned = prediction(vec1x, hold$default)

print(2)

    if (full_mode == TRUE) {
        #prec recall curve
        perfyx <- performance(cv.tuned, "prec", "rec")
        ## The plot obtained with the standard ROCR functions
        ## Not run: 
        plot(perfyx)

        #simple bar chart output for FOPS team
        tempdf = data.frame("default_group" = c("Observed", "Predicted"), "default_pc" = c(mean(as.numeric(hold$default)), mean(as.numeric(respx) - 1)))

        gmdf = ggplot(data = tempdf, aes(default_group, y = default_pc)) + geom_bar(stat = "identity") + theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("% defaulting") + xlab("Data type")
        ggsave(filename = paste(master_path, "Holdout_defaulting.png"), plot = gmdf)
    }
    print(3)



    if (full_mode == TRUE) {
        png(filename = paste(master_path, "Accuracyvcut.png"))
        acc.perf = performance(cv.tuned, measure = "acc")
        plot(acc.perf)
        dev.off()
    }


print(4)

pauc.perf = performance(cv.first, measure = "auc", fpr.stop=0.5)
pAUC = as.numeric(pauc.perf@y.values)*(1/0.5)


auc.perf = performance(cv.tuned, measure = "auc")


tuned_AUC = as.numeric(auc.perf@y.values)

print("######################")
print("######################")
print(paste("TUNED RF AUC SCORE ==", round(tuned_AUC,3) ))
print("----------------------")
print("----------------------")


if(tuned_AUC <= 0.65){
  print(paste("ERROR: AUC score too low (", round(tuned_AUC,3),") stopping script, please contact Mike.Barber@oneacrefund.org"))
  stop()
  }



    print("Loading Caret")
    require(caret, warn.conflicts=TRUE, lib.loc ="C:\\Program Files\\Microsoft SQL Server\\MSSQL13.MSSQLSERVER\\R_SERVICES\\library") 
    print("Caret loaded")
print(summary(train$default))
print("Confusion:")
print(rf.tuned)
y = hold$default # factor of positive / negative cases
predictions = respx # factor of predictions

y = as.factor(y)
predictions= as.factor(predictions)


 #precision <- posPredValue(predictions, y)
 #recall <- sensitivity(predictions, y)

# F1 <- (2 * precision * recall) / (precision + recall)
pred = predict(rf.tuned, newdata = hold , type="response", norm.votes=TRUE) 
table(observed = hold[, "default"], predicted = pred)
 tpr = table(observed = hold[, "default"], predicted = pred)[4] / (table(observed = hold[, "default"], predicted = pred)[4]   +  table(observed = hold[, "default"], predicted = pred)[2]  )
 tnr = table(observed = hold[, "default"], predicted = pred)[1] / (table(observed = hold[, "default"], predicted = pred)[1] + table(observed = hold[, "default"], predicted = pred)[3]   )
 
# 
 print("##################")
 print(paste("sensitivity % , i.e. true positives = ", round(tpr*100,2)))
 print(paste("specificity %, i.e. true negatives = ", round(tnr*100,2)))
 print(paste("false positives %", round((1-tnr)*100,2)))
 print(paste("false negatives %", round((1-tpr)*100,2)))
 print("##################")


##Eval RF --------
#cross validation first
 x = as.numeric(pred) -1
cv = x + (as.numeric(hold_y))
correct_predictions = (sum(cv==0) + sum(cv==2)) /  length(cv) *100

print(paste("Total correct holdout prediction % ==", round(correct_predictions,2 )))
print(paste("Total holdout error  % ==",  100 - round(correct_predictions,2)   )    )

####examine RF --------------
#variable importance



    if (full_mode == TRUE) {
        png(filename = paste(master_path, "VarImp.png"), width = 1080, height = 1080, res = 180)
        par(mfrow = c(1, 1))
        varImpPlot(rf.tuned, main = "RF - group level default classifier")
        dev.off()
        try(dev.off())
    }
    if (full_mode == TRUE) {
  #only run manually
  # load(paste0(RF_inpath,"SUPERFOREST.Rdata"))
  # load(paste(master_path,"train.RData", sep=""))

  #master_path = paste(rfoutpath,"RANDOMFOREST/",sep="")
  varimpo = rf.tuned$importance
  varimpo = invisible(as.data.frame(varimpo))
  varimpo$SD = invisible(as.data.frame(rf.tuned$importanceSD)$MeanDecreaseAccuracy)
  varimpo$feature = rownames(varimpo)
  
  #varimpo =data.frame("feature"=  rownames(as.data.frame(rf.tuned$importance)), "MDA"=as.data.frame(rf.tuned$importance)$MeanDecreaseAccuracy , "SD" =as.data.frame(rf.tuned$importanceSD)$MeanDecreaseAccuracy)
  varimpo = as.data.frame(varimpo)
  gmda =ggplot(data=varimpo, aes(reorder(feature, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy)) + geom_bar(stat = "identity") + theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1)) + ylab("Mean decrease accuracy") + xlab("Feature")  + geom_errorbar(aes(reorder(feature, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, ymin=MeanDecreaseAccuracy-SD,ymax=MeanDecreaseAccuracy+SD),width=0.25) + coord_flip() 
  ggsave(filename=paste(master_path,"Variable_decacc.png"),plot=gmda )
  }

if(1==0){
  #also try this way
  varimpo = varimpo[order(-varimpo$MeanDecreaseAccuracy),] 
  varimpo$feature = rownames(varimpo)
  feat_list = varimpo$feature
  
  png(filename=paste(master_path,"PDP_plot_1.png"), width=1080, height=1080)
  par(mfrow=c(2, 2))
  partialPlot(rf.tuned, train, x.var= varimpo$feature[1], which.class = 1, xlab=varimpo$feature[1], ylab="logit p (--> more likely to default)", add=FALSE, main="",plot=TRUE)
  partialPlot(rf.tuned, train, x.var= varimpo$feature[2], which.class = 1, xlab=varimpo$feature[2], ylab="logit p", add=FALSE, main="",plot=TRUE)
  partialPlot(rf.tuned, train, x.var= varimpo$feature[3], which.class = 1, xlab=varimpo$feature[3], ylab="logit p", add=FALSE, main="",plot=TRUE)
  partialPlot(rf.tuned, train, x.var= varimpo$feature[4], which.class = 1, xlab=varimpo$feature[4], ylab="logit p", add=FALSE, main="",plot=TRUE)
  try(dev.off())
  png(filename=paste(master_path,"PDP_plot_2.png"), width=1080, height=1080)
  par(mfrow=c(2, 2))
  partialPlot(rf.tuned, train, x.var= varimpo$feature[5], which.class = 1, xlab=varimpo$feature[5], ylab="logit p (--> more likely to default)", add=FALSE, main="",plot=TRUE)
  partialPlot(rf.tuned, train, x.var= varimpo$feature[6], which.class = 1, xlab=varimpo$feature[6], ylab="logit p", add=FALSE, main="",plot=TRUE)
  partialPlot(rf.tuned, train, x.var= varimpo$feature[7], which.class = 1, xlab=varimpo$feature[7], ylab="logit p", add=FALSE, main="",plot=TRUE)
  partialPlot(rf.tuned, train, x.var= varimpo$feature[8], which.class = 1, xlab=varimpo$feature[8], ylab="logit p", add=FALSE, main="",plot=TRUE)
  try(dev.off() )

  try(dev.off())
  try(dev.off())
  try(dev.off())
  #end loop  
  } 

    ranfor <- list(rf.tuned, rf.newD, train)

# #sanity checking
print("sanity check: default rates for...")
    #train$default <- as.numeric(as.character(train$default))

    #print(paste(train$District[1], mean(subset(train, District == train$District[1])$default)))
    #print(paste(train$District[4], mean(subset(train, District == train$District[4])$default)))

    #print(paste("<1.5 season:", mean(subset(train, TotalEnrolledSeasons < 1.5)$default)))
    #print(paste(">1.5 season:", mean(subset(train, TotalEnrolledSeasons > 1.5)$default)))

    #print(paste("new mem >0.5:", mean(subset(train, NewMember > 0.5)$default)))
    #print(paste("new mem <0.5:", mean(subset(train, NewMember < 0.5)$default)))



###### SAVE/return RF OBJECT -----


#save(rf.tuned,file = paste(master_path,"SUPERFOREST.RData", sep=""))
#save(rf.newD,file = paste(master_path,"NEW_DISTRICTFOREST.RData", sep=""))



print("######################")
print("######################")
print(paste("TUNED RF AUC SCORE ==", round(tuned_AUC,3) ))
print("----------------------")
print("----------------------")
#save(train,file = paste(master_path,"train.RData", sep=""))


print("-----------------------------------------Finished model building-------------------------------")
return(ranfor)




  }   

#end func
