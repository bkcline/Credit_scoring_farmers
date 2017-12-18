# Author: Michael Barber
# Date: 2017-06-01
# Modified: 2017-10-02
# Description: Cleans and summarizes data for subsequent steps
# Packages Used: "plyr", "dplyr","mice",  "reshape","caTools","ROCR","corrplot","car", "plotly","randomForest", "ggplot2","sp","cowplot","car","forcats","readr
# Blog Reference: Not published


#### define helpful functions

#will return unique val or NA, useful in summarising data
uniqueorna = function(x) {
  if (length(unique(x)) == 1) {
    unique(x)[1]
  } else {
    NA
  }
}



redo_types = function(dataframess){
  #####redo classes -----------
  numnames = c(which(lapply(dataframess, class) =="numeric"), which(lapply(dataframess, class) =="integer")) 
  datenames = c(which(lapply(dataframess, class) =="Date"), which(lapply(dataframess, class) =="difftime")) 
  charnames = c(which(lapply(dataframess, class) =="character"), which(lapply(dataframess, class) =="factor")) 

  
  #numnames = colnames( dataframess[,numnames])
  #datenames = colnames(dataframess[,datenames])
  #charnames = colnames(dataframess[,charnames])
  #intnames = colnames(dataframess[,intnames])
  #dataframess[intnames] = sapply(dataframess[intnames],as.numeric)
  
  #recode data for RF
  for(i in numnames){
    dataframess[i] = as.numeric(dataframess[[i]])  }
  
  for(i in charnames){
    dataframess[i] = as.factor(dataframess[[i]])  }
  
  for(i in datenames){
    dataframess[i] = as.numeric(unlist(dataframess[[i]]))  }
  
  return(dataframess)   }


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


find_name = function(d, nom){
  dx = tolower(d)
  nom = tolower(nom)
  d[grepl(nom, dx)]  }


clear_NA = function(d, threshhold){
    for (i in colnames(d)) {
        xxx = sum(is.na(unlist(d[i])))
        if (xxx / dim(d)[1] > threshhold) {
            d[i] = NULL
        }
    }
        for (i in colnames(d)) {
            xxx = sum(is.nan(unlist(d[i])))
            if (xxx / dim(d)[1] > threshhold) {
                d[i] = NULL
            }
        }

    return(d)
}

#SUPER function to process data

process_data = function(datin, plot_out = FALSE, fakedate = Sys.Date(), devmode = 0) {
    if (devmode == 1) {
        datin <- InputDataSet
    }
    #print(0)
    #print(str(datin))
    #print(dim(datin))
    require(plyr, quietly = T) #data manipulation
    require(dplyr, quietly = T)
    require(ggplot2, quietly = T)
    #require(cowplot)
    #require(car)

    # remove chars from SeasonName except for year and A/B

datin$SeasonName = as.character(datin$SeasonName)
    datin$SeasonName = gsub(" ", "", datin$SeasonName, fixed = TRUE)
datin$SeasonName = na.omit(unlist(strsplit(datin$SeasonName, "[^A-B]+" , fixed = TRUE))) 
    
#    datin$SeasonName = paste0(y, x)



    
    print(paste("Seasons found:", unique(datin$SeasonName)))

    
    


    #datin$EnrollmentDate = as.Date(datin$EnrollmentDate) + 1
    #datin$EnrollmentDate = as.numeric(format(as.Date(datin$EnrollmentDate), "%Y"))
    

    #print(colnames(datin))
    fvrdat_slim = unique(data.frame("SeasonName" = datin$SeasonName, "DistrictName" = datin$DistrictID, "GroupName" = datin$GroupID, "SiteName" = datin$SiteID, "HasSolar" = datin$SolarAdoption, "N_repayments" = datin$NrOfPayments, "average_amount" = datin$AverageGroupPayment, "sum_rep_cut"=datin$sumrepcut , "sd_amount" = datin$StdGroupPayment, "last_date" = datin$maxdate, "first_date" = datin$mindate))

 
    fscdat = unique( data.frame("ClientID" = datin$ClientID, "TotalRepayment" = datin$TotalRepayment, "TotalCredit" = datin$TotalCredit, "LandSize" = datin$LandSize, "NewMember" = datin$NewMember, "TotalEnrolledSeasons" = datin$TotalEnrolledSeasons, "SeasonName" = datin$SeasonName, "GlobalClientID" = datin$GlobalClientID, "DistrictName" = datin$DistrictID, "SiteName" = datin$SiteID, "GroupName" = datin$GroupID, "Facilitator" = datin$Facilitator) )



    devmode = 0

    returned_data = c()
    #recode factors

#print(1)
    for (yearsy in unique(fscdat$SeasonName)) {
        if (devmode == 1) {
            yearsy = unique(fscdat$SeasonName)[2]
        }

        #remove results from last run
        rm(groupdat)
        rm(scdat) 
        rm(scdat_sim)
        rm(scdat_simdrop)

       
       
        print(paste("Season:", yearsy))
        scdat = subset(fscdat, SeasonName == yearsy)
        sumda = subset(fvrdat_slim, SeasonName == yearsy)

        #print(str(scdat))
        
       
        
 
        if (dim(scdat)[1] > 1 & dim(sumda)[1] > 1) {

            year_in <- as.numeric(as.character(gsub("[^0-9]", "", yearsy)))
            x = as.Date(paste(year_in, "01", "01", sep = "-"), format = "%Y-%m-%d")

            print(paste("found",year_in))
            ## assemble data -----------




    
            if (0 == 1) {
                #devmode
                fakedate = as.Date("2017-06-06")
             

            }
            datenow = as.Date(fakedate)

            
         
            diff_year = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(year_in)
            totdays = diff_year * 365
    
#transfer flag used to be here
#print(4)

            if (devmode == 1) {
                #shrink data for dev mode
                x = dim(vr_sim)[1]
                library(caTools)
                vr_sim = sample_n(vr_sim, size = 20000, replace = FALSE)
                #print(paste("DEVMODE: reduce data from", x, "to", dim(vr_sim)[1]))
                Sys.sleep(3)

            }

         


       

            # sumda = vr_sim %>% group_by(GlobalClientID) %>% summarise(last_date = max(Date), first_date = min(Date), sum_rep_cut = sum(Value), N_repayments = length(Date), average_amount = mean(Value), sd_amount = sd(Value), Transfer = mean(Transfer)) %>% as.data.frame()
           

           
            sumda$first_date = as.Date(sumda$first_date) + 1
            
            sumda$last_date = as.Date(sumda$last_date) + 1

            sumda$first_date[sumda$first_date < as.Date("2010-01-01")] = NA
            sumda$last_date[sumda$last_date < as.Date("2010-01-01")] = NA
            sumda = transform(sumda, first.last.date = last_date - first_date)
#print(5)

          

            sumda = transform(sumda, repcut.rate = sum_rep_cut / as.numeric(first.last.date))

           


            #clean 
            sumda$first.last.date <- as.numeric(sumda$first.last.date)
            sumda$first.last.date[sumda$first.last.date == 0] = 1000
            sumda$first.last.date[is.na(sumda$first.last.date)] = 1000

            sumda$repcut.rate[is.na(sumda$repcut.rate)] = 0
            sumda$repcut.rate[is.infinite(sumda$repcut.rate)] = 0
            
            sumda$average_amount[is.na(sumda$average_amount)] = -1
            sumda$sd_amount[is.na(sumda$sd_amount)] = -1
            sumda$first.last.date[sumda$first.last.date == 0] = 1000

            summary(sumda)
            #make group ids unique
            sumda$group.uniq = paste(sumda$DistrictName, sumda$SiteName, sumda$GroupName, sep = "-")
            #print(6)

            
                #replace any char TRUE with bool
            for (ix in colnames(scdat)) {
                if (is.character(scdat[, ix]) == TRUE) {
                    if (isTRUE(unique(scdat[ix])[1, ] == "False" | unique(scdat[ix])[1, ] == "True") == TRUE) {
                        ##print(ix)
                        scdat[ix][scdat[ix] == "False"] = 0
                        scdat[ix][scdat[ix] == "True"] = 1
                        scdat[, ix] = as.numeric(scdat[, ix])
                    }
                }
            }





            #drop any all na cols
            scdat = Filter(function(x)!all(is.na(x)), scdat)

            

            #drop any remaining character cols (for now - we may add these back in)
            scdat = scdat[, !sapply(scdat, is.character)]

         

            #add phones back in
            # scdat_simdrop$Phone_number = scdat_sim$Phone_number


            ###### PART 2 ------ 

            ###further cleaning, feature building, and summarise to group level

                 

#print(8)
            #features
            colnames(scdat) = gsub("20..", "20XX", colnames(scdat))

            if (length(find_name(colnames(scdat), "CycleCred")) > 0) {
                #change in AB credit
                scdat$changeAB = scdat$X20XXA_CycleCredit / scdat$X20XXB_CycleCredit
                scdat$changeAB[scdat$changeAB == Inf] = 0
                scdat$changeAB[is.na(scdat$changeAB)] = 0
            }





            scdat$Final.repaid = (scdat$TotalRepayment / scdat$TotalCredit) * 100
            summary(scdat$Final.repaid)


            #make default flag - this differes from default in that it will end up as a fraction of defaulters in group
            #scdat$default_av = scdat$Final.repaid
            #scdat$default_av[scdat$default_av < 100] = 1
            #scdat$default_av[scdat$default_av >= 100] = 0

          

            if (plot_out == TRUE) {
                ##explore data-------------
                h1 = ggplot(scdat, aes(x = Percent_todate)) + geom_density(bw = 5) + ggtitle("Total repaid by now") + xlim(0, 100)
                h2 = ggplot(scdat, aes(x = Final.repaid)) + geom_density(bw = 5) + ggtitle("Final repaid") + xlim(0, 100)
                multiplot(h1, h2, cols = 2)

                h3 = ggplot(scdat) + geom_point(aes(x = Percent_todate, y = Final.repaid)) + ggtitle("Repayment vs repayment") + xlab("by now % repaid") + ylab("End of season % repaid")
                h4 = ggplot(scdat, aes(x = last_date)) + geom_density(bw = 0.05) + ggtitle("Last repayment (by now)")
                h5 = ggplot(scdat) + geom_point(aes(x = first.last.date, y = Final.repaid)) + xlab("Last - first date") + ggtitle("Length of repayment vs %") + ylab("EOS repaid %")

                multiplot(h3, h4, h5, cols = 3)
            }






#print(9)
            #drop names
            scdat$group.uniq = paste(scdat$DistrictName, scdat$SiteName, scdat$GroupName, sep = "-")
            #print(9.1)
            #sum(is.na(match(scdat$group.uniq, sumda$group.uniq)))
            #print(str(scdat))
            #added to test AUC
            if (1 == 0) {
                scdat$first_date = scale(as.numeric(scdat$first_date))
                scdat$last_date = scale(as.numeric(scdat$last_date))
                scdat$first.last.date = scale(as.numeric(scdat$first.last.date))
            }


            #get group size
            grp = as.data.frame(table(scdat$group.uniq))
            colnames(grp) = c("group.uniq", "grp.size")
            #print(9.2)
            scdat$GlobalClientID=NULL
            scdat = merge(scdat, grp, by = "group.uniq", all.x = TRUE)
            #print(str(scdat))
             #print(9.3)
            xx = colnames(scdat)[grep("factor", lapply(scdat, class))]
            #print(xx)
            facdat = scdat[, c(xx, "group.uniq")]
            #print(str(facdat))
          
            f = unique(facdat)
            ##print(9.4)
            scdat[xx] = NULL
            ##print(10)
         

            groupdat = scdat %>% group_by(group.uniq) %>% summarise_all(funs(mean)) %>% as.data.frame()

            tc = scdat[, c("group.uniq", "TotalCredit")] %>% group_by(group.uniq) %>% summarise("SumCredit"=sum(TotalCredit)) %>% as.data.frame()
            groupdat = merge(groupdat, tc, by = "group.uniq")
 
            xx = setdiff(colnames(sumda), colnames(scdat))
            groupdat = merge(groupdat, sumda[,c(xx,"group.uniq")], by = "group.uniq")
          
            #groupdat = merge(groupdat, facdat, by = "group.uniq", all.x = TRUE)
        
            head(groupdat)
            groupdat = Filter(function(x)!all(is.na(x)), groupdat)

            groupdat$Final.repaid = (groupdat$TotalRepayment / groupdat$TotalCredit) * 100.
            groupdat$Percent_todate = (groupdat$sum_rep_cut / groupdat$SumCredit) * 100
            
            summary(groupdat$Percent_todate)
            summary(groupdat$Final.repaid)

            #clear anything mostly NA
            groupdat = clear_NA(groupdat, 0.95)
            groupdat$ClientID = NULL
            #print(111)
            names(groupdat)[names(groupdat) == 'DistrictName'] <- 'District'
            names(groupdat)[names(groupdat) == 'SiteName'] <- 'Site'

            #print(str(groupdat))
            groupdat$default = groupdat$Final.repaid
            groupdat$default[groupdat$default < 100] = 1
            groupdat$default[groupdat$default >= 100] = 0
            print("defaulters")
            print(table(groupdat$default))
            print(paste("NAs in default?", sum(is.na(groupdat$default))))
            groupdat$default[is.na(groupdat$default)] = 1
            
#print(11)
            print(paste("===Completion rate RATE FOR", year_in, "=", 1 - mean(as.numeric(groupdat$default),na.rm=TRUE ) ) )


            groupdat = as.data.frame(groupdat)





            #clean up group level data
            groupdat$Percent_todate[groupdat$Percent_todate == Inf] = NA


            #final write
#            pathy_outy = "./"
#            dir.create(pathy_outy, recursive = TRUE, showWarnings = FALSE)
 #           temp = Filter(function(x)!all(is.na(x)), temp)
            groupdat = Filter(function(x)!all(is.na(x)), groupdat)
  #          indout = paste(pathy_outy, "/indivdat_merged", year_in, ".csv", sep = "") # data on individuals
   #         grpout = paste(pathy_outy, "/groupdat_merged", year_in, ".csv", sep = "") # data on groups

            #print(12)
            returned_data[[year_in]] = groupdat

        }
}
    #print(13)
    print("-----------------------------------------Finished data cleaning-------------------------------")
    return(returned_data)
}


