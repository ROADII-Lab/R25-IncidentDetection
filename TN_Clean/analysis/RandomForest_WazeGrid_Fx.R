# Random Forest functions for Waze
require(pROC)
######################################################################
# do.rf function for running random forest models in parallel ----

# Arguments:
# train.dat - Data frame containing all the predictors and the response variable

# omits - Vector of column names in the data to omit from the model. Used to generate the model formula, if the formula is not otherwise provided in the `formula` argument.

# response.var - Vector of the data to use as the response variable, e.g. MatchEDT_buffer_Acc, nMatchEDT_buffer_Acc, TN_crash, nTN_total

# model.no - character value to keep track of model number used; cannot be left blank

# formula - If provided, use this formula instead of the automatically generated one for the random forest model

# test.dat - If provided, this will be used to test the random forest model. If not provided, a split of the training data will be used according to the test.split argument. Both test.dat and test.split cannot be provided.

# test.split - if test.dat is not specified, use this to randomly split the training data into two portions by row, with this split value being used as the test proportion of the data.

# rf.inputs - list of arguments to pass to randomForest

# thin.dat - value from 0 to 1 for proportion of the training and test data to use in fitting the model. E.g. thin.dat = 0.2, use only 20% of the training and test data; useful for testing new features. 

do.rf <- function(train.dat, omits, response.var = "MatchEDT_buffer_Acc", model.no,
                  test.dat = NULL, test.split = .30,
                  split.by = NULL,
                  thin.dat = NULL,
                  cutoff = c(0.8, 0.2),
                  rf.inputs = list(ntree.use = 500, avail.cores = 4, mtry = NULL, maxnodes = NULL, nodesize = 5)){
  
  if(!is.null(test.dat) & !missing(test.split)) stop("Specify either test.dat or test.split, but not both")

  class(train.dat) <- "data.frame"

  fitvars <- names(train.dat)[is.na(match(names(train.dat), omits))]
  
  # Remove any rows with NA in predictors
  cc <- complete.cases(train.dat[,fitvars])
  train.dat <- train.dat[cc,]
  
  # Provide mtry if null
  if(is.null(rf.inputs$mtry)){
    mtry.use = if (!is.factor(response.var)) max(floor(length(fitvars)/3), 1) else floor(sqrt(length(fitvars)))
  } else {mtry.use = rf.inputs$mtry}
  
  # Thin data sets if thin.dat provided
  if(!is.null(thin.dat)) {
    train.dat <- train.dat[sample(1:nrow(train.dat), size = nrow(train.dat)*thin.dat),]
    if(!is.null(test.dat)){
      test.dat <- test.dat[sample(1:nrow(test.dat), size = nrow(test.dat)*thin.dat),]
    }
  }
  
  
  # 70:30 split or Separate training and test data
  # Adding options to split by day or by week. Assumes column hextime is available and is a character vector in POSIX format
  if(is.null(test.dat)){
    if(is.null(split.by)){
      trainrows <- sort(sample(1:nrow(train.dat), size = nrow(train.dat)*(1-test.split), replace = F))
    } else {
    
      if(split.by == "day"){
        day.of.year.each <- format(strptime(train.dat$hextime, "%F %T"), "%j")
        day.of.year <- unique(day.of.year.each)
        train.days <- sample(day.of.year, size = length(day.of.year)*(1-test.split), replace = F)
        trainrows <- c(1:nrow(train.dat))[day.of.year.each %in% train.days]
      }
      
      if(split.by == "week"){
        week.of.year.each <- format(strptime(train.dat$hextime, "%F %T"), "%U")
        week.of.year <- unique(week.of.year.each)
        train.weeks <- sample(week.of.year, size = length(week.of.year)*(1-test.split), replace = F)
        trainrows <- c(1:nrow(train.dat))[week.of.year.each %in% train.weeks]
      }
    } # end if is.null split.by else
    
    testrows <- (1:nrow(train.dat))[!1:nrow(train.dat) %in% trainrows]
    rundat = train.dat[trainrows,]
    test.dat.use = train.dat[testrows,]
  } else {
    class(test.dat) <- "data.frame"
    rundat = train.dat
    
    # Remove any rows with NA in predictors from test.dat
    cc <- complete.cases(test.dat[,fitvars])
    test.dat <- test.dat[cc,]
    
    test.dat.use = test.dat
    comb.dat <- rbind(train.dat, test.dat)
  }
  
  
  # Start RF in parallel
  starttime = Sys.time()
  
  # make a cluster of all available cores
  cl <- makeCluster(rf.inputs$avail.cores, useXDR = F) 
  registerDoParallel(cl)
  
  rf.out <- foreach(ntree = rep(rf.inputs$ntree.use/rf.inputs$avail.cores, rf.inputs$avail.cores),
                  .combine = randomForest::combine, .multicombine=T, .packages = 'randomForest') %dopar% 
    randomForest(x = rundat[,fitvars], y = rundat[,response.var], 
                 ntree = ntree, mtry = mtry.use, 
                 maxnodes = rf.inputs$maxnodes, nodesize = rf.inputs$nodesize,
                 keep.forest = T)
  
  stopCluster(cl); rm(cl); gc(verbose = F) # Stop the cluster immediately after finished the RF
  
  timediff = Sys.time() - starttime
  cat(round(timediff,2), attr(timediff, "unit"), "to fit model", model.no, "\n")
  # End RF in parallel

  Nobs <- data.frame(nrow(rundat),
                     sum(as.numeric(as.character(rundat[,response.var])) == 0),
                     sum(as.numeric(as.character(rundat[,response.var])) > 0),
                     length(rundat$nWazeAccident[train.dat$nWazeAccident>0]) )
  
  colnames(Nobs) = c("N", "No Crash", "Crash present", "Waze accident present")
  
  # Begin if factor response variable  
  if(class(rundat[,response.var])=="factor"){
    rf.pred <- predict(rf.out, test.dat.use[fitvars], cutoff = cutoff)
    rf.prob <- predict(rf.out, test.dat.use[fitvars], type = "prob", cutoff = cutoff)

  predtab <- table(test.dat.use[,response.var], rf.pred)
  
  reference.vec <- test.dat.use[,response.var]
  levels(reference.vec) = c("NoCrash", "Crash")
  levels(rf.pred) = c("NoCrash","Crash")
  
  reference.vec <-as.factor(as.character(reference.vec))
  rf.pred <- as.factor(as.character(rf.pred))
  
  (predtab <- table(rf.pred, reference.vec, 
                    dnn = c("Predicted","Observed"))) 
  bin.mod.diagnostics(predtab)
  
  # pROC::roc - response, predictor
  model_auc <- pROC::auc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"])

  pdf(file = file.path(outputdir, 'Figures', paste0("AUC_", model.no, ".pdf")), width = 6, height = 6)
  plot(pROC::roc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"]),
       main = paste0("Model ", model.no),
       grid=c(0.1, 0.2),
       ylim = c(0, 1), xlim = c(1, 0))
  legend("bottomright", legend = round(model_auc, 4), title = "AUC", inset = 0.25)
  
  dev.off()

  out.df <- data.frame(test.dat.use[, c("GRID_ID", "Year", "day", "hour", response.var)], rf.pred, rf.prob)
  out.df$day <- as.numeric(out.df$day)
  names(out.df)[5:8] <- c("Obs", "Pred", "Prob.Noncrash", "Prob.Crash")
  out.df = data.frame(out.df,
                      TN = out.df$Obs == 0 &  out.df$Pred == "NoCrash",
                      FP = out.df$Obs == 0 &  out.df$Pred == "Crash",
                      FN = out.df$Obs == 1 &  out.df$Pred == "NoCrash",
                      TP = out.df$Obs == 1 &  out.df$Pred == "Crash")
  
  
  
  } # end if factor response variable
  
  # Begin if continuous response variable  
  if(class(rundat[,response.var])=="numeric"){
    rf.prob <- predict(rf.out, test.dat.use[fitvars])

    rf.pred <- cut(rf.prob, breaks = c(-100, cutoff[2], 100), include.lowest = T, labels = c("NoCrash","Crash"))

    out.df <- data.frame(test.dat.use[, c("GRID_ID", "Year", "day", "hour", response.var)], rf.pred, rf.prob)
    out.df$day <- as.numeric(out.df$day)
    names(out.df)[5:7] <- c("Obs", "Pred", "Prob.Crash")
    out.df = data.frame(out.df,
                        TN = out.df$Obs == 0 &  out.df$Pred == "NoCrash",
                        FP = out.df$Obs == 0 &  out.df$Pred == "Crash",
                        FN = out.df$Obs == 1 &  out.df$Pred == "NoCrash",
                        TP = out.df$Obs == 1 &  out.df$Pred == "Crash")
    
    
  } # end if continuous response variable
  
  write.csv(out.df,
            file = file.path(outputdir, 'Random_Forest_Output', paste(model.no, "RandomForest_pred.csv", sep = "_")),
            row.names = F)
  
  savelist = c("rf.out", "rf.pred", "rf.prob", "out.df") 
  if(is.null(test.dat)) savelist = c(savelist, "testrows", "trainrows")
  if(!is.null(thin.dat)) savelist = c(savelist, "test.dat.use")
  
  fn = paste(state, "Model", model.no, "RandomForest_Output.RData", sep= "_")
  
  save(list = savelist, file = file.path(outputdir, 'Random_Forest_Output', fn))
  
  if(class(rundat[,response.var])!="factor" & class(rundat[,response.var])=="numeric") stop("response.var is neither a factor nor a numeric")  
  
  # Output is list of three elements: Nobs data frame, predtab table, binary model diagnotics table, and mean squared error
  if(class(rundat[,response.var])=="factor"){
  outlist =  list(Nobs, predtab, diag = bin.mod.diagnostics(predtab), 
         mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                      as.numeric(rf.prob[,"1"]))^2,
         runtime = timediff,
         auc = as.numeric(model_auc) # do not save complete output
    ) 
  }    
  if(class(rundat[,response.var])=="numeric"){
  outlist =  list(Nobs, 
         mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                      as.numeric(rf.prob))^2,
         runtime = timediff
    )
  }
  outlist
} # end do.rf function



# # Model 01
# # Test arguments
# # Variables to test. Use Waze only predictors, and omit grid ID and day as predictors as well. Here also omitting precipitation and neighboring grid cells
# # Omit as predictors in this vector:
# omits = c(grep("GRID_ID", names(w.04), value = T), "day", "hextime", "year", "weekday",
#           "uniqWazeEvents", "nWazeRowsInMatch", "nWazeAccident",
#           "nMatchWaze_buffer", "nNoMatchWaze_buffer",
#           grep("EDT", names(w.04), value = T),
#           "wx",
#           grep("nWazeAcc_", names(w.04), value = T), # neighboring accidents
#           grep("nWazeJam_", names(w.04), value = T) # neighboring jams
#           )
# 
# modelno = "01"
# train.dat = w.04[sample(1:nrow(w.04), size = 10000),]
# test.dat = NULL
# response.var = "MatchEDT_buffer_Acc"
# rf.inputs = list(ntree.use = avail.cores * 50, avail.cores = avail.cores, mtry = NULL, maxnodes = NULL)
# 
# do.rf(train.dat = w.04, omits, response.var = "MatchEDT_buffer_Acc", 
#       model.no = "01", rf.inputs = rf.inputs) 

# Function to re-assess model predictions and diagnostics for an already-fit model ----
#Will not be run by the random forest models
reassess.rf <- function(train.dat, omits, response.var = "MatchEDT_buffer_Acc", model.no,
                        test.dat = NULL,
                        rf.inputs = list(ntree.use = 500, avail.cores = 4, mtry = NULL, maxnodes = NULL, nodesize = 5),
                        cutoff = c(0.8, 0.2),
                        in_aws = F){
  outputdir<-"~/TN/Output"
  class(train.dat) <- "data.frame"

  # Load fitted model
  cat("Loading", model.no, "\n")
  if(in_aws){
    s3load(object = file.path(outputdir, paste("Model", model.no, "RandomForest_Output.RData", sep= "_")),
           bucket = waze.bucket)  
  }else{
    load(object = file.path(outputdir, paste("Model", model.no, "RandomForest_Output.RData", sep= "_")))
  }
  
  
  fitvars <- names(train.dat)[is.na(match(names(train.dat), omits))]
  
  # 70:30 split or Separate training and test data
  if(is.null(test.dat)){
    rundat = train.dat[trainrows,]
    test.dat.use = train.dat[testrows,]
  } else {
    class(test.dat) <- "data.frame"
    rundat = train.dat
    test.dat.use = test.dat
    comb.dat <- rbind(train.dat, test.dat)
  }
  
  Nobs <- data.frame(nrow(rundat),
                     sum(as.numeric(as.character(rundat[,response.var])) == 0),
                     sum(as.numeric(as.character(rundat[,response.var])) > 0),
                     length(rundat$nWazeAccident[train.dat$nWazeAccident>0]) )
  
  colnames(Nobs) = c("N", "No EDT", "EDT present", "Waze accident present")

  if(class(rundat[,response.var])!="factor" | class(rundat[,response.var])=="numeric") stop("response.var is neither a factor nor a numeric")
    
  # Begin if factor response variable  
  if(class(rundat[,response.var])=="factor"){
    rf.pred <- predict(rf.out, test.dat.use[fitvars], cutoff = cutoff)
    rf.prob <- predict(rf.out, test.dat.use[fitvars], type = "prob", cutoff = cutoff)
    
    predtab <- table(test.dat.use[,response.var], rf.pred)
    
    reference.vec <- test.dat.use[,response.var]
    levels(reference.vec) = c("NoCrash", "Crash")
    levels(rf.pred) = c("NoCrash","Crash")
    
    reference.vec <-as.factor(as.character(reference.vec))
    rf.pred <-as.factor(as.character(rf.pred))
    
    (predtab <- table(rf.pred, reference.vec, 
                      dnn = c("Predicted","Observed"))) 
    bin.mod.diagnostics(predtab)
    
    # pROC::roc - response, predictor
    model_auc <- pROC::auc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"])
    
    pdf(file = file.path(outputdir, paste0("AUC_", model.no, ".pdf")), width = 6, height = 6)
    plot(pROC::roc(test.dat.use[,response.var], rf.prob[,colnames(rf.prob)=="1"]),
         main = paste0("Model ", model.no),
         grid=c(0.1, 0.2),
         ylim = c(0, 1), xlim = c(1, 0))
    legend("bottomright", legend = round(model_auc, 4), title = "AUC", inset = 0.25)
    
    #dev.print(device = jpeg, file = paste0("AUC_", model.no, ".jpg"), width = 500, height = 500)
    dev.off()
    
    out.df <- data.frame(test.dat.use[, c("GRID_ID", "day", "hour", response.var)], rf.pred, rf.prob)
    out.df$day <- as.numeric(out.df$day)
    names(out.df)[4:7] <- c("Obs", "Pred", "Prob.Noncrash", "Prob.Crash")
    out.df = data.frame(out.df,
                        TN = out.df$Obs == 0 &  out.df$Pred == "NoCrash",
                        FP = out.df$Obs == 0 &  out.df$Pred == "Crash",
                        FN = out.df$Obs == 1 &  out.df$Pred == "NoCrash",
                        TP = out.df$Obs == 1 &  out.df$Pred == "Crash")
    
    } # end if factor response variable
  
  # Begin if continuous response variable  
  if(class(rundat[,response.var])=="numeric"){
    rf.prob <- predict(rf.out, test.dat.use[fitvars])
    
    rf.pred <- cut(rf.prob, breaks = c(-100, cutoff[2], 100), include.lowest = T, labels = c("NoCrash","Crash"))
    
    out.df <- data.frame(test.dat.use[, c("GRID_ID", "day", "hour", response.var)], rf.pred, rf.prob)
    out.df$day <- as.numeric(out.df$day)
    names(out.df)[4:6] <- c("Obs", "Pred", "Prob.Crash")
    out.df = data.frame(out.df,
                        TN = out.df$Obs == 0 &  out.df$Pred == "NoCrash",
                        FP = out.df$Obs == 0 &  out.df$Pred == "Crash",
                        FN = out.df$Obs == 1 &  out.df$Pred == "NoCrash",
                        TP = out.df$Obs == 1 &  out.df$Pred == "Crash")
    
    
  } # end if continuous response variable
  
  write.csv(out.df,
            file = paste(model.no, "RandomForest_pred.csv", sep = "_"),
            row.names = F)
  
  savelist = c("rf.out", "rf.pred", "rf.prob", "out.df") 
  if(is.null(test.dat)) savelist = c(savelist, "testrows", "trainrows")

  if(in_aws){
    s3save(list = savelist,
           object = file.path(outputdir, paste("Model", model.no, "RandomForest_Output.RData", sep= "_")),
           bucket = waze.bucket)
  }else{
    save(list = savelist,
         file.path(outputdir, paste("Model", model.no, "RandomForest_Output.RData", sep= "_")))
  }

  

  
  # Output is list of three elements: Nobs data frame, predtab table, binary model diagnotics table, and mean squared error
  if(class(rundat[,response.var])=="factor"){
    outlist =  list(Nobs, predtab, diag = bin.mod.diagnostics(predtab), 
                    mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                                 as.numeric(rf.prob[,"1"]))^2,
                    auc = as.numeric(model_auc) # do not save complete output
    ) 
  }    
  if(class(rundat[,response.var])=="numeric"){
    outlist =  list(Nobs, 
                    mse = mean(as.numeric(as.character(test.dat.use[,response.var])) - 
                                 as.numeric(rf.prob))^2
                    )
  }
  outlist
} # end reassss.rf function

