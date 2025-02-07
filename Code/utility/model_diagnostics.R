library(caret)


control <- trainControl(
  method = "cv",        
  number = 2,          
  classProbs = TRUE,    
  summaryFunction = twoClassSummary,  
  savePredictions = TRUE
)


training_frame[[response.var]] <- factor(training_frame[[response.var]],
                                         levels = c("0","1"),
                                         labels = c("Class0", "Class1"))

model_formula <- as.formula(paste(response.var, "~", paste(fitvars, collapse = " + ")))

tune_grid <- expand.grid(mtry = c(3,4,5,6,7))

cores <- detectCores()  

cl <- makeCluster(cores)
registerDoParallel(cl)


rf.model <- train(model_formula,
                  data = training_frame,
                  method = "rf",
                  trControl = control, tuneGrid = tune_grid)

var_imp <- varImp(rf.model)



dir.create(file.path(outputdir, "Random_Forest_Output", "model_diagnostics"))

diag.out <- file.path(outputdir, "Random_Forest_Output", "model_diagnostics")

save(rf.model, file = file.path(diag.out, paste0(modelno,"_RandomForest_diag.Rdata")))
save(var_imp, file = file.path(diag.out, paste0(modelno,"_Variable_Importance")))
