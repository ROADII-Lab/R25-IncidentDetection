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

tune_grid <- expand.grid(mtry = c(2,3))

cores <- detectCores()  

cl <- makeCluster(cores)
registerDoParallel(cl)


rf.model <- train(model_formula,
                  data = training_frame,
                  method = "rf",
                  trControl = control, tuneGrid = tune_grid)

varImp(rf.model)

save.out <- control