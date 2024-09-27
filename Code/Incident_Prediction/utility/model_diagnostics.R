install.packages("caret")
library(caret)


control <- trainControl(
  method = "cv",        
  number = 2,          
  classProbs = TRUE,    
  summaryFunction = twoClassSummary,  
  savePredictions = TRUE
)

model_formula <- as.formula(paste(response.var, "~", paste(fitvars, collapse = " + ")))

tune_grid <- expand.grid(mtry = c(2,3,4,5,6))

rf.model <- train(model_formula,
                  data = training_frame,
                  method = "rf",
                  trControl = control,
                  tunelength = 2)


save.out <- control
