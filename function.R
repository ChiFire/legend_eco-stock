library(caret)
library(mlbench)
library(quantregForest)

getModel = function(target){
     if(target == '^DJI'){
          target = 1
     }else if(target == '^NYA'){
          target = 2
     }else if(target == '^IXIC'){
          target = 3
     }else if(target == '^GSPC'){
          target = 4
     }
     
     data_econ_factors <- read.csv("./data_major.csv")
     date_seq <- seq(as.Date("1992/01/01"),as.Date("2016/10/01"),"months")
     data_econ_factors[1] <- date_seq
     data_econ_factors <- cbind(data_econ_factors[,-1], row.names=(data_econ_factors[,1]))
     data_econ_factors <- lapply(data_econ_factors,as.numeric)
     data_econ_factors <- as.data.frame(data_econ_factors)
     row.names(data_econ_factors) <- date_seq
     
     var_ind <- data_econ_factors[,1:23]
     var_dep <- data_econ_factors[,24:27]
     target <-target
     Target <- var_dep[,target]
     Target.list <- c("$DJI","$NYA","$IXIC","$GSPC")
     
     percent <- function(x, digits = 2, format = "f", ...) {
          paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
     }
     
     fun_measure <- function(actual, predict){
          error = actual - predict
          rmse = sqrt(mean(error^2))
          mae  = mean(abs(error))
          error_percent = abs((actual - predict))/actual
          mape = mean(abs(error_percent))*100
          accuracy = 1-error_percent
          rsq = mean(qrf$rsq)
          result = matrix(NA, nrow = 4,ncol = 2)
          result[1,1] = "RMSE"
          result[1,2] = rmse
          result[2,1] = "MAE"
          result[2,2] = mae
          result[3,1] = "MAPE"
          result[3,2] = mape
          result[4,1] = "RSQ"
          result[4,2] = percent(rsq)
          result <- as.data.frame(result)
          return(result)
     }
     
     fs_data <- as.data.frame(cbind(var_ind,Target))
     control <- trainControl(method="repeatedcv", number=10, repeats=3,savePredictions = TRUE)
     model <- train(Target~., data=fs_data, method="qrf", preProcess="scale",trControl=control)
     importance <- varImp(model, scale=FALSE)
     
     imp_temp = as.data.frame(importance$importance)
     var_temp = as.data.frame(row.names(imp_temp))
     importance_new <- cbind(var_temp,imp_temp)
     row.names(importance_new) <- NULL
     
     imp_level <- 0.6
     imp_var_table <- subset(importance_new,importance_new$Overall >= imp_level)
     imp_var <- as.character(imp_var_table[,1])
     
     used_data_ind <- subset(data_econ_factors,select = imp_var)
     used_data <- as.data.frame(cbind(used_data_ind, Target))
     
     quantiles <- c(0.1,0.5,0.9)
     qrf <- quantregForest(x=used_data_ind,y=used_data$Target,importance = TRUE,
                           quantiles=quantiles, keep.inbag = TRUE,nodesize = 10)
     qrf.predict = predict(qrf, used_data_ind, what = mean)

     a <- qrf.predict
     b <- used_data$Target
     plot_data <- data.frame(cbind(a,b))
     colnames(plot_data) <- c("Predict", "Actual")
     minY <- min(plot_data$Predict,plot_data$Actual)
     maxY <- max(plot_data$Predict,plot_data$Actual)

     plot(date_seq,plot_data$Predict,main = paste("Experiment result of",Target.list[target],
                                                      "Using QRF")
          ,xlab = "Date", ylab = "Price",type="l", lwd = 2
          ,ylim = c(minY,maxY),
          cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, cex.sub = 1.5)
     lines(date_seq,plot_data$Actual,lty=3,lwd=2)
     legend('topleft', c("Predict","Actual"), lty=1:ncol(plot_data))
     return(used_data)
}

getPar = function(used_data){
     library(parcoords)
     parcoords(used_data, rownames = F,
               brushMode = "1D-axes-multi", 
               reorderable = TRUE)
}