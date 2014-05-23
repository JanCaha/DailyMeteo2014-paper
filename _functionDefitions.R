# returns estimates of minimal and maximal values based on limits of sill, range, nugget a models
# vgm_start - is modal variogram selected for the data
# krigingModel - model of the kriging used in the calculations
# dataSet - the data for modelling, gridPred - grid of locations, at which the values should be predicted
# logResults - was the dataset logaritmized prior to the calculation?

# data format of result is list with two vectors - result$min_data, result$max_data

calculateMinMaxPoints <- function(psills, ranges, nuggets, models, vgm_start, 
                                  krigingModel, dataSet, gridPred, logResults=FALSE){
  
  #initial prediction is done based on modal model
  pred = krige(krigingModel, dataSet, gridPred, model = vgm_start)
  
  #were the data logaritmized prior to the calculation?
  if(logResults){
    min_data = exp(pred$var1.pred)
    max_data = exp(pred$var1.pred)
  }
  else{
    min_data = pred$var1.pred
    max_data = pred$var1.pred
  }
  
  # for all combinations of the sills, ranges, nuggers and also models(if there is more than one)
  for (psill in psills){
    for (range in ranges){
      for(nugget in nuggets){
        for(model in models){
          
          #prepare the varigogram
          vgm <- vgm(psill = psill, model= model,range= range, nugget=nugget)
          
          #calculated the krigging
          pred <- krige(krigingModel, dataSet, gridPred, model = vgm)
          
          #if the dataset was logaritmized we need to exponentiate the outcome
          if(logResults){
            temp_data = exp(pred$var1.pred)
          }
          else{
            temp_data = pred$var1.pred
          }
          
          #compare the obtained preditions to minimal and maximal values that we allready have
          #if the value is outside of the range, than adjust the range
          for(i in 1:length(temp_data)){
            if(temp_data[i] < min_data[i]){
              min_data[i] = temp_data[i]
            }
            
            if(max_data[i] < temp_data[i]){
              max_data[i] = temp_data[i]
            }
          }
        }
      } 
    }
  }
  
  #prepare the result and return it from the function
  result <- list(min_data=min_data,max_data=max_data)
  return(result)
}

#function performs numberOfSimulations simulations and compare the obtained results to limits specified in dataLimits dataset
#variables psills, ranges, nuggets, models, krigingModel, dataSet, gridPred and logResult have the same meaning
#as in previous function
#errorLimit specifies the precision, the deviation that is not considered not an error
#return results of simulation in matrix noSimulation times 3, with specific error calculated
calculateSimulations <- function(numberOfSimulations, dataLimits, psills, ranges, nuggets, models, 
                                 krigingModel, dataSet, gridPred, errorLimit, logResults=FALSE){
  
  # propare the matrix for the results
  result = matrix(data=0.0, nrow=numberOfSimulations, ncol=3)
  
  #loop for number os simulations
  for (iteration in 1:numberOfSimulations){  
    
    #number of errors
    numberErrors = 0
    numberErrorsSmall = 0
    numberErrorsBig = 0
    
    #obtain random values of the parameters
    range = runif(1, min = ranges[1], max = ranges[2])
    psill = runif(1, min = psills[1], max = psills[2])
    nugget = runif(1, min = nuggets[1], max = nuggets[2])
    model = sample(models, 1)
    
    #prepare variogram
    vgm <- vgm(psill = psill, model= model, range= range, nugget=nugget)
    #krige the data
    data_kriged <- krige(krigingModel, dataSet, gridPred, model = vgm)
    
    #were the data logaritmized prior to the calculation?
    if(logResults){
      temp_data = exp(data_kriged$var1.pred)
    } else{
      temp_data = data_kriged$var1.pred
    }
    
    #compare the calculated data with the limits 
    #and calculate the number of errors
    for(i in 1:length(temp_data)){
      
      if(temp_data[i]<dataLimits$min_data[i]){
        
        numberErrors = numberErrors +1
        
        if(abs(temp_data[i]-dataLimits$min_data[i])<=errorLimit){
          numberErrorsSmall = numberErrorsSmall + 1
        }
        else{
          numberErrorsBig = numberErrorsBig + 1
        }
          
      }
      
      if(dataLimits$max_data[i]<temp_data[i]){
        
        numberErrors = numberErrors +1
        
        if(abs(temp_data[i]-dataLimits$max_data[i])<=errorLimit){
          numberErrorsSmall = numberErrorsSmall + 1
        }
        else{
          numberErrorsBig = numberErrorsBig + 1
        }
      } 
      
    }
    
    #store the data in the matrix
    result[iteration,1] = numberErrors / length(temp_data)
    result[iteration,2] = numberErrorsBig / length(temp_data)
    result[iteration,3] = numberErrorsSmall / numberErrorsBig
    
    #name the matrix
    colnames(result) <- c("errors", "real_errors", "errors_ratio")
    
    #print the info for the user
    print(paste("Iteration",iteration, "of", numberOfSimulations,".", sep=" "))
    
  }

  return(result)
}