### file CR_kriging.R should be run prior to this file as there are depencies on variables etc. ###



#required packages
require("gstat")
require("sp")

#load functions from file
source("_functionDefitions.R")

#set ranges and variants of variogram variables
#values are based on the estimates of vgm.low and vgm.high
psills = c(90,130)
ranges = c(0.2,0.5)
nuggets = c(50,55)
models = c("Gau")

#starting variogram, 1 alpha cut, best fit etc.
vgm <- vgm.mid

#kriging formula
kModel = pm10 ~ 1

#are the data logaritmized in the formula above?
logData = FALSE

#precision of errors, smaller than this are not considered errors (based on the data precision)
precision = 0.1

#number of simulations to perform, resulting matrix will be this number X 3 of size
### the value set here is as 50 but for purpose of the paper it was 5000 ###
## for testing this should is lowered significantly, because 5000 simulations takes a very long time to calculate###
numberOfSimulations = 50

#calculation of limits, min and max values of the fuzzy surface based on the optimisation scheme
dataLimits = calculateMinMaxPoints(psills, ranges, nuggets, models, vgm, kModel, stations, net, logData)

#calculation of simulations -> number of errors, real errors and the ratio
sims = calculateSimulations(numberOfSimulations, dataLimits, psills, ranges, nuggets, models,
                            kModel, stations, net, precision, logData)

#the first two colums should be show as percentages 
sims[,1:2] = sims[,1:2]*100

#the third colum contains in some cases NA values and also infinity, so these are replaced with 0
sims[is.na(sims[,3]) == TRUE | is.infinite(sims[,3]),3] <- 0

#write the resulting matrix data as text file
write.table(sims, file = "cr_sims.txt", append = FALSE, quote = TRUE, sep = " ",
            na = "NA", dec = ".", col.names = TRUE, row.names = FALSE)

# after this file the simulation_eval.R should be runned