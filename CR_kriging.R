# loading required packages
require("gstat")
require("sp")
require("reshape")
require("ggplot2")
require("qualityTools")
require("RColorBrewer")

#loading data
stations <- read.table(paste(getwd(),"stations.txt",sep="/"), header=T, quote="\"")

# converting data and defining spatial projection
coordinates(stations) <- ~lon + lat
proj4string(stations) <- CRS('+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs')

# checking the normality assumption
qqPlot(stations$pm10)

# creating the empirical variogram
stations.vgm <- variogram(pm10 ~ 1, stations, width=0.2)
stations.vgm
plot(stations.vgm)

# modelling theoretical variograms and their plotting
vgm.high <- vgm(psill = 130, model="Gau",range= 0.2, nugget=55)
plot(stations.vgm, vgm.high)
vgm.low <- vgm(psill = 90, model="Gau",range= 0.5, nugget=53)
plot(stations.vgm, vgm.low)
vgm.mid <- vgm(psill = 114, model="Gau",range= 0.24, nugget=50)
plot(stations.vgm, vgm.mid)

#### plotting all variograms together ####
# creating data frame for the variances
Fitted <- data.frame(dist = seq(0.01, max(stations.vgm$dist), length = 101))
Fitted$low <- variogramLine(vgm.low, dist_vector = Fitted$dist)$gamma
Fitted$mid <- variogramLine(vgm.mid, dist_vector = Fitted$dist)$gamma
Fitted$high <- variogramLine(vgm.high, dist_vector = Fitted$dist)$gamma

#convert the dataframes to a long format
Empirical <- stations.vgm
Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("low", "mid", "high"))
colnames(Modeled)[2] <- "variogram"
ggplot(Empirical, aes(x = dist, y = gamma)) +  geom_point(size = 3) + 
  geom_line(data = Modeled, aes(x = dist, y=value, group = variogram, color = variogram)) +
  labs(title = "Variogram of PM10 in the Czech Republic \n", x = "distance (°)", y = "semivariance \n") + 
  theme(legend.position="none", plot.title = element_text(lineheight=0.8, face="bold", size= 20, family = "sans"), axis.title = element_text(size= 15), axis.text = element_text(size= 13))

#### kriging ####
summary(stations)

# preparing the grid
x <- seq(12.3,18.7,by = 0.01)
y <- seq(48.7,50.8,by = 0.01)
xy <- expand.grid(x,y)

net <- SpatialPoints(xy)
proj4string(net) <- CRS('+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs')
gridded(net) <- TRUE

# interpolation
pm10.kriged.high <- krige(pm10 ~ 1, stations, net, model = vgm.high)
pm10.kriged.low <- krige(pm10 ~ 1, stations, net, model = vgm.low)
pm10.kriged.mid <- krige(pm10 ~ 1, stations, net, model = vgm.mid)

#### plotting of kriging ####
# preparing data for plotting
pm10.kriged <- pm10.kriged.high
pm10.kriged$var1.pred <- NULL
pm10.kriged$var1.var <- NULL

pm10.kriged[['maximal']] <- pm10.kriged.high[["var1.pred"]]
pm10.kriged[['optimal']] <- pm10.kriged.mid[["var1.pred"]]
pm10.kriged[["minimal"]] <- pm10.kriged.low[["var1.pred"]]

pm10.kriged[['high.sd']] <- sqrt(pm10.kriged.high[["var1.var"]])
pm10.kriged[['optimal.sd']] <- sqrt(pm10.kriged.mid[["var1.var"]])
pm10.kriged[["low.sd"]] <- sqrt(pm10.kriged.low[["var1.var"]])

# creating the pallete
pal1 <- colorRampPalette(c("#F7F4F9", "#980043"), space = 'rgb', interpolate = 'linear', bias = 10)(16)
pal2 <- colorRampPalette(c("#FFFFCC", "#BD0026"), space = 'rgb', interpolate = 'linear', bias = 1)(16)

# plotting the data
spplot(pm10.kriged, c("maximal", "optimal", "minimal"), col.regions = pal1)
spplot(pm10.kriged, c("high.sd", "optimal.sd", "low.sd"), col.regions = pal2)
