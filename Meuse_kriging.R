# loading required packages
require("gstat")
require("sp")
require("reshape")
require("ggplot2")
require("qualityTools")
require("RColorBrewer")

#loading data
data(meuse)

# converting data and defining spatial projection
coordinates(meuse) = ~x+y

# checking the normality assumption
qqPlot(meuse$zinc)
qqPlot(log(meuse$zinc))

# creating the empirical variogram
lzn.vgm <- variogram(log(zinc)~1, meuse, width=100)
lzn.vgm
plot(lzn.vgm)

# modelling theoretical variograms and their plotting
vgm.high <- vgm(psill = 0.6, model="Sph",range= 1000, nugget=0.15)
plot(lzn.vgm, vgm.high)
vgm.low <- vgm(psill = 0.5, model="Sph",range = 800, nugget = 0.00)
plot(lzn.vgm, vgm.low)
vgm.mid <- vgm(psill = 0.55, model="Sph",range= 900, nugget=0.06)
plot(lzn.vgm, vgm.mid)

#### plotting all variograms together ####
# creating data frame for the variances
Fitted <- data.frame(dist = seq(0.01, max(lzn.vgm$dist), length = 101))
Fitted$low <- variogramLine(vgm.low, dist_vector = Fitted$dist)$gamma
Fitted$mid <- variogramLine(vgm.mid, dist_vector = Fitted$dist)$gamma
Fitted$high<- variogramLine(vgm.high, dist_vector = Fitted$dist)$gamma

#convert the dataframes to a long format
Empirical <- lzn.vgm
Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("low", "mid", "high"))
colnames(Modeled)[2] <- "variogram"
ggplot(Empirical, aes(x = dist, y = gamma)) +  geom_point(size = 3) + 
  geom_line(data = Modeled, aes(x = dist, y=value, group = variogram, color = variogram)) +
  labs(title = "Variogram of the zinc concentration in the soil | meuse \n", x = "distance (m)", y = "semivariance \n") + 
  theme(legend.position="none", plot.title = element_text(lineheight=0.8, face="bold", size= 20, family = "sans"), axis.title = element_text(size= 15), axis.text = element_text(size= 13))

#### kriging ####
# preparing the grid
data(meuse.grid)
summary(meuse.grid)
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE

# interpolation
kriged.high <- krige(log(zinc)~1, meuse, meuse.grid, model = vgm.high)
kriged.low <- krige(log(zinc)~1, meuse, meuse.grid, model = vgm.low)
kriged.mid <- krige(log(zinc)~1, meuse, meuse.grid, model = vgm.mid)

#### plotting of kriging ####
# preparing data for plotting
kriged <- kriged.high
kriged$var1.pred <- NULL
kriged$var1.var <- NULL

kriged[['maximal']] <- kriged.high[["var1.pred"]]
kriged[['optimal']] <- kriged.mid[["var1.pred"]]
kriged[["minimal"]] <- kriged.low[["var1.pred"]]

kriged[['high.sd']] <- sqrt(kriged.high[["var1.var"]])
kriged[['optimal.sd']] <- sqrt(kriged.mid[["var1.var"]])
kriged[["low.sd"]] <- sqrt(kriged.low[["var1.var"]])

# creating the pallete
pal1 <- colorRampPalette(c("#F7F4F9", "#980043"), space = 'rgb', interpolate = 'linear', bias = 10)(16)
pal2 <- colorRampPalette(c("#FFFFCC", "#BD0026"), space = 'rgb', interpolate = 'linear', bias = 1)(16)

# plotting the data
spplot(kriged, c("maximal", "optimal", "minimal"), col.regions = pal1)
spplot(kriged, c("high.sd", "optimal.sd", "low.sd"), col.regions = pal2)

