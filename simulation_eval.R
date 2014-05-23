require(reshape)
require(ggplot2)
require(sjPlot)
require(psych)

# loading data
cr <- read.table(paste(getwd(),"cr_sims.txt",sep="/"), header=T, quote="\"")
meu <- read.table(paste(getwd(), "meuse_sims.txt",sep="/"), header=T, quote="\"")

# reshaping data because of visualization in ggplot
cr.melt <- melt(cr)
meu.melt <- melt(meu)

##### violin plot ####
v1 <- sjp.grpfrq(cr.melt$value, cr.melt$variable, type="v", axisLabels.x=c("Overal errors (%)", "Real errors (%)", "Ratio of real to systematic errors"), hideLegend = T, showTableSummary = F, title = "Distribution of Errors | PM10 \n")
v2 <- sjp.grpfrq(meu.melt$value, meu.melt$variable, type="v", axisLabels.x=c("Overal errors (%)", "Real errors (%)", "Ratio of real to systematic errors"), hideLegend = T, showTableSummary = F, title = "Distribution of Errors | meuse \n")

#### descriptive statistics ####
describe(cr); sapply(cr, IQR)
describe(meu); sapply(meu, IQR)
