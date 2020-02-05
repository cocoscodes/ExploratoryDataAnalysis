getwd()

# Principles of Analytics Graphics ----

# 1.- always ask "compared to what?" - the result you observe is in relation to what?
# 2.- what is your causal framework for thinking about a question? - show causale mechanism of the observation
# 3.- show multivariate data, the real world is multivariate, more than 2 variables - show are many variables as possible 
# simpsons paradox -is a phenomenon in probability and statistics, in which a trend appears in several different groups of data but disappears or reverses when these groups are combined.
# 4.- intergation of evidence - integrate words, numbers, images, diagrams, etc.
# 5.- describe and document the evidence - the data graphic shoudl tell the complete story
# 6.- content is king - quality, relevance, and integrity of the content makes a succesfull presentation

# Exploratory graphs ----
# exploratory graphs are for personally understand the data, not for presentation purposes

pollution <- read.csv("avgpm25.csv", colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)
summary(pollution$pm25)

boxplot(pollution$pm25, col = "blue")
abline(h=12) # shows the line where the upper limit lies for the EPA

hist(pollution$pm25, col = "green", breaks = 100) # breaks adds the number per column - size
rug(pollution$pm25) # adds exac location of the data points undernead

hist(pollution$pm25, col = "green")
abline(v=12, lwd=2, lty=4) # line with lwd and line type lty
abline(v=median(pollution$pm25), col= "magenta",lwd=4) 

barplot(table(pollution$region),col = "wheat", main = "Number of Counties in Each Region")
# with dply pipeline operator
table(pollution$region) %>% barplot(col = "wheat")

install.packages("maps")
library(maps)
library(dplyr)
map("county", "california")
with(filter(pollution, pm25>12) , points(longitude, latitude))

# Exploratory graphs 2 ----
boxplot(pm25 ~ region, data = pollution, col = "red") # pm25 Y axis and region X axis

# multiple histograms
par(mfrow = c(2,1), mar = c(4, 4, 2, 1)) # mar margins c(bottom, left, top, right)
# mfrow c(nr,nc) number of rows and columns to present the charts together
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

# par is the graphical parameters
par(mfrow = c(1,1))
with(pollution, plot(latitude, pm25, col= region)) 
# plotting with latitude to observ if there any north or south trends
abline(h = 12, lwd = 2, lty = 2)
levels(pollution$region) # east i black default and west is red

# multiple scatter plots
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

## Lattice
library(lattice)
xyplot(pm25 ~ latitude | region, data = pollution)

## ggplot2
library(ggplot2)
qplot(latitude, pm25, data = pollution, facets = . ~ region)

# other resources: https://www.r-graph-gallery.com/index.html

# Plotting systems in R ----
# base plotting - adds one element one by one, cant take back the errors i.e. aadjusting margins
data(airquality)
with(airquality, {
  plot(Temp, Ozone)
  lines(loess.smooth(Temp, Ozone))
})

data(cars)
## Create the plot / draw canvas
with(cars, plot(speed, dist))
## Add annotation
title("Speed vs. Stopping distance")

# Lattice system - xyplot and bwplot, good for putting many plots in one screen
# awkward to specify an entire plot, it is not intuitive, cannot add anything
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))
xyplot(Life.Exp ~ Income | region, data = state, layout = c(2,2))

# ggplot2 system - well rounded and rigorous package, many sthetical features done automatically
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

# Base plotting system ----
# graphics (functions) and GRdevices(devices) are the basic packages

# There are two phases to creating a base plot:
# 1. Initializing a new plot (there are functions to call a devices plot or hist)
# 2. Annotating (adding to) an existing plot (use ?Par)

library(datasets)
## Histogram
hist(airquality$Ozone)

## Boxplot
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")
boxplot(Ozone~Month, airquality, xlab="Month", ylab="Ozone (ppb)",
        col.axis="blue",col.lab="red")
title(main = "Ozone and Wind in New York City")

## Scatterplot
with(airquality, plot(Wind, Ozone))

## Graphic parameters
length(par())
names(par())
par("lty")
par("lwd")
par("pch") # type of point
par("col")
par("bg") # background color
par("mar") # margins size
par("oma") # outer margins
par("mfrow") # plots in rows
par("mfcol") # plots in cols
par("las") # orientation of the axis labels on the plot
par("pin") # size of the plot in inches
par("fg") # foregraound color
par("bty") # show box of the legend or change it
par("cex") # magnify legend according to size

# Graphics functions
# Make the initial plot
with(airquality, plot(Wind, Ozone))
# Add a title
title(main = "Ozone and Wind in New York City")

# add more functions
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", type = "n")) # type = "n" leaves a blank plot
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue", pch=17))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red", pch=8))
legend("topright", pch = c(17,8), col = c("blue", "red"), legend = c("May", "Other Months"))
abline(v=median(airquality$Wind),lty=2,lwd=2)

# Regression line in base
with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", pch = 20))
## Fit a simple linear regression model
model <- lm(Ozone ~ Wind, airquality)
## Draw regression line on plot
abline(model, lwd = 2)

# Multiple base plots
par(mfrow = c(1, 2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
  plot(Temp, Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer = TRUE)
})

# Graphic devices ----
# Vector formats are good for line drawings and plots with solid colors 
# using a modest number of points, while bitmap formats are good for 
# plots with a large number of points, natural scenes or web-based plots.
?Devices

## Make plot appear on screen device like PDF, PNG, JPEG, SVG, and TIFF
with(faithful, plot(eruptions, waiting))
## Annotate with a title
title(main = "Old Faithful Geyser data")

## Open PDF device; create 'myplot.pdf' in my working directory
pdf(file = "myplot.pdf")
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))
## Annotate plot; still nothing on screen
title(main = "Old Faithful Geyser data")
## show the available devices
dev.cur()
## Close the PDF file device
dev.off()
## Now you can view the file 'myplot.pdf' on your computer

# You can change the active graphics device with dev.set(<integer>)

library(datasets)
## Create plot on screen device
with(faithful, plot(eruptions, waiting))
## Add a main title
title(main = "Old Faithful Geyser data")
## Copy my plot to a PNG file
dev.copy(png, file = "geyserplot.png")
## Don't forget to close the PNG device!
dev.off()

# Week 1 Project ----
# plot1
library(data.table)
library(dplyr)
library(chron)
data <- fread("household_power_consumption.txt")
finalData <- subset(data, data$Date == "1/2/2007" | data$Date == "2/2/2007")
finalData <- mutate(finalData,Date=as.Date(Date, format = "%d/%m/%Y"))
finalData <- mutate(finalData,Time=chron(times=finalData$Time))
vars <- names(select(finalData,Global_active_power:Sub_metering_2))
finalData[vars] <- sapply(finalData[vars],as.numeric)

str(finalData)

hist(finalData$Global_active_power, 
     col = "red",main = "Global Active Power",xlab = "Global Active Power (kilowatts)")

dev.copy(png, file = "plot1.png",width=480, height=480)
dev.off()

# plot2
finalData <- mutate(finalData,dateTime=paste(finalData$Date,finalData$Time))
library(lubridate)
finalData <- mutate(finalData,dateTime=ymd_hms(finalData$dateTime))

str(finalData)

plot(finalData$dateTime,finalData$Global_active_power, 
     type="l", xlab = "",ylab="Global Active Power (kilowatts)")

dev.copy(png, file = "plot2.png",width=480, height=480)
dev.off()

# plot3
plot(finalData$dateTime,finalData$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
lines(finalData$dateTime,finalData$Sub_metering_2, type = "l",col="red")
lines(finalData$dateTime,finalData$Sub_metering_3, type = "l",col="blue")
legend("topright", lty = c(1,1,1), col = c("black","red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"))

dev.copy(png, file = "plot3.png",width=480, height=480)
dev.off()

# plot4
par(mfrow=c(2,2))
plot(finalData$dateTime,finalData$Global_active_power, 
     type="l", xlab = "",ylab="Global Active Power (kilowatts)")
plot(finalData$dateTime,finalData$Voltage, 
     type="l", xlab = "datetime",ylab="Volatge")
plot(finalData$dateTime,finalData$Sub_metering_1, type = "l", xlab = "", 
     ylab = "Energy sub metering")
lines(finalData$dateTime,finalData$Sub_metering_2, type = "l",col="red")
lines(finalData$dateTime,finalData$Sub_metering_3, type = "l",col="blue")
legend("topright", lty = c(1,1,1), col = c("black","red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),
       bty = "n",cex = 0.8)
plot(finalData$dateTime,finalData$Global_reactive_power, 
     type="l", xlab = "datetime",ylab="Global_reactive_power")

dev.copy(png, file = "plot4.png",width=480, height=480)
dev.off()

# Lattice plotting system ----
# plots are created in one function call
library(lattice)
xyplot() # scatterplots
bwplot() # boxplots
histogram()
stripplot() # boxplot with points
dotplot() # violin string
splom() # scatterplots matrix
levelplot() # plotting image data
contourplot() # plotting image data

library(datasets)
xyplot(Ozone ~ Wind, data = airquality) # simple scatter plot

airquality <- transform(airquality,Month = factor(Month)) # turn month into factor
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1)) # compare ozone and wind for each month

p <- xyplot(Ozone ~ Wind, data = airquality) # nothing happens
print(p)
xyplot(Ozone ~ Wind, data = airquality) # autoprints

set.seed(10)
x <- rnorm(100)
f <- rep(0:1,each=50)
y <- x+f-f*x+rnorm(100, sd=0.5)
f <- factor(f, labels = c("Group 1","Group 2"))
xyplot(y~x | f, layout=c(2,1))
# custom panel function
xyplot(y~x | f, panel = function(x,y,...){
  panel.xyplot(x,y,...)# first call the default panel function
  panel.abline(h=median(y),lty = 2) # add a horizontal line
})

xyplot(y~x | f, panel = function(x,y,...){
  panel.xyplot(x,y,...)# first call the default panel function
  panel.lmline(x,y,col=2) # add a linear regression line
})

library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)

# ggplot2 - qplot ----
library(ggplot2)
# uses grammar of graphics
qplot()# quick plot
ggplot()# best function

# qplot function
str(mpg)
qplot(displ,hwy,data = mpg, color=drv) # aesthetics

qplot(displ,hwy,data = mpg, geom=c("point","smooth")) # adding elements

qplot(hwy, data = mpg, fill = drv, binwidth = 2) # histogram

qplot(drv, hwy, data = mpg, geom = "boxplot")

# facets
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2) # the ".~" sign after and before the elements defines if the facets are shown vertivally or horizontally
qplot(displ, hwy, data = mpg, facets = . ~ drv)

qplot(displ, hwy, data = mpg, facets = . ~ drv) + geom_smooth()
qplot(displ, hwy, data = mpg, facets = . ~ drv, geom = c("point", "smooth")) # the same as above

load(file = "maacs.Rda")
str(maacs)
qplot(log(eno), data = maacs)
qplot(log(eno), data = maacs, fill = mopos)
qplot(log(eno), data = maacs, geom = "density")
qplot(log(eno), data = maacs, geom = "density", color = mopos)
qplot(log(pm25), log(eno), data = maacs, geom = c("point", "smooth"))
qplot(log(pm25), log(eno), data = maacs, shape = mopos)
qplot(log(pm25), log(eno), data = maacs, color = mopos)
qplot(log(pm25), log(eno), data = maacs, color = mopos,
      geom = c("point", "smooth"), method = "lm")
qplot(log(pm25), log(eno), data = maacs, geom = c("point", "smooth"),
      method = "lm", facets = . ~ mopos)

#ggplot2 - ggplot ----
# Components aesthetics, geoms, facets, stats, scales, coordinate systems
maacs2 <- read.csv("bmi_pm25_no2_sim.csv")
str(maacs2)
g <- ggplot(maacs2, aes(logpm25, NocturnalSympt))
summary(g)
class(g)
print(g)# cant print cause we have not ddded layers
g + geom_point()
g + geom_point() + geom_smooth() # by default uses method loess
g + geom_point() + geom_smooth(method = "lm")
g + geom_point() + geom_smooth(method = "lm") + facet_grid(. ~ bmicat)

# modify further commponents - xlab, ylab, labs, ggtitle. 
# each geom has modifying functions
# for global changes use theme(legend.position = "none") 

g + geom_point(color = "steelblue", size = 4, alpha = 1/2) # alpha is transparency
g + geom_point(aes(color = bmicat), size = 4, alpha = 1/2) # we can map aesthetics within the geom
g + geom_point(aes(color = bmicat)) + labs(title = "MAACS Cohort") +
  labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")
g + geom_point(aes(color = bmicat), size = 2, alpha = 1/2) +
  geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)
g + geom_point(aes(color = bmicat)) + theme_bw(base_family = "Times")

# more complex example - How does the relationship between PM2.5 and nocturnal 
# symptoms vary by BMI category and nitrogen dioxide (NO2)?
cutpoints <- quantile(maacs2$logno2_new, seq(0, 1, length = 4), na.rm = TRUE) # finding the tertiles

maacs2$no2tert <- cut(maacs2$logno2_new, cutpoints) # turning into categorical variables
## See the levels of the newly created factor variable
levels(maacs2$no2tert)

## Setup ggplot with data frame
g <- ggplot(maacs2, aes(logpm25, NocturnalSympt))
## Add layers
g + geom_point(alpha = 1/3) +
  facet_wrap(bmicat ~ no2tert, nrow = 2, ncol = 4) +
  geom_smooth(method="lm", se=FALSE, col="steelblue") + # se are the starndar error bars
  theme_bw(base_family = "Avenir", base_size = 10) +
  labs(x = expression("log " * PM[2.5])) +
  labs(y = "Nocturnal Symptoms") +
  labs(title = "MAACS Cohort")

# Axis limits
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100 ## Outlier!
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()
g + geom_line() + ylim(-3, 3) # NOTE: ggplot eliminates all values outside the limits
g + geom_line() + coord_cartesian(ylim = c(-3, 3)) # coord_cartesian, replicates the plot function

# Hierachichal clustering ----
# The point of clustering is to organize things or observations that are close together
#and separate them into groups.
# • How do we define close?
# • How do we group things?
# • How do we visualize the grouping?
# • How do we interpret the grouping?
# The algorithm is recursive and goes as follows:
# 1. Find closest two things points in your dataset
# 2. Put them together and call them a “point”
# 3. Use your new “dataset” with this new point and repeat
# Define a distance measurement
# • Euclidean distance: A continuous metric which can be thought of in geometric
# terms as the “straight-line” distance between two points.
# • Correlation similarity: Similar in nature to Euclidean distance
# • “Manhattan” distance: on a grid or lattice, how many “city blocks” would you have
# to travel to get from point A to point B?

set.seed(1234)
x <- rnorm(12, rep(1:3, each = 4), 0.2)
y <- rnorm(12, rep(c(1, 2, 1), each = 4), 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
dataFrame <- data.frame(x=x, y=y)
dist(dataFrame)
rdistxy <- as.matrix(dist(dataFrame))
## Remove the diagonal from consideration
diag(rdistxy) <- diag(rdistxy) + 100000
# Find the index of the points with minimum distance
ind <- which(rdistxy == min(rdistxy), arr.ind = TRUE)
ind
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "orange", pch = 19, cex = 2)
par(mfrow = c(1, 2))
plot(x, y, col = "blue", pch = 19, cex = 2, main = "Data")
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "orange", pch = 19, cex = 2)
# Make a cluster and cut it at the right height
library(dplyr)
hcluster <- dist(dataFrame) %>% hclust
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro, h = (hcluster$height[1] + 0.00001))
plot(cutDendro$lower[[11]], yaxt = "n", main = "Begin building tree")
nextmin <- rdistxy[order(rdistxy)][3]
ind <- which(rdistxy == nextmin,arr.ind=TRUE)
ind
hClustering <- data.frame(x=x,y=y) %>% dist %>% hclust
plot(hClustering)

# Pretty dendogram function
myplclust <- function(hclust, lab = hclust$labels, 
                      lab.col = rep(1, length(hclust$labels)), 
                      hang = 0.1, ...) {
  ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
  ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
  ## of labels of the leaves of the tree lab.col: colour for the labels;
  ## NA=default device foreground colour hang: as in hclust & plclust Side
  ## effect: A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang, ...)
  text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
       col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}

hClustering <- data.frame(x = x, y = y) %>% dist %>% hclust
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

# heatmap
dataMatrix <- data.frame(x=x,y=y) %>% data.matrix
heatmap(dataMatrix)

# Plotting with colors in R ----
par(mfrow = c(1, 1))
set.seed(19)
x <- rnorm(30)
y <- rnorm(30)
plot(x, y, col = rep(1:3, each = 10), pch = 19)
legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")

par(mfrow = c(1, 2))
image(volcano, col = heat.colors(10), main = "heat.colors()")
image(volcano, col = topo.colors(10), main = "topo.colors()")

# color utilities in R
sample(colors(),10) # 657 predefined colors in colors()
pal <- colorRamp(c("red", "blue")) # gives a series of combination for RGB red green blue
pal(0) # red
pal(1) # blue 
pal(0.5) # purple-ish
pal(seq(0, 1, len = 10)) # Six vectors (each of length 3) are returned

pal <- colorRampPalette(c("red", "yellow"))
## Just return red and yellow
pal(2) # hexadecimal representation of colors
pal(10) # 10 colors between red and yellow
0xCC # this allows to transform hexadecimal into colorRamp values
0x33 # another example
color.scale?????

rgb(0, 0, 234, maxColorValue = 255) # this generates any color for RBG and return the hexidecimal representation
?rbg # fourth argument is alpha, to be a logical or a numerical value
p3 <- colorRampPalette(c("blue","green"),alpha=.5) # Alpha represents an opacity level
p3(5)

library(RColorBrewer)
display.brewer.all()

cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

set.seed(1)
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

# adding transparency
rgb(1, 0, 0, 0.1)
set.seed(2)
x <- rnorm(2000)
y <- rnorm(2000)
plot(x, y, pch = 19)
plot(x, y, pch = 19, col = rgb(0, 0, 0, 0.15))




