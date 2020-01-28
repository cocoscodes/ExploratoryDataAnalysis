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
pal <- colorRamp(c("red", "blue")) # gives a series of combination for RGB red green blue
pal(0) # red
pal(1) # blue 
pal(0.5) # purple-ish
pal(seq(0, 1, len = 10))

pal <- colorRampPalette(c("red", "yellow"))
## Just return red and yellow
pal(2) # hexadecimal representation of colors
pal(10) # 10 colors between red and yellow

rgb(0, 0, 234, maxColorValue = 255) # this generates any color for RBG and return the hexidecimal representation

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



