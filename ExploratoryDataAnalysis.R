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
# exploratory grpahs are ro personally understand the data, not for presentation purposes

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

# other resources: https://www.r-graph-gallery.com/index.html



























