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
rug(pollution$pm25) # adds exac location in the histogram undernead

hist(pollution$pm25, col = "green")
abline(v=12, lwd=2)
abline(v=median(pollution$pm25), col= "magenta",lwd=4)

barplot(table(pollution$region),col = "wheat", main = "Number of Counties in Each Region")

install.packages("maps")
library(maps)
map("county", "california")
with(filter(pollution, pm25> 15) , points(longitude, latitude)) # not working
