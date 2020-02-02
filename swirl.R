library(swirl)
install_from_swirl("Exploratory Data Analysis")
swirl()
# https://github.com/DataScienceSpecialization/courses/

# lattice
xyplot(Ozone ~ Wind, data = airquality, pch=8, col="red", main="Big Apple Data")
xyplot(Ozone ~ Wind | as.factor(Month), data = airquality,layout=c(5,1))
xyplot(Ozone ~ Wind | Month, data = airquality,layout=c(5,1))
p <- xyplot(Ozone ~ Wind, data = airquality)
names(p) # 45 properties
p[["formula"]] # evaluates this property
p[["x.limits"]]

xyplot(y~x|f,layout=c(2,1)) # not providing explicitly data argument, so xyplot will look in the environment

data("diamonds")
head(diamonds)
xyplot(price~carat|color*cut,data = diamonds,strip = FALSE,pch=20, # strip will label each panel
       xlab = myxlab,ylab = myylab,main=mymain)



