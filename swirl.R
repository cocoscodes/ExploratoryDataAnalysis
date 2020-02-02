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

# ggplot2
str(mpg)
qplot(displ,hwy,data=mpg,color=drv,geom=c("point","smooth"))
qplot(y=hwy,data = mpg,color=drv) # no x value, it uses the X-axis to order the elements from the 1st to the last
qplot(drv,hwy,data = mpg,geom = "boxplot",color=manufacturer)

g <- ggplot(mpg,aes(displ,hwy,color=factor(year)))
g+geom_point()
g+geom_point()+facet_grid(drv~cyl,margins = TRUE)
g+geom_point()+facet_grid(drv~cyl,margins = TRUE)+
  geom_smooth(method = "lm",se=FALSE,size=2,color="black")
g+geom_point()+facet_grid(drv~cyl,margins = TRUE)+
  geom_smooth(method = "lm",se=FALSE,size=2,color="black")+
  labs(x="Displacement",y="Highway Mileage",title = "Swirl Rules!")

str(diamonds)
qplot(price,data = diamonds) # histogram
range(diamonds$price) # minimum and max prices
qplot(price,data=diamonds,binwidth=18497/30)
qplot(price,data=diamonds,binwidth=18497/30,fill=cut)

qplot(price,data=diamonds,geom = "density") # density chart
qplot(price,data=diamonds,geom = "density",color=cut)

qplot(carat,price,data = diamonds) #scatterplots
qplot(carat,price,data = diamonds,shape=cut) # shape changes the point characters
qplot(carat,price,data = diamonds,color=cut)
qplot(carat,price,data = diamonds,color=cut)+geom_smooth(method = "lm")
qplot(carat,price,data = diamonds,color=cut,facets = .~cut)+geom_smooth(method = "lm")

g <- ggplot(diamonds,aes(depth,price))
summary(g)
g+geom_point(alpha=1/3)
cutpoints <- quantile(diamonds$carat,seq(0,1,length = 4),na.rm = TRUE) 
cutpoints
diamonds$car2 <- cut(diamonds$carat,cutpoints) # new variable with carats as categorical variable
g <- ggplot(diamonds,aes(depth,price)) # resetting g to have the new column
g + geom_point(alpha=1/3)+facet_grid(cut~car2)
g + geom_point(alpha=1/3)+facet_grid(cut~car2)+geom_smooth(method = "lm",size=3,color="pink")

ggplot(diamonds,aes(carat,price))+geom_boxplot()+facet_grid(.~cut)


