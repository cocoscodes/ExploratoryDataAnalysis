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

# Hierachichal clusters
# complete linkage - take the greatest distance between the pairs of points in those two clusters
# Average linkage - First you compute an "average" point in each cluster, Then you compute the distances between each cluster average to compute the intercluster distance.
# the number of clusters depends on the distance we decide to cut it

# k means
kmeans(dataFrame,centers = 3)

# Dimensions reduction
# As data scientists, we'd like to find a smaller set of multivariate variables that are
# uncorrelated AND explain as much variance (or variability) of the data as possible. This is a
# statistical approach.
# Two related solutions to these problems are PCA which stands for Principal Component Analysis
# and SVD, Singular Value Decomposition. This latter simply means that we express a matrix X of
# observations (rows) and variables (columns) as the product of 3 other matrices, i.e., X=UDV^t.
# This last term (V^t) represents the transpose of the matrix V.

svd(mat)
matu%*%diag%*%t(matv)
# We'll demonstrate this now. First we have to scale mat, our simple example data matrix.  This
# means that we subtract the column mean from every element and divide the result by the column
# standard deviation. 
svd(scale(mat))
prcomp(scale(mat)) # principal componets

svd1$v[,1]
a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1])
a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])
myImage(svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10]))

# Clustering example
dim(ssd)
names(ssd[,562:563])
table(ssd$subject)
sum(table(ssd$subject))
table(ssd$activity)
sub1 <- subset(ssd,subject==1)
dim(sub1)
names(sub1[,1:12])

par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright",legend=unique(sub1$activity),col=unique(sub1$activity), pch = 1)
par(mfrow=c(1,1))
showMe(1:6)

mdist <- dist(sub1[,1:3])
hclustering <- hclust(mdist)
myplclust(hclustering,lab.col = unclass(sub1$activity))

mdist <- dist(sub1[,10:12])
hclustering <- hclust(mdist)
myplclust(hclustering,lab.col = unclass(sub1$activity))

svd1 <- svd(scale(sub1[,-c(562,563)]))
maxCon <- which.max(svd1$v[,2])
mdist <- dist(sub1[,c(10:12,maxCon)])
hclustering <- hclust(mdist)
myplclust(hclustering,lab.col = unclass(sub1$activity))

names(sub1[maxCon])
kClust <- kmeans(sub1[,-c(562:563)],centers = 6)
table(kClust$cluster,sub1$activity)

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart=100)
table(kClust$cluster, sub1$activity)

dim(kClust$centers)
laying <- which(kClust$size==29)
plot(kClust$centers[laying,1:12],pch=19,ylab = "Laying Cluster")
names(sub1[,1:3])

walkdown <- which(kClust$size==49)
plot(kClust$centers[walkdown,1:12],pch=19,ylab = "Walkdown Cluster")
  
# Case study
pm0 # 1999 data
dim(pm0)
head(pm0)
cnames
cnames <- strsplit(cnames,"|",fixed = TRUE)
names(pm0) <- make.names(cnames[[1]][wcol])
head(pm0)
x0 <- pm0$Sample.Value # pm2.5 values
str(x0)
mean(is.na(x0)) # % of NAs

names(pm1) <- make.names(cnames[[1]][wcol]) # 2012 data
dim(pm1)
x1 <- pm1$Sample.Value # pm2.5 values
mean(is.na(x1)) # % of NAs

summary(x0)
summary(x1)
boxplot(x0,x1)
boxplot(log10(x0),log10(x1)) # faltted the curve by deleting the extreme values, error message given negative values

negative <- x1<0
sum(negative,na.rm = TRUE) # count values
mean(negative,na.rm = TRUE) # % of NAs values
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates),"%Y%m%d")
head(dates)
hist(dates[negative],"month")

str(site0)
str(site1)
both <-intersect(site0,site1) # nyc values intersect for btoh years
both

head(pm0)
cnt0 <- subset(pm0,State.Code==36 & county.site %in% both)
cnt1 <- subset(pm1,State.Code==36 & county.site %in% both)
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

pm0sub <- subset(cnt0,County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(cnt1,County.Code == 63 & Site.ID == 2008)

x0sub <- pm0sub$Sample.Value
x1sub <- pm1sub$Sample.Value

dates0 <- as.Date(as.character(pm0sub$Date),"%Y%m%d")
dates1 <- as.Date(as.character(pm1sub$Date),"%Y%m%d")

par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(dates0,x0sub,pch=20)
abline(h=median(x0sub,na.rm = TRUE),lwd=2)
plot(dates1,x1sub,pch=20)
abline(h = median(x1sub, na.rm = TRUE),lwd=2)

rng <- range(x0sub,x1sub,na.rm = TRUE)
rng

mn0 <- with(pm0,tapply(Sample.Value,State.Code, mean,na.rm=TRUE))
str(mn0)
mn1 <- with(pm1,tapply(Sample.Value,State.Code, mean,na.rm=TRUE))
str(mn1)
summary(mn0)
summary(mn1)

d0 <- data.frame(state = names(mn0),mean=mn0)
d1 <- data.frame(state = names(mn1),mean=mn1)
mrg <- merge(d0,d1,by="state")
dim(mrg)
head(mrg)
with(mrg,plot(rep(1,52),mrg[,2],xlim = c(0.5,2.5)))
with(mrg, points(rep(2, 52), mrg[, 3]))
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])
mrg[mrg$mean.x < mrg$mean.y, ] # states taht got worse
