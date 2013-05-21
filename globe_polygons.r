#install.packages("scatterplot3d")
library(scatterplot3d)
#install.packages("maptools")
library(maptools)
#install.packages("mapproj")
library(mapproj)
#install.packages("shapefiles")
library(shapefiles)
#install.packages("rgeos")
library(rgeos)
#install.packages("cshapes")
library(cshapes)
#install.packages("plotrix")
library(plotrix)

###############################################################
# convertpoints <- function(lat, lon, alt=1, rad=1)
# {
#   lat <- lat * (pi/180)
#   lon <- lon * (pi/180)
#   x <- -rad * cos(lat) * cos(lon)
#   y <- rad * sin(lat) 
#   z <- rad * cos(lat) * sin(lon)
#   return(cbind(x,y,z))  
# }

# convertpoints <- function(lat,lon,alt=1,rad=1)
# {
#   lat <- lat*(pi/180)
#   lon <- lon*(pi/180)
#   x <- rad*sin(lat)*cos(lon)
#   y <- rad*sin(lat)*sin(lon)
#   z <- rad*cos(lat)
#   return(cbind(x,y,z))
# }

plotpoly <- function(object)
{
  if(sum(object[,3]>=0)>2)
  {
    poly <- project(object[object[,3]>=0,])
    polygon(poly,col="white",border="blue")
  }
}

getradius <- function()
{
  equator <- cbind(rep(0,180),(1:180)-180)
  equator3d <- convertpoints(equator[,2],equator[,1])
  equator2d <- project(equator3d)
  return(max(abs(equator2d))) 
}

convertpoints <- function(lon,lat,alt=1,rad=1)
{
  phi <- (lat)*pi/180
  theta <- (lon)*pi/180
  x <- -rad*cos(phi)*cos(theta)
  y <- rad*sin(phi)
  z <- rad*cos(phi)*sin(theta)
  return(cbind(x,y,z))
}

project <- function(object)
{
  sx1 <- ((object[,1]-ex)*(100))/(object[,3]-ez)+ex
  sy1 <- ((object[,2]-ey)*(100))/(object[,3]-ez)+ey
  projectedpoints <- cbind(sx1,sy1)
  return(projectedpoints)
}

rotatex <- function(object,deg=1)
{
  rad <- deg*pi/180
  rotmat <- matrix(c(cos(rad),-sin(rad),0,sin(rad),cos(rad),0,0,0,1),nrow=3,ncol=3,byrow=T)
  object <- as.matrix(object[,1:3]) %*% rotmat
  return(object)
  return(object)
}

rotatez <- function(object,deg=1)
{
  rad <- deg*pi/180
  rotmat <- matrix(c(cos(rad),0,sin(rad),0,1,0,-sin(rad),0,cos(rad)),nrow=3,ncol=3,byrow=T)
  object <- as.matrix(object[,1:3]) %*% rotmat
  return(object)
  return(object)
}

rotatey <- function(object,deg=1)
{
  rad <- deg*pi/180
  rotmat <- matrix(c(1,0,0,0,cos(rad),-sin(rad),0,sin(rad),cos(rad)),nrow=3,ncol=3,byrow=T)
  object <- as.matrix(object[,1:3]) %*% rotmat
  return(object)
}

################################################################

worldfull <- cshp(date=as.Date("2012-06-30"))
worldfull <- gSimplify(worldfull,1)

worldfull <- readShapePoly("C:\\users\\ben\\my documents\\github\\RGlobe\\Data\\Azimuth_LoRes_Nations")
worldfull2 <- gSimplify(worldfull,1)

(worldfull$Nation)
names(worldfull2)
plot(worldfull)
points <- c(NULL,NULL,NULL,NULL)
points <- list()
for(i in 1:length(worldfull))
{
  for(j in 1:length(worldfull@polygons[[i]]@Polygons))
  {
    points[[length(points)+1]] <- cbind((worldfull@polygons[[i]])@Polygons[[j]]@coords[,1],(worldfull@polygons[[i]])@Polygons[[j]]@coords[,2],i,j)
  }
}
points2 <- lapply(points,FUN=function(x) convertpoints(lon=x[,1],lat=x[,2]))
#points2 <- t(points2)
#scatterplot3d(points2[,1],points2[,2],points2[,3],pch=".")


# worldfull <- readShapePoly("C:\\users\\ben\\my documents\\github\\RGlobe\\Data\\Azimuth_LoRes_Nations")
# worldfull$Continent[is.na(worldfull$Continent)] <- "Europe"
# worldfull <- gSimplify(worldfull,.5)
# plot(worldfull)
# axis(1);axis(2)
# points <- c(NULL,NULL,NULL,NULL)
# for(i in 1:length(worldfull))
# {
#   #if(worldfull$Continent[i]!="Antarctica")
#   {
#     for(j in 1:length(worldfull@polygons[[i]]@Polygons))
#     {
#       points <- rbind(points, cbind((worldfull@polygons[[i]])@Polygons[[j]]@coords[,1],(worldfull@polygons[[i]])@Polygons[[j]]@coords[,2],i,j))
#     }
#   }
# }
# summary(points)
# points2 <- apply(points,1,FUN=function(x) convertpoints(lon=x[1],lat=x[2]))
# points2 <- t(points2)
# scatterplot3d(points2[,1],points2[,2],points2[,3],pch=".")

ex <- 0; ey <- 0; ez <- 16;
stars <- cbind(runif(100,-10,10),runif(100,-10,10))
x11(width=8,height=8)
par(mar=c(0,0,0,0))
plot(1,1,type="n",xlim=c(-10,10),ylim=c(-10,10))
checkForKey <- function()
{
  getGraphicsEvent(onKeybd=keydown)
  par(ask=FALSE)
}
keydown <- function(key)
{
  if(key=="Up")
    points2 <<- lapply(points2,FUN=function(x) rotatey(x,deg=5))
  if(key=="Down")
    points2 <<- lapply(points2,FUN=function(x) rotatey(x,deg=-5))
  if(key=="Left")
    points2 <<- lapply(points2,FUN=function(x) rotatez(x,deg=5))
  if(key=="Right")
    points2 <<- lapply(points2,FUN=function(x) rotatez(x,deg=-5))
  if(key=="=")
    points2 <<- lapply(points2,FUN=function(x) rotatex(x,deg=-5))
  if(key=="-")
    points2 <<- lapply(points2,FUN=function(x) rotatex(x,deg=5))
  if(key=="e")
    ez <<- max(5,ez+.5)
  if(key=="d")
    ez <<- max(5,ez-.5)
  dev.control(displaylist="inhibit")
  dev.control(displaylist="enable")
  rect(-10,-10,10,10,col="black",lty=0)
  points(stars[,1],stars[,2],pch=".",col="white")
  polygon(cbind(getradius()*cos((1:360)*pi/180),getradius()*sin((1:360)*pi/180)),lty=0,col="#2790DB")

  lapply(points2, FUN=function(x) plotpoly(x))
  
  #points(project(points2)[points2[,3]>0,1],project(points2)[points2[,3]>0,2],pch=".",col="white")
  checkForKey()
}
lapply(points2, FUN=function(x) plotpoly(x))
checkForKey()

points <- cbind(points,paste(points[,3],points[,4],sep="_"))
plot(1,1,type="n",ylim=c(-90,90),xlim=c(-180,180))
for(i in unique(points[,5]))
  polygon(points[points[,5]==i,])
points[1:100,]

plot(1,1,type="n",ylim=c(-10,10),xlim=c(-10,10))
circle <- cbind(3*cos((1:360)*pi/180),3*sin((1:360)*pi/180))
polygon(cbind(rad*cos((1:360)*pi/180),rad*sin((1:360)*pi/180)),lty=0,col="blue")


Sys.getenv('NUMBER_OF_PROCESSORS') 
install.packages("multicore")
library(multicore)