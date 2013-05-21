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

convertpoints <- function(lat,lon,alt=1,rad=1)
{
  lat <- lat*(pi/180)
  lon <- lon*(pi/180)
  x <- rad*sin(lat)*cos(lon)
  y <- rad*sin(lat)*sin(lon)
  z <- rad*cos(lat)
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

worldfull <- cshp()
plot(worldfull)
test$CNTRY_NAME
#worldfull <- gSimplify(worldfull,1)
points <- c(NULL,NULL,NULL,NULL)
for(i in 1:length(worldfull))
{
  for(j in 1:length(worldfull@polygons[[i]]@Polygons))
  {
    points <- rbind(points, cbind((worldfull@polygons[[i]])@Polygons[[j]]@coords[,1],(worldfull@polygons[[i]])@Polygons[[j]]@coords[,2],i,j))
  }
}
points2 <- apply(points,1,FUN=function(x) convertpoints(x[1],x[2]))
points2 <- t(points2)
scatterplot3d(points2[,1],points2[,2],points2[,3],pch=".")


worldfull <- readShapePoly("C:\\users\\ben\\my documents\\github\\RGlobe\\Data\\Azimuth_LoRes_Nations")
worldfull$Continent[is.na(worldfull$Continent)] <- "Europe"
plot(worldfull)
points <- c(NULL,NULL,NULL,NULL)
for(i in 1:length(worldfull))
{
  if(worldfull$Continent[i]!="Antarctica")
  {
    for(j in 1:length(worldfull@polygons[[i]]@Polygons))
    {
      points <- rbind(points, cbind((worldfull@polygons[[i]])@Polygons[[j]]@coords[,1],(worldfull@polygons[[i]])@Polygons[[j]]@coords[,2],i,j))
    }
  }
}
summary(points)
points2 <- apply(points,1,FUN=function(x) convertpoints(x[1],x[2]))
points2 <- t(points2)
scatterplot3d(points2[,1],points2[,2],points2[,3],pch=".")

ex <- 0; ey <- 0; ez <- -16;

x11()
plot(1,1,type="n",xlim=c(-10,10),ylim=c(-10,10))
checkForKey <- function()
{
  getGraphicsEvent(onKeybd=keydown)
  par(ask=FALSE)
}
keydown <- function(key)
{
  if(key=="Up")
    points2 <<- rotatey(points2,deg=355)
  if(key=="Down")
    points2 <<- rotatey(points2,deg=5)
  if(key=="Left")
    points2 <<- rotatez(points2,deg=355)
  if(key=="Right")
    points2 <<- rotatez(points2,deg=5)
  if(key=="=")
    points2 <<- rotatex(points2,deg=5)
  if(key=="-")
    points2 <<- rotatex(points2,deg=355)
  dev.control(displaylist="inhibit")
  dev.control(displaylist="enable")
  rect(-10,-10,10,10,col="white")
  points(project(points2)[,1],project(points2)[,2],pch=".")
  print(c(ex,ey,ez))
  checkForKey()
}
points(project(points2)[,1],project(points2)[,2],pch=".")
checkForKey()
