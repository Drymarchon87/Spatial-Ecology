#######################################################
##                SPATIAL ANALYSES CODE              ##
##                                                   ##
##                     M. Pierson                    ##
#######################################################

## I recommend using packrat so that all code and packages are contained and can be 
## run on any version of R. If you do not use packrat there is a chance that later versions
## of R will not be compatible with the current packages and could cripple your ability to 
## run certain codes at later dates. 

## This is an example, modify your pathway as needed:
# packrat::init("~/Dropbox/Gila Monster Data/GM_Study")

########################
## SPATIAL ANALYSES   ##
########################

# required packages
library(adehabitatHR) #for home range calculations
library(data.table) #manipulate S3 and S4 data tables
library(ggplot2) #for graphic output
library(ggfortify) #to allow ggplot2 to read spatial data
library(grid) #to add annotations to the output
# library(OpenStreetMap) #for obtaining raster images
library(pbapply) #needed for progress bar
library(plotly) #for interactive xy plot
library(rgdal) #for converting spatial data
library(sp) #for converting spatial data
library(rgeos)
# library(raster)
library(mapview)

############################################
##          TRACKING INTENSITY            ##
############################################

library(tidyverse)
library(lubridate)
library(plotly)
library(scales)


TrackData <- read.csv("SC_movement.csv")
View(TrackData)

TrackData$DATE <- mdy(TrackData$DATE) #use lubridate to specify incoming date format; 
#tell lubridate that the current format is "mdy", it then converts column to "ymd" format
str(TrackData) #look at DATE format

pT <- ggplot(TrackData, aes(DATE, LIZARDNUMBER)) +
  geom_count (color = "blue") + #set symbol size proportional to # of overlapping observations (same day)
  scale_x_date(date_breaks = "12 month", labels = date_format("%Y"))
pT

########################
## PLOT ALL animals:  ##
########################

## I have left my file names etc intact so that you can see them as an example, you will 
## just have to replace the rquired fields with your data and or objects.

#####################################################################
## Plot spdf of animal locations, individuals or all animals as needed

# Load .csv file and assighn to object:
GM.df<-read.csv("GM_Final_Data.csv")

# set map projection string, you can change the object name as you wish, but you need to go 
# within each looping function to change that proj4string (I recommend not to):
CRS.SC<-CRS("+proj=utm +zone=12 +ellps=WGS84 +units=m +no_defs")

# Using the function "SpatialPoints" we create an object of class SpatialPoints.
# We have to specify the coordinates, whereas the bbox is automatically generated.
GM.sp <- SpatialPoints(GM.df[,c("EASTING","NORTHING")], proj4string=CRS.SC)

# can easily plot it
plot(GM.sp, axes=TRUE)
# View(GM.df)
GM.spdf <- SpatialPointsDataFrame(coords=GM.df[,6:7], data=GM.df[,1:4], proj4string=CRS.SC)

plot(GM.spdf,axes=TRUE)

plot(GM.spdf, axes=TRUE, pch=21, cex=.2) # change point size

# 4.2 Advanced plotting with spplot()
spplot(GM.spdf, axes=TRUE, pch=21, cex=.2, zcol="YEAR", col.regions=rainbow(7))

## Advanced mapping with mapview. Mapview is an interactive mapping tool that has several 
## different map types you can manually cycle through.You can also assign only those maps that 
## you wish to have in your map bank.:
mapview(GM.spdf)

mapview(GM.spdf, cex=.2, zcol="LIZARDNUMBER", col.regions=rainbow(18), legend=F)

#########################################
## Plot all individuals for HR analyses

#load data with fake date and time
data <- read.csv("GM_Final_Data.csv")
View(data)

#interactive examination of data points for outliers with Plotly
p <- ggplot() + geom_point(data=data, aes(EASTING,NORTHING, color=LIZARDNUMBER)) +
  labs(x="Easting", y="Northing")
ggplotly(p)

#split into multiple files for individuals
lapply(split(data, data$LIZARDNUMBER),
       function(x)write.csv(x, file = paste(x$LIZARDNUMBER[1],".csv"), row.names = FALSE))

#create list of individual files created in previous step #edit pattern based on individual ids
files <- list.files(path = ".", pattern = "[MF]+[0-9]", full.names = TRUE)

#creating spatial data frame for all points
x <- as.data.frame(data$EASTING)
y <- as.data.frame(data$NORTHING)
xy <- c(x,y)
data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS.SC)

#creating homerange in adehabitat for all points
xy <- SpatialPoints(data.proj@coords)
mcp.out <- mcp(xy, percent=100, unout="ha")
#plot(xy)
#plot(mcp.out)
mcp_area <- as.data.frame(mcp.out@data$area)
colnames(mcp_area) <- "Hectares"
write.table(mcp_area, "MCP_Area.csv", sep = ",", row.names = TRUE)

#KDE creation for all points
kde<-kernelUD(xy, h="href", kern="bivnorm", grid=1000)
ver <- getverticeshr(kde, 95)
ver$area
kde@h$h
#plot(data.proj@coords)
#plot(ver)
kde_area <- as.data.frame(ver$area)
colnames(kde_area) <- "Hectares"
write.table(kde_area, "KDE_Area.csv", sep = ",", row.names = TRUE)

#mcp plot for all points
mcp.points <- cbind((data.frame(xy)),data$LIZARDNUMBER)
colnames(mcp.points) <- c("x","y", "lizardnumber")
mcp.poly <- fortify(mcp.out, region = "id")
mcp.plot <- ggplot()+
  geom_polygon(data=mcp.poly, aes(x=mcp.poly$long, y=mcp.poly$lat))+
  geom_point(data=mcp.points, aes(x=x, y=y,color = lizardnumber))
mcp.plot

#kde plot for all points
kde.points <- cbind((data.frame(data.proj@coords)),data$LIZARDNUMBER)
colnames(kde.points) <- c("x","y","lizardnumber")
kde.poly <- fortify(ver, region = "id")
kde.plot <- ggplot()+
  geom_polygon(data=kde.poly, aes(x=kde.poly$long, y=kde.poly$lat))+
  geom_point(data=kde.points, aes(x=x, y=y, color = lizardnumber))
kde.plot

########################## INDIVIDAL ANALYSES #########################
## BY YEAR
## MCP without raster
## Code set to run individual lizard by ID. Files for "by year" analyses are set in ID 
## subfolders. You can manipulate these groupings specifically to your needs. For example: by 
## year, month, season, etc.
############################

# BREAK DOWN INDIVIUAL ANIMAL DATA BY YEAR and or SEASON, *NOTE MUST GO INTO DROPBOX FOR EACH ONE AND 
# MOVE "YEAR" FILES TO THEIR INDIVIDUAL FOLDERS (if stored in DropBox).
setwd("~/Dropbox/Gila Monster Data/GM_Study/F104")
library(readr)
M112<-read_csv("M112 .csv")
View(F66)

## split into multiple files for individual for YEAR:
lapply(split(M112, M112$YEAR),
       function(x)write.csv(x, file = paste(x$YEAR[3],".csv"), row.names = FALSE))

## split into multiple files for individual and for SEASON:
lapply(split(F104, F104$SEASON),
       function(x)write.csv(x, file = paste(x$SEASON[4],".csv"), row.names = FALSE))

# remove unwanted objects:
rm(F36,M215) # be careful, this works and you will loose these object you assign to this function:

###################################################
##        INDIVIDUAL SPATIAL ANALYSIS            ## 
###################################################
## NOTE: Depending on how you structure your data, you can run analyses by the batch wihtout having to 
## individually run manually. If you need to run an analyses on a single animal, year, season, etc.
## then you will need to set working directory to that specific file for that animal if previously 
## seperated. 
setwd("~/Dropbox/Gila Monster Data/GM_Study/M67")

###############################
# Plot spdf of animal locations
M67.df<-read.csv("M67 .csv")

# Using the function "SpatialPoints" we create an object of class SpatialPoints.
# We have to specify the coordinates, whereas the bbox is automatically generated.
# M67.sp <- SpatialPoints(M67.df[,c("EASTING","NORTHING")], proj4string=CRS.SC)

# can easily plot it
# plot(M67.sp, axes=TRUE)
# View(GM.df)
M67 <- SpatialPointsDataFrame(coords=M67.df[,6:7], data=M67.df[,1:4], proj4string=CRS.SC)

# plot(M67,axes=TRUE)

plot(M67, axes=TRUE, pch=21, cex=.5) # change point size

# 4.2 Advanced plotting with spplot()
spplot(M67, axes=TRUE, pch=21, cex=.5, zcol="YEAR", col.regions=rainbow(7))

# Advanced mapping with mapview
mapview(M67)

# mapview(M67, cex=.5, zcol="LIZARDNUMBER", col.regions=rainbow(1), legend=F)


#########################
##        MCP          ## 
#########################
## RUN THE LOOPING FUNCTION BELOW TO CREATE THE FUNCTION NEEDED TO CREATE POLYGONS, THEN TURN 
## THE LOOPING FUNCTION OFF.

# Set projection forestring, only if needed:
#CRS.SC<-CRS("+proj=utm +zone=12 +ellps=WGS84 +units=m +no_defs")

# SET WORKING DIRECTORY TO INDIDUAL SUBFOLDER
library(readr)
# setwd("~/Dropbox/Gila Monster Data/GM_Study/M67")
# F104<-read_csv("F104 .csv")
# View(F104)

# Function for running animal MCP, set percentage to percentage of points to be used for 
## analysis:
## individual mcp run
mcp_analysis("./2007 .csv", percentage=100)
mcp_analysis("./2007 .csv", percentage=95)

## Batch process individuals for mcps, takes ~12sec
# lapply(files,mcp_analysis)
# pblapply(files, mcp_analysis) #runs with progressbar

###########################################################################################
## looping function for mcp ******BY YEAR******

# mcp_analysis <- function(filename, percentage){
#   data <- read.csv(file = filename)
#   x <- as.data.frame(data$EASTING)
#   y <- as.data.frame(data$NORTHING)
#   xy <- c(x,y)
#   data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS.SC)
#   xy <- SpatialPoints(data.proj@coords)
#   mcp.out <- mcp(xy, percentage, unout="ha")
#   area <- as.data.frame(round(mcp.out@data$area,4))
#   .rowNamesDF(area, make.names=TRUE) <- data$YEAR
#   write.table(area,file="MCP_Hectares.csv",
#               append=TRUE,sep=",", col.names=FALSE, row.names=TRUE)
#   mcp.points <- cbind((data.frame(xy)),data$YEAR)
#   colnames(mcp.points) <- c("x","y", "year")
#   mcp.poly <- fortify(mcp.out, region = "id")
#   units <- grid.text(paste(round(mcp.out@data$area,2)," ha"), x=0.9,  y=0.95,
#                      gp=gpar(fontface=4, cex=0.9), draw = FALSE)
#   mcp.plot <- ggplot() +
#     geom_polygon(data=mcp.poly, aes(x=mcp.poly$long, y=mcp.poly$lat), alpha=0.5) +
#     geom_point(data=mcp.points, aes(x=x, y=y)) + theme_bw() +
#     labs(x="Easting (m)", y="Northing (m)", title=mcp.points$year) +
#     theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5)) +
#     annotation_custom(units)
#   mcp.plot
# }

## looping function for mcp ******BY YEAR******
###########################################################################################



####################################################
### *****WORKING***** MCP Polygon *by year  
####################################################

M67_MCP<-mcp_analysis.POLY("./M67 .csv", percentage= 100)
M67_MCP

###########################################################################################
##  ***USE THIS*** FOR MCP POLYGON OUTPUTS FOR INTERSECT CALCULATIONS AND MAPPING PURPOSES:
## RUN THE LOOPING FUNCTION TO CREATE THE FUNCTION NEEDED TO CREATE POLYGONS, THEN TURN THE 
## LOOPING FUNCTION OFF.

## Set working directory as needed:
setwd("~/Dropbox/Gila Monster Data/GM_Study/M112")
## File name = X_MCP

## Use mapviewOption() to specify which map types you wish to have in map bank:
# mapviewOptions() #run to view function options and slots.
mapviewOptions(basemaps = c("OpenStreetMap","Esri.WorldImagery","OpenTopoMap"),
               na.color = "magenta",
               layers.control.pos = "topleft")

M112_MCP<-mcp_analysis.POLY("./M112 .csv", percentage= 100)
mapView(M112_MCP, zcol="id")

############################################################################################
#################### looping function #################### looping function ################

# mcp_analysis.POLY <- function(filename, percentage){
#   data <- read.csv(file = filename,stringsAsFactors = FALSE)
#   data.sp <- data[, c("LIZARDNUMBER", "EASTING", "NORTHING")]
#   coordinates(data.sp) <- c("EASTING", "NORTHING")
#   proj4string(data.sp) <- CRS.SC
#   mcp_out <- mcp(data.sp, percentage, unout="ha")
# }

#################### looping function  ################### looping function ################
############################################################################################


########################################################
##                    KDE ANALYSES                    ##
##                  *** WORKING ***                   ##
########################################################

## RUN THE LOOPING FUNCTION TO CREATE THE FUNCTION NEEDED TO CREATE POLYGONS, THEN TURN THE 
## LOOPING FUNCTION OFF.

# Set working directory if needed:
#setwd("~/Dropbox/Gila Monster Data/GM_Study/F114")

## Function for running animal KDE:
kde_analysis.href.plot("./M112 .csv", percentage=95)
## Batch process:
#lapply(files,kde_analysis.href.plot, percentage=95)

###################################################
##  looping function for kde ****href bandwidth****

########################################################################################
################# looping function ################### looping function ################

# kde_analysis.href.plot <- function(filename, percentage){
#   data <- read.csv(file = filename)
#   x <- as.data.frame(data$EASTING)
#   y <- as.data.frame(data$NORTHING)
#   xy <- c(x,y)
#   data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS.SC)
#   xy <- SpatialPoints(data.proj@coords)
#   kde<-kernelUD(xy, h="href", kern="bivnorm", grid=1000)
#   ver <- getverticeshr(kde, percentage)
#   area <- as.data.frame(round(ver$area,4))
#   .rowNamesDF(area, make.names=TRUE) <- data$YEAR
#   write.table(area,file="KDE_Hectares.csv",
#               append=TRUE,sep=",", col.names=FALSE, row.names=TRUE)
#   kde.points <- cbind((data.frame(data.proj@coords)),data$YEAR)
#   colnames(kde.points) <- c("x","y","year")
#   kde.poly <- fortify(ver, region = "id")
#   units <- grid.text(paste(round(ver$area,2)," ha"), x=0.9,  y=0.95,
#                      gp=gpar(fontface=4, cex=0.9), draw = FALSE)
#   kde.plot <- ggplot() +
#     geom_polygon(data=kde.poly, aes(x=kde.poly$long, y=kde.poly$lat), alpha = 0.5) +
#     geom_point(data=kde.points, aes(x=x, y=y)) + theme_bw() +
#     labs(x="Easting (m)", y="Northing (m)", title=kde.points$year) +
#     theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5)) +
#     annotation_custom(units)
#   kde.plot
# }

# individual kde with ***href*** run Plot, with change to "percentage"
#kde_analysis.href.plot("./2008 .csv", percentage=95)
# run all individuals for kde, takes 3min
#lapply(files,kde_analysis, percentage=95)     # **NOT TESTED**
#pblapply(files, kde_analysis, percentage=95)  #runs with progressbar  **NOT TESTED**


######################################################################
######  kde POLYGON  href, by year or by all years
######################################################################
## RUN THE LOOPING FUNCTION TO CREATE THE FUNCTION NEEDED TO CREATE POLYGONS, THEN TURN THE 
## LOOPING FUNCTION OFF.

# set working directory for individual animals if needed:
#setwd("~/Dropbox/Gila Monster Data/GM_Study/M69")

M112_KDEPoly<-kde_analysis.href.polygon("./M112 .csv", percentage= 95)
mapView(M112_KDEPoly)
F36_95<-kde_analysis.href.polygon("./F36 .csv", percentage= 95)
#plot(F36_95)
mapView(F36_95)

## change polygon opacity using alpha.regions=x in mapview:
mapView(F36_95, alpha.regions=0.3)


###################################
##  Mapview with multiple polygons:

library(plainview)
# mapview w list of objects
#mapview(M69_95,alpha.regions=0.3) + mapview(F147_95,alpha.regions=0.2)

# Have to specify each attribute needed then reassign to an abject for mapview():
m1 = mapview(M69_95, zcol = "id", col.regions = c("blue"),alpha.regions=0.3)
m2 = mapview(F147_95, zcol = "id", col.regions = c("red"),alpha.regions=0.3)
m1
m2
m3 = mapview(M119_95, zcol = "id", col.regions = c("blue"),alpha.regions=0.3)
m4 = mapview(F36_95, zcol = "id", col.regions = c("red"),alpha.regions=0.3)

# add each newly assign polygon to eachother for n layers
m1+m2
m3+m4

############################################################################################
####################  Looping function #################### Looping function ###############

# kde_analysis.href.polygon <- function(filename, percentage){
#   data <- read.csv(file = filename)
#   x <- as.data.frame(data$EASTING)
#   y <- as.data.frame(data$NORTHING)
#   xy <- c(x,y)
#   data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS.SC)
#   xy <- SpatialPoints(data.proj@coords)
#   kde<-kernelUD(xy, h="href", kern="bivnorm", grid=1000)
#   ver <- getverticeshr(kde, percentage)
#   ver@proj4string<-CRS.SC
#   area <- as.data.frame(round(ver$area,4))
#   .rowNamesDF(area, make.names=TRUE) <- data$YEAR
#   write.table(area,file="KDE_Hectares.csv", 
#               append=TRUE,sep=",", col.names=FALSE, row.names=TRUE)
#   kde.points <- cbind((data.frame(data.proj@coords)),data$YEAR)
#   colnames(kde.points) <- c("x","y","year")
#   kde.poly <- fortify(ver, region = "id")
#   units <- grid.text(paste(round(ver$area,2)," ha"), x=0.9,  y=0.95,
#                      gp=gpar(fontface=4, cex=0.9), draw = FALSE)
#   ver
# }
####################  Looping function  ################### Looping function ###############
############################################################################################


####################################
##       Create raster of UD      ##
##            Heat Map            ##
####################################

#setwd("~/Dropbox/Gila Monster Data/GM_Study/F36")

library(mapview)
F36.raster<-kde_analysis.href.raster("./2008 .csv")
mapview(F36.raster)
# plot(F36.raster)

############################################################################################
####################  Looping function #################### Looping function ###############

kde_analysis.href.raster <- function(filename){
  data <- read.csv(file = filename)
  x <- as.data.frame(data$EASTING)
  y <- as.data.frame(data$NORTHING)
  xy <- c(x,y)
  data.proj <- SpatialPointsDataFrame(xy,data, proj4string = CRS.SC)
  xy <- SpatialPoints(data.proj@coords)
  kde<-kernelUD(xy, h="href", kern="bivnorm", grid=1000)
  kde<-as(kde, "SpatialGridDataFrame")
  kde@proj4string<- CRS.SC
  kde
}

####################  Looping function  ################### Looping function ###############
############################################################################################


#################################################################
##                Temporal Trajectory/Distance                 ##
#################################################################

#*********WORKING**********

# #trajectory analysis and distance over time

## Individual animal should already be assighned to an object from above. This set should
## also be across all years.  For analysis of each year, you will just use the "year .csv"
## file within the subfolder.

# View(F36)

# trajectory analysis:

#pblapply(files, traj_analysis)
traj_analysis("M67 .csv")
traj_analysis("2007 .csv")

############################################################################################
####################  Looping function #################### Looping function ###############

# traj_analysis <- function(filename){
#   relocs_data <- read.csv(file = filename)
#   relocs <- as.ltraj(cbind(relocs_data$EASTING, relocs_data$NORTHING),id=relocs_data$LIZARDNUMBER, typeII = FALSE, date=NULL)
#   relocs.df <- ld(relocs)
#   relocs_dist <- as.data.frame(sum(sapply(relocs.df$dist, sum, na.rm=TRUE)))
#   colnames(relocs_dist) <- "Total Distance"
#   name <- relocs.df$id[1]
#   row.names(relocs_dist) <- name
#   relocs_units <- grid.text(paste(round(relocs_dist,2),"m"), x=0.9, y=0.9, 
#                             gp=gpar(fontface=3, col="black", cex=0.9), draw = FALSE)
#   reloc.plot <- ggplot() + theme_classic() + geom_path(data=relocs.df, aes(x=x,y=y), linetype = "dashed", colour = "red",
#                                                        arrow = arrow(length=unit(.5,"cm"), angle = 20, ends="last", type = "closed")) +
#     geom_point(data=relocs.df, aes(x=x, y=y)) + geom_point(data=relocs.df, aes(x=x[1], 
#                                                                                y=y[1]), size = 3, color = "darkgreen", pch=0) +
#     labs(x="Easting (m)", y="Northing (m)", title=relocs.df$id[1]) +
#     theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5)) +
#     annotation_custom(relocs_units)
#   reloc.plot
# }

####################  Looping function  ################### Looping function ###############
############################################################################################


##################################
## DISTANCE OVER TIME ANALYSES: ##
##################################

#pblapply(files, dist_analysis)
dist_analysis("F36 .csv")
dist_analysis("2007 .csv")

#remove individual files after analysis, this works so be careful!
#unlink(files, recursive = FALSE)

# dist_analysis <- function(filename){
#   relocs_data <- read.csv(file = filename)
#   relocs <- as.ltraj(cbind(relocs_data$EASTING, relocs_data$NORTHING),id=relocs_data$LIZARDNUMBER, typeII = FALSE, date=NULL)
#   relocs.df <- ld(relocs)
#   relocs_dist <- as.data.frame(sum(sapply(relocs.df$dist, sum, na.rm=TRUE)))
#   colnames(relocs_dist) <- "Total Distance"
#   name <- relocs.df$id[1]
#   row.names(relocs_dist) <- name
#   write.table(relocs_dist,file="reloc_dist.csv", 
#               append=TRUE,sep=",", col.names=FALSE, row.names=TRUE)
#   dist.plot
# }



############################################
##          TRACKING INTENSITY            ##
############################################

setwd("~/Dropbox/Gila Monster Data/GM_Study/Movement") 

library(tidyverse)
library(lubridate)
library(plotly)
library(scales)


TrackData <- read.csv("SC_movement.csv")
View(TrackData)

TrackData$DATE <- mdy(TrackData$DATE) #use lubridate to specify incoming date format; 
#tell lubridate that the current format is "mdy", it then converts column to "ymd" format
str(TrackData) #look at DATE format

pT <- ggplot(TrackData, aes(DATE, LIZARDNUMBER)) +
  geom_count (color = "blue") + #set symbol size proportional to # of overlapping observations (same day)
  scale_x_date(date_breaks = "12 month", labels = date_format("%Y"))
pT

#####################################
##          Bootstrap MCP          ##
#####################################

getClass("Spatial")
getClass("SpatialPoints")

# An object of class SpatialPoints is a Spatial with one more slot:
# 3) coords: a matrix containing the coordinates of the points

# upload a dataframe with XY coordinates

setwd("~/Dropbox/Gila Monster Data/GM_Study/M255") # please set your working directory
M255.df<-read.csv("M255 .csv")

# # have a look at it
# head(F36.df) # First few rows
# summary(F36.df) # Statistical summaries
# str(F36.df) # Aspects of its structure


# Using the function "SpatialPoints" we create an object of class SpatialPoints.
# We have to specify the coordinates, whereas the bbox is automatically generated.
M255.sp <- SpatialPoints(M255.df[,c("EASTING","NORTHING")], proj4string=CRS.SC)
# summary(F36.sp)
# bbox(F36.sp)
# proj4string(F66.sp)
coordinates(M255.sp) # to have a look at the coordinates slot

plot(M255.sp, axes=TRUE)

library(move)
# ?move

hrBootstrap(x=M255.sp, rep=100, unin='m', unout='ha')

# rm(M255.df)

# Return to primary directory
setwd("~/Dropbox/Gila Monster Data/GM_Study") 

################################
##      SPATIAL ANALYSES      ##
##      MCP MAPS/OVERLAP      ##
################################

library(rgeos)

# creating interactive map of male vs. female MCPs:
# mapview(M67_MCP,legend=F)+mapview(M69_MCP,legend=F)+mapview(M255_MCP,legend=F)+
#   mapview(M215_MCP,legend=F)+mapview(M14_MCP,legend=F)+mapview(M119_MCP,legend=F)+
#   mapview(M112_MCP,legend=F)+mapview(F66_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F36_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F252_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+ 
#   mapview(F214_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F200_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F147_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F146_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F137_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F135_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F114_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)+
#   mapview(F104_MCP,legend=F, zcol = "id", col.regions = c("red"),alpha.regions=0.3)

## Shortcut is to rbind() males together and females together then use mapview to map them:

Male.MCP <- rbind(M67_MCP,M69_MCP,M255_MCP,M215_MCP,M14_MCP,M119_MCP,M112_MCP)
# mapview(Male.MCP)
Female.MCP <- rbind(F66_MCP,F36_MCP,F252_MCP,F214_MCP,F200_MCP,F147_MCP,F146_MCP,F137_MCP,
                    F135_MCP,F114_MCP,F104_MCP)
# mapview(Female.MCP)

mapview(Male.MCP, legend=F, zcol="id", col.regions = c("blue"), alpha.regions=0.3) + 
  mapview(Female.MCP, legend=F, zcol = "id", col.regions = c("red"), alpha.regions=0.3)

##############################
## MCP OVERLAP BETWEEN SEXES:

Male.MCP
Female.MCP

MCP_Intersect<-gIntersection(Male.MCP, Female.MCP,byid=T)
MCP_Intersect$area<-gArea(MCP_Intersect, byid=T)/10000
MCP_Intersect
mapView(MCP_Intersect,legend=F, col.regions = c("purple"), alpha.regions=0.3)

## A table:
kable(MCP_Intersect, format = "pandoc", caption = 'Home Range Overlap by Sex')

#################################
## NET MCP OVERLAP WITHIN SEXES :

F_OL1<-rbind(F36_MCP,F146_MCP)
F_OL2<-rbind(F66_MCP,F146_MCP)
F_OL3<-rbind(F104_MCP,F137_MCP,F147_MCP)
F_OL4<-rbind(F36_MCP,F66_MCP)
F_OL5<-rbind(F137_MCP,F135_MCP)

Female_Intersect_1<-gIntersection(F66_MCP,F_OL1,byid=F)
Female_Intersect_1$area<-gArea(Female_Intersect_1, byid=T)/10000
Female_Intersect_1
mapView(Female_Intersect_1,legend=F, col.regions = c("red"), alpha.regions=0.3)

Female_Intersect_2<-gIntersection(F36_MCP,F_OL2,byid=F)
Female_Intersect_2$area<-gArea(Female_Intersect_2, byid=T)/10000
Female_Intersect_2
mapView(Female_Intersect_2,legend=F, col.regions = c("red"), alpha.regions=0.3)

Female_Intersect_3<-gIntersection(F135_MCP,F_OL3,byid=F)
Female_Intersect_3$area<-gArea(Female_Intersect_3, byid=T)/10000
Female_Intersect_3
mapView(Female_Intersect_3,legend=F, col.regions = c("red"), alpha.regions=0.3)

Female_Intersect_4<-gIntersection(F146_MCP,F_OL4,byid=F)
Female_Intersect_4$area<-gArea(Female_Intersect_4, byid=T)/10000
Female_Intersect_4
mapView(Female_Intersect_4,legend=F, col.regions = c("red"), alpha.regions=0.3)

Female_Intersect_5<-gIntersection(F147_MCP,F_OL5,byid=F)
Female_Intersect_5$area<-gArea(Female_Intersect_5, byid=T)/10000
Female_Intersect_5
mapView(Female_Intersect_5,legend=F, col.regions = c("red"), alpha.regions=0.3)

## M215:M119
Male_Intersect_1<-gIntersection(M215_MCP, M119_MCP,byid=T)
Male_Intersect_1$area<-gArea(Male_Intersect_1, byid=T)/10000
Male_Intersect_1
mapView(Male_Intersect_1,legend=F, col.regions = c("blue"), alpha.regions=0.3)


## M14:M69
Male_Intersect_2<-gIntersection(M14_MCP, M69_MCP,byid=T)
Male_Intersect_2$area<-gArea(Male_Intersect_2, byid=T)/10000
Male_Intersect_2
mapView(Male_Intersect_2,legend=F, col.regions = c("blue"), alpha.regions=0.3)

#############################
## NET OVERLAP BETWEEN SEXES:

MF_OL1<-rbind(F135_MCP,F137_MCP,F147_MCP,F146_MCP,F66_MCP)

Net_Inter_1<-gIntersection(M69_MCP,MF_OL1,byid=F)
Net_Inter_1$area<-gArea(Net_Inter_1, byid=T)/10000
Net_Inter_1
mapView(Net_Inter_1,legend=F, col.regions = c("green"), alpha.regions=0.3)

## TOTAL NET OVERLAP OF MALES:FEMALES:
OL_Complex1<-rbind(F66_MCP,F146_MCP,M119_MCP,M215_MCP)
OL_Complex2<-rbind(F66_MCP,F146_MCP,M14_MCP,F147_MCP,F137_MCP,F135_MCP)
OL_Complex3<-rbind(M69_MCP,F147_MCP,F137_MCP,F104_MCP)
OL_Complex4<-rbind(F146_MCP,M69_MCP,F147_MCP)
OL_Complex5<-rbind(F137_MCP,F135_MCP,M69_MCP,M14_MCP,M67_MCP)
OL_Complex6<-rbind(F36_MCP,F66_MCP,M69_MCP,M14_MCP)
OL_Complex7<-rbind(F36_MCP,F146_MCP,M69_MCP)
OL_Complex8<-rbind(M215_MCP,F36_MCP)
OL_Complex9<-rbind(M119_MCP,F36_MCP)

Net_Intersect_1<-gIntersection(F36_MCP,OL_Complex1,byid=F)
Net_Intersect_1$area<-gArea(Net_Intersect_1, byid=T)/10000
Net_Intersect_1
mapView(Net_Intersect_1,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_2<-gIntersection(M69_MCP,OL_Complex2,byid=F)
Net_Intersect_2$area<-gArea(Net_Intersect_2, byid=T)/10000
Net_Intersect_2
mapView(Net_Intersect_2,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_3<-gIntersection(F135_MCP,OL_Complex3,byid=F)
Net_Intersect_3$area<-gArea(Net_Intersect_3, byid=T)/10000
Net_Intersect_3
mapView(Net_Intersect_3,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_4<-gIntersection(M14_MCP,OL_Complex4,byid=F)
Net_Intersect_4$area<-gArea(Net_Intersect_4, byid=T)/10000
Net_Intersect_4
mapView(Net_Intersect_4,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_5<-gIntersection(F147_MCP,OL_Complex5,byid=F)
Net_Intersect_5$area<-gArea(Net_Intersect_5, byid=T)/10000
Net_Intersect_5
mapView(Net_Intersect_5,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_6<-gIntersection(F146_MCP,OL_Complex6,byid=F)
Net_Intersect_6$area<-gArea(Net_Intersect_6, byid=T)/10000
Net_Intersect_6
mapView(Net_Intersect_6,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_7<-gIntersection(F66_MCP,OL_Complex7,byid=F)
Net_Intersect_7$area<-gArea(Net_Intersect_7, byid=T)/10000
Net_Intersect_7
mapView(Net_Intersect_7,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_8<-gIntersection(M119_MCP,OL_Complex8,byid=F)
Net_Intersect_8$area<-gArea(Net_Intersect_8, byid=T)/10000
Net_Intersect_8
mapView(Net_Intersect_8,legend=F, col.regions = c("green"), alpha.regions=0.3)

Net_Intersect_9<-gIntersection(M215_MCP,OL_Complex9,byid=F)
Net_Intersect_9$area<-gArea(Net_Intersect_9, byid=T)/10000
Net_Intersect_9
mapView(Net_Intersect_9,legend=F, col.regions = c("green"), alpha.regions=0.3)



