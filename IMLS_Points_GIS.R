# Load and extract spatial data for the East Woods IMLS plots
# Lots of stealing from the script that I used to locate my permanent plots: 
#    EastWoods-MonitoringPlots/plot_selection/script/1_plot_selection.R

# Libraries
library(raster); library(rgdal); library(rgeos)
library(lubridate)

# File paths
path.ew <- "~/Desktop/Research/EastWoods-MonitoringPlots/plot_selection/"
google.gis <- "/Volumes/GoogleDrive/My Drive/East Woods/GIS_files"
dir.terrain <- file.path(path.ew, "../EastWoods_GIS")
path.gis <- "/Volumes/GIS/"

# The raw IMLS
imls.all <- readOGR(file.path(google.gis, "IMLS_posts/imlsposts.shp"))
imls.all <- imls.all[coordinates(imls.all)[,1]>0,] # There's one weird plot that needs to be filtered out
summary(imls.all)
plot(imls.all)

dem <- raster(file.path(google.gis, "DEMs/ewoods/")) # Elevation looks like its in feet
imls.ll <- spTransform(imls.all, CRS("+proj=longlat"))
imls.utm16 <- spTransform(imls.all, projection(dem))
imls.df <- data.frame(x.nad83=coordinates(imls.all)[,1], y.nad83=coordinates(imls.all)[,2],
                      x.utm16=coordinates(imls.utm16)[,1], y.utm16=coordinates(imls.utm16)[,2],
                      lon=coordinates(imls.ll)[,1], lat=coordinates(imls.ll)[,2],
                      PlotID=imls.all$CORNER, point.ID=1:nrow(imls.all))
summary(imls.df)


# Topography
dem <- raster(file.path(google.gis, "DEMs/ewoods/")) # Elevation looks like its in feet
# pre-calculated additional terrain variables
slope <- raster(file.path(google.gis, "topography_CR/eastwoods_slope"))
aspect <- raster(file.path(google.gis, "topography_CR/eastwoods_aspect"))
tpi <- raster(file.path(google.gis, "topography_CR/eastwoods_tpi"))


imls.df$elev   <- extract(dem   , spTransform(imls.all, projection(dem)))
imls.df$slope  <- extract(slope , spTransform(imls.all, projection(dem)))
imls.df$aspect <- extract(aspect, spTransform(imls.all, projection(dem)))
imls.df$tpi    <- extract(tpi   , spTransform(imls.all, projection(dem)))
summary(imls.df)



# # Soils 
soil_type <- readOGR("/Volumes/GIS/Collections/soils/soil_types.shp")
plot(soil_type)
summary(soil_type)

imls.soils <- extract(soil_type, imls.all)
imls.soils <- merge(imls.soils, imls.df[,c("point.ID", "PlotID")])
summary(imls.soils)
dim(imls.soils)


# 
# # Existing SSURGO data; can try using FEDDATA to get better
# ssurgo <- readOGR("/Volumes/GIS/Collections/soils/SSURGO_region.shp")
# summary(ssurgo)

# soils <- FedData::get_ssurgo(imls.all, label="IMLS_Points", extraction.dir="SSURGO_IMLS")
# summary(soils$spatial)
# text.key <- read.csv("SSURGO_IMLS/IMLS_Points_SSURGO_chtexturegrp.csv")
# summary(text.key)
# 
# test <- read.csv("SSURGO_IMLS/IMLS_Points_SSURGO_muaggatt.csv")
# summary(test)

# Woodland boundarys
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
# woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird 
imls.df$wooded <- over(imls.all, woods)[,1]
imls.df$wooded <- as.factor(car::recode(imls.df$ewoods, "'0'='yes'"))
summary(imls.df)


# Getting Management Info
path.burn <- "/Volumes/GIS/Collections/MarKGIS/Management Unit Plans/Controlled Burn Areas.gdb"
burn.layers <- ogrListLayers(path.burn)

burn <- readOGR(file.path(path.burn), "Completed_Burn_Areas")
burn$Burn_Date <- as.Date(burn$Burn_Date)
burn$Year <- lubridate::year(burn$Burn_Date)
burn$Month <- lubridate::month(burn$Burn_Date)
burn$season <- as.factor(ifelse(burn$Month<7, "spring", "fall"))
burn[grep("2004", burn$NOTES),"Year"] <- 2004
burn[grep("2005", burn$NOTES),"Year"] <- 2005
burn[grep("2010", burn$NOTES),"Year"] <- 2010
burn[grep("2011", burn$NOTES),"Year"] <- 2011
burn[grep("2013", burn$NOTES),"Year"] <- 2013
burn[grep("Pizzo Burn 2008", burn$NOTES),"Year"] <- 2008
burn[grep("Pizzo Burn 2006", burn$NOTES),"Year"] <- 2006
burn[grep("SPRING", toupper(burn$NOTES)),"season"] <- "spring"
burn[grep("FALL", toupper(burn$NOTES)),"season"] <- "fall"
burn$Year <- as.numeric(burn$Year)
burn[burn$NOTES==" ","NOTES"] <- NA
summary(burn)


burn <- spTransform(burn, projection(imls.all))
summary(burn)
dim(burn)

data.frame(burn[burn$Year==2013 & !is.na(burn$Year) & is.na(burn$Burn_Date),])
plot(burn[burn$Year==2013 & !is.na(burn$Year) & is.na(burn$Burn_Date),])

imls.burn <- extract(burn[!is.na(burn$Year),], imls.all)
imls.burn <- merge(imls.burn, imls.df[,c("point.ID", "PlotID", "lon", "lat")])
summary(imls.burn[is.na(imls.burn$Year),])
summary(imls.burn[imls.burn$Year==2013 & !is.na(imls.burn$Year),])
summary(imls.burn)

library(ggplot2)
burn.df2 <- aggregate(imls.burn[!is.na(imls.burn$Burn_Date),c("Burn_Date")], by=imls.burn[!is.na(imls.burn$Burn_Date),c("PlotID", "lat", "lon", "Burn_Date", "Year", "Month", "season")], FUN=length)
names(burn.df2)[which(names(burn.df2)=="x")] <- "n.entry"
burn.df2$Acres <- aggregate(imls.burn[!is.na(imls.burn$Burn_Date),c("Acres")], by=imls.burn[!is.na(imls.burn$Burn_Date),c("PlotID", "lat", "lon", "Burn_Date", "Year", "Month", "season")], FUN=mean)[,"x"]
summary(burn.df2)
summary(burn.df2[burn.df2$n.entry>1,])
summary(burn.df2[burn.df2$Acres==0,])

burn.yr  <- aggregate(burn.df2[,c("Burn_Date")], by=burn.df2[,c("PlotID", "lat", "lon", "Year", "season")], FUN=length)
summary(burn.yr)
burn.yr[burn.yr$x>1,]

burn.sum  <- aggregate(burn.df2[,c("Burn_Date")], by=burn.df2[,c("PlotID", "lat", "lon")], FUN=length)
names(burn.sum)[which(names(burn.sum)=="x")] <- "burn.n"
burn.sum$burn.first  <- aggregate(burn.df2[,c("Year")], by=burn.df2[,c("PlotID", "lat", "lon")], FUN=min)[,"x"]
burn.sum$burn.last  <- aggregate(burn.df2[,c("Year")], by=burn.df2[,c("PlotID", "lat", "lon")], FUN=max)[,"x"]
burn.sum <- merge(burn.sum, imls.df[,c("PlotID", "lon", "lat")], all=T)
burn.sum[is.na(burn.sum$burn.n), "burn.n"] <- 0
summary(burn.sum)

burn.df2[burn.df2$PlotID=="LL-90" & burn.df2$Year==2017,]

pdf("/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/figures/Burn_Summaries.pdf")
ggplot(data=burn.sum) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=burn.n)) +
  # geom_point(data=burn.sum[burn.sum$burn.n==0,], aes(x=lon, y=lat), color="black") +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()

ggplot(data=burn.sum) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=burn.first)) +
  # geom_point(data=burn.sum[burn.sum$burn.n==0,], aes(x=lon, y=lat), color="black") +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()

ggplot(data=burn.sum) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=burn.last)) +
  # geom_point(data=burn.sum[burn.sum$burn.n==0,], aes(x=lon, y=lat), color="black") +
  scale_color_distiller(palette = "Spectral") +
  theme_bw()
dev.off()

# summary(imls.df)

# summary(harvest)
# mgmt  <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Boundaries/New Management Units.shp")
# harvest <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/Canopy Thinning/Canopy Thinning.shp")

# imls.mgmt <- extract(mgmt, imls.all)
# imls.mgmt$point.ID <- as.factor(imls.mgmt$point.ID)
# summary(imls.mgmt)

imls.df$AreaName <- car::recode(imls.mgmt$East_West, "'East Side'='East Woods'; 'Hidden Lake Forest Preser'='Hidden Lake'")
imls.df$AreaName2 <- imls.mgmt$CommonName
imls.df$MgmtUnit <- imls.mgmt$UnitNumber
imls.df$MgmtUnitArea <- imls.mgmt$Acres
imls.df$ComClass <- imls.mgmt$ComClass
summary(imls.df)

# # imls.harvest <- over(imls.all, harvest)
# imls.harvest <- over(imls.all, harvest)
# summary(imls.harvest)
# dim(imls.harvest)

# imls.df$CanopyHarvest <- imls.harvest$Year

# imls.burn <- extract(burn, imls.all)
# imls.burn <- merge(imls.burn, imls.df[,c("point.ID", "PlotID")])
# summary(imls.burn)
# 
# 
# dim(imls.burn)


# Save all the info except the burn history
path.out <- "/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Analyses_Rollinson/data_processed"
write.csv(imls.df, file.path(path.out, "point_info_GIS.csv"), row.names=F)
write.csv(imls.burn, file.path(path.out, "point_info_GIS_burnhistory.csv"), row.names=F)
write.csv(imls.harvest, file.path(path.out, "point_info_GIS_canopyharvest.csv"), row.names=F)
write.csv(imls.soils, file.path(path.out, "point_info_GIS_soils.csv"), row.names=F)
