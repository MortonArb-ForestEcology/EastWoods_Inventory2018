# Focusing on change in invasive species dominance and understory diversity in the East Woods
library(readxl); library(ggplot2)

# Load some handy spatial maps
library(sp); library(rgdal); library(raster)
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_2011-2020_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")

woods.fort <- fortify(woods)
summary(woods.fort)
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods"
path.2018 <- file.path(path.ew, "Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018")
path.2007 <- file.path(path.ew, "Inventory 2007")
path.out <- file.path(path.ew, "Inventory 2018/Analyses_Rollinson")
google.gis <- "/Volumes/GoogleDrive/My Drive/East Woods/GIS_files"
morton.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search

# Load in Spatial File so we can look at patterns & changes as a map
imls.spat <- read.csv(file.path(path.ew, "Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS.csv"))
imls.spat$PlotID2 <- imls.spat$PlotID
imls.spat$PlotID <- gsub("-", "", imls.spat$PlotID)
summary(imls.spat); head(imls.spat)


# -----------------------------------------------------------
# # Starting with Shrubs/Invasive Species: 2018 vs 2007
# -----------------------------------------------------------
shrub.2018 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22 (1).xlsx"), sheet = "Shrub Layer")
names(shrub.2018) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Category", "Category.Sapling", "Count", "Notes")
# shrub.2018 <- data.frame(shrub.2018)
shrub.2018$Sampler <- as.factor(shrub.2018$Sampler)
shrub.2018$PlotID <- as.factor(shrub.2018$PlotID)
shrub.2018$Spp.Code <- as.factor(shrub.2018$Spp.Code)
shrub.2018$Spp.Name <- as.factor(shrub.2018$Spp.Name)
shrub.2018$Category <- as.factor(shrub.2018$Category)
shrub.2018$Category.Sapling <- as.factor(shrub.2018$Category.Sapling)
shrub.2018$Notes <- as.factor(shrub.2018$Notes)
shrub.2018$Year <- 2018
summary(shrub.2018)

shrub.2007 <- read_excel(file.path(path.2007, "Summer Vegetation Sampling-RevisedFinal_2_20.CORRECTED PLOTS.xls"), sheet="Shrub Layer", skip=1, na=c("-", ""))
names(shrub.2007) <- c("PlotID", "Spp.Code", "Spp.Name", "Count.Alive", "Count.Dead", "Native", "Category")
shrub.2007$PlotID <- as.factor(shrub.2007$PlotID)
shrub.2007$Spp.Code <- as.factor(shrub.2007$Spp.Code)
shrub.2007$Spp.Name <- as.factor(shrub.2007$Spp.Name)
shrub.2007$Category <- as.factor(shrub.2007$Category)
shrub.2007$Native   <- as.factor(shrub.2007$Native  )
shrub.2007[!is.na(shrub.2007$Count.Alive) & shrub.2007$Count.Alive=="100+","Count.Alive"] <- 100
shrub.2007$Count.Alive <- as.numeric(shrub.2007$Count.Alive)
shrub.2007$PlotID <- sub("-", "", shrub.2007$PlotID)
shrub.2007$Year <- 2007
shrub.2007$Count <- shrub.2007$Count.Alive
summary(shrub.2007)


shrub.bind <- rbind(shrub.2007[,c("Year", "PlotID", "Spp.Name", "Count", "Category")], 
                    shrub.2018[,c("Year", "PlotID", "Spp.Name", "Count", "Category")])
shrub.bind[grep("Lonicera", shrub.bind$Spp.Name),"Invasive"] <- "Lonicera"
shrub.bind[grep("Rhamnus", shrub.bind$Spp.Name),"Invasive"] <- "Rhamnus"
summary(shrub.bind)

shrub.agg <- aggregate(shrub.bind[,c("Count")], by=shrub.bind[,c("Year", "PlotID", "Invasive")], FUN=sum)
summary(shrub.agg)

shrub.agg <- merge(shrub.agg, imls.spat[,c("PlotID", "x.nad83", "y.nad83", "x.utm16", "y.utm16", "lon", "lat", "PlotID2", "wooded", "AreaName")], all.x=T, all.y=T)
shrub.agg[is.na(shrub.agg$Count), "Count"] <- 0
summary(shrub.agg)


  

# Looking at the change
invasive.shrub <- rbind(data.frame(imls.spat, Invasive="Lonicera"),
                        data.frame(imls.spat, Invasive="Rhamnus"))

for(PLOT in unique(invasive.shrub$PlotID)){
  L07 <- shrub.agg[shrub.agg$PlotID==PLOT & shrub.agg$Year==2007 & shrub.agg$Invasive=="Lonicera", "Count"]
  L18 <- shrub.agg[shrub.agg$PlotID==PLOT & shrub.agg$Year==2018 & shrub.agg$Invasive=="Lonicera", "Count"]
  
  if(length(L07)==0) L07 <- 0
  if(length(L18)==0) L18 <- 0
  invasive.shrub[invasive.shrub$PlotID==PLOT & invasive.shrub$Invasive == "Lonicera", "change"] <- L18-L07
  
  R07 <- shrub.agg[shrub.agg$PlotID==PLOT & shrub.agg$Year==2007 & shrub.agg$Invasive=="Rhamnus", "Count"]
  R18 <- shrub.agg[shrub.agg$PlotID==PLOT & shrub.agg$Year==2018 & shrub.agg$Invasive=="Rhamnus", "Count"]
  
  if(length(R07)==0) R07 <- 0
  if(length(R18)==0) R18 <- 0
  invasive.shrub[invasive.shrub$PlotID==PLOT & invasive.shrub$Invasive == "Rhamnus", "change"] <- R18-R07
}
summary(invasive.shrub)

pdf(file.path(path.out, "figures", "InvasiveSpecies_Densities.pdf"))
ggplot(data=shrub.agg[!is.na(shrub.agg$Invasive),]) +
  coord_equal() +
  facet_grid(Year ~ Invasive) +
  geom_point(data=imls.spat, aes(x=lon, y=lat), color="gray50") +
  geom_point(aes(x=lon, y=lat, color=Count)) +
  scale_color_distiller(palette = "Spectral")+ 
  theme_bw() +
  theme(panel.background=element_rect(fill="black"),
        panel.grid = element_blank())


ggplot(data=invasive.shrub[!is.na(shrub.agg$Invasive),]) +
  coord_equal() +
  facet_grid(~ Invasive) +
  geom_point(aes(x=lon, y=lat, color=change)) +
  scale_color_distiller(palette = "RdBu") + 
  theme_bw() +
  theme(panel.background=element_rect(fill="black"),
        panel.grid = element_blank())


ggplot(data=invasive.shrub[!is.na(invasive.shrub$Invasive) & invasive.shrub$change<0 & !is.na(invasive.shrub$change),]) +
  ggtitle("Invasive Species Loss") +
  coord_equal() +
  facet_grid(~ Invasive) +
  geom_point(data=imls.spat, aes(x=lon, y=lat), color="gray50") +
  geom_point(aes(x=lon, y=lat, color=change)) +
  scale_color_distiller(palette = "YlGn") + 
  theme_bw() +
  theme(panel.background=element_rect(fill="black"),
        panel.grid = element_blank())


ggplot(data=invasive.shrub[!is.na(invasive.shrub$Invasive) & invasive.shrub$change>0 & !is.na(invasive.shrub$change),]) +
  ggtitle("Invasive Species GAIN") +
  coord_equal() +
  facet_grid(~ Invasive) +
  geom_point(data=invasive.shrub, aes(x=lon, y=lat), color="gray50") +
  geom_point(aes(x=lon, y=lat, color=change)) +
  scale_color_distiller(palette = "OrRd", trans="reverse")+ 
  theme_bw() +
  theme(panel.background=element_rect(fill="black"),
        panel.grid = element_blank())
dev.off()
# -----------------------------------------------------------

# -----------------------------------------------------------
# Now daring to work with the herbaceous communities
# -----------------------------------------------------------
# -----------------------------------------------------------
