# High Priority: What are the changes in basal area over all plots for Quercus spp., Fraxinus spp., Acer spp., Quercus alba, Q. rubra, & Q. macrocarpa, 
library(readxl); library(ggplot2)

# -----------------------------------------------------------
# Looking at Tree Composition and Density: 2018 vs 2007
# -----------------------------------------------------------
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
imls.spat$PlotID <- as.factor(imls.spat$PlotID)
summary(imls.spat); head(imls.spat)

plots.ew <- imls.spat$PlotID[which(imls.spat$wooded=="East Woods")]
plots.hl <- imls.spat$PlotID[which(imls.spat$wooded=="Hidden Lake")]

ggplot(data=imls.spat) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=wooded))


# ----------------------------
# Read in and format data
# ----------------------------
# 2018 Trees
tree.2018 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Tree Layer")
names(tree.2018) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "DBH", "Canopy", "Decay", "Vigor", "Notes")
tree.2018$Sampler <- as.factor(tree.2018$Sampler)
tree.2018$PlotID <- as.factor(tree.2018$PlotID)
tree.2018$Spp.Code <- as.factor(tree.2018$Spp.Code)
tree.2018$Spp.Name <- as.factor(tree.2018$Spp.Name)
tree.2018$DBH <- as.numeric(tree.2018$DBH)
tree.2018$Canopy <- as.factor(tree.2018$Canopy)
tree.2018$Decay <- as.factor(tree.2018$Decay)
tree.2018$Vigor <- as.factor(tree.2018$Vigor)
tree.2018 <- tree.2018[!is.na(tree.2018$Date),]
# tree.2018 <- data.frame(tree.2018[!is.na(tree.2018$Date),])
tree.2018 <- data.frame(tree.2018)
tree.2018$Status <- as.factor(ifelse(tree.2018$Canopy=="S", "dead", "live"))
summary(tree.2018)


spp.use <- c("Quercus rubra", "Quercus ellipsoidalis")

tree.sub <- tree.2018[tree.2018$Spp.Name %in% spp.use,]
summary(tree.sub)

library(sp); library(rgdal)
sp.woods <- readOGR("/Volumes/GoogleDrive/My Drive/East Woods/GIS_files/east_woods/Woodland.shp")
sp.roads <- readOGR("/Volumes/GoogleDrive/My Drive/East Woods/GIS_files/roads/circ_veh_rd_2011-2020_ctrln.shp")
# sp.woods <- spTransform(sp.woods, projection(imls.spat))
# summary(as.factor(sp.roads$name))
# sp.roads


png("~/Desktop/EastWoods_QURU.png", height=6, width=8, units="in", res=180)  
ggplot(data=imls.spat[imls.spat$wooded %in% c("East Woods", "Hidden Lake"),]) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat), color="gray50", size=1.25) +
  geom_point(data=imls.spat[imls.spat$PlotID %in% tree.sub[tree.sub$Spp.Name==spp.use[1], "PlotID"],], aes(x=lon, y=lat, color="Q. rubra"), size=3) +
  theme_bw()
dev.off()

png("~/Desktop/EastWoods_QUEL.png", height=6, width=8, units="in", res=180)  
ggplot(data=imls.spat[imls.spat$wooded %in% c("East Woods", "Hidden Lake"),]) +
    coord_equal() +
    geom_point(aes(x=lon, y=lat), color="gray50", size=1.25) +
    geom_point(data=imls.spat[imls.spat$PlotID %in% tree.sub[tree.sub$Spp.Name==spp.use[2], "PlotID"],], aes(x=lon, y=lat, color="Q. elipsoidalis"), size=3) +
  theme_bw()
dev.off()
