# High Priority: What are the changes in basal area over all plots for Quercus spp., Fraxinus spp., Acer spp., Quercus alba, Q. rubra, & Q. macrocarpa, 
library(readxl); library(ggplot2)
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods"
path.figs <- file.path(path.ew, "Inventory 2018/Analyses_Rollinson/figures")
# -----------------------------------------------------------
# Looking at Tree Composition and Density: 2018 vs 2007
# -----------------------------------------------------------
dat.gis <- read.csv(file.path(path.ew, "Inventory 2018/Analyses_Rollinson/data_processed", "point_info_GIS.csv"))
dat.gis$PlotID2 <- dat.gis$PlotID
dat.gis$PlotID <- as.factor(gsub("-", "", dat.gis$PlotID))
dat.gis$MgmtUnit <- ifelse(is.na(dat.gis$wooded), "Non-Wooded", 
                           ifelse(dat.gis$wooded=="Hidden Lake", "Hidden Lake", 
                                  ifelse(dat.gis$unit=="South 40 South", "Annual Burn", 
                                         ifelse(!is.na(dat.gis$unit), "Mixed Management", "No Management"))))
dat.gis$MgmtUnit[is.na(dat.gis$MgmtUnit)] <- "No Management"
dat.gis$MgmtUnit <- as.factor(dat.gis$MgmtUnit)
summary(dat.gis)

# ------------------------------
# Get the 2018 East Woods Inventory tree data:
# ------------------------------
path.2018 <- file.path(path.ew, "Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018")
# path.out <- file.path(path.ew, "Inventory 2018/Analyses_Rollinson")

# 2018 Trees
tree.2018 <- readxl::read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22 (1).xlsx"), sheet = "Tree Layer")
names(tree.2018) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "DBH", "Canopy", "Decay", "Vigor", "Notes")
tree.2018$Sampler <- as.factor(tree.2018$Sampler)
tree.2018$PlotID <- as.factor(tree.2018$PlotID)
tree.2018$Spp.Code <- as.factor(tree.2018$Spp.Code)
tree.2018$Spp.Name <- as.factor(tree.2018$Spp.Name)
tree.2018$DBH <- as.numeric(tree.2018$DBH)
tree.2018$Canopy <- as.factor(tree.2018$Canopy)
tree.2018$Decay <- as.factor(tree.2018$Decay)
tree.2018$Vigor <- as.factor(tree.2018$Vigor)
tree.2018$Density <- 1/(pi*8.92^2) # 250m plot
tree.2018 <- tree.2018[!is.na(tree.2018$Date),]
# tree.2018 <- data.frame(tree.2018[!is.na(tree.2018$Date),])
tree.2018 <- data.frame(tree.2018)
tree.2018$Status <- as.factor(ifelse(tree.2018$Canopy=="S", "dead", "live"))
tree.2018 <- tree.2018[tree.2018$DBH>10 & 
                         !(is.na(tree.2018$Status) | tree.2018$Status=="dead" | tree.2018$Spp.Name=="Unidentified") &
                         tree.2018$PlotID %in% dat.gis[dat.gis$MgmtUnit!="Non-Wooded","PlotID"],]

tree.2018$Genus <- as.factor(unlist(lapply(stringr::str_split(tree.2018$Spp.Name, " "), function(x) x[1])))
summary(tree.2018)
tree.2018$n.stems <- 1
summary(droplevels(tree.2018$Spp.Name))
# ------------------------------

# ------------------------------
# Aggregate to plot-level stats to find low-Oak areas
# ------------------------------
dat.plot.gen <- aggregate(tree.2018[,c("DBH")],
                          by=tree.2018[,c("PlotID", "Genus")],
                          FUN=mean)
names(dat.plot.gen)[names(dat.plot.gen)=="x"] <- "DBH.mean"
dat.plot.gen[,c("n.stems", "Density")] <- aggregate(tree.2018[,c("n.stems", "Density")],
                                                    by=tree.2018[,c("PlotID", "Genus")],
                                                    FUN=sum)[,c("n.stems", "Density")]
summary(dat.plot.gen)

# creating a wide matrix; multiple ways of doing it, but I'm goign to try tidyR
plot.gen.wide <- tidyr::spread(dat.plot.gen[,c("PlotID", "Genus", "n.stems")], Genus, n.stems)
summary(plot.gen.wide)

# Find plots which have maples, but nothing else in the overstory
rows.acer <- which(!is.na(plot.gen.wide$Acer) &  apply(plot.gen.wide[,! names(plot.gen.wide) %in% c("PlotID", "Acer")], 1, FUN=function(x){all(is.na(x))}))

dat.plot.gen[dat.plot.gen$PlotID %in% plot.gen.wide$PlotID[rows.acer],]  

png(file.path(path.figs, "AcerOnly_Locations.png"), height=6, width=8, units="in", res=180)
ggplot(data=dat.gis) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=MgmtUnit), alpha=0.5) +
  geom_point(data = dat.gis[dat.gis$PlotID %in% plot.gen.wide$PlotID[rows.acer],], 
             aes(x=lon, y=lat, color="Acer Only"), color="black", size=2) +
  theme_minimal()
dev.off()

summary(tree.2018[tree.2018$PlotID %in% plot.gen.wide$PlotID[rows.acer],])
write.csv(tree.2018[tree.2018$PlotID %in% plot.gen.wide$PlotID[rows.acer],], file.path(path.ew, "Inventory 2018/Analyses_Rollinson/data_processed", "PlotData_AcerOnly.csv"), row.names=F)

summary(dat.gis)
# ------------------------------
