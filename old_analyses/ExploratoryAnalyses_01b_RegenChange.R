# Exploratory Analyses: Oak Regen -- Temporal and Spatial Patterns
# Christy Rollinson's Initial Thoughts:
# Dominant tree/forest type; interpolated maps of Oak relative oak importance value 
# Current and change in oak regeneration (seedling density)
# Current and change in woody invasives
# Current total basal area & change in basal area

# Reply from Kurt Dreisilker (email 31 Aug, 2018)
# I would like to use this data to develop a trajectory for the East Woods.  
# For example, based on these two sets of data (2007 & 2018) we have seen something decline (e.g., oak basal area) and another thing (e.g., oak recruitment) remain unchanged.  Based on this information, we propose the following management action items to commence in the near future.  We may need to look at management units to help break down the data. We will also need to look at the impact of volunteers on the East Woods, specifically since the funding was provided to enhance our volunteer stewardship effort. 


library(readxl); library(ggplot2)

# Load some handy spatial maps
library(sp); library(rgdal); library(raster)
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_2011-2020_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")

woods.fort <- fortify(woods)
summary(woods.fort)
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
summary(imls.spat); head(imls.spat)



# ----------------------------
# Read in and format data
# ----------------------------
# 2018 Trees
tree.2018 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22 (1).xlsx"), sheet = "Tree Layer")
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

unique(tree.2018$Spp.Code)[order(unique(tree.2018$Spp.Code))]

# 2007 Trees
tree.2007a <- read_excel(file.path(path.2007, "Summer Vegetation Sampling-RevisedFinal_2_20.CORRECTED PLOTS.xls"), sheet="Tree Plots", skip=1)
names(tree.2007a) <- c("PlotID", "Spp.Code", "Spp.Name", "DBH.live", "DBH.dead", "Native")
tree.2007a$PlotID <- as.factor(tree.2007a$PlotID)
tree.2007a$Spp.Code <- as.factor(tree.2007a$Spp.Code)
tree.2007a$Spp.Name <- as.factor(tree.2007a$Spp.Name)
tree.2007a$DBH.live <- as.numeric(tree.2007a$DBH.live)
tree.2007a$DBH.dead <- as.numeric(tree.2007a$DBH.dead)
tree.2007a$Native <- as.factor(tree.2007a$Native)
tree.2007a$Native[tree.2007a$Native=="-"] <- NA
tree.2007a <- data.frame(tree.2007a)
summary(tree.2007a)
head(data.frame(tree.2007a))

# Stackign to make match 2018 format better
tree.2007 <- stack(tree.2007a[,c("DBH.live", "DBH.dead")])
names(tree.2007) <- c("DBH", "Status")
tree.2007$Status <- car::recode(tree.2007$Status, "'DBH.live'='live'; 'DBH.dead'='dead'")
tree.2007[,c("PlotID", "Spp.Code", "Spp.Name", "Native")] <- tree.2007a[,c("PlotID", "Spp.Code", "Spp.Name", "Native")]
tree.2007 <- tree.2007[!(is.na(tree.2007$DBH) & !tree.2007$Spp.Code %in% c("??", "NONE")) & !(tree.2007$Spp.Code %in% c("??", "NONE") & tree.2007$Status=="dead"),]
tree.2007[tree.2007$Spp.Code %in% c("??", "NONE") & is.na(tree.2007$DBH), "Status"] <- NA
tree.2007 <- droplevels(tree.2007)
tree.2007$PlotID <- sub("-", "", tree.2007$PlotID)
summary(tree.2007)
summary(tree.2007[is.na(tree.2007$DBH),])

# Doing some data cleaning to match 2007 & 2018
unique(tree.2018$Spp.Code)[order(unique(tree.2018$Spp.Code))]
unique(tree.2007$Spp.Code)[order(unique(tree.2007$Spp.Code))]
tree.2018$Spp.Code <- toupper(tree.2018$Spp.Code)

unique(tree.2018$Spp.Code)[!unique(tree.2018$Spp.Code) %in% unique(tree.2007$Spp.Code)]
unique(tree.2007$Spp.Code)[!unique(tree.2007$Spp.Code) %in% unique(tree.2018$Spp.Code)]

tree.2007[tree.2007$Spp.Code=="??",]
tree.2007$Spp.Code <- car::recode(tree.2007$Spp.Code, "'??'='ERROR'; 'NONE'='NO TREES'; 'UNIDENTIFIED SP'='UNIDENTIFIED'")
tree.2018$Spp.Code <- ifelse(substr(tree.2018$Spp.Code, nchar(tree.2018$Spp.Code), nchar(tree.2018$Spp.Code))==".", substr(tree.2018$Spp.Code, 1, nchar(tree.2018$Spp.Code)-1), tree.2018$Spp.Code)

tree.2007[tree.2007$Spp.Code=="PSEMEN",]

unique(tree.2018$Spp.Name)[order(unique(tree.2018$Spp.Name))]
unique(tree.2007$Spp.Name)[order(unique(tree.2007$Spp.Name))]

tree.2007$Spp.Name <- sub("species", "spp", tree.2007$Spp.Name)
tree.2018$Spp.Name <- sub("sp.", "spp", tree.2018$Spp.Name)

tree.2007[tree.2007$Spp.Name=="rttf",]
tree.2007[tree.2007$Spp.Name=="??",]
tree.2007$Spp.Name <- car::recode(tree.2007$Spp.Name, "'rttf'='Acer spp'; '??'='ERROR'; 'Hickory spp'='Carya spp'; 'Fraxinus - Collections'='Fraxinus spp'; 'Unidentified spp'='Unidentified'; 'NONE'='No trees'; 'Cladrastis lutea'='Cladrastis kentukea'")
tree.2018$Spp.Name <- car::recode(tree.2018$Spp.Name, "")

tree.2018$Spp.Name <- car::recode(tree.2018$Spp.Name, "'Quercus alb'='Quercus alba'; 'tilia americana'='Tilia americana'; 'Cladastris kentukea'='Cladrastis kentukea'")

# We have quite a bit of difference in tree IDs from year to year
unique(tree.2018$Spp.Name)[!unique(tree.2018$Spp.Name) %in% unique(tree.2007$Spp.Name)]
unique(tree.2007$Spp.Name)[!unique(tree.2007$Spp.Name) %in% unique(tree.2018$Spp.Name)]

tree.2007[tree.2007$Spp.Name=="Ribes spp",]

# Note; not all plots were sampled in both years
unique(tree.2007$PlotID)[!unique(tree.2007$PlotID) %in% unique(tree.2018$PlotID)]
unique(tree.2018$PlotID)[!unique(tree.2018$PlotID) %in% unique(tree.2007$PlotID)]

# Final Summarires
tree.2007$PlotID <- as.factor(tree.2007$PlotID)
tree.2007$Spp.Name <- as.factor(tree.2007$Spp.Name)
tree.2018$Spp.Code <- as.factor(tree.2018$Spp.Code)
tree.2018$Spp.Name <- as.factor(tree.2018$Spp.Name)
tree.2018$Status <- as.factor(tree.2018$Status)
tree.2007$Year <- 2007
tree.2018$Year <- 2018
summary(tree.2007)
summary(tree.2018)

dat.tree.raw <- rbind(tree.2007[,c("Year", "PlotID", "Spp.Name", "Spp.Code", "Status", "DBH")], tree.2018[,c("Year", "PlotID", "Spp.Name", "Spp.Code", "Status", "DBH")])
dat.tree.raw$Genus <- as.factor(unlist(lapply(stringr::str_split(dat.tree.raw$Spp.Name, " "), function(x) x[1])))
dat.tree.raw$BA <- pi*(dat.tree.raw$DBH/2)^2 # in cm2
summary(dat.tree.raw)

dat.tree <- aggregate(dat.tree.raw[dat.tree.raw$Status=="live",c("DBH")],
                      by=dat.tree.raw[dat.tree.raw$Status=="live",c("Year", "PlotID", "Spp.Name", "Genus")],
                      FUN=length)
names(dat.tree)[which(names(dat.tree)=="x")] <- c("Count")
dat.tree$DBH.mean <- aggregate(dat.tree.raw[dat.tree.raw$Status=="live",c("DBH")],
                               by=dat.tree.raw[dat.tree.raw$Status=="live",c("Year", "PlotID", "Spp.Name", "Genus")],
                               FUN=mean, na.rm=T)$x
dat.tree$DBH.sd   <- aggregate(dat.tree.raw[dat.tree.raw$Status=="live",c("DBH")],
                               by=dat.tree.raw[dat.tree.raw$Status=="live",c("Year", "PlotID", "Spp.Name", "Genus")],
                               FUN=sd, na.rm=T)$x
dat.tree$BA.tot   <- aggregate(dat.tree.raw[dat.tree.raw$Status=="live",c("BA")],
                               by=dat.tree.raw[dat.tree.raw$Status=="live",c("Year", "PlotID", "Spp.Name", "Genus")],
                               FUN=sum, na.rm=T)$x
summary(dat.tree)
# ----------------------------

# ----------------------------
# Reading in the sapling data
# ----------------------------
# 2018 shrubs
shrub.2018 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22 (1).xlsx"), sheet = "Shrub Layer")
names(shrub.2018) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Category", "Category.Sapling", "Count", "Notes")
shrub.2018$Sampler <- as.factor(shrub.2018$Sampler)
shrub.2018$PlotID <- as.factor(shrub.2018$PlotID)
shrub.2018$Spp.Code <- as.factor(shrub.2018$Spp.Code)
shrub.2018$Spp.Name <- as.factor(shrub.2018$Spp.Name)
shrub.2018$Category <- as.factor(shrub.2018$Category)
shrub.2018$Category.Sapling <- as.factor(shrub.2018$Category.Sapling)
# shrub.2018 <- shrub.2018[!is.na(shrub.2018$Date),]
# shrub.2018 <- data.frame(shrub.2018[!is.na(shrub.2018$Date),])
shrub.2018 <- data.frame(shrub.2018)
summary(shrub.2018)

# summary(droplevels(shrub.2018[shrub.2018$Category.Sapling==">5","Spp.Name"]))
summary(droplevels(shrub.2018[!is.na(shrub.2018$Category.Sapling),"Spp.Name"]))


shrub.2007 <- read_excel(file.path(path.2007, "Summer Vegetation Sampling-RevisedFinal_2_20.CORRECTED PLOTS.xls"), sheet="Shrub Layer", skip=1, na=c("", "-"))
names(shrub.2007) <- c("PlotID", "Spp.Code", "Spp.Name", "Count.Alive", "Count.Dead", "Native.Status", "Category")
shrub.2007$PlotID <- as.factor(shrub.2007$PlotID)
shrub.2007$Spp.Code <- as.factor(shrub.2007$Spp.Code)
shrub.2007$Spp.Name <- as.factor(shrub.2007$Spp.Name)
shrub.2007$Native.Status <- as.factor(shrub.2007$Native.Status)
shrub.2007$Category <- as.factor(shrub.2007$Category)
shrub.2007 <- data.frame(shrub.2007)
shrub.2007$PlotID <- as.factor(sub("-", "", shrub.2007$PlotID))
summary(shrub.2007)

shrub.2007$Category <- car::recode(shrub.2007$Category, "'T'='UT'")
shrub.2007$Count.Alive <- ifelse(shrub.2007$Count.Alive=="100+", 100, shrub.2007$Count.Alive)
shrub.2007$Count.Alive <- as.numeric(shrub.2007$Count.Alive)

summary(droplevels(shrub.2007[shrub.2007$Category=="TS","Spp.Name"]))
summary(droplevels(shrub.2007[shrub.2007$Category=="UT","Spp.Name"]))

# Comparing
summary(droplevels(shrub.2007[shrub.2007$Category=="TS","Spp.Name"]))
summary(droplevels(shrub.2018[!is.na(shrub.2018$Category.Sapling),"Spp.Name"]))

# Adding some factors
shrub.2018$Year <- 2018
shrub.2007$Year <- 2007
names(shrub.2007); names(shrub.2018)
names(shrub.2007)[which(names(shrub.2007)=="Count.Alive")] <- "Count"
shrub.2018$Category2 <- as.factor(ifelse(shrub.2018$Category=="SH", "shrub", "tree"))
shrub.2007$Category2 <- as.factor(ifelse(shrub.2007$Category=="S", "shrub", "tree"))
summary(shrub.2007)
summary(shrub.2018)

dat.shrub <- rbind(shrub.2007[,c("Year","PlotID", "Spp.Code", "Spp.Name", "Count")],
                   shrub.2018[,c("Year", "PlotID", "Spp.Code", "Spp.Name", "Count")])
dat.shrub$Genus <- as.factor(unlist(lapply(stringr::str_split(dat.shrub$Spp.Name, " "), function(x) x[1])))
summary(dat.shrub)
# ----------------------------

# ----------------------------
# Looking at the seedling data
# ----------------------------
# -------------
# 2018 Data
# -------------
herb.2018.spr <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22 (1).xlsx"), sheet = "Herbaceous")
names(herb.2018.spr) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Cover.Est", "Cover.Class", "Count", "Notes")
herb.2018.spr$Sampler <- as.factor(herb.2018.spr$Sampler)
herb.2018.spr$PlotID <- as.factor(herb.2018.spr$PlotID)
herb.2018.spr$Spp.Code <- as.factor(herb.2018.spr$Spp.Code)
herb.2018.spr$Spp.Name <- as.factor(herb.2018.spr$Spp.Name)
herb.2018.spr$Cover.Class <- as.ordered(herb.2018.spr$Cover.Class)
herb.2018.spr <- data.frame(herb.2018.spr)
herb.2018.spr$Season <- as.factor("spring")
summary(herb.2018.spr)

herb.2018.sum <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Summer Herb Data Sheet_AESedits_Oct-22.xlsx"), sheet = "Summer Herbaceous")
names(herb.2018.sum) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Cover.Est", "Cover.Class", "Count", "Notes")
herb.2018.sum$Sampler <- as.factor(herb.2018.sum$Sampler)
herb.2018.sum$PlotID <- as.factor(herb.2018.sum$PlotID)
herb.2018.sum$Spp.Code <- as.factor(herb.2018.sum$Spp.Code)
herb.2018.sum$Spp.Name <- as.factor(herb.2018.sum$Spp.Name)
herb.2018.sum$Cover.Class <- as.ordered(herb.2018.sum$Cover.Class)
herb.2018.sum <- data.frame(herb.2018.sum)
herb.2018.sum$Season <- as.factor("summer")
summary(herb.2018.sum)

herb.2018 <- rbind(herb.2018.spr, herb.2018.sum)
herb.2018 <- aggregate(herb.2018[,c("Cover.Est", "Count")],
                       by=herb.2018[,c("PlotID", "Spp.Code", "Spp.Name")],
                       FUN=max, na.rm=T)
herb.2018[herb.2018$Cover.Est==-Inf, "Cover.Est"] <- NA
herb.2018[herb.2018$Count==-Inf, "Count"] <- NA

summary(herb.2018)
summary(droplevels(herb.2018[!is.na(herb.2018$Count),"Spp.Name"]))
# -------------

# -------------
# 2007 Data
# -------------
herb.2007.spr <- read_excel(file.path(path.2007, "Spring Vegetation Sampling FINAL-Sp 07.CORRECTED PLOTS.xls"), sheet="Ground Layer", skip=2, na=c("", "-"))
names(herb.2007.spr) <- c("PlotID", "Spp.Code", "Spp.Name", "Cover.Est", "Pres.Abs", "Native.Status", "Form", "Habit")
herb.2007.spr$PlotID <- as.factor(herb.2007.spr$PlotID)
herb.2007.spr$Spp.Code <- as.factor(herb.2007.spr$Spp.Code)
herb.2007.spr$Spp.Name <- as.factor(herb.2007.spr$Spp.Name)
herb.2007.spr$Native.Status <- as.factor(herb.2007.spr$Native.Status)
herb.2007.spr$Form <- as.factor(herb.2007.spr$Form)
herb.2007.spr$Habit <- as.factor(herb.2007.spr$Habit)
herb.2007.spr <- data.frame(herb.2007.spr)
summary(herb.2007.spr)

herb.2007.sum <- read_excel(file.path(path.2007, "Summer Vegetation Sampling-RevisedFinal_2_20.CORRECTED PLOTS.xls"), sheet="Ground Layer", skip=1, na=c("", "-"))
names(herb.2007.sum) <- c("PlotID", "Spp.Code", "Spp.Name", "Cover.Est", "Pres.Abs", "Native.Status", "Form", "Habit")
herb.2007.sum$PlotID <- as.factor(herb.2007.sum$PlotID)
herb.2007.sum$Spp.Code <- as.factor(herb.2007.sum$Spp.Code)
herb.2007.sum$Spp.Name <- as.factor(herb.2007.sum$Spp.Name)
herb.2007.sum$Native.Status <- as.factor(herb.2007.sum$Native.Status)
herb.2007.sum$Form <- as.factor(herb.2007.sum$Form)
herb.2007.sum$Habit <- as.factor(herb.2007.sum$Habit)
herb.2007.sum <- data.frame(herb.2007.sum)
summary(herb.2007.sum)

herb.2007 <- rbind(herb.2007.spr, herb.2007.sum)
herb.2007$Form <- as.factor(tolower(herb.2007$Form))
herb.2007 <- aggregate(herb.2007[,c("Cover.Est", "Pres.Abs")],
                       by=herb.2007[,c("PlotID", "Spp.Code", "Spp.Name", "Form", "Habit", "Native.Status")],
                       FUN=max, na.rm=T)
# herb.2007[herb.2007$Cover.Est==-Inf, "Cover.Est"] <- NA
herb.2007[herb.2007$Pres.Abs==-Inf, "Pres.Abs"] <- NA
herb.2007$PlotID <- as.factor(sub("-", "", herb.2007$PlotID))

summary(herb.2007)

summary(droplevels(herb.2007[herb.2007$Form=="tree","Spp.Name"]))
# -------------

names(herb.2007); names(herb.2018)
herb.2007$Year <- 2007
herb.2018$Year <- 2018
summary(herb.2007[herb.2007$Spp.Name=="Quercus rubra",])
summary(herb.2018[herb.2018$Spp.Name=="Quercus rubra",])

dat.herb <- rbind(herb.2007[,c("Year", "PlotID", "Spp.Code", "Spp.Name", "Cover.Est")],
                  herb.2018[,c("Year", "PlotID", "Spp.Code", "Spp.Name", "Cover.Est")])
dat.herb$Genus <- as.factor(unlist(lapply(stringr::str_split(dat.herb$Spp.Name, " "), function(x) x[1])))
summary(dat.herb)
# ----------------------------


# ----------------------------
# Merging in Spatial information so we can start with some graphing
# ----------------------------
summary(dat.tree)
summary(dat.shrub)
summary(dat.herb)

# Summarizing just the Quercus data
dat.quercus <- data.frame(imls.spat[,c("PlotID", "x.nad83", "y.nad83", "x.utm16", "y.utm16", "lon", "lat", "PlotID2", "wooded", "AreaName")],
                          Year=rep(c(2007, 2018), each=nrow(imls.spat)),
                          tree.count=NA,
                          tree.BA=NA,
                          shrub.count = NA,
                          seed.cover = NA
                          )
summary(dat.quercus)

for(PLOT in unique(dat.quercus$PlotID)){
  ind.07 <- which(dat.quercus$PlotID==PLOT & dat.quercus$Year==2007)
  ind.18 <- which(dat.quercus$PlotID==PLOT & dat.quercus$Year==2018)
  
  tree.2007 <- dat.tree[dat.tree$Genus=="Quercus" & dat.tree$PlotID==PLOT & dat.tree$Year==2007,]
  tree.2018 <- dat.tree[dat.tree$Genus=="Quercus" & dat.tree$PlotID==PLOT & dat.tree$Year==2018,]
  if(nrow(tree.2007)>0){
    dat.quercus[ind.07,"tree.count"] <- sum(tree.2007$Count, na.rm=T)
    dat.quercus[ind.07,"tree.BA"] <- sum(tree.2007$BA.tot, na.rm=T)
    
    if(nrow(tree.2018)==0){
      dat.quercus[ind.18,"tree.count.change"] <- -sum(tree.2007$Count, na.rm=T)
      dat.quercus[ind.18,"tree.BA.change"] <- -sum(tree.2007$BA.tot, na.rm=T)
    } 
  }
  if(nrow(tree.2018)>0){
    dat.quercus[ind.18,"tree.count"] <- sum(tree.2018$Count, na.rm=T)
    dat.quercus[ind.18,"tree.BA"] <- sum(tree.2018$BA.tot, na.rm=T)
    
    if(nrow(tree.2007)>0){
      dat.quercus[ind.18,"tree.count.change"] <- sum(tree.2018$Count, na.rm=T) - sum(tree.2007$Count, na.rm=T)
      dat.quercus[ind.18,"tree.BA.change"] <- sum(tree.2018$BA.tot, na.rm=T) - sum(tree.2007$BA.tot, na.rm=T)
      
    } else {
      dat.quercus[ind.18,"tree.count.change"] <- sum(tree.2018$Count, na.rm=T)
      dat.quercus[ind.18,"tree.BA.change"] <- sum(tree.2018$BA.tot, na.rm=T)
    }
  }
  
  
  shrub.07 <- dat.shrub[dat.shrub$Genus=="Quercus" & dat.shrub$PlotID==PLOT & dat.shrub$Year==2007,]
  shrub.18 <- dat.shrub[dat.shrub$Genus=="Quercus" & dat.shrub$PlotID==PLOT & dat.shrub$Year==2018,]
  if(nrow(shrub.07)>0) dat.quercus[ind.07,"shrub.count"] <- sum(shrub.07$Count, na.rm=T)
  if(nrow(shrub.18)>0) dat.quercus[ind.18,"shrub.count"] <- sum(shrub.18$Count, na.rm=T)
  
  if(nrow(shrub.07)>0 & nrow(shrub.18)==0) dat.quercus[ind.18,"shrub.count.change"] <- -sum(shrub.07$Count, na.rm=T)
  if(nrow(shrub.07)==0 & nrow(shrub.18)>0) dat.quercus[ind.18,"shrub.count.change"] <- sum(shrub.18$Count, na.rm=T)
  if(nrow(shrub.07)>0 & nrow(shrub.18)>0) dat.quercus[ind.18,"shrub.count.change"] <- sum(shrub.18$Count, na.rm=T) - sum(shrub.07$Count, na.rm=T)
  
  herb.07 <- dat.herb[dat.herb$Genus=="Quercus" & dat.herb$PlotID==PLOT & dat.herb$Year==2007,]
  herb.18 <- dat.herb[dat.herb$Genus=="Quercus" & dat.herb$PlotID==PLOT & dat.herb$Year==2018,]
  if(nrow(herb.07)>0) dat.quercus[ind.07,"seed.cover"] <- sum(herb.07$Cover.Est, na.rm=T)
  if(nrow(herb.18)>0) dat.quercus[ind.18,"seed.cover"] <- sum(herb.18$Cover.Est, na.rm=T)

  if(nrow(herb.07)>0 & nrow(herb.18)==0) dat.quercus[ind.18,"seed.cover.change"] <- -sum(herb.07$Cover.Est, na.rm=T)
  if(nrow(herb.07)==0 & nrow(herb.18)>0) dat.quercus[ind.18,"seed.cover.change"] <- sum(herb.18$Cover.Est, na.rm=T)
  if(nrow(herb.07)>0 & nrow(herb.18)>0) dat.quercus[ind.18,"seed.cover.change"] <- sum(herb.18$Cover.Est, na.rm=T) - sum(herb.07$Cover.Est, na.rm=T)
  
}
summary(dat.quercus)
summary(dat.quercus[dat.quercus$Year==2018,])

# Looking at changes in the tree layer
length(which(dat.quercus$Year==2007 & !is.na(dat.quercus$tree.BA))); length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$tree.BA)))
length(which(dat.quercus$Year==2018 & dat.quercus$tree.BA.change<0))/length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$tree.BA.change)))
length(which(dat.quercus$Year==2018 & dat.quercus$tree.BA.change>0))/length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$tree.BA.change)))

# Shrub/sapling layer
length(which(dat.quercus$Year==2007 & !is.na(dat.quercus$shrub.count))); length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$shrub.count)))
length(which(dat.quercus$Year==2018 & dat.quercus$shrub.count.change<0))/length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$shrub.count.change)))
length(which(dat.quercus$Year==2018 & dat.quercus$shrub.count.change>0))/length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$shrub.count.change)))

# Seedling layer
length(which(dat.quercus$Year==2007 & !is.na(dat.quercus$seed.cover))); length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$seed.cover)))
length(which(dat.quercus$Year==2018 & dat.quercus$seed.cover.change<0))/length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$seed.cover.change)))
length(which(dat.quercus$Year==2018 & dat.quercus$seed.cover.change>0))/length(which(dat.quercus$Year==2018 & !is.na(dat.quercus$seed.cover.change)))

# ----------------------------


# ----------------------------
# Looking at 2007 & 2018 side by side
# ----------------------------
png(file.path(path.out, "figures/OakDynamics", "Quercus_Tree_BA_Comparision.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.quercus[!is.na(dat.quercus$tree.BA),]) +
  ggtitle("Total Live Oak Basal Area & Stem Density") +
  # coord_equal(xlim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  coord_equal(xlim=range(dat.quercus[,"x.nad83"], na.rm=T), ylim=range(dat.quercus[,"y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=tree.BA, size=tree.count)) +
  scale_color_gradient2(name="Basal Area", low="red", high="blue3", mid="white", midpoint=median(dat.quercus$tree.BA, na.rm=T)) +
  scale_size_continuous(name="Stem\nDensity") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures/OakDynamics", "Quercus_Sampling_Count_Comparision.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.quercus[!is.na(dat.quercus$shrub.count),]) +
  ggtitle("Oak Sapling Count") +
  # coord_equal(xlim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  coord_equal(xlim=range(dat.quercus[,"x.nad83"], na.rm=T), ylim=range(dat.quercus[,"y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=shrub.count), size=3) +
  scale_color_gradient2(name="Number Saplings", low="red", high="blue3", mid="white", midpoint=median(dat.quercus$shrub.count, na.rm=T)) +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures/OakDynamics", "Quercus_Seedling_Cover_Comparision.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.quercus[!is.na(dat.quercus$seed.cover),]) +
  ggtitle("Oak Seedling Cover") +
  # coord_equal(xlim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  coord_equal(xlim=range(dat.quercus[,"x.nad83"], na.rm=T), ylim=range(dat.quercus[,"y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=seed.cover), size=3) +
  scale_color_gradient2(name="Seedling Cover", low="red", high="blue3", mid="white", midpoint=median(dat.quercus$seed.cover, na.rm=T)) +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()
# ----------------------------

# ----------------------------
# Lookign at change in Each layer
# ----------------------------
png(file.path(path.out, "figures/OakDynamics", "Quercus_Tree_BA_Change.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.quercus[dat.quercus$Year==2018 & !is.na(dat.quercus$tree.BA.change),]) +
  ggtitle("Change in Oak Basal Area") +
  # coord_equal(xlim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  coord_equal(xlim=range(dat.quercus[,"x.nad83"], na.rm=T), ylim=range(dat.quercus[,"y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=tree.BA.change), size=3) +
  scale_color_gradient2(name="Basal Area", low="red", high="green3", mid="white") +
  # scale_size_continuous(name="Stem\nDensity") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures/OakDynamics", "Quercus_Tree_Count_Change.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.quercus[dat.quercus$Year==2018 & !is.na(dat.quercus$tree.count.change),]) +
  ggtitle("Change in Oak Stem Count") +
  # coord_equal(xlim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  coord_equal(xlim=range(dat.quercus[,"x.nad83"], na.rm=T), ylim=range(dat.quercus[,"y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=tree.count.change), size=3) +
  scale_color_gradient2(name="Number Stems", low="red", high="green3", mid="white") +
  # scale_size_continuous(name="Stem\nDensity") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures/OakDynamics", "Quercus_Sampling_Count_Change.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.quercus[dat.quercus$Year==2018 & !is.na(dat.quercus$shrub.count.change),]) +
  ggtitle("Change in Oak Sapling Tally") +
  # coord_equal(xlim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  coord_equal(xlim=range(dat.quercus[,"x.nad83"], na.rm=T), ylim=range(dat.quercus[,"y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=shrub.count.change), size=3) +
  scale_color_gradient2(name="Number Stems", low="red", high="green3", mid="white") +
  # scale_size_continuous(name="Stem\nDensity") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures/OakDynamics", "Quercus_Seedling_Cover_Change.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.quercus[dat.quercus$Year==2018 & !is.na(dat.quercus$seed.cover.change),]) +
  ggtitle("Change in Oak Seedling Cover") +
  # coord_equal(xlim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.quercus[dat.quercus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  coord_equal(xlim=range(dat.quercus[,"x.nad83"], na.rm=T), ylim=range(dat.quercus[,"y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=seed.cover.change), size=3) +
  scale_color_gradient2(name="Percent Cover", low="red", high="green3", mid="white") +
  # scale_size_continuous(name="Stem\nDensity") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()
# ----------------------------
