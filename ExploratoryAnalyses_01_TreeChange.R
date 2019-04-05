# Exploratory Analyses
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
summary(shrub.2007)

shrub.2007$Category <- car::recode(shrub.2007$Category, "'T'='UT'")
shrub.2007$Count.Alive <- ifelse(shrub.2007$Count.Alive=="100+", 100, shrub.2007$Count.Alive)
shrub.2007$Count.Alive <- as.numeric(shrub.2007$Count.Alive)

summary(droplevels(shrub.2007[shrub.2007$Category=="TS","Spp.Name"]))
summary(droplevels(shrub.2007[shrub.2007$Category=="UT","Spp.Name"]))

# Comparing
summary(droplevels(shrub.2007[shrub.2007$Category=="TS","Spp.Name"]))
summary(droplevels(shrub.2018[!is.na(shrub.2018$Category.Sapling),"Spp.Name"]))

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
summary(herb.2007)

summary(droplevels(herb.2007[herb.2007$Form=="tree","Spp.Name"]))
# -------------
# ----------------------------


# ----------------------------
# Putting data together & looking at changes
# ----------------------------
dat.tree.all <- rbind(tree.2007[,c("Year", "PlotID", "Spp.Name", "Spp.Code", "Status", "DBH")], tree.2018[,c("Year", "PlotID", "Spp.Name", "Spp.Code", "Status", "DBH")])
dat.tree.all$Genus <- as.factor(unlist(lapply(stringr::str_split(dat.tree.all$Spp.Name, " "), function(x) x[1])))
dat.tree.all$BA <- pi*(dat.tree.all$DBH/2)^2 # in cm2
summary(dat.tree.all)

# -------
# Doing some full-plot summaries
# -------
dat.plot <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Status")], FUN=sum, na.rm=F)
names(dat.plot)[which(names(dat.plot)=="x")] <- c("BA.tot")
dat.plot$density <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Status")], FUN=length)[,"x"]
dat.plot$DBH.mean <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Status")], FUN=mean)[,"x"]
dat.plot$DBH.sd <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Status")], FUN=sd)[,"x"]
summary(dat.plot)

# Add back in our plots without any trees
# dat.plot <- merge(dat.plot, dat.tree.all[is.na(dat.tree.all$Status), c("Year", "PlotID", "Status")], all=T)
# summary(dat.plot)

dat.plot <- merge(dat.plot, imls.spat[,c("PlotID", "x.nad83", "y.nad83", "x.utm16", "y.utm16", "lon", "lat", "PlotID2", "wooded", "AreaName")], all.x=T)
summary(dat.plot)

pb <- txtProgressBar(min=0, max=length(unique(dat.plot$PlotID)), style = 3)
pb.ind=0
for(PLT in unique(dat.plot$PlotID)){
  pb.ind=pb.ind+1
  setTxtProgressBar(pb, pb.ind)
  if(all(is.na(dat.plot[dat.plot$PlotID==PLT, "Status"]))) next
  for(STAT in c("live", "dead")){
    ind.2007 <- which(dat.plot$PlotID==PLT & dat.plot$Year==2007 & dat.plot$Status==STAT)
    ind.2018 <- which(dat.plot$PlotID==PLT & dat.plot$Year==2018 & dat.plot$Status==STAT)
    
    dat.plot[ind.2018, "BA.diff"] <- max(0, dat.plot[ind.2018, "BA.tot"]) - max(0,dat.plot[ind.2007, "BA.tot"])
    dat.plot[ind.2018, "dens.diff"] <- max(0, dat.plot[ind.2018, "density"]) - max(0,dat.plot[ind.2007, "density"])
    
    if(STAT!="live") next # If we're not working with live stuff, we can't calculate the dominant taxa
    
    dat.tmp <- dat.tree.all[dat.tree.all$PlotID==PLT & dat.tree.all$Status==STAT,]
    if(nrow(dat.tmp)<1) next
    if(all(is.na(dat.tmp$PlotID))) next
    
    dat.plot[ind.2007,"Spp.Rich"] <- length(unique(dat.tmp[dat.tmp$Year==2007, "Spp.Name"]))
    dat.plot[ind.2018,"Spp.Rich"] <- length(unique(dat.tmp[dat.tmp$Year==2018, "Spp.Name"]))
    dat.plot[ind.2018, "Rich.diff"] <- max(0,dat.plot[ind.2018, "Spp.Rich"]) - max(0, dat.plot[ind.2007, "Spp.Rich"])
    
    # Finding dominant species
    dat.tmp2 <- aggregate(dat.tmp$BA, by=dat.tmp[,c("Year", "Spp.Name")], FUN=sum)
    names(dat.tmp2)[which(names(dat.tmp2)=="x")] <- "BA"
    dat.tmp2$dens <- aggregate(dat.tmp$BA, by=dat.tmp[,c("Year", "Spp.Name")], FUN=length)[,"x"]
    
    dat.tmp2[dat.tmp2$Year==2007, "IV.BA"] <- dat.tmp2[dat.tmp2$Year==2007, "BA"]/sum(dat.tmp2[dat.tmp2$Year==2007, "BA"])
    dat.tmp2[dat.tmp2$Year==2007, "IV.dens"] <- dat.tmp2[dat.tmp2$Year==2007, "dens"]/sum(dat.tmp2[dat.tmp2$Year==2007, "dens"])
    dat.tmp2[dat.tmp2$Year==2018, "IV.BA"] <- dat.tmp2[dat.tmp2$Year==2018, "BA"]/sum(dat.tmp2[dat.tmp2$Year==2018, "BA"])
    dat.tmp2[dat.tmp2$Year==2018, "IV.dens"] <- dat.tmp2[dat.tmp2$Year==2018, "dens"]/sum(dat.tmp2[dat.tmp2$Year==2018, "dens"])
    
    dat.tmp2$IV.mean <- apply(dat.tmp2[,c("IV.BA", "IV.dens")], 1, mean)
    
    if(nrow(dat.tmp2[dat.tmp2$Year==2007,])>0){
      dat.plot[ind.2007,"Spp.Dom"] <- dat.tmp2[dat.tmp2$Year==2007 & dat.tmp2$IV.mean==max(dat.tmp2$IV.mean[which(dat.tmp2$Year==2007)]),"Spp.Name"]
    }
    if(nrow(dat.tmp2[dat.tmp2$Year==2018,])>0){
      dat.plot[ind.2018,"Spp.Dom"] <- dat.tmp2[dat.tmp2$Year==2018 & dat.tmp2$IV.mean==max(dat.tmp2$IV.mean[which(dat.tmp2$Year==2018)]),"Spp.Name"]
    }
    
    if(nrow(dat.plot[c(ind.2007,ind.2018),])<2) {
      dat.plot[ind.2018, "Spp.Change"] <- "CHANGE"
    } else {
      dat.plot[ind.2018, "Spp.Change"] <- ifelse(dat.plot[ind.2018,"Spp.Dom"]!=dat.plot[ind.2007,"Spp.Dom"], "CHANGE", "NO")
    }
  } # End Status loop

  
} # End plot loop


dat.plot$Genus <- as.factor(unlist(lapply(stringr::str_split(dat.plot$Spp.Dom, " "), function(x) x[1])))
dat.plot$oak.dom <- ifelse(dat.plot$Genus=="Quercus", dat.plot$Spp.Dom, "Other")
summary(dat.plot)

summary(droplevels(dat.plot[dat.plot$Year==2007,"Spp.Dom"]))

png(file.path(path.out, "figures", "BasalArea_Density_TotLive.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Status=="live",]) +
  ggtitle("Total Live Basal Area & Stem Density") +
  coord_equal(xlim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=BA.tot, size=density)) +
  scale_color_gradient2(name="Basal Area", low="white", high="blue3", mid="white") +
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

png(file.path(path.out, "figures", "SpeciesRichness_TotLive.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Status=="live",]) +
  ggtitle("Species Richness") +
  coord_equal(xlim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=Spp.Rich, size=BA.tot)) +
  scale_color_gradient2(name="# Species", low="white", high="orange3", mid="white") +
  scale_size_continuous(name="Basal\nArea") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures", "SpeciesDominant_TotLive.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Status=="live",]) +
  ggtitle("Dominant Species") +
  coord_equal(xlim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=Spp.Dom, size=BA.tot)) +
  scale_color_discrete(name="Dominant\nSpecies") +
  # scale_color_gradient2(name="# Species", low="white", high="orange3", mid="white") +
  scale_size_continuous(name="Basal\nArea") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures", "Change_SpeciesDominant_TotLive.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Status=="live" & dat.plot$Year==2018,]) +
  ggtitle("Sites with Change in Dominant Species") +
  coord_equal(xlim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  # facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=Spp.Change, size=BA.tot)) +
  scale_color_manual(name="Dominant\nSpecies", values=c("orange2", "gray30")) +
  # scale_color_gradient2(name="# Species", low="white", high="orange3", mid="white") +
  scale_size_continuous(name="Basal\nArea") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures", "SpeciesDominant_TotLive_Genus.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Status=="live",]) +
  ggtitle("Dominant Genus") +
  coord_equal(xlim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  facet_wrap(~Year) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=Genus, size=BA.tot)) +
  scale_color_discrete(name="Dominant\nGenus") +
  # scale_color_gradient2(name="# Species", low="white", high="orange3", mid="white") +
  scale_size_continuous(name="Basal\nArea") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size=rel(2))) +
  theme(legend.position = "bottom")
dev.off()

png(file.path(path.out, "figures", "Change_BasalArea_TotLive.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Year==2018 & dat.plot$Status=="live",]) +
  ggtitle("Change in Basal Area 2007-2018") +
  coord_equal(xlim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  # facet_wrap(~Status) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=BA.diff, size=BA.tot)) +
  scale_color_gradient2(name="Basal Area\nChange", low="#d8b365", high="#5ab4ac", mid="white", midpoint=0) +
  scale_size_continuous(name="Basal Area\n2018") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
dev.off()

png(file.path(path.out, "figures", "Change_Density_TotLive.png"), height=8, width=10, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Year==2018 & dat.plot$Status=="live",]) +
  ggtitle("Change in Tree Density 2007-2018") +
  coord_equal(xlim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(dat.plot[dat.plot$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  # facet_wrap(~Status) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=dens.diff, size=density)) +
  scale_color_gradient2(name="Density\nChange", low="#d8b365", high="#5ab4ac", mid="white", midpoint=0) +
  scale_size_continuous(name="Density\n2018") +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
dev.off()
# -------

# -------
# Looking at Genus-level changes
# -------
plot.genus <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Genus", "Status")], FUN=sum)
names(plot.genus)[which(names(plot.genus)=="x")] <- c("BA.tot")
plot.genus$density <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Genus", "Status")], FUN=length)[,"x"]
plot.genus$DBH.mean <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Genus", "Status")], FUN=mean)[,"x"]
plot.genus$DBH.sd <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Genus", "Status")], FUN=sd)[,"x"]
summary(plot.genus)

# Come up with an additional data frame looking at changes by genus between 2007 & 2008
plot.genus <- merge(plot.genus, imls.spat[,c("PlotID", "x.nad83", "y.nad83", "x.utm16", "y.utm16", "lon", "lat", "PlotID2", "wooded", "AreaName")], all.x=T)
summary(plot.genus)

plot.genus[,c("IV.BA", "IV.dens", "IV.mean", "BA.diff", "density.diff")] <- NA
# Calculating relative importance value
pb <- txtProgressBar(min=0, max=length(unique(plot.genus$PlotID)), style = 3)
pb.ind=0
for(PLT in unique(plot.genus$PlotID)){
  plt.live.ind <- which(plot.genus$PlotID==PLT & plot.genus$Status=="live")
  dat.plt <- plot.genus[plt.live.ind,]
  
  BA.2018.tot <- sum(dat.plt[dat.plt$Year==2018, "BA.tot"], na.rm=T)
  BA.2007.tot <- sum(dat.plt[dat.plt$Year==2007, "BA.tot"], na.rm=T)
  dens.2018.tot <- sum(dat.plt[dat.plt$Year==2018, "density"], na.rm=T)
  dens.2007.tot <- sum(dat.plt[dat.plt$Year==2007, "density"], na.rm=T)
  
  for(TAX in unique(dat.plt$Genus)){
    if(nrow(dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2007, ])>0){
      ba.2007 <- dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2007, "BA.tot"]
      dens.2007 <- dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2007, "density"]
      
      dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2007, "IV.BA"] <- ba.2007/BA.2007.tot
      dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2007, "IV.dens"] <- dens.2007/dens.2007.tot
    } else {
      ba.2007 <- 0
      dens.2007 <- 0
    }
    
    if(nrow(dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2018, ])>0){
      ba.2018 <- dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2018, "BA.tot"]
      dens.2018 <- dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2018, "density"]
      
      dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2018, "IV.BA"] <- ba.2018/BA.2018.tot
      dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2018, "IV.dens"] <- dens.2018/dens.2018.tot
    } else {
      ba.2018 <- 0
      dens.2018 <- 0
      
      dat.tmp <- dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2007, ]
      dat.tmp$Year <- 2018
      dat.tmp[,c("BA.tot", "density", "DBH.mean", "DBH.sd")] <- NA
      
      warning(paste0("Lost Genera: ", TAX, " in plot ", PLT))
    }

    dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2018, "BA.diff"] <- ba.2018 - ba.2007
    dat.plt[dat.plt$Genus==TAX & dat.plt$Year==2018, "density.diff"] <- dens.2018 - dens.2007
    
  } # End Taxa loop
  
  plot.genus[plt.live.ind,] <- dat.plt
  
  pb.ind=pb.ind+1
  setTxtProgressBar(pb, pb.ind)
} # End plot loop
plot.genus$IV.mean <- apply(plot.genus[,c("IV.BA", "IV.dens")], 1, mean)
summary(plot.genus)

summary(plot.genus[plot.genus$Genus=="Quercus" & plot.genus$Status=="live",])
head(plot.genus[plot.genus$Genus=="Quercus" & plot.genus$Status=="live",])



png(file.path(path.out, "figures", "Change_BasalArea_Quercus.png"), height=8, width=10, units="in", res=180)
ggplot(data=plot.genus[plot.genus$Year==2018 & plot.genus$Status=="live"  & plot.genus$Genus=="Quercus",]) +
  ggtitle("Change in Basal Area 2007-2018: Quercus") +
  # facet_wrap(~Genus) +
  geom_polygon(data=woods.fort[woods.fort$hole==F,], aes(x=long, y=lat, group=group), fill="green4", alpha=0.33) +
  geom_polygon(data=woods.fort[woods.fort$hole==T,], aes(x=long, y=lat, group=group), fill="gray60", alpha=1) +
  geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=2, color="black") +
  geom_path(data=paths, aes(x=long, y=lat, group=group), size=1, color="brown", linetype="dashed") +
  geom_point(data=imls.spat, aes(x=x.nad83, y=y.nad83), size=1, color="black") +
  geom_point(aes(x=x.nad83, y=y.nad83, color=BA.diff, size=BA.tot)) +
  scale_color_gradient2(name="Basal Area\nChange", low="#d8b365", high="#5ab4ac", mid="white", midpoint=0) +
  scale_size_continuous(name="Basal Area\n2018") +
  coord_equal(xlim=range(plot.genus[plot.genus$AreaName!="Hidden Lake","x.nad83"], na.rm=T), ylim=range(plot.genus[plot.genus$AreaName!="Hidden Lake","y.nad83"], na.rm=T)) +
  theme_bw() +
  theme(panel.background=element_rect("gray60"),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
dev.off()
# -------


# ----------------------------


# -----------------------------------------------------------
