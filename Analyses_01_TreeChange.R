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


# ----------------------------
# Merge datasets into a long format to make code less redundant below; 
# we'll have to reshape for analyeses
# ----------------------------
dat.tree.all <- rbind(tree.2007[,c("Year", "PlotID", "Spp.Name", "Spp.Code", "Status", "DBH")], tree.2018[,c("Year", "PlotID", "Spp.Name", "Spp.Code", "Status", "DBH")])
dat.tree.all$Genus <- as.factor(unlist(lapply(stringr::str_split(dat.tree.all$Spp.Name, " "), function(x) x[1])))
dat.tree.all$BA <- pi*(dat.tree.all$DBH/2)^2 # in cm2
summary(dat.tree.all)

write.csv(dat.tree.all, file.path(path.out, "data_processed", "InventoryData_Trees_Merged_2007_2017.csv"), row.names=F)
# ----------------------------
# -----------------------------------------------------------

# -----------------------------------------------------------
# Analysis of change in key groups from 2007 to 2018
# Note: We do not have tagged trees, this must be done at the plot level
# Note: Because of inconsistencies in speices ID or naming, the aggregation for 
#       each level needs to be done separately for genus and taxa-scale analses
# Analysis Groups: Quercus spp., Fraxinus spp., Acer spp., Quercus alba, Q. rubra, & Q. macrocarpa, 
# -----------------------------------------------------------

# ----------------------------
# Quick comparison of species found in each genus
# ----------------------------
# Quercus
length(which(dat.tree.all$Status=="live" & dat.tree.all$Year==2007 & dat.tree.all$Genus=="Quercus"))
length(which(dat.tree.all$Status=="live" & dat.tree.all$Year==2018 & dat.tree.all$Genus=="Quercus"))

summary(droplevels(dat.tree.all[dat.tree.all$Status=="live" & dat.tree.all$Year==2007 & dat.tree.all$Genus=="Quercus", "Spp.Name"]))
summary(droplevels(dat.tree.all[dat.tree.all$Status=="live" & dat.tree.all$Year==2018 & dat.tree.all$Genus=="Quercus", "Spp.Name"]))

# Acer
length(which(dat.tree.all$Status=="live" & dat.tree.all$Year==2007 & dat.tree.all$Genus=="Acer"))
length(which(dat.tree.all$Status=="live" & dat.tree.all$Year==2018 & dat.tree.all$Genus=="Acer"))

summary(droplevels(dat.tree.all[dat.tree.all$Status=="live" & dat.tree.all$Year==2007 & dat.tree.all$Genus=="Acer", "Spp.Name"]))
summary(droplevels(dat.tree.all[dat.tree.all$Status=="live" & dat.tree.all$Year==2018 & dat.tree.all$Genus=="Acer", "Spp.Name"]))

# Fraxinus
length(which(dat.tree.all$Status=="live" & dat.tree.all$Year==2007 & dat.tree.all$Genus=="Fraxinus"))
length(which(dat.tree.all$Status=="live" & dat.tree.all$Year==2018 & dat.tree.all$Genus=="Fraxinus"))

summary(droplevels(dat.tree.all[dat.tree.all$Status=="live" & dat.tree.all$Year==2007 & dat.tree.all$Genus=="Fraxinus", "Spp.Name"]))
summary(droplevels(dat.tree.all[dat.tree.all$Status=="live" & dat.tree.all$Year==2018 & dat.tree.all$Genus=="Fraxinus", "Spp.Name"]))

# ----------------------------

# ----------------------------
# Genus-level Comparisons
# ----------------------------
gen.interest <- c("Quercus", "Acer", "Fraxinus")  

dat.plot.gen <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus")], FUN=sum, na.rm=F)
names(dat.plot.gen)[which(names(dat.plot.gen)=="x")] <- c("BA.tot")
dat.plot.gen$density <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus")], FUN=length)[,"x"]
dat.plot.gen$DBH.mean <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus")], FUN=mean)[,"x"]
dat.plot.gen$DBH.sd <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus")], FUN=sd)[,"x"]
summary(dat.plot.gen)

png(file.path(path.out, "figures", "BasalArea_Comparison_Genera_Focal.png"), height=6, width=6, units="in", res=180)
ggplot(data=dat.plot.gen[dat.plot.gen$Status=="live" & dat.plot.gen$Genus %in% gen.interest, ]) +
  facet_wrap(~Genus) +
  geom_boxplot(aes(x=as.factor(Year), y=BA.tot, fill=as.factor(Year))) +
  scale_x_discrete(name="Year") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  guides(fill=F) +
  theme_bw() 
dev.off()

# Reshaping to directly compare 2007 & 2008
dat.plot.gen$Year <- as.factor(dat.plot.gen$Year)
plot.gen.ba <- tidyr::spread(dat.plot.gen[dat.plot.gen$Status=="live",c("PlotID", "Genus", "Year", "BA.tot")], Year, BA.tot)
plot.gen.ba[is.na(plot.gen.ba)] <- 0
plot.gen.ba$Change <- plot.gen.ba$`2018` - plot.gen.ba$`2007`
summary(plot.gen.ba)

# Summarizing Change
change.gen <- aggregate(plot.gen.ba$Change, by=list(plot.gen.ba$Genus), FUN=mean)
names(change.gen) <- c("Genus", "diff.mean")
change.gen$diff.sd <- aggregate(plot.gen.ba$Change, by=list(plot.gen.ba$Genus), FUN=sd)[,"x"]
summary(change.gen)

png(file.path(path.out, "figures", "BasalArea_Change_Genera_Focal.png"), height=6, width=6, units="in", res=180)
ggplot(data=plot.gen.ba[plot.gen.ba$Genus %in% gen.interest, ]) +
  facet_grid(Genus~.) +
  geom_histogram(aes(x=Change, fill=Genus)) +
  geom_text(data=change.gen[change.gen$Genus %in% gen.interest, ], x=-12500, y=125, aes(label=paste0("mean = ", round(diff.mean, 0), "\nsd = ", round(diff.sd, 0)))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(name="Change in Basal Area (cm2)") +
  scale_y_continuous(name="Number Plots Showing Change") +
  guides(fill=F) +
  theme_bw() 
dev.off()

# ----------------------------


# ----------------------------
# Species-level comparisons
# ----------------------------
spp.quercus <- c("alba", "rubra", "macrocarpa")

# Species-level aggregations
dat.plot.spp <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus", "Spp.Name")], FUN=sum, na.rm=F)
names(dat.plot.spp)[which(names(dat.plot.spp)=="x")] <- c("BA.tot")
dat.plot.spp$density <- aggregate(dat.tree.all$BA, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus", "Spp.Name")], FUN=length)[,"x"]
dat.plot.spp$DBH.mean <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus", "Spp.Name")], FUN=mean)[,"x"]
dat.plot.spp$DBH.sd <- aggregate(dat.tree.all$DBH, by=dat.tree.all[,c("Year", "PlotID", "Status", "Genus", "Spp.Name")], FUN=sd)[,"x"]
summary(dat.plot.spp)

png(file.path(path.out, "figures", "BasalArea_Comparison_Species_Quercus_Focal.png"), height=6, width=6, units="in", res=180)
ggplot(data=dat.plot.spp[dat.plot.spp$Status=="live" & dat.plot.spp$Spp.Name %in% paste("Quercus", spp.quercus), ]) +
  facet_wrap(~Spp.Name) +
  geom_boxplot(aes(x=as.factor(Year), y=BA.tot, fill=as.factor(Year))) +
  scale_x_discrete(name="Year") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  guides(fill=F) +
  theme_bw() 
dev.off()

# Reshaping to directly compare 2007 & 2008
dat.plot.spp$Year <- as.factor(dat.plot.spp$Year)
plot.spp.ba <- tidyr::spread(dat.plot.spp[dat.plot.spp$Status=="live",c("PlotID", "Genus", "Spp.Name", "Year", "BA.tot")], Year, BA.tot)
plot.spp.ba[is.na(plot.spp.ba)] <- 0
plot.spp.ba$Change <- plot.spp.ba$`2018` - plot.spp.ba$`2007`
summary(plot.spp.ba)

# Summarizing Change
change.spp <- aggregate(plot.spp.ba$Change, by=plot.spp.ba[,c("Genus", "Spp.Name")], FUN=mean)
names(change.spp)[which(names(change.spp)=="x")] <- "diff.mean"
change.spp$diff.sd <- aggregate(plot.spp.ba$Change, by=plot.spp.ba[,c("Genus", "Spp.Name")], FUN=sd)[,"x"]
summary(change.spp)

png(file.path(path.out, "figures", "BasalArea_Change_Species_Quercus_Focal.png"), height=6, width=6, units="in", res=180)
ggplot(data=plot.spp.ba[plot.spp.ba$Spp.Name %in% paste("Quercus", spp.quercus), ]) +
  facet_grid(Spp.Name~.) +
  geom_histogram(aes(x=Change, fill=Spp.Name)) +
  geom_text(data=change.spp[change.spp$Spp.Name %in% paste("Quercus", spp.quercus), ], x=-7500, y=40, aes(label=paste0("mean = ", round(diff.mean, 0), "\nsd = ", round(diff.sd, 0)))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(name="Change in Basal Area (cm2)") +
  scale_y_continuous(name="Number Plots Showing Change") +
  guides(fill=F) +
  theme_bw() 
dev.off()
# ----------------------------
# -----------------------------------------------------------
