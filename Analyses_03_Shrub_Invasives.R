# High Priority: (May-June) How has the shrub layer stem count and percent cover changed over all plots and for East Woods vs. Hidden Lake for the following categories: only native shrubs, only invasive shrubs, all shrubs?
# High Priority: (May-June) How have the plots with Rhamnus and Lonicera spp. (individually and combined) changed over time across all plots, for East Woods only, Hidden Lake only, and East Woods vs. Hidden Lake?
# There are only a few plots that need to be called out for the analysis of how volunteers have impacted the invasive shrubs in Hidden Lake, which are: 
#       HH-147, GG-146, FF-146, DD-145, DD-148, CC-144, BB-144, Y-147, KK-145

# -----------------------------------------------------------
# load packages and relevant file paths
# -----------------------------------------------------------
library(ggplot2)
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods"
path.2018 <- file.path(path.ew, "Inventory 2018/Final Data from AES/Final data (4th round) from AES.10.24.2018")
path.2007 <- file.path(path.ew, "Inventory 2007")
path.out <- file.path(path.ew, "Inventory 2018/Analyses_Rollinson")
google.gis <- "/Volumes/GoogleDrive/My Drive/East Woods/GIS_files"
morton.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere 

plot.vols <- c("HH147", "GG146", "FF146", "DD145", "DD148", "CC144", "BB144", "Y147", "KK145")

# Invasive list from Kurt form 'East Woods Questions for AES data' Google Doc
spp.invasive <- c("Alliaria petiolata", "Ambrosia trifida", "Barbarea vulgaris", "Berberis thunbergii", "Celastrus orbiculatus", "Coronilla varia", "Dipsacus sylvestris", "Dipsacus sp.", "Euonymus alata", "Euonymus alatus", "Euonymus europaeus", "Euonymus fortunei", "Ficaria verna", "Hesperis matronalis", "Lonicera maackii", "Lonicera tatarica", "Melilotus alba", "Melilotus officinalis", "Melilotus sp.", "Pastinaca sativa", "Phalaris arundinacea", "Pyrus calleryana", "Rhamnus cathartica", "Rosa multiflora", "Torilis japonica", "Ailanthus altissima", "Phellodendron amurense", "Cirsium arvense", "Cirsium vulgare", "Viburnum opulus", "Ulmus pumila", "Rhamnus frangula", "Ligustrum sp.", "Acer platanoides")

# Load in Spatial File so we can look at patterns & changes as a map
imls.spat <- read.csv(file.path(path.ew, "Inventory 2018/Analyses_Rollinson/data_processed/point_info_GIS.csv"))
imls.spat$PlotID2 <- imls.spat$PlotID
imls.spat$PlotID <- gsub("-", "", imls.spat$PlotID)
imls.spat$PlotID <- as.factor(imls.spat$PlotID)
summary(imls.spat); head(imls.spat)

imls.spat$MgmtUnit <- ifelse(is.na(imls.spat$wooded), "Non-Wooded", 
                           ifelse(imls.spat$wooded=="Hidden Lake", "Hidden Lake", 
                                  ifelse(imls.spat$unit=="South 40 South", "Annual Burn", 
                                         ifelse(!is.na(imls.spat$unit), "Mixed Management", "No Management"))))
imls.spat$MgmtUnit[is.na(imls.spat$MgmtUnit)] <- "No Management"

imls.spat$MgmtUnit2 <- ifelse(imls.spat$PlotID %in% plot.vols, "Volunteers", imls.spat$MgmtUnit)
imls.spat$MgmtUnit3 <- ifelse(imls.spat$MgmtUnit=="Hidden Lake", "No Management", imls.spat$MgmtUnit)
imls.spat$MgmtUnit <- factor(imls.spat$MgmtUnit, levels=c("Annual Burn", "Mixed Management", "No Management", "Hidden Lake", "Non-Wooded"))
imls.spat$MgmtUnit2 <- factor(imls.spat$MgmtUnit2, levels=c("Annual Burn", "Mixed Management", "No Management", "Hidden Lake", "Volunteers", "Non-Wooded"))
imls.spat$MgmtUnit2 <- as.factor(imls.spat$MgmtUnit2)
imls.spat$MgmtUnit3 <- as.factor(imls.spat$MgmtUnit3)
summary(imls.spat)


plots.ew <- imls.spat$PlotID[which(imls.spat$wooded=="East Woods")]
plots.hl <- imls.spat$PlotID[which(imls.spat$wooded=="Hidden Lake")]

# -----------------------------------------------------------

# -----------------------------------------------------------
# Reading and formatting Shrub Datasets
# -----------------------------------------------------------
shrub.2018 <- data.frame(readxl::read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-22.xlsx"), sheet = "Shrub Layer"))
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

shrub.2018 <- merge(shrub.2018, imls.spat[,c("PlotID", "x.nad83", "y.nad83", "x.utm16", "y.utm16", "lon", "lat", "PlotID2", "wooded", "unit", "MgmtUnit", "MgmtUnit2", "MgmtUnit3")], all.x=T, all.y=T)
shrub.2018[is.na(shrub.2018$Count), "Count"] <- 0
summary(shrub.2018)
shrub.2018[is.na(shrub.2018$MgmtUnit2),]


shrub.2007 <- data.frame(readxl::read_excel(file.path(path.2007, "Summer Vegetation Sampling-RevisedFinal_2_20.CORRECTED PLOTS.xls"), sheet="Shrub Layer", skip=1, na=c("-", "")))
names(shrub.2007) <- c( "PlotID", "Spp.Code", "Spp.Name", "Count.Alive", "Count.Dead", "Native", "Category")
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

shrub.2007 <- merge(shrub.2007, imls.spat[,c("PlotID", "x.nad83", "y.nad83", "x.utm16", "y.utm16", "lon", "lat", "PlotID2", "wooded", "unit", "MgmtUnit", "MgmtUnit2", "MgmtUnit3")], all.x=T, all.y=T)
shrub.2007[is.na(shrub.2007$Count), "Count"] <- 0
summary(shrub.2007)


unique(shrub.2007[shrub.2007$Native=="Alien","Spp.Name"])

shrub.bind <- rbind(shrub.2007[,c("Year", "PlotID", "MgmtUnit", "MgmtUnit2", "MgmtUnit3", "Spp.Name", "Count", "Category")], 
                    shrub.2018[,c("Year", "PlotID", "MgmtUnit", "MgmtUnit2", "MgmtUnit3", "Spp.Name", "Count", "Category")])

shrub.bind[shrub.bind$Spp.Name %in% c("NONE", "No shrubs/seedlings"), "Spp.Name"] <- NA
shrub.bind$Spp.Name <- car::recode(shrub.bind$Spp.Name, "'Viburnumspecies'='Viburnum species'; 
                                                         'Unidentified species'='UNIDENTIFIED'; 
                                                         'Unidentified sp.'='UNIDENTIFIED'; 
                                                         '??'='UNIDENTIFIED'")
shrub.bind$Spp.Name <- gsub("species", "sp.", shrub.bind$Spp.Name)

shrub.bind[grep("Lonicera", shrub.bind$Spp.Name), "Invasive"] <- "Lonicera"
shrub.bind[grep("Rhamnus", shrub.bind$Spp.Name), "Invasive"] <- "Rhamnus"
shrub.bind[is.na(shrub.bind$Invasive) & shrub.bind$Spp.Name %in% spp.invasive, "Invasive"] <- "Other Invasive"
shrub.bind$Invasive[is.na(shrub.bind$Invasive)] <- "non-invasive"
shrub.bind$Invasive <- as.factor(shrub.bind$Invasive)
shrub.bind$Invasive2 <- as.factor(ifelse(shrub.bind$Invasive=="non-invasive", "non-invasive", "invasive"))
shrub.bind$PlotID <- as.factor(shrub.bind$PlotID)
# shrub.bind$MgmtUnit2[is.na(shrub.bind$MgmtUnit2)] <- "Other"
summary(shrub.bind)

shrub.agg <- aggregate(shrub.bind[,c("Count")], by=shrub.bind[,c("Year", "PlotID", "MgmtUnit", "MgmtUnit2", "MgmtUnit3", "Invasive", "Invasive2")], FUN=sum)
names(shrub.agg)[which(names(shrub.agg)=="x")] <- c("Count")
shrub.agg$PlotID <- as.factor(shrub.agg$PlotID)
shrub.agg$Invasive <- factor(shrub.agg$Invasive, levels=c("Lonicera", "Rhamnus", "Other Invasive", "non-invasive"))
summary(shrub.agg)

# -----------------------------------------------------------
# Comparing across groups & years
# -----------------------------------------------------------
png(file.path(path.out, "figures", "Shrub_Invasives_Density_Comparison_Plot_MgmtUnit2.png"), height=6, width=8, units="in", res=180)
ggplot(data=shrub.agg) +
  ggtitle("Invasive Shrubs") +
  facet_grid(Invasive ~ Year, scales="free") +
  geom_boxplot(aes(x=MgmtUnit3, y=log(Count), fill=MgmtUnit2)) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.x = element_text(angle=-30, hjust=0),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold"))
dev.off()

png(file.path(path.out, "figures", "Shrub_Invasives_Density_Comparison_Plot_MgmtUnit2_Year.png"), height=6, width=8, units="in", res=180)
ggplot(data=shrub.agg) +
  ggtitle("Invasive Shrubs") +
  facet_grid(Invasive ~ MgmtUnit3, scales="free") +
  geom_boxplot(aes(x=as.factor(Year), y=log(Count), fill=MgmtUnit2)) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.spacing = unit(0, "lines"),
        axis.text.x = element_text(angle=-30, hjust=0),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold"))
dev.off()
# -----------------------------------------------------------


# -----------------------------------------------------------
# Looking at the change
# -----------------------------------------------------------
# Reshaping to directly compare 2007 & 2008
shrub.agg$Year <- as.factor(shrub.agg$Year)
shrub.agg2 <- tidyr::spread(shrub.agg[,c("PlotID", "MgmtUnit", "MgmtUnit2", "MgmtUnit3", "Invasive", "Invasive2", "Year", "Count")], Year, Count, fill=0)
# shrub.agg2[is.na(shrub.agg2)] <- 0
shrub.agg2$Change <- shrub.agg2$`2018` - shrub.agg2$`2007`
summary(shrub.agg2)
 
mgmt.grp.agg <- aggregate(shrub.agg2$Change, 
                          by=shrub.agg2[,c("MgmtUnit2", "MgmtUnit3", "Invasive")], FUN=mean)
names(mgmt.grp.agg)[names(mgmt.grp.agg)=="x"] <- "Change.mean"
mgmt.grp.agg$Change.sd <- aggregate(shrub.agg2$Change, by=shrub.agg2[,c("MgmtUnit2", "Invasive")], FUN=sd)$x

# Hand-picking the x & y label locations
mgmt.grp.agg[mgmt.grp.agg$MgmtUnit2=="Annual Burn", "lab.x"] <- -17
mgmt.grp.agg[mgmt.grp.agg$MgmtUnit2=="Hidden Lake", "lab.x"] <- 5
mgmt.grp.agg[mgmt.grp.agg$MgmtUnit2=="Mixed Management", "lab.x"] <- -45
mgmt.grp.agg[mgmt.grp.agg$MgmtUnit2=="No Management", "lab.x"] <- 5
mgmt.grp.agg[mgmt.grp.agg$MgmtUnit2=="Non-Wooded", "lab.x"] <- -90
mgmt.grp.agg[mgmt.grp.agg$MgmtUnit2=="Volunteers", "lab.x"] <- -25

mgmt.grp.agg[mgmt.grp.agg$Invasive=="Lonicera", "lab.y"] <- 0.16
mgmt.grp.agg[mgmt.grp.agg$Invasive=="Rhamnus", "lab.y"] <- 0.12
mgmt.grp.agg[mgmt.grp.agg$Invasive=="Other Invasive", "lab.y"] <- 0.22
mgmt.grp.agg[mgmt.grp.agg$Invasive=="non-invasive", "lab.y"] <- 0.5

png(file.path(path.out, "figures", "Shrub_Invasives_Change_DensityPlot_MgmtUnit3.png"), height=6, width=8, units="in", res=180)
ggplot(data=shrub.agg2[,]) +
  facet_grid(Invasive~MgmtUnit3, scales="free") +
  # geom_histogram(aes(x=Change, fill=MgmtUnit2)) +
  geom_density(aes(x=Change, fill=MgmtUnit2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_text(data=mgmt.grp.agg, aes(x=lab.x, y=lab.y, label=paste0("mean=",round(Change.mean, 1),"\nSD=",round(Change.sd, 1))),vjust=0.5, hjust=0, size=3) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.spacing = unit(0, "lines"),
        # axis.text.x = element_text(angle=-30, hjust=0),
        # axis.title.x = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold"))
dev.off()

png(file.path(path.out, "figures", "Shrub_Invasives_Change_DensityPlot_MgmtUnit2.png"), height=6, width=8, units="in", res=180)
ggplot(data=shrub.agg2[,]) +
  facet_grid(Invasive~MgmtUnit2, scales="free") +
  # geom_histogram(aes(x=Change, fill=MgmtUnit2)) +
  geom_density(aes(x=Change, fill=MgmtUnit2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_text(data=mgmt.grp.agg, aes(x=lab.x, y=lab.y, label=paste0("mean=",round(Change.mean, 1),"\nSD=",round(Change.sd, 1))),vjust=0.5, hjust=0, size=3) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        panel.spacing = unit(0, "lines"),
        # axis.text.x = element_text(angle=-30, hjust=0),
        # axis.title.x = element_blank(),
        plot.title = element_text(hjust=0.5, face="bold"))
dev.off()

change.plot <- aggregate(shrub.agg2$Change, by=list(shrub.agg2[,c("Invasive2")]), FUN=mean)
names(change.plot)[which(names(change.plot)=="x")] <- "Dens.diff.mean"
change.plot$Dens.diff.sd <- aggregate(shrub.agg2$Change, by=list(shrub.agg2[,c("Invasive2")]), FUN=sd)[,"x"]
change.plot

shrub.agg2$Invasive <- factor(shrub.agg2$Invasive, levels=c("Lonicera", "Rhamnus", "Other Invasive", "non-invasive"))

png(file.path(path.out, "figures", "Shrub_Invasives_Density_Comparison_Plot_MgmtUnit.png"), height=6, width=8, units="in", res=180)
ggplot(data=shrub.agg2[shrub.agg2$Invasive2=="invasive", ]) +
  # facet_wrap(~Invasive2, scales="free") +
  geom_boxplot(aes(x=Invasive, y=Change, fill=MgmtUnit2)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name="Invasive Group") +
  scale_y_continuous(name="Change in Number Stems") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "Shrub_Invasives_Density_Comparison_Plot_MgmtUnit.png"), height=6, width=8, units="in", res=180)
ggplot(data=shrub.agg2[shrub.agg2$Invasive2=="non-invasive", ]) +
  # facet_wrap(~Invasive2, scales="free") +
  geom_boxplot(aes(x=Invasive, y=Change, fill=MgmtUnit2)) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_discrete(name="Invasive Group") +
  scale_y_continuous(name="Change in Number Stems") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

# -----------------------------------------------------------
