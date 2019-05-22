# Performing initial analyses on impacts of management on tree basal area
# Does management have an impact on basal area of Quercus, Acer, and Fraxinus? 
#  -- What are the changes in basal area for no thinning & burning (Hidden Lake AND the no burn zone), only high frequency burning (annual burn zone), and intermediate level of thinning and burning?

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

gen.interest <- c("Quercus", "Acer", "Fraxinus")  
spp.quercus <- c("alba", "rubra", "macrocarpa")
# -----------------------------------------------------------

# -----------------------------------------------------------
# Read in relevant raw data sets
# -----------------------------------------------------------
# Raw tree data
dat.tree.all <- read.csv(file.path(path.out, "data_processed", "InventoryData_Trees_Merged_2007_2017.csv"))
summary(dat.tree.all); dim(dat.tree.all)

# Easy spatial data
dat.gis <- read.csv(file.path(path.out, "data_processed", "point_info_GIS.csv"))
dat.gis$PlotID2 <- dat.gis$PlotID
dat.gis$PlotID <- as.factor(gsub("-", "", dat.gis$PlotID))
dat.gis$MgmtUnit <- ifelse(is.na(dat.gis$wooded), "Non-Wooded", 
                           ifelse(dat.gis$wooded=="Hidden Lake", "Hidden Lake", 
                                  ifelse(dat.gis$unit=="South 40 South", "Annual Burn", 
                                         ifelse(!is.na(dat.gis$unit), "Mixed Management", "No Management"))))
dat.gis$MgmtUnit[is.na(dat.gis$MgmtUnit)] <- "No Management"
dat.gis$MgmtUnit <- as.factor(dat.gis$MgmtUnit)
summary(dat.gis)

ggplot(data=dat.gis) +
  coord_equal() +
  geom_point(aes(x=lon, y=lat, color=MgmtUnit))

# Add plot info to raw tree data
dat.tree.all <- merge(dat.tree.all, dat.gis[,c("PlotID", "wooded", "unit", "MgmtUnit")], all.x=T)
summary(dat.tree.all); dim(dat.tree.all)
# -----------------------------------------------------------


# -----------------------------------------------------------
# Aggregating to plot-level for both species and genera
# -----------------------------------------------------------
# -----------------------------
# Plot-level aggregation
# -----------------------------
dat.plot <- aggregate(dat.tree.all[,c("BA")], 
                      by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Status")], 
                      FUN=sum, na.rm=T)
names(dat.plot)[which(names(dat.plot)=="x")] <- "BA.tot"
dat.plot$n <- aggregate(dat.tree.all[,c("BA")], 
                        by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Status")],
                        FUN=length)[,"x"]
dat.plot$DBH.mean <- aggregate(dat.tree.all[,c("DBH")], 
                               by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Status")], 
                               FUN=mean, na.rm=T)[,"x"]
dat.plot$DBH.sd <- aggregate(dat.tree.all[,c("DBH")], 
                             by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Status")], 
                             FUN=sd, na.rm=T)[,"x"]
summary(dat.plot)

# Reshaping to directly compare 2007 & 2008
dat.plot$Year <- as.factor(dat.plot$Year)
plot.ba <- tidyr::spread(dat.plot[dat.plot$Status=="live",c("PlotID", "MgmtUnit", "Year", "BA.tot")], Year, BA.tot)
plot.ba[is.na(plot.ba)] <- 0
plot.ba$Change <- plot.ba$`2018` - plot.ba$`2007`
summary(plot.ba)
plot.n <- tidyr::spread(dat.plot[dat.plot$Status=="live",c("PlotID", "MgmtUnit", "Year", "n")], Year, n)
plot.n[is.na(plot.n)] <- 0
plot.n$Change <- plot.n$`2018` - plot.n$`2007`
summary(plot.n)

# Summarizing Change
change.plot <- aggregate(plot.ba$Change, by=list(plot.ba[,c("MgmtUnit")]), FUN=mean)
names(change.plot)[which(names(change.plot)=="x")] <- "BA.diff.mean"
change.plot$BA.diff.sd <- aggregate(plot.ba$Change, by=list(plot.ba[,c("MgmtUnit")]), FUN=sd)[,"x"]
change.plot$n.diff.mean <- aggregate(plot.n$Change, by=list(plot.n[,c("MgmtUnit")]), FUN=mean)[,"x"]
change.plot$n.diff.sd <- aggregate(plot.n$Change, by=list(plot.n[,c("MgmtUnit")]), FUN=mean)[,"x"]
change.plot
# -----------------------------

# -----------------------------
# Genus-level aggregation
# -----------------------------
dat.plot.gen <- aggregate(dat.tree.all[,c("BA")], 
                          by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Status")], 
                          FUN=sum, na.rm=T)
names(dat.plot.gen)[which(names(dat.plot.gen)=="x")] <- "BA.tot"
dat.plot.gen$n <- aggregate(dat.tree.all[,c("BA")], 
                            by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Status")], 
                            FUN=length)[,"x"]
dat.plot.gen$DBH.mean <- aggregate(dat.tree.all[,c("DBH")], 
                                   by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Status")], 
                                   FUN=mean, na.rm=T)[,"x"]
dat.plot.gen$DBH.sd <- aggregate(dat.tree.all[,c("DBH")], 
                                 by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Status")], 
                                 FUN=sd, na.rm=T)[,"x"]
summary(dat.plot.gen)

# Reshaping to directly compare 2007 & 2008
dat.plot.gen$Year <- as.factor(dat.plot.gen$Year)
plot.gen.ba <- tidyr::spread(dat.plot.gen[dat.plot.gen$Status=="live",c("PlotID", "MgmtUnit", "Genus", "Year", "BA.tot")], Year, BA.tot)
plot.gen.ba[is.na(plot.gen.ba)] <- 0
plot.gen.ba$Change <- plot.gen.ba$`2018` - plot.gen.ba$`2007`
summary(plot.gen.ba)

plot.gen.n <- tidyr::spread(dat.plot.gen[dat.plot.gen$Status=="live",c("PlotID", "MgmtUnit", "Genus", "Year", "n")], Year, n)
plot.gen.n[is.na(plot.gen.n)] <- 0
plot.gen.n$Change <- plot.gen.n$`2018` - plot.gen.n$`2007`
summary(plot.gen.n)

# Summarizing Change
change.gen <- aggregate(plot.gen.ba$Change, by=plot.gen.ba[,c("Genus", "MgmtUnit")], FUN=mean)
names(change.gen)[which(names(change.gen)=="x")] <- "BA.diff.mean"
change.gen$BA.diff.sd <- aggregate(plot.gen.ba$Change, by=plot.gen.ba[,c("Genus", "MgmtUnit")], FUN=sd)[,"x"]
change.gen$n <- aggregate(plot.gen.n$Change, by=plot.gen.n[,c("Genus", "MgmtUnit")], FUN=mean)[,"x"]
change.gen$diff.sd <- aggregate(plot.gen.n$Change, by=plot.gen.n[,c("Genus", "MgmtUnit")], FUN=sd)[,"x"]
change.gen <- change.gen[order(change.gen$Genus),]
head(change.gen)
summary(change.gen)
# -----------------------------

# -----------------------------
# Species-level aggregation
# -----------------------------
dat.plot.spp <- aggregate(dat.tree.all[,c("BA")], 
                          by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Spp.Name", "Status")], 
                          FUN=sum, na.rm=T)
names(dat.plot.spp)[which(names(dat.plot.spp)=="x")] <- "BA.tot"
dat.plot.spp$n <- aggregate(dat.tree.all[,c("BA")], 
                            by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Spp.Name", "Status")], 
                            FUN=length)[,"x"]
dat.plot.spp$DBH.mean <- aggregate(dat.tree.all[,c("DBH")], 
                                   by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Spp.Name", "Status")], 
                                   FUN=mean, na.rm=T)[,"x"]
dat.plot.spp$DBH.sd <- aggregate(dat.tree.all[,c("DBH")], 
                                   by=dat.tree.all[,c("Year", "PlotID", "wooded", "MgmtUnit", "Genus", "Spp.Name", "Status")], 
                                   FUN=sd, na.rm=T)[,"x"]
summary(dat.plot.spp)

# Reshaping to directly compare 2007 & 2008
dat.plot.spp$Year <- as.factor(dat.plot.spp$Year)
plot.spp.ba <- tidyr::spread(dat.plot.spp[dat.plot.spp$Status=="live",c("PlotID", "MgmtUnit", "Genus", "Spp.Name", "Year", "BA.tot")], Year, BA.tot)
plot.spp.ba[is.na(plot.spp.ba)] <- 0
plot.spp.ba$Change <- plot.spp.ba$`2018` - plot.spp.ba$`2007`
summary(plot.spp.ba)

plot.spp.n <- tidyr::spread(dat.plot.spp[dat.plot.spp$Status=="live",c("PlotID", "MgmtUnit", "Genus", "Spp.Name", "Year", "n")], Year, n)
plot.spp.n[is.na(plot.spp.n)] <- 0
plot.spp.n$Change <- plot.spp.n$`2018` - plot.spp.n$`2007`
summary(plot.spp.n)

# Summarizing Change
change.spp <- aggregate(plot.spp.ba$Change, by=plot.spp.ba[,c("MgmtUnit", "Genus", "Spp.Name")], FUN=mean)
names(change.spp)[which(names(change.spp)=="x")] <- "BA.diff.mean"
change.spp$BA.diff.sd <- aggregate(plot.spp.ba$Change, by=plot.spp.ba[,c("MgmtUnit", "Genus", "Spp.Name")], FUN=sd)[,"x"]
change.spp$n.diff.mean <- aggregate(plot.spp.n$Change, by=plot.spp.n[,c("MgmtUnit", "Genus", "Spp.Name")], FUN=mean)[,"x"]
change.spp$n.diff.sd <- aggregate(plot.spp.n$Change, by=plot.spp.n[,c("MgmtUnit", "Genus", "Spp.Name")], FUN=sd)[,"x"]
summary(change.spp)
# -----------------------------

# -----------------------------------------------------------

# -----------------------------------------------------------
# Figures and analyses making comparisons
# -----------------------------------------------------------
# -----------------------------
# Management Comparisons (total BA)
# -----------------------------
write.csv(change.plot, file.path(path.out, "data_summary", "BasalArea_Change_Plot_Management.csv"), row.names=F)

plot.ba$MgmtUnit2 <- as.factor(ifelse(plot.ba$MgmtUnit=="Hidden Lake", "No Management", paste(plot.ba$MgmtUnit)))
plot.n$MgmtUnit2 <- as.factor(ifelse(plot.n$MgmtUnit=="Hidden Lake", "No Management", paste(plot.n$MgmtUnit)))
summary(plot.ba)


# Quercus Analysis
anova.ba.mgmt <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.ba)
summary(anova.ba.mgmt)
anova(anova.ba.mgmt)

anova.n.mgmt <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.n)
summary(anova.n.mgmt)
anova(anova.n.mgmt)


png(file.path(path.out, "figures", "BasalArea_Comparison_Plot_MgmtUnit.png"), height=6, width=8, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Status=="live", ]) +
  facet_grid(.~Year, scales="free_y") +
  geom_boxplot(aes(x=MgmtUnit, y=BA.tot, fill=MgmtUnit)) +
  scale_x_discrete(name="Management Unit") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "NumStems_Comparison_Plot_MgmtUnit.png"), height=6, width=8, units="in", res=180)
ggplot(data=dat.plot[dat.plot$Status=="live", ]) +
  facet_grid(.~Year, scales="free_y") +
  geom_boxplot(aes(x=MgmtUnit, y=n, fill=MgmtUnit)) +
  scale_x_discrete(name="Management Unit") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "BasalArea_Change_Plot_MgmtUnit.png"), height=6, width=6, units="in", res=180)
ggplot(data=plot.ba) +
  # facet_grid(Genus~.) +
  geom_histogram(aes(x=Change, fill=MgmtUnit)) +
  # geom_text(data=change.gen[change.gen$Genus %in% gen.interest, ], x=-12500, y=125, aes(label=paste0("mean = ", round(diff.mean, 0), "; sd = ", round(diff.sd, 0)))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(name="Change in Basal Area (cm2)") +
  scale_y_continuous(name="Number Plots Showing Change") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank())
dev.off()

png(file.path(path.out, "figures", "NumStems_Change_Plot_MgmtUnit.png"), height=6, width=6, units="in", res=180)
ggplot(data=plot.n) +
  # facet_grid(Genus~.) +
  geom_histogram(aes(x=Change, fill=MgmtUnit)) +
  # geom_text(data=change.gen[change.gen$Genus %in% gen.interest, ], x=-12500, y=125, aes(label=paste0("mean = ", round(diff.mean, 0), "; sd = ", round(diff.sd, 0)))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(name="Change in Basal Area (cm2)") +
  scale_y_continuous(name="Number Plots Showing Change") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank())
dev.off()

# -----------------------------

# -----------------------------
# Genus-level comparisons
# -----------------------------
write.csv(change.gen[change.gen$Genus %in% gen.interest,], file.path(path.out, "data_summary", "BasalArea_Change_Genera_Management.csv"), row.names=F)

plot.gen.ba$MgmtUnit2 <- as.factor(ifelse(plot.gen.ba$MgmtUnit=="Hidden Lake", "No Management", paste(plot.gen.ba$MgmtUnit)))
plot.gen.n$MgmtUnit2 <- as.factor(ifelse(plot.gen.n$MgmtUnit=="Hidden Lake", "No Management", paste(plot.gen.n$MgmtUnit)))
summary(plot.gen.ba)

# Quercus Analysis
anova.ba.mgmt.quer <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.gen.ba[plot.gen.ba$Genus=="Quercus", ])
summary(anova.ba.mgmt.quer)
anova(anova.ba.mgmt.quer)

anova.n.mgmt.quer <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.gen.n[plot.gen.n$Genus=="Quercus", ])
summary(anova.n.mgmt.quer)
anova(anova.n.mgmt.quer)

# Acer Analysis
anova.ba.mgmt.acer <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.gen.ba[plot.gen.ba$Genus=="Acer", ])
summary(anova.ba.mgmt.acer)
anova(anova.ba.mgmt.acer)

anova.n.mgmt.acer <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.gen.n[plot.gen.n$Genus=="Acer", ])
summary(anova.n.mgmt.acer)
anova(anova.n.mgmt.acer)

# Fraxinus Analysis
anova.ba.mgmt.frax <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.gen.ba[plot.gen.ba$Genus=="Fraxinus", ])
summary(anova.ba.mgmt.frax)
anova(anova.ba.mgmt.frax)

anova.n.mgmt.frax <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.gen.n[plot.gen.n$Genus=="Fraxinus", ])
summary(anova.n.mgmt.frax)
anova(anova.n.mgmt.frax)

png(file.path(path.out, "figures", "BasalArea_Comparison_Genera_Focal_MgmtUnit.png"), height=6, width=8, units="in", res=180)
ggplot(data=dat.plot.gen[dat.plot.gen$Status=="live" & dat.plot.gen$Genus %in% gen.interest, ]) +
  facet_grid(Genus~Year, scales="free_y") +
  geom_boxplot(aes(x=MgmtUnit, y=BA.tot, fill=MgmtUnit)) +
  scale_x_discrete(name="Management Unit") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "NumStems_Comparison_Genera_Focal_MgmtUnit.png"), height=6, width=8, units="in", res=180)
ggplot(data=dat.plot.gen[dat.plot.gen$Status=="live" & dat.plot.gen$Genus %in% gen.interest, ]) +
  facet_grid(Genus~Year, scales="free_y") +
  geom_boxplot(aes(x=MgmtUnit, y=n, fill=MgmtUnit)) +
  scale_x_discrete(name="Management Unit") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "BasalArea_Comparison_Genera_Focal_MgmtUnit_2018.png"), height=6, width=6, units="in", res=180)
ggplot(data=dat.plot.gen[dat.plot.gen$Status=="live" & dat.plot.gen$Genus %in% gen.interest & dat.plot.gen$Year==2018, ]) +
  facet_grid(Genus~Year, scales="free_y") +
  geom_boxplot(aes(x=MgmtUnit, y=BA.tot, fill=MgmtUnit)) +
  scale_x_discrete(name="Year") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "BasalArea_Change_Genera_Focal_MgmtUnit.png"), height=6, width=6, units="in", res=180)
ggplot(data=plot.gen.ba[plot.gen.ba$Genus %in% gen.interest, ]) +
  facet_grid(Genus~.) +
  geom_histogram(aes(x=Change, fill=MgmtUnit)) +
  # geom_text(data=change.gen[change.gen$Genus %in% gen.interest, ], x=-12500, y=125, aes(label=paste0("mean = ", round(diff.mean, 0), "; sd = ", round(diff.sd, 0)))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(name="Change in Basal Area (cm2)") +
  scale_y_continuous(name="Number Plots Showing Change") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank())
dev.off()

png(file.path(path.out, "figures", "NumStems_Change_Genera_Focal_MgmtUnit.png"), height=6, width=6, units="in", res=180)
ggplot(data=plot.gen.n[plot.gen.n$Genus %in% gen.interest, ]) +
  facet_grid(Genus~.) +
  geom_histogram(aes(x=Change, fill=MgmtUnit)) +
  # geom_text(data=change.gen[change.gen$Genus %in% gen.interest, ], x=-12500, y=125, aes(label=paste0("mean = ", round(diff.mean, 0), "; sd = ", round(diff.sd, 0)))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(name="Change in Basal Area (cm2)") +
  scale_y_continuous(name="Number Plots Showing Change") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank())
dev.off()
# -----------------------------

# -----------------------------
# Species-level comparisons for Oak only
# -----------------------------
write.csv(change.spp[change.spp$Spp.Name %in% paste("Quercus", spp.quercus),], file.path(path.out, "data_summary", "BasalArea_Change_Species_Management.csv"), row.names=F)

plot.spp.ba$MgmtUnit2 <- as.factor(ifelse(plot.spp.ba$MgmtUnit=="Hidden Lake", "No Management", paste(plot.spp.ba$MgmtUnit)))
plot.spp.n$MgmtUnit2 <- as.factor(ifelse(plot.spp.n$MgmtUnit=="Hidden Lake", "No Management", paste(plot.spp.n$MgmtUnit)))
summary(plot.spp.ba)

# Quercus alba Analysis
anova.ba.mgmt.qual <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.spp.ba[plot.spp.ba$Spp.Name=="Quercus alba", ])
summary(anova.ba.mgmt.qual)
anova(anova.ba.mgmt.qual)

anova.n.mgmt.qual <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.spp.n[plot.spp.n$Spp.Name=="Quercus alba", ])
summary(anova.n.mgmt.qual)
anova(anova.n.mgmt.qual)

# Quercus rubra Analysis
anova.ba.mgmt.quru <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.spp.ba[plot.spp.ba$Spp.Name=="Quercus rubra", ])
summary(anova.ba.mgmt.quru)
anova(anova.ba.mgmt.quru)

anova.n.mgmt.quru <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.spp.n[plot.spp.n$Spp.Name=="Quercus rubra", ])
summary(anova.n.mgmt.quru)
anova(anova.n.mgmt.quru)

# Quercus macrocarpa Analysis
anova.ba.mgmt.quma <- lm(Change ~ relevel(MgmtUnit, ref="No Management"), data=plot.spp.ba[plot.spp.ba$Spp.Name=="Quercus macrocarpa", ])
summary(anova.ba.mgmt.quma)
anova(anova.ba.mgmt.quma)

anova.ba.mgmt.quma2 <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.spp.ba[plot.spp.ba$Spp.Name=="Quercus macrocarpa", ])
summary(anova.ba.mgmt.quma2)
anova(anova.ba.mgmt.quma2)

anova.n.mgmt.quma <- lm(Change ~ relevel(MgmtUnit, ref="No Management"), data=plot.spp.n[plot.spp.n$Spp.Name=="Quercus macrocarpa", ])
summary(anova.n.mgmt.quma)
anova(anova.n.mgmt.quma)

anova.n.mgmt.qumaB <- lm(Change ~ relevel(MgmtUnit, ref="No Management")-1, data=plot.spp.n[plot.spp.n$Spp.Name=="Quercus macrocarpa", ])
summary(anova.n.mgmt.qumaB)
anova(anova.n.mgmt.qumaB)

anova.n.mgmt.quma2 <- lm(Change ~ relevel(MgmtUnit2, ref="No Management"), data=plot.spp.n[plot.spp.n$Spp.Name=="Quercus macrocarpa", ])
summary(anova.n.mgmt.quma2)
anova(anova.n.mgmt.quma2)


png(file.path(path.out, "figures", "BasalArea_Comparison_Species_Quercus_Focal_MgmtUnit.png"), height=6, width=8, units="in", res=180)
ggplot(data=dat.plot.spp[dat.plot.spp$Status=="live" & dat.plot.spp$Genus %in% gen.interest, ]) +
  facet_grid(Genus~Year, scales="free_y") +
  geom_boxplot(aes(x=MgmtUnit, y=BA.tot, fill=MgmtUnit)) +
  scale_x_discrete(name="Management Unit") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "BasalArea_Comparison_Species_Quercus_Focal_MgmtUnit_2018.png"), height=6, width=6, units="in", res=180)
ggplot(data=dat.plot.spp[dat.plot.spp$Status=="live" & dat.plot.spp$Genus %in% gen.interest & dat.plot.spp$Year==2018, ]) +
  facet_grid(Genus~Year, scales="free_y") +
  geom_boxplot(aes(x=MgmtUnit, y=BA.tot, fill=MgmtUnit)) +
  scale_x_discrete(name="Year") +
  scale_y_continuous(name="Total Basal Area (cm2)") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0))
dev.off()

png(file.path(path.out, "figures", "BasalArea_Change_Species_Quercus_Focal_MgmtUnit.png"), height=6, width=6, units="in", res=180)
ggplot(data=plot.spp.ba[plot.spp.ba$Genus %in% gen.interest, ]) +
  facet_grid(Genus~.) +
  geom_histogram(aes(x=Change, fill=MgmtUnit)) +
  # geom_text(data=change.spp[change.spp$Genus %in% gen.interest, ], x=-12500, y=125, aes(label=paste0("mean = ", round(diff.mean, 0), "; sd = ", round(diff.sd, 0)))) +
  geom_vline(xintercept = 0, linetype="dashed") +
  scale_x_continuous(name="Change in Basal Area (cm2)") +
  scale_y_continuous(name="Number Plots Showing Change") +
  # guides(fill=F) +
  theme_bw() +
  theme(legend.position="top",
        legend.title=element_blank())
dev.off()
# -----------------------------
# -----------------------------

# -----------------------------------------------------------
