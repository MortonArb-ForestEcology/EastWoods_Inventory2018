# Exploratory Analyses
# Christy Rollinson's Initial Thoughts:
# Dominant tree/forest type; interpolated maps of Oak relative oak importance value 
# Current and change in oak regeneration (seedling density)
# Current and change in woody invasives
# Current total basal area & change in basal area

# Reply from Kurt Dreisilker (email 31 Aug, 2018)
# I would like to use this data to develop a trajectory for the East Woods.  
# For example, based on these two sets of data (2007 & 2018) we have seen something decline (e.g., oak basal area) and another thing (e.g., oak recruitment) remain unchanged.  Based on this information, we propose the following management action items to commence in the near future.  We may need to look at management units to help break down the data. We will also need to look at the impact of volunteers on the East Woods, specifically since the funding was provided to enhance our volunteer stewardship effort. 


library(readxl)

# -----------------------------------------------------------
# Looking at Tree Composition and Density: 2018 vs 2007
# -----------------------------------------------------------
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods"
path.2018 <- file.path(path.ew, "Inventory 2018/Final Data from AES/")
path.2007 <- file.path(path.ew, "Inventory 2007")
# ----------------------------
# Read in and format data
# ----------------------------
# 2018 Trees
tree.2018 <- read_excel(file.path(path.2018, "18-0073 Morton 2018 Spring Veg Data_WO-edits_Aug30.xlsx"), sheet = "Tree Layer")
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
tree.2018$Status <- ifelse(tree.2018$Canopy=="S", "dead", "live")
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
tree.2007 <- tree.2007[!(is.na(tree.2007$DBH) & !tree.2007$Spp.Code %in% c("??", "NONE")),]
tree.2007 <- droplevels(tree.2007)
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
tree.2018$Spp.Code <- ifelse(substr(tree.2018$Spp.Code, nchar(tree.2018$Spp.Code), nchar(tree.2018$Spp.Code))==".", substr(tree.2018$Spp.Code, nchar(tree.2018$Spp.Code), nchar(tree.2018$Spp.Code)-1), tree.2018$Spp.Code)

tree.2007[tree.2007$Spp.Code=="PSEMEN",]

unique(tree.2007$Spp.Name)[order(unique(tree.2007$Spp.Name))]
tree.2007[tree.2007$Spp.Name=="rttf",]
tree.2007[tree.2007$Spp.Name=="??",]
tree.2007$Spp.Name <- car::recode(tree.2007$Spp.Name, "'rttf'='Acer species'; '??'='ERROR'")

# Dropping unnecessary row
tree.2007 <- tree.2007[tree.2007$Spp.Code==""]
summary(tree.2007)
# ----------------------------

# ----------------------------
# ----------------------------
# ----------------------------

# -----------------------------------------------------------
