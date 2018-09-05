library(readxl)
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/"

# ------------------------------------
# Tree Layer
# ------------------------------------
dat.tree <- read_excel(file.path(path.ew, "East Woods Spring vegetation data.final.xlsx"), sheet = "Tree Layer")
names(dat.tree) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "DBH", "Canopy", "Decay", "Vigor", "Notes")
dat.tree$Sampler <- as.factor(dat.tree$Sampler)
dat.tree$PlotID <- as.factor(dat.tree$PlotID)
dat.tree$Spp.Code <- as.factor(dat.tree$Spp.Code)
dat.tree$Spp.Name <- as.factor(dat.tree$Spp.Name)
dat.tree$DBH <- as.numeric(dat.tree$DBH)
dat.tree$Canopy <- as.factor(dat.tree$Canopy)
dat.tree$Decay <- as.factor(dat.tree$Decay)
dat.tree$Vigor <- as.factor(dat.tree$Vigor)
dat.tree <- dat.tree[!is.na(dat.tree$Date),]
# dat.tree <- data.frame(dat.tree[!is.na(dat.tree$Date),])
summary(dat.tree)
dim(dat.tree)

summary(dat.tree$Spp.Code)
summary(dat.tree$Spp.Name)
summary(dat.tree$DBH)
summary(dat.tree$Vigor)
summary(dat.tree$Decay)

dat.tree[dat.tree$Spp.Name=="Cladastris kentukea",]

hist(as.numeric(dat.tree$DBH))
hist(as.numeric(dat.tree$DBH))
# ------------------------------------


# ------------------------------------
# Herb Layer
# ------------------------------------
dat.herb <- read_excel(file.path(path.ew, "East Woods Spring vegetation data.final.xlsx"), sheet = "Herbaceous")
names(dat.herb) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Cover", "Cover.Class", "Count.Woody", "Notes")
# dat.herb <- data.frame(dat.herb)
dat.herb$Sampler <- as.factor(dat.herb$Sampler)
dat.herb$PlotID <- as.factor(dat.herb$PlotID)
dat.herb$Spp.Code <- as.factor(dat.herb$Spp.Code)
dat.herb$Spp.Name <- as.factor(dat.herb$Spp.Name)
dat.herb$Notes <- as.factor(dat.herb$Notes)
summary(dat.herb)

unique(dat.herb$PlotID)

summary(dat.herb$PlotID)
summary(dat.herb$Spp.Code)
length(unique(dat.herb$Spp.Code))
length(unique(dat.herb$Spp.Name))
levels(dat.herb$Spp.Name)
summary(dat.herb$Spp.Name)

summary(dat.herb[!is.na(dat.herb$Count.Woody),])
data.frame(dat.herb[!is.na(dat.herb$Count.Woody) & dat.herb$Count.Woody>10,])

# ------------------------------------


# ------------------------------------
# Shrub Layer
# ------------------------------------
dat.shrub <- read_excel(file.path(path.ew, "East Woods Spring vegetation data.final.xlsx"), sheet = "Shrub Layer")
names(dat.shrub) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Category", "Category.Sapling", "Count", "Notes")
# dat.shrub <- data.frame(dat.shrub)
dat.shrub$Sampler <- as.factor(dat.shrub$Sampler)
dat.shrub$PlotID <- as.factor(dat.shrub$PlotID)
dat.shrub$Spp.Code <- as.factor(dat.shrub$Spp.Code)
dat.shrub$Spp.Name <- as.factor(dat.shrub$Spp.Name)
dat.shrub$Category <- as.factor(dat.shrub$Category)
dat.shrub$Category.Sapling <- as.factor(dat.shrub$Category.Sapling)
dat.shrub$Notes <- as.factor(dat.shrub$Notes)
summary(dat.shrub)


summary(dat.shrub$Category.Sapling)
dat.shrub[!is.na(dat.shrub$Count) & dat.shrub$Count>50,]
summary(dat.shrub[dat.shrub$Count==0,])

hist(dat.shrub$Count)
unique(dat.shrub$PlotID)

summary(dat.shrub$Spp.Name)
data.frame(dat.shrub[dat.shrub$Spp.Name=="Leitneria floridana",])
# ------------------------------------

# ------------------------------------
# Downed Wood
# ------------------------------------
dat.cwd <- read_excel(file.path(path.ew, "East Woods Spring vegetation data.final.xlsx"), sheet = "DW Layer")
dat.cwd <- dat.cwd[!is.na(dat.cwd$Date),]
names(dat.cwd) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "DBH.1", "DBH.2", "Canopy", "Decay", "Length", "Notes")
dat.cwd$Sampler <- as.factor(dat.cwd$Sampler)
dat.cwd$PlotID <- as.factor(dat.cwd$PlotID)
dat.cwd$Spp.Code <- as.factor(dat.cwd$Spp.Code)
dat.cwd$Spp.Name <- as.factor(dat.cwd$Spp.Name)
dat.cwd$Canopy <- as.factor(dat.cwd$Canopy)
dat.cwd$Decay <- as.factor(dat.cwd$Decay)
# dat.cwd$Sample <- as.factor(dat.cwd$Sample)
dat.cwd$DBH.1b <- as.numeric(dat.cwd$DBH.1)

summary(dat.cwd)

dat.cwd[is.na(dat.cwd$DBH.1b),]
dat.cwd[is.na(dat.cwd$DBH.2),]

data.frame(dat.cwd[dat.cwd$Decay==0 & !is.na(dat.cwd$Decay),])
# ------------------------------------
