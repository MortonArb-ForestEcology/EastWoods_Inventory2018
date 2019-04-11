library(readxl)
path.ew <- "/Volumes/GoogleDrive/My Drive/East Woods/Inventory 2018/Final Data from AES/AES_Final3"

# ------------------------------------
# Tree Layer
# ------------------------------------
dat.tree <- read_excel(file.path(path.ew, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-12.xlsx"), sheet = "Tree Layer")
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

summary(dat.tree[dat.tree$Spp.Name=="Unidentified",])
summary(dat.tree[is.na(dat.tree$DBH),])
dat.tree[is.na(dat.tree$DBH) & dat.tree$Spp.Code!="No trees",]

# Red flags
dat.tree[substr(dat.tree$Spp.Name,1,8)=="Lonicera",] # Lonicera with DBH>10 cm???
dat.tree[dat.tree$Spp.Name=="tilia americana",] # Typo
dat.tree[dat.tree$Spp.Name=="Quercus alb",] # Typo

dat.tree[dat.tree$Spp.Name=="Cladastris kentukea",] # Oddball species; no note of tag
dat.tree[dat.tree$Spp.Name=="Liriodendron tulipifera",] # Haven't seen in the woods, but hard to mistake


hist(as.numeric(dat.tree$DBH))
hist(as.numeric(dat.tree$DBH))
# ------------------------------------


# ------------------------------------
# Shrub Layer
# ------------------------------------
dat.shrub <- read_excel(file.path(path.ew, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-12.xlsx"), sheet = "Shrub Layer")
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

summary(dat.shrub$Spp.Code)
summary(dat.shrub$Spp.Name)

length(unique(dat.shrub$Spp.Code))
length(unique(dat.shrub$Spp.Name))


dat.shrub[dat.shrub$Spp.Name=="Leitneria floridana",]
dat.shrub[dat.shrub$Spp.Name=="Ptelea trifoliata",]


summary(dat.shrub$Category.Sapling)
dat.shrub[!is.na(dat.shrub$Count) & dat.shrub$Count>50,]
summary(dat.shrub[dat.shrub$Count==0,])
summary(dat.shrub[is.na(dat.shrub$Count) ,])


hist(dat.shrub$Count)
unique(dat.shrub$PlotID)

summary(dat.shrub$Spp.Name)
data.frame(dat.shrub[dat.shrub$Spp.Name=="Leitneria floridana",])
# ------------------------------------

# ------------------------------------
# Downed Wood
# ------------------------------------
dat.cwd <- read_excel(file.path(path.ew, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-12.xlsx"), sheet = "DW Layer")
dat.cwd <- dat.cwd[!is.na(dat.cwd$Date),]
names(dat.cwd) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "DBH.1", "DBH.2", "Canopy", "Decay", "Length", "Notes")
dat.cwd$Sampler <- as.factor(dat.cwd$Sampler)
dat.cwd$PlotID <- as.factor(dat.cwd$PlotID)
dat.cwd$Spp.Code <- as.factor(dat.cwd$Spp.Code)
dat.cwd$Spp.Name <- as.factor(dat.cwd$Spp.Name)
dat.cwd$Canopy <- as.factor(dat.cwd$Canopy)
dat.cwd$Decay <- as.factor(dat.cwd$Decay)
# dat.cwd$Sample <- as.factor(dat.cwd$Sample)
# dat.cwd$DBH.1b <- as.numeric(dat.cwd$DBH.1)

summary(dat.cwd)

dat.cwd[is.na(dat.cwd$Length),]

# Messed up date
nrow(dat.cwd[dat.cwd$Date>as.POSIXct("2018-09-01"),])

length(unique(dat.cwd$Spp.Code))
length(unique(dat.cwd$Spp.Name))
unique(dat.cwd$Spp.Code)[order(unique(dat.cwd$Spp.Code))]
unique(dat.cwd$Spp.Name)[order(unique(dat.cwd$Spp.Name))]

dat.cwd[is.na(dat.cwd$DBH.1b),]
dat.cwd[is.na(dat.cwd$DBH.2),]

data.frame(dat.cwd[dat.cwd$Decay==0 & !is.na(dat.cwd$Decay),])
# ------------------------------------

# ------------------------------------
# Soil Cover
# ------------------------------------
dat.cover <- read_excel(file.path(path.ew, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-12.xlsx"), sheet = "Soil Cover")
dat.cover <- data.frame(dat.cover[,1:8])
names(dat.cover) <- c("Date", "Sampler", "PlotID", "Moss", "Logs", "Rocks", "Bare", "notes")
dat.cover$Sampler <- as.factor(dat.cover$Sampler)
dat.cover$PlotID <- as.factor(dat.cover$PlotID)
dat.cover$Moss <- as.ordered(dat.cover$Moss)
dat.cover$Logs <- as.ordered(dat.cover$Logs)
dat.cover$Rocks <- as.ordered(dat.cover$Rocks)
dat.cover$Bare <- as.ordered(dat.cover$Bare)
summary(dat.cover)

unique(dat.cover$Moss)
dat.cover[dat.cover$Moss=="8",]

unique(dat.cover$Logs)
dat.cover[dat.cover$Logs=="50",]
dat.cover[dat.cover$Logs=="10",]

unique(dat.cover$Rocks)

unique(dat.cover$Bare)

dat.cover[is.na(dat.cover$Moss),]
# ------------------------------------

# ------------------------------------
# Herb Layer -- Summer
# ------------------------------------
dat.herb <- read_excel(file.path(path.ew, "18-0073 Morton 2018 Spring Veg Data_AES-edits_Oct-12.xlsx"), sheet = "Herbaceous")
names(dat.herb) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Cover", "Cover.Class", "Count.Woody", "Notes")
# dat.herb <- data.frame(dat.herb)
dat.herb$Sampler <- as.factor(dat.herb$Sampler)
dat.herb$PlotID <- as.factor(dat.herb$PlotID)
dat.herb$Spp.Code <- as.factor(dat.herb$Spp.Code)
dat.herb$Spp.Name <- as.factor(dat.herb$Spp.Name)
dat.herb$Notes <- as.factor(dat.herb$Notes)
summary(dat.herb)


summary(dat.herb$PlotID)
# summary(dat.herb$Spp.Code)

length(unique(dat.herb$Spp.Code))
length(unique(dat.herb$Spp.Name))
levels(dat.herb$Spp.Name)
summary(dat.herb$Spp.Name)

spp.code2 <- unique(dat.herb$Spp.Code)
spp.code2 <- spp.code2[order(spp.code2)]

spp.name2 <- unique(dat.herb$Spp.Name)
spp.name2 <- spp.name2[order(spp.name2)]

nrow(dat.herb[dat.herb$Cover==0,])
dat.herb[dat.herb$Cover==0,]
dat.herb[is.na(dat.herb$Cover),]

dat.herb[is.na(dat.herb$Cover.Class),]
dat.herb[dat.herb$Cover==0,]

summary(dat.herb[!is.na(dat.herb$Count.Woody),])
data.frame(dat.herb[!is.na(dat.herb$Count.Woody) & dat.herb$Count.Woody>10,])

# ------------------------------------

# ------------------------------------
# Herb Layer -- Summer
# ------------------------------------
dat.herb2 <- read_excel(file.path(path.ew, "18-0073 Morton 2018 Summer Herb Data Sheet_AESedits_Oct-12.xlsx"), sheet = "Summer Herbaceous")
names(dat.herb2) <- c("Date", "Sampler", "PlotID", "Spp.Code", "Spp.Name", "Cover", "Cover.Class", "Count.Woody", "Notes")
# dat.herb2 <- data.frame(dat.herb2)
dat.herb2$Sampler <- as.factor(dat.herb2$Sampler)
dat.herb2$PlotID <- as.factor(dat.herb2$PlotID)
dat.herb2$Spp.Code <- as.factor(dat.herb2$Spp.Code)
dat.herb2$Spp.Name <- as.factor(dat.herb2$Spp.Name)
dat.herb2$Notes <- as.factor(dat.herb2$Notes)
summary(dat.herb2)


dat.herb2[dat.herb2$Cover==0,]
dat.herb2[is.na(dat.herb2$Cover),]

summary(dat.herb2$PlotID)
# summary(dat.herb2$Spp.Code)

length(unique(dat.herb2$Spp.Code))
length(unique(dat.herb2$Spp.Name))
levels(dat.herb2$Spp.Name)
summary(dat.herb2$Spp.Name)

spp.code2 <- unique(dat.herb2$Spp.Code)
spp.code2 <- spp.code2[order(spp.code2)]

spp.name2 <- unique(dat.herb2$Spp.Name)
spp.name2 <- spp.name2[order(spp.name2)]

nrow(dat.herb2[dat.herb2$Cover==0,])
dat.herb2[dat.herb2$Cover==0,]


dat.herb2[is.na(dat.herb2$Cover.Class),]
dat.herb2[dat.herb2$Cover==0,]

summary(dat.herb2[!is.na(dat.herb2$Count.Woody),])
data.frame(dat.herb2[!is.na(dat.herb2$Count.Woody) & dat.herb2$Count.Woody>10,])

# ------------------------------------


