library(rgeos); library(rgdal)
library(raster)
library(ggplot2)

# Checking the Natural Resources Management geodatabases
path.home <- getwd() # Storing our base working directory

# Set the paths & names of the geodatabases we want
path.burn <- "/Volumes/GIS/Collections/Natural Resources Management/Burn"
path.thin <- "/Volumes/GIS/Collections/Natural Resources Management/Winter Clearing"
gdb.burn <- "Controlled Burn Areas Draft.gdb"
gdb.thin <- "Winter Clearing.gdb"

# Working with the burn data
burn.layers <- ogrListLayers(file.path(path.burn, gdb.burn))

burn <- readOGR(file.path(path.burn, gdb.burn), "Burned_Area_Master")
burn$Burn_Date2 <- as.Date(burn$Burn_Date)
burn$Year <- lubridate::year(burn$Burn_Date2)
burn[grep("2013", burn$NOTES),"Year"] <- 2013
burn[grep("Pizzo", burn$NOTES),"Year"] <- 2008
plot(burn)
summary(burn)
dim(burn)

summary(burn[is.na(burn$Id),])
summary(burn[burn$Location==" ",])

burn.df <- data.frame(burn)

summary(burn.df[is.na(burn.df$Burn_Date2),])

# Tracking down things that have no burn date, but do have notes
summary(burn.df[is.na(burn.df$Burn_Date2) & burn.df$NOTES!=" ",])
test <- burn.df[is.na(burn.df$Burn_Date2) & burn.df$NOTES!=" ",c("NOTES")]

trow <- 1:length(test)
test[!1:length(test) %in% grep("2013", test) & !1:length(test) %in% grep("Pizzo", test)]
summary(burn.df[is.na(burn.df$Burn_Date2) & burn.df$NOTES==" " & !grep,])

plot(burn[is.na(burn.df$Burn_Date) & burn$NOTES!=" " & !1:nrow(burn) %in% grep("2013", burn$NOTES) & !1:nrow(burn) %in% grep("Pizzo", burn$NOTES),])
summary(burn[is.na(burn.df$Burn_Date) & burn$NOTES!=" " & !1:nrow(burn) %in% grep("2013", burn$NOTES) & !1:nrow(burn) %in% grep("Pizzo", burn$NOTES),])
dim(burn[is.na(burn.df$Burn_Date) & burn$NOTES!=" " & !1:nrow(burn) %in% grep("2013", burn$NOTES) & !1:nrow(burn) %in% grep("Pizzo", burn$NOTES),])

# Looking at things with no dates and no notes
summary(burn.df[is.na(burn.df$Burn_Date2) & burn.df$NOTES==" ",])
plot(burn[is.na(burn$Burn_Date2) & burn$NOTES==" ",])

# ------------------------------
# Trying a different layer to see what it looks like
ew1 <- readOGR(file.path(path.burn, gdb.burn), "East_Woods_1")
ew3 <- readOGR(file.path(path.burn, gdb.burn), "East_Woods_3")
bigrock <- readOGR(file.path(path.burn, gdb.burn), "Burn_Units_Big_Rock_Woods_East")
# ------------------------------
