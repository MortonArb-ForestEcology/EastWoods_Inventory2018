library(rgeos); library(rgdal)
library(raster)
library(ggplot2)

# Checking the Natural Resources Management geodatabases
path.home <- getwd() # Storing our base working directory

# Set the paths & names of the geodatabases we want
path.burn <- "/Volumes/GIS/Collections/MarKGIS/Management Unit Plans/"
path.thin <- "/Volumes/GIS/Collections/Natural Resources Management/Winter Clearing"
gdb.burn <- "Controlled Burn Areas.gdb/"
gdb.thin <- "Winter Clearing.gdb"

# ------------------------------------------------------------------------------
# Working with the burn data
# ------------------------------------------------------------------------------
burn.layers <- ogrListLayers(file.path(path.burn, gdb.burn))

burn <- readOGR(file.path(path.burn, gdb.burn), "Completed_Burn_Areas")
# burn$Burn_Date2 <- as.Date(burn$Burn_Date)
# burn$Year <- lubridate::year(burn$Burn_Date2)
# burn[grep("2013", burn$NOTES),"Year"] <- 2013
# burn[grep("Pizzo", burn$NOTES),"Year"] <- 2008
plot(burn)
summary(burn)
dim(burn)

summary(burn[is.na(burn$Id),])
summary(burn[burn$Location==" ",])

burn.df <- data.frame(burn)

summary(burn.df[is.na(burn.df$Burn_Date2),])

# Tracking down things that have no burn date, but do have notes
summary(burn.df[is.na(burn.df$Burn_Date) & burn.df$NOTES!=" ",])
test <- burn.df[is.na(burn.df$Burn_Date) & burn.df$NOTES!=" ",c("NOTES")]

trow <- 1:length(test)
test[!1:length(test) %in% grep("2013", test) & !1:length(test) %in% grep("Pizzo", test)]
summary(burn.df[is.na(burn.df$Burn_Date2) & burn.df$NOTES==" " & !grep,])

plot(burn[is.na(burn.df$Burn_Date) & burn$NOTES!=" " & !1:nrow(burn) %in% grep("2013", burn$NOTES) & !1:nrow(burn) %in% grep("Pizzo", burn$NOTES),])
summary(burn[is.na(burn.df$Burn_Date) & burn$NOTES!=" " & !1:nrow(burn) %in% grep("2013", burn$NOTES) & !1:nrow(burn) %in% grep("Pizzo", burn$NOTES),])
dim(burn[is.na(burn.df$Burn_Date) & burn$NOTES!=" " & !1:nrow(burn) %in% grep("2013", burn$NOTES) & !1:nrow(burn) %in% grep("Pizzo", burn$NOTES),])

# Looking at things with no dates and no notes
summary(burn.df[is.na(burn.df$Burn_Date2) & burn.df$NOTES==" ",])
plot(burn[is.na(burn$Burn_Date2) & burn$NOTES==" ",])
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Working with the thinning data
# ------------------------------------------------------------------------------
thin.layers <- ogrListLayers(file.path(path.thin, gdb.thin))

thin1 <- readOGR(file.path(path.thin, gdb.thin), "Winter_Clearing")
thin1$Year.Finish <- as.numeric(paste0(substr(thin1$Year,1,2), substr(thin1$Year,nchar(paste(thin1$Year))-1,nchar(paste(thin1$Year)))))
summary(thin1)
summary(thin1$Year)



plot(thin1)

thin.bob <- readOGR(file.path(path.thin, gdb.thin), "Bob_Fahey_Research")
thin.res <- readOGR(file.path(path.thin, gdb.thin), "Winter_Thinning_06_07_Subplots")
summary(thin.bob)
summary(thin.res)

plot(thin1)
plot(thin.bob, add=T, col="red", pch=19)
plot(thin.res, add=T, col="blue", pch=9)

write.csv(data.frame(thin.bob), "Fahey_ResearchPlots.csv", row.names=F)
# ------------------------------------------------------------------------------
