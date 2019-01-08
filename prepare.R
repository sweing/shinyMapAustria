# ----------------------------------------------
# BASE
# ----------------------------------------------
rm(list=ls())
source("./init.r", chdir = TRUE)
loadPackages(c("rgdal", "rgeos", "ggplot2", "openxlsx"))
#loadPackages("rmapshaper")
# ----------------------------------------------

# ----------------------------------------------
# CONFIG
# ----------------------------------------------
tol <- 100
gemeindenVersion <- 2017
# ----------------------------------------------

# ----------------------------------------------
# LOAD GEMEINDE SHAPE FILE
# ----------------------------------------------
shapePathGEMEINDEN <- file.path("./tmp", "GEMEINDEN", gemeindenVersion)
shapeFileGEMEINDEN <- paste0("STATISTIK_AUSTRIA_GEM_", gemeindenVersion, "0101")
shapeGEMEINDEN <- readOGR(dsn = shapePathGEMEINDEN, layer = shapeFileGEMEINDEN, use_iconv = TRUE, encoding = "UTF-8")
tmp <- gSimplify(shapeGEMEINDEN, tol=tol, topologyPreserve=TRUE)
shapeGEMEINDEN <- SpatialPolygonsDataFrame(tmp, data=shapeGEMEINDEN@data)
if(length(shapeGEMEINDEN) != length(tmp))
    stop()
# ----------------------------------------------


# ----------------------------------------------
# LOAD NUTS0 SHAPE FILE
# ----------------------------------------------
shapePathNUTS0 <- file.path("./tmp", "NUTS0", "1M")
shapeFileNUTS0 <- "NUTS_RG_01M_2016_4326_LEVL_0"
shapeNUTS0 <- readOGR(dsn = shapePathNUTS0, layer = shapeFileNUTS0, use_iconv = TRUE, encoding = "UTF-8")
# ----------------------------------------------

# ----------------------------------------------
# LOAD NUTS2 SHAPE FILE
# ----------------------------------------------
shapePathNUTS2 <- file.path("./tmp", "NUTS2")
shapeFileNUTS2 <- "STATISTIK_AUSTRIA_NUTS2_20160101"
shapeNUTS2 <- readOGR(dsn = shapePathNUTS2, layer = shapeFileNUTS2, use_iconv = TRUE, encoding = "UTF-8")
tmp <- gSimplify(shapeNUTS2, tol=tol, topologyPreserve=TRUE)
shapeNUTS2 <- SpatialPolygonsDataFrame(tmp, data=shapeNUTS2@data)
# ----------------------------------------------

# ----------------------------------------------
# LOAD NUTS3 SHAPE FILE
# ----------------------------------------------
shapePathNUTS3 <- file.path("./tmp", "NUTS3")
shapeFileNUTS3 <- "STATISTIK_AUSTRIA_NUTS3_20160101"
shapeNUTS3 <- readOGR(dsn = shapePathNUTS3, layer = shapeFileNUTS3, use_iconv = TRUE, encoding = "UTF-8")
tmp <- gSimplify(shapeNUTS3, tol=tol, topologyPreserve=TRUE)
shapeNUTS3 <- SpatialPolygonsDataFrame(tmp, data=shapeNUTS3@data)
# ----------------------------------------------

# ----------------------------------------------
# LAU-2 NUTS3 CORRESPONDANCE
# ----------------------------------------------
listLAU <- read.xlsx(file.path("./tmp/EU-28_LAU_2017_NUTS_2016_AT.xlsx"), sheet = 1, colNames=TRUE)
listLAU <- listLAU[1:2]
setnames(listLAU, c("NUTS3_ID", "LAU_ID"))
# ----------------------------------------------

# ----------------------------------------------
# CREATE RANDOM DATA, APPEND TO ALL SHAPEFILES
# ----------------------------------------------

shapeGEMEINDEN@data$RandNum1 <- runif(nrow(shapeGEMEINDEN@data))
shapeGEMEINDEN@data$RandNum2 <- runif(nrow(shapeGEMEINDEN@data))

shapeGEMEINDEN@data <- merge(shapeGEMEINDEN@data, listLAU, by.x="ID", by.y="LAU_ID", all.x=TRUE)
shapeGEMEINDEN@data[is.na(shapeGEMEINDEN@data$NUTS3_ID),]$NUTS3_ID <- "AT130"
shapeGEMEINDEN@data$NUTS2_ID <- as.factor(substr(as.character(shapeGEMEINDEN@data$NUTS3_ID), 1, 4))

variableNames <- c("RandNum1", "RandNum2")

shapeNUTS3@data <- cbind(shapeNUTS3@data, aggregate(shapeGEMEINDEN@data[, variableNames], list(NUTS3_ID = shapeGEMEINDEN@data$NUTS3_ID), mean)[1:length(variableNames)+1])
shapeNUTS3@data$NUT2_ID <- as.factor(substr(as.character(shapeNUTS3@data$ID), 1, 4))
shapeNUTS3@data$NUT0_ID <- as.factor(substr(as.character(shapeNUTS3@data$ID), 1, 2))

shapeNUTS2@data <- cbind(shapeNUTS2@data, aggregate(shapeNUTS3@data[, variableNames], list(NUT2_ID = shapeNUTS3@data$NUT2_ID), mean)[1:length(variableNames)+1])

for(i in 1:length(variableNames)){
    variableName <- variableNames[i]
    shapeNUTS0@data[[variableName]] <- 0
    shapeNUTS0@data[shapeNUTS0@data$NUTS_ID == "AT",][[variableName]] = aggregate(shapeNUTS3@data[, variableNames], list(NUT0_ID = shapeNUTS3@data$NUT0_ID), mean)[1,i+1]
}
setnames(shapeNUTS0@data, "NUTS_NAME", "NAME")
shapeNUTS0.sub <- shapeNUTS0[shapeNUTS0$NUTS_ID == "AT" | shapeNUTS0$NUTS_ID == "EE" ,]
# ----------------------------------------------

# ----------------------------------------------
# TRANSFORM & SAVE ALL SHAPE FILES
# ----------------------------------------------
shapeNUTS0.sub <- spTransform(shapeNUTS0.sub, CRS("+proj=longlat +ellps=GRS80"))
shapeNUTS2 <- spTransform(shapeNUTS2, CRS("+proj=longlat +ellps=GRS80"))
shapeNUTS3 <- spTransform(shapeNUTS3, CRS("+proj=longlat +ellps=GRS80"))
shapeGEMEINDEN <- spTransform(shapeGEMEINDEN, CRS("+proj=longlat +ellps=GRS80"))

saveData(shapeNUTS0.sub, "NUTS0.rData")
saveData(shapeNUTS2, "NUTS2.rData")
saveData(shapeNUTS3, "NUTS3.rData")
saveData(shapeGEMEINDEN, "GEMEINDEN.rData")
# ----------------------------------------------