require(sp)
require(rgdal)
require(raster)

dirs1 <- c(
"/data/Jakku/germany-height/3d-extract-map-berlin",
"/data/Jakku/germany-height/3d-extract-map-hh",
"/data/Jakku/germany-height/3d-extract-map-nrw",
"/data/Jakku/germany-height/3d-extract-map-potsdam",
"/data/Jakku/germany-height/3d-extract-map-thueringen",
"/data/Jakku/germany-height/3d-extract-map-vienna")

dirs2 <- c(
"/data/Jakku/germany-height/sample/all/pred/be",
"/data/Jakku/germany-height/sample/all/pred/hh",
"/data/Jakku/germany-height/sample/all/pred/nw",
"/data/Jakku/germany-height/sample/all/pred/po",
"/data/Jakku/germany-height/sample/all/pred/th",
NA)

ftiles <- c(
"/data/Jakku/germany-height/sample/berlin/berlin.til",
"/data/Jakku/germany-height/sample/hh/hamburg.til",
"/data/Jakku/germany-height/sample/nrw/north-rhine-westphalia.til",
"/data/Jakku/germany-height/sample/potsdam/potsdam.til",
"/data/Jakku/germany-height/sample/thueringen/thuringia.til",
"/data/Jakku/germany-height/sample/vienna/vienna.til")


for (site in 1:6){

  tiles <- readLines(ftiles[site])[-1]
  nt <- length(tiles)

  z <- as.list(rep(NA, nt))
  z_ <- as.list(rep(NA, nt))

  for (t in 1:nt){

    if (!file.exists(sprintf("%s/%s/3D-HEIGHT.tif", dirs1[site], tiles[t]))){
      z[[t]] <- matrix(NA, 3000, 3000)
    } else {
      z_avg <- raster(sprintf("%s/%s/3D-HEIGHT.tif", dirs1[site], tiles[t]))[]
    }
    z[[t]] <- matrix(z_avg, 3000, 3000)

    z_avg <- raster(sprintf("%s/%s/NEWFILE_MLP.tif", dirs2, tiles[t]))[]/10
    z_[[t]] <- matrix(z_avg, 3000, 3000)

    cat("done with", t, "\n")

  }

  z <- unlist(z)
  z_ <- unlist(z_)

layout(matrix(1:2, 2, 1))
hist(z[z>0],  breaks=seq(-100,1000,1), xlim=c(0,50))
hist(z_[z>0],  breaks=seq(-100,1000,1), xlim=c(0,50))

  del <- which(is.na(z) | is.na(z_))
  if (length(del)>0) z  <- z[-del]
  if (length(del)>0) z_ <- z_[-del]

  del <- which(z == 0)
  if (length(del)>0) z  <- z[-del]
  if (length(del)>0) z_ <- z_[-del]

  hz  <- hist(z,  breaks=seq(-100,1000,1), plot=FALSE)
  hz_ <- hist(z_, breaks=seq(-100,1000,1), plot=FALSE)

  write.table(cbind(hz$mids, hz$counts, hz_$counts), sprintf("/data/Jakku/germany-height/hist-pred6/%s", gsub(".til", ".txt", basename(ftiles[site]))), row.names=FALSE, col.names=FALSE, sep=" ")

}

