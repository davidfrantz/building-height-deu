#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)


# test if there is at least one argument: if not, return an error
if (length(args) != 1) {
  stop("Give input file as argument.n", call.=FALSE)
}


#dinp <- "/mnt/mat_stocks/20_data/3d-Modell-NRW"
#dout <- "/mnt/mat_stocks/20_data/3d-Modell-NRW/tables6"
dout <- "/data/Jakku/germany-height/3d-extract/tables/hh2"
finp <- args[1]


#epsg <- unique(gsub(".*EPSG([^_]*).*", "\\1", finp))
#print(epsg)


proj4 <- "+init=epsg:25832 +proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


bldg <- read.table("/mnt/mat_stocks/20_data/3d-Modell-NRW/NRW-building-codes-flags.txt", header=FALSE, sep="\t")
codes <- bldg[which(bldg[,3] == 1),1]


suppressMessages(suppressWarnings(require(sp)))
suppressMessages(suppressWarnings(require(rgdal)))



tab <- numeric(0)

gml <- readLines(finp, warn=FALSE)

 
ostart <- grep("<core:cityObjectMember>", gml)
oend   <- grep("</core:cityObjectMember>", gml)
nobj <- length(ostart)

if (nobj < 1) stop("no object")
#if (length(ostart) < 1 | is.na(ostart)) stop()
#if (length(oend) < 1   | is.na(ostart)) stop()
if (length(ostart) < 1) stop("no object")
if (length(oend) < 1  ) stop("no object")

for (o in 1:nobj){

  sub <- gml[ostart[o]:oend[o]]
  
  bstart    <- grep("<bldg:BuildingPart", sub)
  bend      <- grep("</bldg:BuildingPart", sub)
  nbuilding <- length(bstart)
  #print(nbuilding)

  if (nbuilding < 1){
    bstart    <- 1
    bend      <- length(sub)
    nbuilding <- 1
  }
  
  bldfun <- gsub("\t", "", gsub(" +", "", gsub("<[^<>]*>", "", grep("bldg:function", sub, value=TRUE))))
  #if (!bldfun %in% codes) next
  
  for (b in 1:nbuilding){
  
    subsub <- sub[bstart[b]:bend[b]]

    z <- as.numeric(gsub("<[^<>]*>", "", grep("measuredHeight", subsub, value=TRUE)))
    if (z < 2) next

    linestart <- grep("<gml:LineString>", subsub)
    lineend   <- grep("</gml:LineString>", subsub)
    nlines    <- length(linestart)

    if (nlines < 1) next

    coords <- numeric(0)

    for (l in 1:nlines){

      subsubsub <- subsub[linestart[l]:lineend[l]]
      subsubsub <- gsub("\t", "", subsubsub)
      coo       <- unlist(strsplit(gsub("<[^<>]*>", "", subsubsub), " +"))
      coo       <- coo[which(coo!="")]
      ncoo      <- length(coo)/3
      
      coo_ <- matrix(as.numeric(coo), ncoo, 3, byrow=TRUE)[,1:2]
      ncoo_ <- dim(coo_)[1]
      ncoords <- dim(coords)[1]

      if (!is.null(ncoords)){
        
        dist_start_1 <- sqrt((coords[1,1]-coo_[1,1])^2 + (coords[1,2]-coo_[1,2])^2)
        dist_start_2 <- sqrt((coords[1,1]-coo_[ncoo_,1])^2 + (coords[1,2]-coo_[ncoo_,2])^2)
        dist_end_1 <- sqrt((coords[ncoords,1]-coo_[1,1])^2 + (coords[ncoords,2]-coo_[1,2])^2)
        dist_end_2 <- sqrt((coords[ncoords,1]-coo_[ncoo_,1])^2 + (coords[ncoords,2]-coo_[ncoo_,2])^2)
        
        if (min(dist_start_1, dist_start_2) < min(dist_end_1, dist_end_2)){
          coords <- coords[ncoords:1,]
          dist1 <- dist_start_1
          dist2 <- dist_start_2
        } else {
          dist1 <- dist_end_1
          dist2 <- dist_end_2
        }

        if (dist2 < dist1) coo_ <- coo_[ncoo_:1,]

      }

      coords <- rbind(coords, coo_)
      #plot(coords, pch=".")
      #text(coords, labels=1:dim(coords)[1])
      #Sys.sleep(3)

    }


    area <- 0
    for (c in 1:(dim(coords)[1]-1)){
      area <- area + 
        coords[c,1]*coords[c+1,2] - coords[c,2]*coords[c+1,1]
    }
    area <- abs(area/2)
if (area > 900000) break

    geo <- project(coords, proj4, inv=TRUE)
    cen <- colMeans(geo)
    tab <- rbind(tab, c(cen, z, area, bldfun))


  }

  #if (o %% 25 == 0 | o == nobj){
  #  par(bg="black", fg="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
  #  col <- heat.colors(25)[pmin(25,round(tab[,3]))]
  #  plot(tab[,1], tab[,2], col=col, pch=19, cex=0.1, xlab="Longitude", ylab="Latitude", main="Cologne")
  #}

}

if (length(tab) < 1) stop("no retrieved object")

mz <- mean(as.numeric(tab[,3]))

ftab <- sprintf("%s/%s_%s.txt", dout, basename(dirname(finp)), basename(finp))
ftif <- sprintf("%s/%s_%s.tif", dout, basename(dirname(finp)), basename(finp))

town <- gsub(".*[0-9]*_", "", gsub("_EPSG.*", "", basename(dirname(finp))))

write.table(tab, ftab, col.names=FALSE, row.names=FALSE, sep=" ", quote=FALSE)


tiff(ftif, width = 8.8, height = 8.8, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")

  par(bg="black", fg="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
  col <- heat.colors(25)[pmin(25,round(as.numeric(tab[,3])))]
  plot(as.numeric(tab[,1]), as.numeric(tab[,2]), col=col, pch=19, cex=0.1, xlab="Longitude", ylab="Latitude", main=sprintf("%s, mean height: %.1f", town, mz), font=2, font.lab=2)

dev.off() -> garbage








