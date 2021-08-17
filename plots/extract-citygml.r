dout <- "D:/Dropbox/paper/3D/plots-sample6"

finp <- "Z:/MAT_STOCKS/20_data/3D-Modell-NRW/3d-gm_lod2_05315000_Köln_EPSG25832_CityGML/LoD2_355_5644_1_NW.gml"

proj4 <- "+init=epsg:25832 +proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "


bldg <- read.table("Z:/MAT_STOCKS/20_data/3d-Modell-NRW/NRW-building-codes-flags.txt", header=FALSE, sep="\t")
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

colramp <- colorRampPalette(c(rgb(0,0,1),rgb(0,1,1),rgb(1,1,0),rgb(1,0,0)))

ftif <- file.path(dout, "extract-citygml.tif")

tiff(ftif, width = 8.8, height = 8.8, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")

par(bg="white", fg="black", col.axis="black", col.lab="black", col.main="black", col.sub="black", mai=c(0.4,0.4,0.2,0.2))

plot(0, type="n", xlim=c(4105750,4105890), ylim=c(3095580,3095720), xaxs="i", yaxs="i", col=col, cex=0.1, xlab="Longitude", ylab="Latitude", main="Cologne")

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
  
  bldfun <- gsub(" +", "", gsub("<[^<>]*>", "", grep("bldg:function", sub, value=TRUE)))
  if (!bldfun %in% codes) next
  
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
    storage.mode(coords) <- "numeric"
    xy <- project(geo, proj4_laea)
      
    cen <- colMeans(xy)
    tab <- rbind(tab, c(cen, z, area, bldfun))
    
    if (cen[1] > 4105725 & cen[1] < 4105900 & cen[2] > 3095500 & cen[2] < 3095750){
      col <- colramp(30)[pmin(30,round(as.numeric(z)))]
      polygon(xy[,1], xy[,2], col=col, border="grey40")
      #lines(xy)
    }


  }

  #if (o %% 25 == 0 | o == nobj){
  #  par(bg="black", fg="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
  #  col <- heat.colors(25)[pmin(25,round(as.numeric(tab[,3])))]
  #  plot(tab[,1], tab[,2], col=col, pch=19, cex=0.1, xlab="Longitude", ylab="Latitude", main="Cologne")
  #}

}


ulx <- 4105720
uly <- 3095750

#col <- heat.colors(25)[pmin(25,round(as.numeric(tab[,3])))]
#points(tab[,1], tab[,2], col=col, pch=19, cex=0.1)

abline(v=ulx+seq(0,by=10, length.out=50))
abline(h=uly-seq(0,by=10, length.out=50))

center <- c(4105835,3095665)

#polygon(c(center[1]-5,center[1]+5,center[1]+5,center[1]-5), c(center[2]-5,center[2]-5,center[2]+5,center[2]+5), col=NA, border="magenta", lwd=2)

symbols(center[1], center[2], 50, add=TRUE, inches=FALSE, fg="magenta")
symbols(center[1], center[2], 5, add=TRUE, inches=FALSE, fg="magenta")
points(center[1], center[2], col="magenta", pch=3, lwd=2)

near <- which(sqrt((center[1]-as.numeric(tab[,1]))^2 + (center[2]-as.numeric(tab[,2]))^2) < 50)

points(tab[near,1], tab[near,2], col=1, pch=19, cex=as.numeric(tab[near,4])/200)


dev.off() 








