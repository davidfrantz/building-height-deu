require(maptools)

coords <- matrix(c(13.408333,52.518611), 1, 2) # berlin


#dates <- format(strptime(paste(2018, seq(1,365,15)), "%Y %j"), "%Y%m%d")
dates <- format(strptime(paste(2018, 1:365), "%Y %j"), "%Y%m%d")
#dates <- format(strptime(paste(2018, 1:12, 15), "%Y %m %d"), "%Y%m%d")
dates_astro <- c("20180320", "20180721", "20180923", "20181221")

sun <- rep(NA, length(dates))

for (d in 1:length(dates)){

  sun[d] <- solarpos(coords, as.POSIXct(strptime(sprintf("%s 10:30:00Z", dates[d]), "%Y%m%d %H:%M:%S")))[2]

}

heights <- 0:30

len <- matrix(NA, length(heights), length(dates))

for (h in 1:length(heights)){
  
  len[h,] <- heights[h]/tan(sun*pi/180)
  
}

#persp(len, xlab="building height", ylab="date", zlab="shadow length", phi=20, theta=300)

ftif <- "D:/Dropbox/paper/3D/plots-sample6/shadow-length.tif"

tiff(ftif, width = 8.8, height = 6, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)


  plot(0, type="n", xlim=c(0,30), ylim=c(0,150), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  lines(len[,which(dates == dates_astro[1])], col="lawngreen")
  lines(len[,which(dates == dates_astro[2])], col="forestgreen")
  lines(len[,which(dates == dates_astro[3])], col="orange")
  lines(len[,which(dates == dates_astro[4])], col="blue")

  abline(h=50, lty=3)

  axis(1, font=2)
  axis(2, font=2)
  
  box()

  mtext("building height", 1, line=2, font=2)
  mtext("shadow length", 2, line=2, font=2)

  legend("topleft", legend=c("spring equinox", "summer solstice", "fall equinox", "winter solstice", "radius"), col=c("lawngreen","forestgreen","orange","blue",1), lty=c(rep(1,4),3),  inset=c(0.01,0.01), bty="n")

dev.off()



s10m <- rep(NA, length(heights))
for (h in 1:length(heights)) if (min(abs(len[h,1:182]-10))<0.5) s10m[h] <- which.min(abs(len[h,1:182]-10))

s25m <- rep(NA, length(heights))
for (h in 1:length(heights)) if (min(abs(len[h,1:182]-25))<0.5) s25m[h] <- which.min(abs(len[h,1:182]-25))

s50m <- rep(NA, length(heights))
for (h in 1:length(heights)) if (min(abs(len[h,1:182]-50))<0.5) s50m[h] <- which.min(abs(len[h,1:182]-50))

w10m <- rep(NA, length(heights))
for (h in 1:length(heights)) if (min(abs(len[h,182:365]-10))<0.5) w10m[h] <- which.min(abs(len[h,182:365]-10))+181

w25m <- rep(NA, length(heights))
for (h in 1:length(heights)) if (min(abs(len[h,182:365]-25))<0.5) w25m[h] <- which.min(abs(len[h,182:365]-25))+181

w50m <- rep(NA, length(heights))
for (h in 1:length(heights)) if (min(abs(len[h,182:365]-50))<0.5) w50m[h] <- which.min(abs(len[h,182:365]-50))+181

l10m <- w10m-s10m
l25m <- w25m-s25m
l50m <- w50m-s50m

l10m[1:(min(which(!is.na(l10m)))-1)] <- 365
l25m[1:(min(which(!is.na(l25m)))-1)] <- 365
l50m[1:(min(which(!is.na(l50m)))-1)] <- 365

l10m[(max(which(!is.na(l10m)))+1):31] <- 0
l25m[(max(which(!is.na(l25m)))+1):31] <- 0
#l50m[(max(which(!is.na(l50m)))+1):31] <- 0

ftif <- "D:/Dropbox/paper/3D/plots-sample6/shadow-length2.tif"

tiff(ftif, width = 18.1, height = 6, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  layout(matrix(1:2, 1, 2))
  
  par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)


  plot(0, type="n", xlim=c(0,30), ylim=c(0,150), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  lines(len[,which(dates == dates_astro[1])], col="lawngreen")
  lines(len[,which(dates == dates_astro[2])], col="forestgreen")
  lines(len[,which(dates == dates_astro[3])], col="orange")
  lines(len[,which(dates == dates_astro[4])], col="blue")

  abline(h=50, lty=3)

  axis(1, font=2)
  axis(2, font=2)
  
  box()

  mtext("building height", 1, line=2, font=2)
  mtext("shadow length", 2, line=2, font=2)

  legend("topleft", legend=c("spring equinox", "summer solstice", "fall equinox", "winter solstice", "radius"), col=c("lawngreen","forestgreen","orange","blue",1), lty=c(rep(1,4),3),  inset=c(0.01,0.01), bty="n")
  
  lab_upperleft(1)

  plot(0, type="n", xlim=c(0,30), ylim=c(0,365), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  lines(heights, l10m, type="l", col="red")
  lines(heights, l25m, type="l", col="forestgreen")
  lines(heights, l50m, type="l", col="blue")
  
  abline(v=10, lty=3)

  axis(1, font=2)
  axis(2, font=2)
  
  box()

  mtext("building height", 1, line=2, font=2)
  mtext("number of shadow-free days", 2, line=2, font=2)
  
  
  legend("bottomleft", legend="distance\nfrom building\n", inset=c(-0.05, 0.25), bty="n")
  legend("bottomleft", legend=c("10m", "25m", "50m"), col=c("red","forestgreen","blue"), lty=1,  inset=c(0.01,0.01), bty="n")

  lab_upperleft(2)

dev.off()


