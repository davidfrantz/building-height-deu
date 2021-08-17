mx <- 50

ftif <- "D:/Dropbox/paper/3D/plots-sample6/LoD2-all-hist-with-saturation-cv.tif"


dinp <- "J:/germany-height/histograms/cv"

finp <- c(
"berlin.txt",
"hamburg.txt",
"north-rhine-westphalia.txt",
"potsdam.txt",
"thuringia.txt")
#"vienna.txt")



#lab <- c("Berlin", "Hamburg", "North Rhine\nWestphalia", "Potsdam", "Thuringia", "Vienna")
lab <- c("\nBerlin", "\nHamburg", "North Rhine\nWestphalia", "\nPotsdam", "\nThuringia")
ymax <- c(1e5, 1e5, 1e7, 1e4, 1e6)

h <- as.list(rep(NA, 5))

share <- matrix(NA, 5, 5)

tiff(ftif, width = 18.1, height = 10, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  layout(matrix(1:6, 2, 3, byrow=TRUE))
  par(mai=c(0.4, 0.4, 0.2, 0.15), cex=1)

  for (site in 1:5){
  
    h[[site]] <- read.table(file.path(dinp, finp[site]), header=FALSE)

    plot(h[[site]][,1], h[[site]][,2], type="n", xlim=c(1,mx), ylim=c(1,ymax[site]), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE, log="y")
    lines(h[[site]][,1], h[[site]][,2])
    lines(h[[site]][,1], h[[site]][,3], col=2)
    
    zcross <- h[[site]][,1][max(which(h[[site]][,3] > h[[site]][,2]))]
    #ncross <- h[[site]][,2][min(which(h[[site]][,2]>h[[site]][,3] & h[[site]][,1] > 15))]
    
    abline(v=zcross, col="grey40")

    box()
    
    share[site,1] <- sum(h[[site]][h[[site]][,1]>2  & h[[site]][,1]<  6,2])/sum(h[[site]][,2])*100
    share[site,2] <- sum(h[[site]][h[[site]][,1]>6  & h[[site]][,1]< 15,2])/sum(h[[site]][,2])*100
    share[site,3] <- sum(h[[site]][h[[site]][,1]>15 & h[[site]][,1]< 30,2])/sum(h[[site]][,2])*100
    share[site,4] <- sum(h[[site]][h[[site]][,1]>30 & h[[site]][,1]<150,2])/sum(h[[site]][,2])*100
    share[site,5] <- sum(h[[site]][h[[site]][,1]>150,2])/sum(h[[site]][,2])*100


    mtext(zcross, at=zcross, 3, line=0., font=4, col="grey40")
    #axis(3, at=zcross, font=4, col.axis="grey40")
    axis(1, at=seq(0, mx, 5), font=2)
    axis(2, at=c(1e0,1e1,1e2,1e3,1e4,1e5,1e6,1e7), font=2)

    mtext("Height (m)",  1, line=2, font=2)
    mtext("Count", 2, line=2, font=2)

    legend("bottomleft", legend=c(
    sprintf("3D building model"),
    sprintf("prediction")
    ), col=c("black", "red"), lwd=1, lty=1, inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE)

    legend("bottomleft", legend=lab[site], inset=c(-0.05,0.2), bty="n", cex=1, text.font=2, xpd=TRUE)
    lab_upperleft(site, ylog=TRUE)

  }


  dinp <- "J:/germany-height/sample/all/train"

  flog <- c(
  "model-hh-nw-po-th-be-cal.log",
  "model-be-nw-po-th-hh-cal.log",
  "model-be-hh-po-th-nw-cal.log",
  "model-be-hh-nw-th-po-cal.log",
  "model-be-hh-nw-po-th-cal.log")

  lab <- c("Berlin", "Hamburg", "North Rhine Westphalia", "Potsdam", "Thuringia")

  plot(0, type="n", xlim=c(0,mx), ylim=c(2,mx), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE, log="y")
  lines(2:mx, 2:mx, col="grey20", lty=3)
  abline(h=seq(0, mx, 5),   col="grey80", lty=2)

  for (site in 1:5){

    tmp <- readLines(sprintf("%s/%s", dinp, flog[site]))
    tmp <- tmp[grep("RMSE", tmp)[1]:grep("RMSE", tmp)[2]]
    tmp <- tmp[grep("->", tmp)]
    tmp <- gsub(".* :::", "", tmp)
    X <- as.numeric(gsub("->.*", "", tmp))
    Y <- as.numeric(gsub(".*->", "", tmp))

    del <- which(X<2 | Y<2)
    if (length(del)>0){
      X <- X[-del]
      Y <- Y[-del]
    }
    
    sX <- split(X, round(X))
    sY <- split(Y, round(X))
    
    sXm <- sapply(sX, median)
    sYm <- sapply(sY, median)

    lines(sXm, sYm, col=site+1)

  }

  box()
  

  axis(1, at=seq(0, mx, 5), font=2)
  axis(2, at=seq(0, mx, 5), font=2)

  mtext("Reference (m)",  1, line=2, font=2)
  mtext("Prediction (m)", 2, line=2, font=2)

  legend("bottomright", legend=lab, col=2:6, ncol=1, lwd=1, inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE)
  lab_upperleft(6, ylog=TRUE)
  
dev.off()

