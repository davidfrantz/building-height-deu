roof <- read.table("D:/Dropbox/paper/3D/plots-sample6/monthly-near-avg-roof.txt",  skip=3, header=FALSE)[,2]/10000
sbahn <- read.table("D:/Dropbox/paper/3D/plots-sample6/monthly-near-avg-sbahn.txt",  skip=3, header=FALSE)[,2]/10000


ftif <- "D:/Dropbox/paper/3D/plots-sample6/shadow-effect.tif"

tiff(ftif, width = 8.8, height = 6, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)


  plot(0, type="n", xlim=c(1,12), ylim=c(0,0.25), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  lines(roof, col=4)
  lines(sbahn, col=2)

  m <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  
  axis(1, at=1:12, labels=m, font=2, las=1)
  axis(2, font=2)
  
  box()

  mtext("average NIR reflectance",  2, line=2, font=2)

  legend("topleft", legend=c("Building roof"), col=c(4), lwd=1,  inset=c(0.01,0.01), bty="n")
  legend("left", legend=c("Shadowed ground (unvegetated)"), col=c(2), lwd=1,  inset=c(0.01,0.01), bty="n")

dev.off()

