
mx <- 50


st <- 1
ncol <- 25
require(viridis)
col <- viridis(ncol)
m <- 0



Y <- as.numeric(readLines("J:/germany-height/sample/vienna/3dmodel-globalmodel.txt"))
X <- as.numeric(readLines("J:/germany-height/sample/vienna/pred-globalmodel.txt"))/10
W <- as.numeric(readLines("J:/germany-height/sample/vienna/density.txt"))
del <- which(X<0 | Y<0)
if (length(del)>0){
  X <- X[-del]
  Y <- Y[-del]
}

w <- W[round(Y)+1]


ftif <- "D:/Dropbox/paper/3D/plots-sample6/LoD2-vienna-val-globalmodel.tif"

tiff(ftif, width = 8.8, height = 8.8, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)


  plot(0, type="n", xlim=c(0,mx), ylim=c(0,mx), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  polygon(c(0,mx,mx,0),c(0,0,mx,mx),col=1,border=NA)
  for (i in seq(1,mx,st)){
  for (j in seq(1,mx,st)){

    pos <- which(Y >= i-st & Y < i &
          X >= j-st & X < j)
    n <- length(pos)
    if (n < 1) next
    
    if (n > ncol) n <- ncol
    co <- col[n]

    if (n > m) m <- n

    polygon(c(j-st,j,j,j-st), c(i-st,i-st,i,i), col=co, border=NA)

  }
  }

  box()

  mod <- lm(Y~X)
  mod0 <- lm(Y~0+X)
  wmod <- lm(Y~X, weights=w)
  wmod0 <- lm(Y~0+X, weights=w)
  
  abline(0, 1,  col="white")
  abline(mod,   col="red")
  abline(mod0,  col="orange", lty=3)
  abline(wmod,  col="green")
  abline(wmod0, col="cyan",   lty=3)

  axis(1, at=seq(0, mx, 5), font=2)
  axis(2, at=seq(0, mx, 5), font=2)

  mtext("Reference (m)",  2, line=2, font=2)
  mtext("Prediction (m)", 1, line=2, font=2)

  legend("topleft", legend=c(
  sprintf("y = %.2f + %.2f x | Rsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
  sprintf("y = %.2f + %.2f x' | Rsq = %.2f", coef(wmod)[1], coef(wmod)[2], summary(wmod)$r.squared),
  sprintf("y = %.2f x | Rsq = %.2f", coef(mod0), summary(mod0)$r.squared),
  sprintf("y = %.2f x' | Rsq = %.2f", coef(wmod0), summary(wmod0)$r.squared),
  sprintf("RMSE  = %.2f", sqrt(mean((Y-X)^2))),
  sprintf("RMSE' = %.2f", sqrt(weighted.mean((Y-X)^2, w)))
  ), col=c("red","green","orange","cyan",NA,NA), lwd=1, lty=c(1,1,3,3,NA,NA), inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.col="white")

    
    inc <- 1/ncol
    
    for (k in 1:ncol){
      polygon(c(45,48,48,45), 9.5+c(rep((k-1)*inc,2),rep(k*inc,2))*10, col=col[k], border=NA)
    }
    
    text(mean(c(45,48)), 8, 1, font=2, col="white", adj=0.5, , cex=0.7)
    text(mean(c(45,48)), 21, sprintf(">%d", ncol), font=2, col="white", adj=0.5, cex=0.7)

    

dev.off()

