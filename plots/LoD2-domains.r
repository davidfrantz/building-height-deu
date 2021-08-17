require(MASS)

mx <- 50

ftif <- "D:/Dropbox/paper/3D/plots-sample6/LoD2-domains.tif"

dinp <- "J:/germany-height/sample/berlin/svr-feature-reduction-domains"

lab <- c("Berlin (10% left-out)\nradar", "Berlin (10% left-out)\noptical", "Berlin (10% left-out)\nradar+optical")

short <- c("rad", "opt", "all")

ncol <- c(20,20,20)

tiff(ftif, width = 18.1, height = 18.1/3, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  layout(matrix(1:3, 1, 3))
  par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)

  for (site in 1:3){
  
    st <- 1
    require(viridis)
    col <- viridis(ncol[site])
    m <- 0


    X <- as.numeric(readLines(file.path(dinp, sprintf("%s-reference.txt", short[site]))))
    Y <- as.numeric(readLines(file.path(dinp, sprintf("%s-prediction.txt", short[site]))))
    W <- as.numeric(readLines(file.path(dinp, "../density.txt")))
    
    del <- which(X<0 | Y<0)
    if (length(del)>0){
      X <- X[-del]
      Y <- Y[-del]
    }

    w <- W[round(X)+1]
    
    #sX <- split(X, round(X))
    #sY <- split(Y, round(X))
    #
    #sXm <- sapply(sX, median)
    #sYm <- sapply(sY, median)
    #lines(sXm, sYm, col=site+1, lwd=2); next


    plot(0, type="n", xlim=c(0,mx), ylim=c(0,mx), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
    polygon(c(0,mx,mx,0),c(0,0,mx,mx),col=1,border=NA)
    for (i in seq(1,mx,st)){
    for (j in seq(1,mx,st)){

      pos <- which(Y >= i-st & Y < i &
            X >= j-st & X < j)
      n <- length(pos)
      if (n < 1) next
      
      if (n > ncol[site]) n <- ncol[site]
      co <- col[n]

      if (n > m) m <- n

      polygon(c(j-st,j,j,j-st), c(i-st,i-st,i,i), col=co, border=NA)

    }
    }

    box()

    mod <- lm(Y~X)
    mod0 <- lm(Y~0+X)
    wmod <- rlm(Y~X, weights=w)
    wmod0 <- rlm(Y~0+X, weights=w)
    
    abline(0, 1,  col="white")
    abline(mod,   col="red")
    abline(mod0,  col="orange", lty=3)
    abline(wmod,  col="green")
    abline(wmod0, col="cyan",   lty=3)

    #lines(sXm, sYm, col=site+1, lwd=2)

#abline(v=25, col="white", lwd=2)
    axis(1, at=seq(0, mx, 5), font=2)
    axis(2, at=seq(0, mx, 5), font=2)

    mtext("Reference (m)",  1, line=2, font=2)
    mtext("Prediction (m)", 2, line=2, font=2)

    legend("topleft", legend=c(
    sprintf("y = %.2f + %.2f x | Rsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
    sprintf("y = %.2f x | Rsq = %.2f", coef(mod0), summary(mod0)$r.squared),
    sprintf("y = %.2f + %.2f x'", coef(wmod)[1], coef(wmod)[2]),
    sprintf("y = %.2f x'", coef(wmod0)),
    sprintf("RMSE  = %.2f", sqrt(mean((Y-X)^2))),
    sprintf("RMSE' = %.2f", sqrt(weighted.mean((Y-X)^2, w)))
    ), col=c("red","orange","green","cyan",NA,NA), lwd=1, lty=c(1,3,1,3,NA,NA), inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.col="white")

    legend("bottomright", legend=lab[site], inset=c(0.01,0.01), bty="n", cex=1, text.font=2, xpd=TRUE, text.col="white")
    
    inc <- 1/ncol[site]
    
    for (k in 1:ncol[site]){
      polygon(c(46,49,49,46), 12.5+c(rep((k-1)*inc,2),rep(k*inc,2))*10, col=col[k], border=NA)
    }
    
    text(mean(c(46,49)), 11, 1, font=2, col="white", adj=0.5, , cex=0.7)
    text(mean(c(46,49)), 24, sprintf(">%d", ncol[site]), font=2, col="white", adj=0.5, cex=0.7)

    lab_upperleft(site)

  }
  
dev.off()

