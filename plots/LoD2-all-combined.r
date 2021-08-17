require(MASS)

mx <- 50

ftif <- "D:/Dropbox/paper/3D/plots-sample6/LoD2-all-cv-combined.tif"

dinp <- "J:/germany-height/sample/all/train"

flog <- c(
"model-70-30.log",
"model-hh-nw-po-th-be-cal.log",
"model-be-nw-po-th-hh-cal.log",
"model-be-hh-po-th-nw-cal.log",
"model-be-hh-nw-th-po-cal.log",
"model-be-hh-nw-po-th-cal.log")

fden <- c(
"",
"J:/germany-height/sample/berlin/density.txt",
"J:/germany-height/sample/hh/density.txt",
"J:/germany-height/sample/nrw/density.txt",
"J:/germany-height/sample/potsdam/density.txt",
"J:/germany-height/sample/thueringen/density.txt")


lab <- c("Total\n30% Left-out", "Berlin", "Hamburg", "North Rhine Westphalia", "Potsdam", "Thuringia")#, "Vienna")

ncol <- c(200,200,200,200,200,200)

tiff(ftif, width = 18.1, height = 18.1/3*2, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")

  layout(matrix(1:6, 2, 3, byrow=TRUE))
  par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)

  for (site in 1:6){
  
    st <- 1
    require(viridis)
    col <- viridis(ncol[site])
    m <- 0


    if (site > 1){
      tmp <- readLines(sprintf("%s/%s", dinp, flog[site]))
      tmp <- tmp[grep("RMSE", tmp)[1]:grep("RMSE", tmp)[2]]
      tmp <- tmp[grep("->", tmp)]
      tmp <- gsub(".* :::", "", tmp)
      X <- as.numeric(gsub("->.*", "", tmp))
      Y <- as.numeric(gsub(".*->", "", tmp))
      W <- as.numeric(readLines(fden[site]))
    } else {
      tmp <- readLines(sprintf("%s/%s", dinp, flog[site]))
      tmp <- tmp[grep("RMSE", tmp)[1]:grep("RMSE", tmp)[2]]
      tmp <- tmp[grep("->", tmp)]
      tmp <- gsub(".* :::", "", tmp)
      X <- as.numeric(gsub("->.*", "", tmp))
      Y <- as.numeric(gsub(".*->", "", tmp))
      W <- rep(0, 1000)
      for (site2 in 2:6){
      W <- W+as.numeric(readLines(fden[site2]))
      }
      W <- W/5
    }
    
    del <- which(X<0 | Y<0)
    if (length(del)>0){
      X <- X[-del]
      Y <- Y[-del]
    }

    w <- W[round(X)+1]


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
    wmod <- lm(Y~X, weights=w)
    wmod0 <- lm(Y~0+X, weights=w)
    
    abline(0, 1,  col="white")
    abline(mod,   col="red")
    abline(mod0,  col="orange", lty=3)
    abline(wmod,  col="green")
    abline(wmod0, col="cyan",   lty=3)


    axis(1, at=seq(0, mx, 5), labels=FALSE, font=2)
    axis(2, at=seq(0, mx, 5), labels=FALSE, font=2)
    axis(1, at=seq(0, mx, 10), font=2)
    axis(2, at=seq(0, mx, 10), font=2)

    mtext("Reference (m)",  1, line=2, font=2)
    mtext("Prediction (m)", 2, line=2, font=2)

    legend("topleft", legend=c(
    sprintf("y = %.2f + %.2f x | Rsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
    sprintf("y = %.2f + %.2f x' | Rsq = %.2f", coef(wmod)[1], coef(wmod)[2], summary(wmod)$r.squared),
    sprintf("y = %.2f x | Rsq = %.2f", coef(mod0), summary(mod0)$r.squared),
    sprintf("y = %.2f x' | Rsq = %.2f", coef(wmod0), summary(wmod0)$r.squared),
    sprintf("RMSE  = %.2f", sqrt(mean((Y-X)^2))),
    sprintf("RMSE' = %.2f", sqrt(weighted.mean((Y-X)^2, w)))
    ), col=c("red","green","orange","cyan",NA,NA), lwd=1, lty=c(1,1,3,3,NA,NA), inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.col="white")

    legend("bottomright", legend=lab[site], inset=c(0.01,0.01), bty="n", cex=1, text.font=2, xpd=TRUE, text.col="white")
    
    inc <- 1/ncol[site]
    
    for (k in 1:ncol[site]){
      polygon(c(45,48,48,45), 9.5+c(rep((k-1)*inc,2),rep(k*inc,2))*10, col=col[k], border=NA)
    }
    
    text(mean(c(45,48)), 8, 1, font=2, col="white", adj=0.5, , cex=0.7)
    text(mean(c(45,48)), 21, sprintf(">%d", ncol[site]), font=2, col="white", adj=0.5, cex=0.7)

    
    lab_upperleft(site)

  }
  
dev.off()

