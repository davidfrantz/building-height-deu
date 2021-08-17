require(raster)

frantz <- raster("J:/germany-height/li/UrbanStructure/Version-1.0/OutputData_CombinedModel/eu/height-DEU-1km.tif")[]
schug  <- raster("J:/germany-height/li/UrbanStructure/Version-1.0/OutputData_CombinedModel/eu/fractions-DEU-1km.tif")[]
li     <- raster("J:/germany-height/li/UrbanStructure/Version-1.0/OutputData_CombinedModel/eu/eu_h_c_mn_DEU.tif")[]

del <- which(is.na(frantz) | is.na(li))

frantz <- frantz[-del]
li     <- li[-del]
schug  <- schug[-del]

frantz <- frantz/10


mx <- 50

ftif <- "D:/Dropbox/paper/3D/plots-sample6/li-vs-frantz.tif"


tiff(ftif, width = 18.1, height = 18.1/3, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  layout(matrix(1:3, 1, 3, byrow=TRUE))
  par(mai=c(0.6, 0.6, 0.15, 0.15), cex=1)


  st <- 1
  require(viridis)
  require(RColorBrewer)
  ncol <- 100
  ncol_f <- 101
  col <- viridis(ncol)
  col_f <- colorRampPalette(c("darkgreen", "lightgoldenrod", "darkred"))(ncol_f)
  m <- 0



  X <- li
  Y <- frantz
  F <- schug
  #W <- 

  #w <- W[round(X)+1]


  plot(0, type="n", xlim=c(0,mx), ylim=c(0,mx), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  polygon(c(0,mx,mx,0),c(0,0,mx,mx),col=1,border=NA)
  for (i in seq(1,mx,st)){
  for (j in seq(1,mx,st)){

    pos <- which(Y >= i-st & Y < i &
          X >= j-st & X < j)
    n <- length(pos)
    if (n < 1) next


    if (sqrt(n) > ncol) n <- ncol^2
    co <- col[sqrt(n)]

    if (n > m) m <- n

    polygon(c(j-st,j,j,j-st), c(i-st,i-st,i,i), col=co, border=NA)

  }
  }

    box()

    mod <- lm(Y~X)
    mod0 <- lm(Y~0+X)

    abline(0, 1,  col="white")
    abline(mod,   col="red")
    abline(mod0,  col="orange", lty=3)

    axis(1, at=seq(0, mx, 5), font=2)
    axis(2, at=seq(0, mx, 5), font=2)
    mtext("Building height (m)",  1, line=2, font=2)
    mtext("Li et al. 2020a",  1, line=3, font=2)
    mtext("Building height (m)",  2, line=2, font=2)
    mtext("This study",  2, line=3, font=2)

    
    legend("topleft", legend=c(
    sprintf("y = %.2f + %.2f x | Rsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
    sprintf("y = %.2f x | Rsq = %.2f", coef(mod0), summary(mod0)$r.squared),
    sprintf("RMSE  = %.2f", sqrt(mean((Y-X)^2)))
    ), col=c("red","orange",NA,NA), lwd=1, lty=c(1,3,NA,NA), inset=c(0.01,0.01), bty="n", cex=0.7, xpd=TRUE, text.col="white")

    #legend("bottomright", legend=lab, inset=c(0.01,0.01), bty="n", cex=1, text.font=2, xpd=TRUE, text.col="white")
    
    inc <- 1/(ncol^2)
    
    for (k in 1:(ncol^2)){
      polygon(c(45,48,48,45), 9.5+c(rep((k-1)*inc,2),rep(k*inc,2))*10, col=col[sqrt(k)], border=NA)
      #if (k^2 %in% c(9, 100, 250, 529, 729)) lines(c(45,48), 9.5+rep(k*inc,2)*10)
    }

    text(mean(c(45,48)), 8, 1, font=2, col="white", adj=0.5, , cex=0.7)
    text(mean(c(44.5,48)), 21, sprintf(">%d", ncol^2), font=2, col="white", adj=0.5, cex=0.7)

    
lab_upperleft(1)


mx <- 100
my <- 50


    #Y <- li[schug>50]
    #F <- schug[schug>50]
    Y <- li
    F <- schug
    F2 <- F*F

    s <- split(Y, F)
    boxplot(s, outline=FALSE, xlim=c(0,mx), ylim=c(0,my), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
    
    mod <- lm(Y~F)
    mod2 <- lm(Y~F+F2)
    abline(mod,   col="red")
    lines(0:100, coef(mod2)[1]+(0:100)*coef(mod2)[2]+(0:100)*(0:100)*coef(mod2)[3],   col="pink")


    legend("topleft", legend=c(
    sprintf("y = %.2f + %.2f x | Rsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
    sprintf("y = %.2f + %.2f x + %.4f x² | Rsq = %.2f", coef(mod2)[1], coef(mod2)[2], coef(mod2)[3], summary(mod2)$r.squared)
    ), col=c("red","pink"), lwd=1, lty=c(1,1), inset=c(0.01,0.01), bty="n", cex=0.7, xpd=TRUE)
    
    axis(1, at=seq(0, mx, 10), font=2)
    axis(2, at=seq(0, my, 5), font=2)
    
    mtext("Built-up fraction (%)", 1, line=2, font=2)
    mtext("Schug et al. 2020", 1, line=3, font=2)
    mtext("Building height (m)",  2, line=2, font=2)
    mtext("Li et al. 2020a",  2, line=3, font=2)



lab_upperleft(2)


    #Y <- frantz[schug>50]
    #F <- schug[schug>50]
    Y <- frantz
    F <- schug
    F2 <- F*F
    
    s <- split(Y, F)
    boxplot(s, outline=FALSE, xlim=c(0,mx), ylim=c(0,my), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)
  
    mod <- lm(Y~F)
    mod2 <- lm(Y~F+F2)
    abline(mod,   col="red")
    lines(0:100, coef(mod2)[1]+(0:100)*coef(mod2)[2]+(0:100)*(0:100)*coef(mod2)[3],   col="pink")

    legend("topleft", legend=c(
    sprintf("y = %.2f + %.2f x | Rsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
    sprintf("y = %.2f + %.2f x + %.4f x² | Rsq = %.2f", coef(mod2)[1], coef(mod2)[2], coef(mod2)[3], summary(mod2)$r.squared)
    ), col=c("red","pink"), lwd=1, lty=c(1,1), inset=c(0.01,0.01), bty="n", cex=0.7, xpd=TRUE)
    
    axis(1, at=seq(0, mx, 10), font=2)
    axis(2, at=seq(0, my, 5), font=2)

    mtext("Built-up fraction (%)", 1, line=2, font=2)
    mtext("Schug et al. 2020", 1, line=3, font=2)
    mtext("Building height (m)",  2, line=2, font=2)
    mtext("This study",  2, line=3, font=2)


lab_upperleft(3)



dev.off()
