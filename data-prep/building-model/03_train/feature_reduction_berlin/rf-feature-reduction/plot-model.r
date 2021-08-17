fname <- "J:/germany-height/sample6-berlin/rf-feature-reduction/model-rfr-cal.log"

log <- readLines(fname)

log_1 <- log[1:grep("Self", log)]
log_2 <- log[grep("Self", log): length(log)]

dat_1 <- grep(":::", log_1, value=TRUE)
dat_1 <- gsub(".*::: ", "", dat_1)
response_1 <- as.numeric(gsub(" -> .*", "", dat_1))
predict_1  <- as.numeric(gsub(".* -> ", "", dat_1))

dat_2 <- grep(":::", log_2, value=TRUE)
dat_2 <- gsub(".*::: ", "", dat_2)
response_2 <- as.numeric(gsub(" -> .*", "", dat_2))
predict_2  <- as.numeric(gsub(".* -> ", "", dat_2))



#predict_1  <- predict_1[-which(response_1 > 25)]
#response_1 <- response_1[-which(response_1 > 25)]
#predict_2  <- predict_2[-which(response_2 > 25)]
#response_2 <- response_2[-which(response_2 > 25)]

#mx <- max(c(response, predict))
mx <- 50
#mx <- 250


layout(matrix(1:2, 1, 2))

smoothScatter(response_1, predict_1, xlim=c(0,mx), ylim=c(0,mx),  colramp=rainbow)
abline(0, 1, lwd=2)
abline(h=2, lwd=2)
abline(v=2, lwd=2)
abline(lm(predict_1~response_1), lwd=2, col="yellow")

smoothScatter(response_2, predict_2, xlim=c(0,mx), ylim=c(0,mx),  colramp=rainbow)
abline(0, 1, lwd=2)
abline(h=2, lwd=2)
abline(v=2, lwd=2)
abline(lm(predict_2~response_2), lwd=2, col="yellow")
abline(lm(predict_2[response_2<mx]~response_2[response_2<mx]), lwd=2, col="green")


st <- 1
ncol <- 115
require(viridis)
col <- viridis(ncol)
m <- 0

for (set in 1:2){

  if (set == 1) ftif <- gsub(".log", "_self.tif", fname)
  if (set == 2) ftif <- gsub(".log", "_val.tif", fname)
  
  if (set == 1){ X <- response_1; Y <- predict_1}
  if (set == 2){ X <- response_2; Y <- predict_2}
    
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

    modall <- lm(Y~X)
    mod30  <- lm(Y[X<30]~X[X<30])
    mod0all <- lm(Y~0+X)
    mod030 <- lm(Y[X<30]~0+X[X<30])

    abline(0, 1, col="white")
    abline(modall, col="red")
    abline(mod30,  col="red", lty=2)
    abline(mod0all, col="green")
    abline(mod030,  col="green", lty=2)

    axis(1, at=seq(0, mx, 5), font=2)
    axis(2, at=seq(0, mx, 5), font=2)

    mtext("Reference (m)",  1, line=2, font=2)
    mtext("Prediction (m)", 2, line=2, font=2)
    
    legend("topleft", legend=c(
    sprintf("y = %.2f + %.2f x [all] | R² = %.2f | RMSE = %.2f", coef(modall)[1], coef(modall)[2], summary(modall)$r.squared, sqrt(mean((Y-X)^2))),
    sprintf("y = %.2f + %.2f x [z<30] | R² = %.2f | RMSE = %.2f", coef(mod30)[1], coef(mod30)[2], summary(mod30)$r.squared, sqrt(mean((Y[X<30]-X[X<30])^2))),
    sprintf("y = %.2f x [all] | R² = %.2f | RMSE = %.2f", coef(mod0all), summary(mod0all)$r.squared, sqrt(mean((Y-X)^2))),
    sprintf("y = %.2f x [z<30] | R² = %.2f | RMSE = %.2f", coef(mod030), summary(mod030)$r.squared, sqrt(mean((Y[X<30]-X[X<30])^2)))
    ), col=c(2,2,3,3), lwd=1, lty=c(1:2,1:2), inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.col="white")


  dev.off()

}




