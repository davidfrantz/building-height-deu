

ftif <- sprintf("D:/Dropbox/paper/3D/plots-sample6/rfr-vs-svr-mape.tif")

short <- c("rad", "opt", "all")
lab <- c("Berlin (10% left-out)\nradar", "Berlin (10% left-out)\noptical", "Berlin (10% left-out)\nradar+optical")


tiff(ftif, width = 18.1, height = 18.1/3, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
  
  layout(matrix(1:3, 1, 3))
  par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)


  for (site in 1:3){

  ml <- "svr"
  dinp <- sprintf("J:/germany-height/sample/berlin/%s-feature-reduction-domains", ml)
  tmp <- readLines(sprintf("%s/%s_model-%s-calval.log", dinp, short[site], ml))
  tmp <- tmp[grep("RMSE", tmp)[1]:grep("RMSE", tmp)[2]]
  tmp <- tmp[grep("->", tmp)]
  tmp <- gsub(".* :::", "", tmp)
  X_SV <- as.numeric(gsub("->.*", "", tmp))
  Y_SV <- as.numeric(gsub(".*->", "", tmp))
  W_SV <- as.numeric(readLines(file.path(dinp, "../density.txt")))


  ml <- "rfr"
  dinp <- sprintf("J:/germany-height/sample/berlin/%s-feature-reduction-domains", ml)
  tmp <- readLines(sprintf("%s/%s_model-%s-calval.log", dinp, short[site], ml))
  tmp <- tmp[grep("RMSE", tmp)[1]:grep("RMSE", tmp)[2]]
  tmp <- tmp[grep("->", tmp)]
  tmp <- gsub(".* :::", "", tmp)
  X_RF <- as.numeric(gsub("->.*", "", tmp))
  Y_RF <- as.numeric(gsub(".*->", "", tmp))
  W_RF <- as.numeric(readLines(file.path(dinp, "../density.txt")))





  M_RF <- sapply(split(abs((X_RF-Y_RF)/X_RF), ceiling(X_RF)-1), mean)
  M_SV <- sapply(split(abs((X_SV-Y_SV)/X_SV), ceiling(X_SV)-1), mean)


  x <- X_RF[X_RF > 0]
  y <- Y_RF[X_RF > 0]
  w <- W_RF
  mape_rf  <- mean(abs((x-y)/x), na.rm=TRUE)*100
  wmape_rf <- weighted.mean(abs((x-y)/x), w[ceiling(x)+1], na.rm=TRUE)*100

  x <- X_SV[X_SV > 0]
  y <- Y_SV[X_SV > 0]
  w <- W_SV
  mape_sv  <- mean(abs((x-y)/x), na.rm=TRUE)*100
  wmape_sv <- weighted.mean(abs((x-y)/x), w[ceiling(x)+1], na.rm=TRUE)*100



    plot(0, type="n", xlim=c(0,50), ylim=c(0,100), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)

    for (i in 2:50){
      polygon(c(i,i+1,i+1,i), c(0,0,rep(W_SV[i+1]/max(W_SV)*(100) , 2)), col="grey95", border="grey90")
    }

    abline(h=0)
    lines(as.integer(names(M_SV))+0.5,M_SV*100, col=1)
    lines(as.integer(names(M_RF))+0.5,M_RF*100, col=2)

    legend("topright", legend=c(
    sprintf("MAPE RFR: %6.2f%%", mape_rf),
    sprintf("MAPE SVR: %6.2f%%", mape_sv),
    sprintf("MAPE' RFR: %6.2f%%", wmape_rf),
    sprintf("MAPE' SVR: %6.2f%%", wmape_sv)), text.col=c(2,1,2,1), inset=c(0.125,0.20), bty="n", cex=0.8, xpd=TRUE)

    axis(1, at=seq(0, 50, 5), labels=FALSE, font=2)
    axis(2, at=seq(0, 100, 5), labels=FALSE, font=2)
    axis(1, at=seq(0, 50, 10), font=2)
    axis(2, at=seq(0, 100, 10), font=2)


    mtext("Reference (m)",  1, line=2, font=2)
    mtext("MAPE (%)", 2, line=2, font=2)
    
    legend("topright", legend=lab[site], inset=c(0.01,0.01), bty="n", cex=1, text.font=2, xpd=TRUE)
    

    box()

    lab_upperleft(site)


  }

dev.off()
