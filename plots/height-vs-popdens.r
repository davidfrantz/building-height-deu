nuts <- as.list(rep(NA, 3))


z <- read.table("D:/Dropbox/paper/3D/plots-sample6/nuts3.txt", header=TRUE, sep=",")
pop <- read.table("D:/Dropbox/paper/3D/plots-sample6/popdens/demo_r_d3dens.tsv", sep="\t", header=TRUE, na.strings=": ")

pop <- pop[which(pop$code %in% z$NUTS_ID),]

dim(pop)
dim(z)

z <- z[order(z$NUTS_ID),]
pop <- pop[order(pop$code),]

cbind(as.character(z$NUTS_ID), as.character(pop$code))

#nuts[[3]]  <- cbind(z$MEAN/10, pop$X2018, z$AREA / (z$TOTALAREA*pop$X2018))
nuts[[3]]  <- cbind(z$MEAN/10, pop$X2018)

z <- read.table("D:/Dropbox/paper/3D/plots-sample6/nuts2.txt", header=TRUE, sep=",")
pop <- read.table("D:/Dropbox/paper/3D/plots-sample6/popdens/tgs00024.tsv", sep="\t", header=TRUE, na.strings=": ")

pop <- pop[which(pop$code %in% z$NUTS_ID),]

dim(pop)
dim(z)

z <- z[order(z$NUTS_ID),]
pop <- pop[order(pop$code),]

cbind(as.character(z$NUTS_ID), as.character(pop$code))

#nuts[[2]]  <- cbind(z$MEAN/10, pop$X2018, z$AREA / (z$TOTALAREA*pop$X2018))
nuts[[2]]  <- cbind(z$MEAN/10, pop$X2018)

z <- read.table("D:/Dropbox/paper/3D/plots-sample6/nuts1.txt", header=TRUE, sep=",")
pop <- read.table("D:/Dropbox/paper/3D/plots-sample6/popdens/nuts1.txt", sep="\t", header=TRUE, na.strings=": ")

pop <- pop[which(pop$code %in% z$NUTS_ID),]

dim(pop)
dim(z)

z <- z[order(z$NUTS_ID),]
pop <- pop[order(pop$code),]

cbind(as.character(z$NUTS_ID), as.character(pop$code))

#nuts[[1]] <- cbind(z$MEAN/10, pop$X2018, z$AREA / (z$TOTALAREA*pop$X2018))
nuts[[1]] <- cbind(z$MEAN/10, pop$X2018)

#########################################################################

ftif <- "D:/Dropbox/paper/3D/plots-sample6/height-vs-popdens-global-model.tif"


tiff(ftif, width = 18.1, height = 6, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")

layout(matrix(1:3, 1, 3))

par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)

  for (i in 1:3){

    plot(log(nuts[[i]][,2]), nuts[[i]][,1], pch=19, ylim=c(2.5,17.5), xlim=log(c(10,10000)), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)

    mod <- lm(nuts[[i]][,1]~log(nuts[[i]][,2]))
    abline(mod, col=2)

    box()

    axis(2, at=seq(0, 50, 2.5), font=2)
    axis(1, at=log(c(1,10,100,1000,10000)), labels=c(1,10,100,1000,10000), font=2)

    mtext("mean building height (m)",  2, line=2, font=2)
    mtext("population density (cap/km²)", 1, line=2, font=2)

    legend("topleft", legend=
    sprintf("y = %.2f + %.2f log(x) | Rsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
    col="red", lwd=1, inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.col="red")
    legend("bottomright", legend=
    sprintf("NUTS-%d", i), inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.font=3)

  }

dev.off()

#########################################################################

ftif <- "D:/Dropbox/paper/3D/plots-sample6/area-vs-popdens.tif"


tiff(ftif, width = 18.1, height = 6, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")

layout(matrix(1:3, 1, 3))

par(mai=c(0.4, 0.4, 0.15, 0.15), cex=1)

  for (i in 1:3){

    plot(log(nuts[[i]][,2]), log(nuts[[i]][,3]), pch=19, ylim=log(c(50,1000)), xlim=log(c(10,10000)), xaxs="i", yaxs="i", xlab="", ylab="", axes=FALSE)

    mod <- lm(log(nuts[[i]][,3])~log(nuts[[i]][,2]))
    abline(mod, col=2)

    box()

    axis(2, at=log(c(5,50,100,250,500,1000)), labels=c(5,50,100,250,500,1000), font=2)
    axis(1, at=log(c(1,10,100,1000,10000)), labels=c(1,10,100,1000,10000), font=2)

    mtext("building area per capita (m²/cap)",  2, line=2, font=2)
    mtext("population density (cap/km²)", 1, line=2, font=2)

    legend("topleft", legend=
    sprintf("log(y) = %.2f + %.2f log(x)\nRsq = %.2f", coef(mod)[1], coef(mod)[2], summary(mod)$r.squared),
    col="red", lwd=1, inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.col="red")
    legend("bottomright", legend=
    sprintf("NUTS-%d", i), inset=c(0.01,0.01), bty="n", cex=0.8, xpd=TRUE, text.font=3)

  }

dev.off()


