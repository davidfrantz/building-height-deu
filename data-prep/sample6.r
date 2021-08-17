site <- "nrw"
tile_site <- "DE_NW"

finp <- dir(sprintf("/data/Jakku/germany-height/3d-extract/tables/%s", site), ".txt", full.names=TRUE)
ninp <- length(finp)

tab <- as.list(rep(NA, ninp))
 
for (i in 1:ninp){
  
  tab[[i]] <- read.table(finp[i], header=FALSE)
  cat("done with", i, "of", ninp, "\n")
}

x <- unlist(sapply(tab, function(x)x[,1]))
y <- unlist(sapply(tab, function(x)x[,2]))
z <- unlist(sapply(tab, function(x)x[,3]))
a <- unlist(sapply(tab, function(x)x[,4]))
f <- unlist(sapply(tab, function(x)x[,5]))

f <- as.character(f)

xr <- range(x)
yr <- range(y)

sum(a)
sum(a*z)



require(sp)
require(rgdal)
require(raster)

proj4 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
map <- project(cbind(x,y), proj4, inv=FALSE)
x_map <- map[,1]
y_map <- map[,2]

ulx <- 2456026.250000
uly <- 4574919.500000
size <- 30000.000000

tiles <- readLines(sprintf("/data/Jakku/mat_stocks/tiles/%s.txt", tile_site))
nt <- length(tiles)
tx <- as.integer(substr(tiles, 2, 5))
ty <- as.integer(substr(tiles, 8, 11))

# one-time area-weighted average height generation
if (0 > 1){
for (t in 1:nt){

  xmin <- ulx + tx[t]*size
  xmax <- ulx + tx[t]*size + size
  ymax <- uly - ty[t]*size
  ymin <- uly - ty[t]*size - size
  
  pos <- which(x_map >= xmin & x_map < xmax & y_map >= ymin & y_map < ymax)
  if (length(pos) == 0) next
  
  sub_x     <- x[pos]
  sub_y     <- y[pos]
  sub_x_map <- x_map[pos]
  sub_y_map <- y_map[pos]
  sub_z     <- z[pos]
  sub_a     <- a[pos]
  sub_f     <- f[pos]

  fmask <- sprintf("/data/Jakku/mat_stocks/mask/DEU/%s/DEU.tif", tiles[t])
  state <- raster(fmask)[]

  z_avg <- matrix(NA, 3000, 3000)
  a_sum <- matrix(NA, 3000, 3000)
  v_avg <- matrix(NA, 3000, 3000)

  for (i in 0:2999){
  
    cy_map <- ymax - i*10 - 5
    near <- which(abs(cy_map-sub_y_map) <= 50)
    if (length(near) == 0) next 
    sub_sub_x     <- sub_x[near]
    sub_sub_y     <- sub_y[near]
    sub_sub_x_map <- sub_x_map[near]
    sub_sub_y_map <- sub_y_map[near]
    sub_sub_z     <- sub_z[near]
    sub_sub_a     <- sub_a[near]
    sub_sub_f     <- sub_f[near]

  for (j in 0:2999){

    p <- i*3000+j +1
    if (!state[p]) next
    
    cx_map <- xmin + j*10 + 5
    cy_map <- ymax - i*10 - 5
    
    dist <- sqrt((cx_map-sub_sub_x_map)^2 + (cy_map-sub_sub_y_map)^2)
    near <- which(dist <= 50)
    if (length(near) == 0){ 
      z_avg[i+1,j+1] <- 0; 
      next
    }
    if (min(dist) > 5){ 
      z_avg[i+1,j+1] <- 0; 
      next
    }

    z_avg[i+1,j+1] <- weighted.mean(sub_sub_z[near], sub_sub_a[near])
    a_sum[i+1,j+1] <- sum(sub_sub_a[near])
    v_avg[i+1,j+1] <- z_avg[i+1,j+1]*a_sum[i+1,j+1]

  }
  cat("done with", t, i, "\n")
  }

  tmp_z <- raster(fmask)
  tmp_z[] <- z_avg

  tmp_a <- raster(fmask)
  tmp_a[] <- a_sum

  tmp_v <- raster(fmask)
  tmp_v[] <- v_avg

  dir.create(sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s", site, tiles[t]))
  writeRaster(tmp_z, sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s/3D-HEIGHT.tif", site, tiles[t]), format="GTiff", overwrite=TRUE)
  writeRaster(tmp_a, sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s/3D-AREA.tif",   site, tiles[t]), format="GTiff", overwrite=TRUE)
  writeRaster(tmp_v, sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s/3D-VOLUME.tif", site, tiles[t]), format="GTiff", overwrite=TRUE)
  

}
}



x <- as.list(rep(NA, nt))
y <- as.list(rep(NA, nt))
z <- as.list(rep(NA, nt))
a <- as.list(rep(NA, nt))
v <- as.list(rep(NA, nt))

for (t in 1:nt){

  if (!dir.exists(sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s", site, tiles[t]))) next

  z_avg <- raster(sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s/3D-HEIGHT.tif", site, tiles[t]))[]
#  a_sum <- raster(sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s/3D-AREA.tif",   site, tiles[t]))[]
#  v_avg <- raster(sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/%s/3D-VOLUME.tif", site, tiles[t]))[]
  z_avg <- matrix(z_avg, 3000, 3000)
#  a_sum <- matrix(a_sum, 3000, 3000)
#  v_avg <- matrix(v_avg, 3000, 3000)

  yx_ind <- which(!is.na(z_avg) & z_avg > 0, arr.ind=TRUE)

  xmin <- ulx + tx[t]*size
  ymax <- uly - ty[t]*size

  x[[t]] <- xmin + (yx_ind[,1]-1)*10 + 5
  y[[t]] <- ymax - (yx_ind[,2]-1)*10 - 5
  z[[t]] <- z_avg[yx_ind]
#  a[[t]] <- a_sum[yx_ind]
#  v[[t]] <- v_avg[yx_ind]
  cat("done with", t, "\n")
}  

x <- unlist(x)
y <- unlist(y)
z <- unlist(z)
#a <- unlist(a)
#v <- unlist(v)

if (sum(is.na(x)) > 0){ 
  del <- which(is.na(x))
  x <- x[-del]
  y <- y[-del]
  z <- z[-del]
}


#write.table(cbind(x,y,z,a,v), sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/full-sample.txt", site), 
#  col.names=c("X", "Y", "Z", "A", "V"), row.names=FALSE, quote=FALSE, sep=" ")
write.table(cbind(x,y,z), sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/full-sample-height.txt", site), 
  col.names=c("X", "Y", "Z"), row.names=FALSE, quote=FALSE, sep=" ")
#write.table(cbind(x,y,a), sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/full-sample-area.txt", site), 
#  col.names=c("X", "Y", "A"), row.names=FALSE, quote=FALSE, sep=" ")
#write.table(cbind(x,y,v), sprintf("/data/Jakku/germany-height/3d-extract/maps/%s/full-sample-volume.txt", site), 
#  col.names=c("X", "Y", "V"), row.names=FALSE, quote=FALSE, sep=" ")





xr <- range(x)
yr <- range(y)

mindist <- 50
nwanted <- 1000
  
sample <- numeric(0)


# from here

heights <- unique(c(t(cbind(50:3, 3:50)[1:24,])))
#heights <- 3:50


for (i in heights){
  
  
  if (i < 50){ 
    sub <- which(z < i & z >= i-1)
  } else {
    sub <- which(z >= i)
  }
  n_ <- length(sub)
  x_ <- x[sub]
  y_ <- y[sub]
  z_ <- z[sub]
  v_ <- rep(TRUE, n_)
  

  k <- 1
  iter <- 1
  

  ntodo <- min(n_, nwanted)
#print(ntodo); print(nwanted)
  while (k < ntodo & iter < 1e5 & sum(v_) > 0){
#print(k); print(iter)
    if (ntodo < nwanted){
      p <- iter
    } else {
      p <- round(runif(1, 1, n_))
    }
    if (!v_[p]) next

    if (length(sample)>0){
      dist <- sqrt((sample[,1]-x_[p])^2 + (sample[,2]-y_[p])^2)
    } else {
      dist <- mindist+1
    }

    if (min(dist) > mindist){
      sample <- rbind(sample, c(x_[p], y_[p], z_[p]))
      k <- k+1
    }

    v_[p] <- FALSE

    iter <- iter +1
    
  }

  par(bg="black", fg="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
  col <- rev(heat.colors(25))[pmin(25,round(sample[,3]))]
  plot(sample[,1], sample[,2], col=col, pch=19, cex=0.1, xlab="Longitude", ylab="Latitude", main=sprintf("sample %s", site), font=2, font.lab=2, xlim=xr, ylim=yr)

  cat("done with", i, ", added", k, ", iter:", iter, "\n"); flush.console()

}

N2 <- dim(sample)[1]

# up to here
N <- dim(sample)[1]

# do again for val sample

# background sample


x_ <- seq(min(x),max(x), 50)
y_ <- seq(min(y),max(y), 50)


#xy_ <- imgPOISSDISC(length(y_), length(x_), 3, 30, 3, plot=FALSE)
xy_ <- imgPOISSDISC(length(y_), length(x_), 30, 300, 3, plot=FALSE)
xy_ <- cbind(x_[xy_[,2]],y_[xy_[,1]])
plot(xy_, pch=19, cex=0.2)

x_ <- as.list(rep(NA, nt))
y_ <- as.list(rep(NA, nt))

for (t in 1:nt){

  if (!file.exists(sprintf("/data/Jakku/mat_stocks/mask/DEU/%s/%s.tif", tiles[t], tile_site))) next

  state <- raster(sprintf("/data/Jakku/mat_stocks/mask/DEU/%s/%s.tif", tiles[t], tile_site))[]
  state <- matrix(state, 3000, 3000)

  yx_ind <- which(!is.na(state) & state == 1, arr.ind=TRUE)

  xmin <- ulx + tx[t]*size
  xmax <- ulx + tx[t]*size + size
  ymax <- uly - ty[t]*size
  ymin <- uly - ty[t]*size - size

  x_state <- xmin + (yx_ind[,1]-1)*10 + 5
  y_state <- ymax - (yx_ind[,2]-1)*10 - 5
  
  pos <- which(xy_[,1] >= xmin & xy_[,1] < xmax & xy_[,2] >= ymin & xy_[,2] < ymax)
  if (length(pos) == 0) next
  
  xy_sub <- xy_[pos,]
  
  del <- rep(0, dim(xy_sub)[1])

  if (length(x_state) < 9000000){
    for (s in 1:dim(xy_sub)[1]){
      dist <- sqrt((xy_sub[s,1]-x_state)^2 + (xy_sub[s,2]-y_state)^2)
      if (min(dist) > 10) del[s] <- 1
      #cat("done with", s, "\n")
    }
  }

  if (sum(del) > 0) xy_sub <- xy_sub[-which(del==1),]
  if (length(xy_sub) == 0){
    next
  } else if (length(xy_sub) == 2){
    x_[[t]] <- xy_sub[1]
    y_[[t]] <- xy_sub[2]

  } else {
    x_[[t]] <- xy_sub[,1]
    y_[[t]] <- xy_sub[,2]
  }
  
  plot(unlist(x_), unlist(y_), pch=19, cex=0.2)
  
  cat("done with", t, "\n")
}

x_ <- unlist(x_)
y_ <- unlist(y_)


if (sum(is.na(x_)) > 0){ 
  del <- which(is.na(x_))
  x_ <- x_[-del]
  y_ <- y_[-del]
}





#xy_ <- expand.grid(x_, y_)

mindist <- 100 # ~ 100m
#maxdist <- 1000 # ~ 1000m

bgsample <- matrix(NA, length(x_), 3)

for (i in 1:length(x_)){

  dist <- sqrt((x-x_[i])^2 + (y-y_[i])^2)
  if (min(dist) > mindist){ # & min(dist) < maxdist){
    bgsample[i,] <- c(x_[i], y_[i], 0)
    points(x_[i], y_[i],  col="white", pch=19, cex=0.25)
  } else points(x_[i], y_[i],  col="cyan", pch=19, cex=0.25)

}

del <- which(is.na(bgsample[,1]))
if (length(del)>0) bgsample <- bgsample[-del,]


  par(bg="black", fg="white", col.axis="white", col.lab="white", col.main="white", col.sub="white")
  col <- rev(heat.colors(25))[pmin(25,round(sample[,3]))]
  plot(sample[,1], sample[,2], col=col, pch=19, cex=0.1, xlab="Longitude", ylab="Latitude", main=sprintf("sample %s", site), font=2, font.lab=2, xlim=xr, ylim=yr)
  points(bgsample[,1], bgsample[,2],  col="green", pch=19, cex=0.2)


hist(sample[,3], breaks=seq(0,330,5))

write.table(rbind(sample,bgsample), sprintf("/data/Jakku/germany-height/sample/%s/sample2-cal.txt", site), sep=" ", col.names=FALSE, row.names=FALSE)

