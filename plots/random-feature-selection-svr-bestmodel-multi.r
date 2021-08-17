d <- "J:/germany-height/sample/berlin/svr-feature-reduction-random"
dplot <- "D:/Dropbox/paper/3D/plots-sample6"

full <- "J:/germany-height/sample/berlin/full-model/all_model.log"

f <- dir(d, "^50.*log", full.names=TRUE)
n <- length(f)

logs <- as.list(rep(NA, n))

for (i in 1:n) logs[[i]] <- tail(readLines(f[i]))
  
rmse <- as.numeric(gsub(".* ", "", sapply(logs, grep, pattern="RMSE", value=TRUE)))
rsq <- as.numeric(gsub(".* ", "", sapply(logs, grep, pattern="Rsq", value=TRUE)))
slope <- as.numeric(gsub(".* ", "", gsub(" x.*", "", sapply(logs, grep, pattern="Rsq", value=TRUE))))
offset <- as.numeric(gsub(" .*", "", gsub("y = ", "", sapply(logs, grep, pattern="Rsq", value=TRUE))))


logfull <- tail(readLines(full))
rmsefull <- as.numeric(gsub(".* ", "", grep(logfull, pattern="RMSE", value=TRUE)))
rsqfull <- as.numeric(gsub(".* ", "", grep(logfull, pattern="Rsq", value=TRUE)))
slopefull <- as.numeric(gsub(".* ", "", gsub(" x.*", "", grep(logfull, pattern="Rsq", value=TRUE))))
offsetfull <- as.numeric(gsub(" .*", "", gsub("y = ", "", grep(logfull, pattern="Rsq", value=TRUE))))


plot(rmse)
plot(rsq)
plot(slope)
plot(offset)


range(offset, na.rm=TRUE)
range(slope,  na.rm=TRUE)
range(rsq,  na.rm=TRUE)
range(rmse,  na.rm=TRUE)

fname <- f[which.max(slope)]

ftif <- file.path(dplot, "random-feature-selection-svr-bestmodel-multi.tif")

tiff(ftif, width = 18.1, height = 9, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")

  layout(matrix(c(rep(1:4,each=3), rep(5:7, each=4), rep(5:7, each=4)), 3, 12, byrow=TRUE))
  par(mai=c(0.1, 0.4, 0.2, 0.2), cex=1)

{

  boxplot(offset, ylim=c(4,7), yaxs="i", axes=FALSE, xlab="", ylab="")
  abline(h=offsetfull, col=2)
  axis(2, font=2)
  mtext("offset",  side=2, line=2, font=2)
  box(bty="l")
  points(1, offset[which.max(slope)], cex=0.8, pch=19)
  points(1, offset[which.max(slope)], cex=1.8, pch=21)
  lab_upperleft(1)
  
  boxplot(slope, ylim=c(0.5,0.7), yaxs="i", axes=FALSE, xlab="", ylab="")
  abline(h=slopefull, col=2)
  axis(2, font=2)
  mtext("slope",  side=2, line=2, font=2)
  box(bty="l")
  points(1, slope[which.max(slope)], cex=0.8, pch=19)
  points(1, slope[which.max(slope)], cex=1.8, pch=21)
  lab_upperleft(2)
  
  boxplot(rsq, ylim=c(0.55,0.75), yaxs="i", axes=FALSE, xlab="", ylab="")
  abline(h=rsqfull, col=2)
  axis(2, font=2)
  mtext("Rsq",  side=2, line=2, font=2)
  box(bty="l")
  points(1, rsq[which.max(slope)], cex=0.8, pch=19)
  points(1, rsq[which.max(slope)], cex=1.8, pch=21)
  lab_upperleft(3)
  
  boxplot(rmse, ylim=c(5,8), yyaxs="i", axes=FALSE, xlab="", ylab="")
  abline(h=rmsefull, col=2)
  axis(2, font=2)
  mtext("RMSE",  side=2, line=2, font=2)
  box(bty="l")
  points(1, rmse[which.max(slope)], cex=0.8, pch=19)
  points(1, rmse[which.max(slope)], cex=1.8, pch=21)
  lab_upperleft(4)


}


par(mai=c(0.65, 0.4, 0.2, 0.2), cex=1)

{
#stm <- c("MIN", "Q10", "Q25", "Q50", "Q75", "Q90", "MAX", "AVG", "STD", "RNG", "IQR", "SKW", "KRT")
#index <- c("BLU", "BNR", "GRN", "MNW", "NDB", "NDV", "NIR", "RE1", "RE2", "RE3", "RED", "SW1", "SW2", "TCB", "TCG", "TCW", "BVH", "BVV")

stm <- c("A_000%", "B_010%", "C_025%", "D_050%", "E_075%", "F_090%", "G_100%", "H_Average", "I_Std. dev.", "J_Range", "K_IQR", "L_Skewness", "M_Kurtosis")

index <- c("Blue", "NIR2", "Green", "mNDWI", "NDBI", "NDVI", "NIR", "RE1", "RE2", "RE3", "RED", "SWIR1", "SWIR2", "TC Bright", "TC Green", "TC Wet", "VH", "VV")
index <- c("A_Blue", "H_NIR2", "B_Green", "Q_mNDWI", "R_NDBI", "P_NDVI", "G_NIR", "D_RE1", "E_RE2", "F_RE3", "C_RED", "I_SWIR1", "J_SWIR2", "M_TC Bright", "N_TC Green", "O_TC Wet", "L_VH", "K_VV")

txt_short <- c("BHT", "OPN", "ERO", "CLS", "THT", "GRD", "DIL")
txt_long  <- c("Blackhat", "Opening", "Erosion", "Closing", "Tophat", "Gradient", "Dilation")
txt_long <- c("F_Blackhat", "D_Opening", "A_Erosion", "C_Closing", "G_Tophat", "E_Gradient", "B_Dilation")






stm_ <- rep(stm, length(index))
index_ <- rep(index, each=13)


f <- dir(d, "^50.*_feature-prm.txt", full.names=TRUE)
features <- readLines(f[which.max(slope)])

txt_sel <- substr(gsub(".* = ", "", features), 9, 11)
id <- as.integer(gsub(".* ", "", features))

for (i in 1:length(txt_sel)) txt_sel[i] <- txt_long[which(txt_short == txt_sel[i])]


stm_sel <- stm_[id]
index_sel <- index_[id]

tab_index <- rep(0, length(index))
for (i in 1:length(index)) tab_index[i] <- sum(index_sel == sort(index)[i])
names(tab_index) <- sort(index)

tab_stm <- rep(0, length(stm))
for (i in 1:length(stm)) tab_stm[i] <- sum(stm_sel == sort(stm)[i])
names(tab_stm) <- sort(stm)

tab_txt <- rep(0, length(txt_long))
for (i in 1:length(txt_long)) tab_txt[i] <- sum(txt_sel == sort(txt_long)[i])
names(tab_txt) <- sort(txt_long)



stm_cols <- c(
"#FFED6F", "#FFFF99", "#E78AC3", "#FDB462", "#33A02C", "#66A61E", "#FDC086",
"#7FC97F", "#FFF2AE", "#F0027F", "#8DA0CB", "#DECBE4", "#B3B3B3")
index_cols <- c(
"#666666", "#DECBE4", "#E5C494", "#FDB462", "#F1E2CC", "#F781BF", "#B15928",
"#CCEBC5", "#1F78B4", "#8DD3C7", "#B3B3B3", "#FBB4AE", "#F4CAE4", "#E41A1C",
"#FFF2AE", "#FDC086", "#FF7F00", "#FDCDAC")
txt_cols <- c("#FDB462", "#FB8072", "#386CB0", "#CCEBC5", "#E6AB02", "#FFFF99", "#BC80BD")


#ftif <- file.path(dplot, "random-feature-selection-svr-numbers.tif")


  for (i in 1:3){

    if (i == 1){ what <- tab_index; cols <- index_cols }
    if (i == 2){ what <- tab_stm  ; cols <- stm_cols   }
    if (i == 3){ what <- tab_txt  ; cols <- txt_cols   }

    barplot(what, col=cols[as.factor(names(what))], ylim=c(0,20), font=2, las=3, cex.names=0.9, names.arg=gsub("^.*_", "", names(what)))
    mtext("# of selected features", 2, line=2, font=2)
    lab_upperleft(i+4)
  }

}

dev.off()



#write.table(data.frame(index=index_sel, stm=stm_sel, txt=txt_sel), file.path(dirname(fname), "selected-features.txt"), quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)

