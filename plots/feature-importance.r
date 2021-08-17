fname <- "J:/germany-height/sample/berlin/rf-feature-reduction/model-rfr-cal.log"
dplot <- "D:/Dropbox/paper/3D/plots-sample6"

log <- readLines(fname)

vlog <- grep("Variable", log, value=TRUE)[-1]

v  <- as.integer(gsub(".* +([^ ]*):.*", "\\1", vlog))
vi <- as.numeric(gsub(".*: ", "", vlog))


newv <- v[1:(length(v)/2)]

stm <- c("MIN", "Q10", "Q25", "Q50", "Q75", "Q90", "MAX", "AVG", "STD", "RNG", "IQR", "SKW", "KRT")
index <- c("BLU", "BNR", "GRN", "MNW", "NDB", "NDV", "NIR", "RE1", "RE2", "RE3", "RED", "SW1", "SW2", "TCB", "TCG", "TCW", "BVH", "BVV")
txt <- c("BHT", "OPN", "ERO", "CLS", "THT", "GRD", "DIL")

stm <- c("A_000%", "B_010%", "C_025%", "D_050%", "E_075%", "F_090%", "G_100%", "H_Average", "I_Std. dev.", "J_Range", "K_IQR", "L_Skewness", "M_Kurtosis")
index <- c("A_Blue", "H_NIR2", "B_Green", "Q_mNDWI", "R_NDBI", "P_NDVI", "G_NIR", "D_RE1", "E_RE2", "F_RE3", "C_RED", "I_SWIR1", "J_SWIR2", "M_TC Bright", "N_TC Green", "O_TC Wet", "L_VH", "K_VV")
txt <- c("F_Blackhat", "D_Opening", "A_Erosion", "C_Closing", "G_Tophat", "E_Gradient", "B_Dilation")

vi_ <- vi-vi
vi_[v+1] <- vi

stm_ <- rep(stm, length(index)*length(txt))
index_ <- rep(rep(index, each=13), length(txt))
txt_ <- rep(txt, each=length(stm)*length(index))

plot(vi_, type="h")
abline(v=seq(1, by=234, length.out=7))


N <- max(which(vi >= 0.001))
V <- v[1:N]+1
N

library(RColorBrewer)
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
stm_cols <- sample(col_vector, length(stm))

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
index_cols <- sample(col_vector, length(index))

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
txt_cols <- sample(col_vector, length(txt))
stm_cols <- c(
"#FFED6F", "#FFFF99", "#E78AC3", "#FDB462", "#33A02C", "#66A61E", "#FDC086",
"#7FC97F", "#FFF2AE", "#F0027F", "#8DA0CB", "#DECBE4", "#B3B3B3")
index_cols <- c(
"#666666", "#DECBE4", "#E5C494", "#FDB462", "#F1E2CC", "#F781BF", "#B15928",
"#CCEBC5", "#1F78B4", "#8DD3C7", "#B3B3B3", "#FBB4AE", "#F4CAE4", "#E41A1C",
"#FFF2AE", "#FDC086", "#FF7F00", "#FDCDAC")
txt_cols <- c("#FDB462", "#FB8072", "#386CB0", "#CCEBC5", "#E6AB02", "#FFFF99", "#BC80BD")


stm_sel <- stm_[V]
index_sel <- index_[V]
txt_sel <- txt_[V]

tab_index <- rep(0, length(index))
for (i in 1:length(index)) tab_index[i] <- sum(index_sel == sort(index)[i])
names(tab_index) <- sort(index)

tab_stm <- rep(0, length(stm))
for (i in 1:length(stm)) tab_stm[i] <- sum(stm_sel == sort(stm)[i])
names(tab_stm) <- sort(stm)

tab_txt <- rep(0, length(txt))
for (i in 1:length(txt)) tab_txt[i] <- sum(txt_sel == sort(txt)[i])
names(tab_txt) <- sort(txt)


#layout(matrix(c(1:3, 4:6, rep(7, 3)), 3, 3, byrow=TRUE))

ftif <- file.path(dplot, "feature-importance.tif")

tiff(ftif, width = 18.1, height = 10, units = "cm", pointsize = 8,
  compression="lzw", res=600, type="cairo", antialias="subpixel")
{
  layout(matrix(c(rep(1:3,4),rep(7,3),rep(4:6,4)), 9, 3, byrow=TRUE))

  par(mai=c(0.2, 0.4, 0.2, 0.15), cex=1)

  for (i in 1:3){

    if (i == 1){ what <- index_; cols <- index_cols }
    if (i == 2){ what <- stm_  ; cols <- stm_cols   }
    if (i == 3){ what <- txt_  ; cols <- txt_cols   }

    boxplot(split(vi_, what), col=cols[as.factor(names(split(vi_, what)))], ylim=c(0.00001,0.1), log="y", yaxs="i", axes=FALSE, xlab="", ylab="", pch="*")
    fig_upperleft()
    lab_upperleft(i, ylog=TRUE)
    axis(2, font=2)
    mtext(gsub("^.*_", "", names(split(vi_, what))), at=1:length(cols), side=1, line=2.75, font=2, las=3, cex=0.95, adj=0.5)
    mtext("feature importance (log)", 2, line=2, font=2)
    box()

  }


  #par(mai=c(0.2, 0.4, 0.4, 0.15), cex=1)

  for (i in 1:3){

    if (i == 1){ what <- tab_index; cols <- index_cols }
    if (i == 2){ what <- tab_stm  ; cols <- stm_cols   }
    if (i == 3){ what <- tab_txt  ; cols <- txt_cols   }

    barplot(what, col=cols[as.factor(names(what))], ylim=c(0,85), font=2, axisnames=FALSE)
    #barplot(table(what[V]), col=cols[as.factor(names(split(vi_, what)))], ylim=c(0,85), font=2, las=3, cex.names=0.95)
    #mtext(names(table(what[V])), at=1:length(cols), side=1, font=2, las=3)
    mtext("# of selected features", 2, line=2, font=2)
    lab_upperleft(i+3)
    
  }
}
dev.off()


write.table(data.frame(index=index_[v+1], stm=stm_[v+1], txt=txt_[v+1], vi=vi), file.path(dirname(fname), "feature-importance.txt"), quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)



for (i in 1:3){

  if (i == 1){ what <- index_; cols <- index_cols }
  if (i == 2){ what <- stm_  ; cols <- stm_cols   }
  if (i == 3){ what <- txt_  ; cols <- txt_cols   }

  plot(vi, xlim=c(0,N), pch=19, col=cols[as.factor(txt_[v])])

}


legend("topright", legend=txt, col=as.factor(txt), pch=19)
legend("top", legend=index, pch=as.factor(index))
legend("top", inset=c(0.5,0), legend=stm, pch=19, cex=as.factor(stm))



tab <- read.table("J:/germany-height/sample6-berlin/features-cal.txt", header=FALSE)
tab <- tab[,(v[1:N]+1)]
write.table(tab, "J:/germany-height/sample6-berlin/features-cal_rf-subset.txt", col.names=FALSE, row.names=FALSE, sep=" ")



features <- paste(rep(c(
"INPUT_FEATURE = TEXTURE_BHT.tif",
"INPUT_FEATURE = TEXTURE_OPN.tif",
"INPUT_FEATURE = TEXTURE_ERO.tif",
"INPUT_FEATURE = TEXTURE_CLS.tif",
"INPUT_FEATURE = TEXTURE_THT.tif",
"INPUT_FEATURE = TEXTURE_GRD.tif",
"INPUT_FEATURE = TEXTURE_DIL.tif"), each=234),
rep(1:234, 7))

#writeClipboard(features[v[1:N]+1])
write(features[v[1:N]+1], file.path(dirname(fname), "feature-prm.txt"))


