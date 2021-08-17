dat <- read.table("J:/germany-height/sample6-berlin/features-cal_rf-subset.txt", header=FALSE)
nf <- dim(dat)[2]

prm <- readLines("J:/germany-height/sample6-berlin/rf-feature-reduction/feature-prm.txt")
fid <- as.integer(gsub(".* ", "", prm))
# <  209 OPTICAL
# >= 209 RADAR

train0 <- readLines("J:/germany-height/sample6-berlin/svr-feature-reduction-random-s1only/train-svr-main.prm")

dout <- "J:/germany-height/sample6-berlin/svr-feature-reduction-random-s1only"

lab_ <- c("all", "opt", "rad")
features_ <- list(1:nf, which(fid<209), which(fid>=209))


for (i in 1:3){

  train <- gsub("FEATUREFILE", sprintf("%s_features-calval_svr-subset.txt", lab_[i]), train0)
  train <- gsub("MODELFILE", sprintf("%s_model-svr-calval.xml", lab_[i]), train)

  write(features_[[i]], sprintf("%s/%s_feature-id.txt", dout, lab_[i]), ncolumns=1)
  write(prm[features_[[i]]], sprintf("%s/%s_feature-prm.txt", dout, lab_[i]), ncolumns=1)
  write(train, sprintf("%s/%s_train-svr-subset.prm", dout, lab_[i]), ncolumns=1)
  write.table(dat[,features_[[i]]], sprintf("%s/%s_features-calval_svr-subset.txt", dout, lab_[i]), row.names=FALSE, col.names=FALSE, sep=" ")

}

