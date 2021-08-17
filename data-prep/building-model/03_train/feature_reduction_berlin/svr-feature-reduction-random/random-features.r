dat <- read.table("J:/germany-height/sample6-berlin/features-cal_rf-subset.txt", header=FALSE)
nf <- dim(dat)[2]

prm <- readLines("J:/germany-height/sample6-berlin/rf-feature-reduction/feature-prm.txt")

train0 <- readLines("J:/germany-height/sample6-berlin/svr-feature-reduction-random/train-svr-main.prm")


for (nf_new in seq(55,75,5)){

  niter <- 100

  dout <- "J:/germany-height/sample6-berlin/svr-feature-reduction-random"

  for (i in 1:niter){

    features <- round(runif(nf_new, 1, nf))
    
    train <- gsub("FEATUREFILE", sprintf("%02d_%03d_features-calval_svr-subset.txt", nf_new, i), train0)
    train <- gsub("MODELFILE", sprintf("%02d_%03d_model-svr-calval.xml", nf_new, i), train)

    write(features, sprintf("%s/%02d_%03d_feature-id.txt", dout, nf_new, i), ncolumns=1)
    write(prm[features], sprintf("%s/%02d_%03d_feature-prm.txt", dout, nf_new, i), ncolumns=1)
    write(train, sprintf("%s/%02d_%03d_train-svr-subset.prm", dout, nf_new, i), ncolumns=1)
    write.table(dat[,features], sprintf("%s/%02d_%03d_features-calval_svr-subset.txt", dout, nf_new, i), row.names=FALSE, col.names=FALSE, sep=" ")

  }

}

