# Load files
# Set path
pth <- "data_pilot/"
fl <- list.files(pth)
dat <- data.frame()
# Loop over files in directory and bind by row, adding filename as id
for (f in fl) {
    tmp <- read.csv(paste(pth, f, sep=""), stringsAsFactors = F)
    tmp <- data.frame(cbind(id=f, tmp))
    dat <- rbind(dat, tmp)
}