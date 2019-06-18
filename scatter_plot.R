library(tuneR) #waveライブラリ
library(fractaldim) #フラクタmotiル次元ライブラリ
library(snowfall) #並列計算ライブラリ
library(pcaPP) #pcaPPライブラリ
library(wavelets) #waveletライブラリ/

readWaveFile <- function(start, end, dirName) {
  for(i in start:end) {
    print(paste("file number", i))
    waveFileName <- paste(dirName, i,".wav",sep = "")
    baseWave <- readWave(filename = waveFileName)
    if(i==start) {
      base <- baseWave@left
    } else {
      base <- c(base, baseWave@left)
    }
  }
  rm(baseWave)
  return (base)
}

#reading wavefile and choice ch1
#print("read wave file and choice ch1")
# save csv

calculateFdForPlot <- function(start, end, dirName, loopNumber) {
  base <- readWaveFile(start, end, dirName)
  for (i in 1:loopNumber) {
    print(i)
#    sample_rate <- 51.2*1000/11
    matrix <- matrix(,1,15360000)
#    for (j in 1:15360000) {
#      samplepoint <- 11*(j-1)+1+15360000*(i-1)
#      downsample[1,j] <- base[samplepoint]
#    }
#    matrix <- matrix(downsample,4000,3840)
    start <- (i-1)*15360000+1
    stop <- i*15360000
    matrix <- matrix(base[start:stop],4000,3840)
#    cal <- apply(matrix,2,fd.estim.madogram)
    cal <- apply(matrix,2,mean)
    fd <- matrix(,1,3840)
    for (j in 1:3840) {
#      fd[1,j] <- cal[[j]][["fd"]]
      fd[1,j] <- cal[j]
    } #用意した配列にフラクタル次元データを書き出し
    if(i==1){
      fd_plot <- var(fd)
    }else{
      fd_plot <- c(fd_plot,var(fd))
    }
  }
  return (fd_plot)
}


for (type in 1:4) {
  print(paste("Now type is", type))
  if(type==1) {
    ch <- 1
    day <- 0220
    dir <- "../../20190220/ch1/20190220_144135/B4-01_20190220_144135/Wave/WR_B4-01_20190220_144134_00"
    x1 <- calculateFdForPlot(51, 53, dir, 36)
  } else if (type==2) {
    ch <- 2
    day <- 0220
    dir <- "../../20190220/ch2/20190220_144140/B4-01_20190220_144140/Wave/WR_B4-01_20190220_144139_00"
    y1 <- calculateFdForPlot(51, 53, dir, 36)    
  } else if (type==3) {
    ch <- 1
    day <- 0523
    dir <- "../ch1/20190523_120720/B4-01_20190523_120720/Wave/WR_B4-01_20190523_120724_00"
    x2 <- calculateFdForPlot(51, 53, dir, 36)
  } else if (type==4) {
    ch <- 2
    day <- 0523
    dir <- "../ch2/20190523_120626/B4-01_20190523_120626/Wave/WR_B4-01_20190523_120621_00"
    y2 <- calculateFdForPlot(51, 53, dir, 36)
  }
}
  #wavefilename <- paste("../ch1/20190523_120720/B4-01_20190523_120720/Wave/WR_B4-01_20190523_120724_0",i,".wav",sep = "")
  #wavefilename <- paste("../ch2/20190523_120626/B4-01_20190523_120626/Wave/WR_B4-01_20190523_120621_0",i,".wav",sep = "")
  #wavefilename <- paste("../../20190220/ch1/20190220_144135/B4-01_20190220_144135/Wave/WR_B4-01_20190220_144134_0",i,".wav",sep = "")
  #wavefilename <- paste("../../20190220/ch2/20190220_144140/B4-01_20190220_144140/Wave/WR_B4-01_20190220_144139_0", i,".wav",sep = "")
# base <- readWaveFile(121,121, 0220, 1, "../ch1/20190523_120720/B4-01_20190523_120720/Wave/WR_B4-01_20190523_120724_0")

#51.2kHz fractal dim / 5min
print("calculate Fractal-Dim @51.2kHz")
#for (i in 1:12) {
#  print(i)
#  start <- (i-1)*15360000+1
#  stop <- i*15360000
#  matrix <- matrix(base[start:stop],4000,3840)
#  cal <- apply(matrix,2,fd.estim.madogram)
#  fd <- matrix(,1,3840)
#  for (j in 1:3840) {
#    fd[1,j] <- cal[[j]][["fd"]]
#  } #用意した配列にフラクタル次元データを書き出し
#  
#  if(i==1){
#    fd_plot <- fd
#  }else{
#    fd_plot <- c(fd_plot,fd)
#  }
#}
#}
#write.csv(fd_mean, "output.csv")
#png("fd_51.2kHz_x56.png")#plot
#plot(fd_mean[,1],fd_mean[,2],col='red',pch=20,ylim=c(1,2),main="Fractal-Dimension_madogram_51.2kHz_1000RPM",xlab="times(x300sec)",ylab="Fractal-Dimension")
#arrows(fd_mean[,1],fd_mean[,2],fd_mean[,1],fd_mean[,2]-fd_mean[,3],angle=90,length=0.02,lwd=1,col="red")
#arrows(fd_mean[,1],fd_mean[,2],fd_mean[,1],fd_mean[,2]+fd_mean[,3],angle=90,length=0.02,lwd=1,col="red")
#dev.off()

par(mai = c(0.85, 0.8, 0.34, 0.35))
plot(0, 0, type = "n", xlim = c(min(x1,x2), max(x1, x2)), ylim = c(min(y1,y2), max(y1, y2)),xlab = "FD-ch1", ylab = "FD-ch2")
points(x1, y1, col = "blue", pch = 16, cex=0.1)
points(x2, y2, col = "orange", pch = 16, cex=0.1)