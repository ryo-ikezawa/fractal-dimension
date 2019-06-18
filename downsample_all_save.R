library(tuneR)
library(fractaldim)
library(snowfall) #並列計算ライブラリ
library(pcaPP) #pcaPPライブラリ
library(wavelets) #waveletライブラリ


#main_file_name <- "/media/hem/BC04FC6004FC1ED8/Takasago/20190220/ch1/20190220_144135/B4-01_20190220_144135/Wave/WR_B4-01_20190220_144134_" #insert file path
main_file_name <- "../ch1/20190523_120720/B4-01_20190523_120720/Wave/WR_B4-01_20190523_120724_"
#main_file_name <- ""
file_number <- 30 #insert file number
fractal_N <- 4000 #insert number of data for calculating one fractal dimension

for (n in 1:46) {
  downsample_step <- n
  

for (m in 21:file_number) {
  print(m)
  if (m<10) {
    file_name <- paste(main_file_name,"000",m,".wav",sep="")
  }else{
    if (m<100) {
      file_name <- paste(main_file_name,"00",m,".wav",sep="")
    }else{
      file_name <- paste(main_file_name,"0",m,".wav",sep="")
    }
  }
  
  base <- readWave(file_name)
  basewave <- base@left
  basewave_number <- length(basewave)
  downsample_limit <- floor(basewave_number/downsample_step)
  downsample <- matrix(,1,downsample_limit)
  for (j in 1:downsample_limit) {
    samplepoint <- downsample_step*(j-1)+1
    downsample[1,j] <- basewave[samplepoint]
  }
  downsample_number <- length(downsample)
  downsample_f <- 51200/downsample_step
  fractal_M <- floor(downsample_number/fractal_N)
  
  cal_matrix <- matrix(downsample,fractal_N,fractal_M)
  cal_fd <- apply(cal_matrix,2,fd.estim.madogram)
  collect_fd <- matrix(,1,fractal_M)
  for (k in 1:fractal_M) {
    collect_fd[1,k] <- cal_fd[[k]][["fd"]]
  }
  
  if(m==21){
    fd_downsample <- c(m,mean(collect_fd),sd(collect_fd))
  }else{
    fd_downsample <- rbind(fd_downsample,c(m,mean(collect_fd),sd(collect_fd)))
  }
}
}

