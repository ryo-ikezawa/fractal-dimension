library(tuneR) #waveライブラリ
library(fractaldim) #フラクタmotiル次元ライブラリ
library(snowfall) #並列計算ライブラリ
library(pcaPP) #pcaPPライブラリ
library(wavelets) #waveletライブラリ/

#basewave <- readWave("/media/hem/BC04FC6004FC1ED8/Takasago/20190213-20160220_1st_test/ch1/20190213_134214/B4-01_20190213_134214/Wave/WR_B4-01_20190213_134213_0011.wav")
#basewave <- readWave("/media/hem/BC04FC6004FC1ED8/Takasago/20190213-20160220_1st_test/ch2/20190213_132743/B4-01_20190213_132743/Wave/WR_B4-01_20190213_132742_0011.wav")
basewave <- readWave("../ch1/20190523_120720/B4-01_20190523_120720/Wave/WR_B4-01_20190523_120724_0121.wav")
fft_basewave <- basewave@left[1:51200]
#fft_basewave <- filtfilt(fir_filter,basewave@left[1:51200])
sampling <-  51200
n <- 0:(sampling-1)
samplefreq <- 51200
t <- n/samplefreq
f <- n*samplefreq/sampling
spec <- abs(fft(fft_basewave))^2
plot(f,spec,type="h",xlim=c(1,25600))
