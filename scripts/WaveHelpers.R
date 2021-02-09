GenerateSignal = function(SamplingFreq,Duration,SineWaveFreq,Noise,Delay){
  #SamplingFrequency = samples per year
  #Duration in years
  #SineWaveFreq = c(1,0.1,0.08) would be 1 cycle per year, 0.1/y, 0.08/y
  #Noise = scalar standard deviation
  #Delay = scalar 0-1 as proportion of time series
  
  fs = SamplingFreq                    #Sampling frequency (samples per year)
  dt = 1/fs                  # seconds per sample
  StopTime = Duration             # seconds
  t = seq(0,StopTime-dt,dt)
  myNoise = rnorm(length(t),mean=0,sd=Noise)
  
  GeneratedSignal = matrix(NA,nrow=length(SineWaveFreq),ncol=length(t))
  
  # Cycle through the sine wave frequencies
  for (i in 1:length(SineWaveFreq)){
    # fs = SamplingFreq  = 12                  #Sampling frequency (samples per year)
    # dt = 1/fs                  # seconds per sample
    # StopTime = Duration             # seconds
    # t = seq(0,StopTime-dt,dt)
    F = SineWaveFreq[i]                      # Sine wave frequency (hertz)
    GeneratedSignal[i,] = sin(2*pi*F*t)
  }
  ST = seq(1,length(t),1)
  GeneratedSignalAll = data.frame(myTime=t,SeqTime=ST,mySignal=colSums(GeneratedSignal)+myNoise)
  # If Delay>0 then delay the signal
  if (Delay>0){
    iDelay = Delay * length(t)
    ZeroSignal = rep(0,iDelay)+myNoise[1:iDelay]
    iEnd = length(t)-iDelay
    GeneratedSignalAll$mySignal = c(ZeroSignal,GeneratedSignalAll$mySignal[1:iEnd])
  }

  plot(GeneratedSignalAll$myTime,GeneratedSignalAll$mySignal,type='l',xlab='Year',ylab='Generated Signal')
  abline(h=0,lty=2)
  
  return(GeneratedSignalAll)
}

