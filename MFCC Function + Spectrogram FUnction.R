## Next Step Sample 

# Normal, Angry, Excite, sad

library(wrassp)

library(readr)

library(tuneR)

library(signal)

library(oce)



# Normal


normal = 'normal.wav'

normal = readWave(normal)

# Structure of File

str(normal)





# Angry


angry = 'angry.wav'

angry = readWave(angry)

# Structure of File

str(angry)




# Excitement

exci = 'excitement.wav'

exci = readWave(exci)

# Structure of File

str(exci)



# Sad

sad = 'sad.wav'

sad = readWave(sad)

# Structure of File

str(sad)




#########################################################

# MFCC FUNCTION

func.mfcc = function(x) {
  sr1 = x@samp.rate
  
  mel.fc = melfcc(x, sr = sr1,
                  wintime = 0.015,        
                  hoptime = 0.005,       
                  # numcep = 2,            
                  sumpower = TRUE,       
                  nbands = 40,            
                  # bwidth = 1,            
                  preemph = 0.95,         
                  # frames_in_rows = TRUE
  )
  
}

mfcc.normal = func.mfcc(normal)

mfcc.angry = func.mfcc(angry)

mfcc.exci = func.mfcc(exci)

# Remove NaN  Row
mfcc.exci = mfcc.exci[-c(1,2,3),]

mfcc.sad = func.mfcc(sad)


###################################


## Plot Spectrogram of Mel Frequency


# Spectrogram parameters

nfft = 512

window = 1500

overlap = 500

spectrogram = function(a,b,c) {
  
  # Duration in sec
  dur = length(a / (b@samp.rate))
  
  # Fequency in Hz
  fs = b@samp.rate
  
  
  ## Plot Spectrogram
  spec = specgram(x = a,
                  n = nfft,
                  Fs = fs,
                  window = window,
                  overlap = overlap)
  
  P = abs(spec$S)
  
  # Normalize
  
  P = P/max(P)
  
  # Convert to dB
  P = 10*log10(P)
  
  # Config time axis
  t = spec$t
  
  
  # Plotting
  imagep(x = t,
         y = spec$f,
         z = t(P),
         main = c,
         col = oce.colorsViridis,
         ylab = 'Frequency [Hz]',
         xlab = 'Time [s]',
         drawPalette = T,
         decimate = F)
  
}


spec.normal = spectrogram(mfcc.normal, normal, 'Normal- mel Spectrogram')

spec.angry = spectrogram(mfcc.angry, angry,'Angry - mel Spectrogram')

spec.exci = spectrogram(mfcc.exci, exci, 'Excitement - mel Spectrogram')

spec.sad = spectrogram(mfcc.sad, sad, 'Sad - mel Spectrogram')






###########

# SOLUTION IS HERE:

# 1. We can limit the element size from all files and can get the same matrix

# e.g normal = normal[c(1: 2000000),]
# e.g angry = angry[c(1: 2000000), ] like wise all files.. 
# So that you'll not get un-even matrix and vector soze will fit datafram for prediction

# 2. Once we have same matrix for all audio files (MFCC), convert it into vectors
# 3. After converting to vectors combine the class file and tag accord to Id numbers.


## NOW QUESTIONS: TASK TO DO..

# Function from start to end.. staring from readwave() till MFCC completion.. 

# WHat if some file are 10 mins and some are 3 mins.. can't cut 10 min audio to 3 min 
# IN problem project some client calls can run for 10 mins some can end in 2 mins. 
# In that how can we use that file .. if that vector size will not be same and hence no data.frame on it for dectation classification. 


