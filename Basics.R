

library(wrassp)

library(readr)

library(tuneR)

library(signal)

library(oce)



# Load File

path = 'male.wav'

audio = readWave('male.wav')

# Structure of File

str(audio)

# play(audio)

## Plot Wave 

par(mfrow = c(2,1))

s1 = (audio@left) / 2^(audio@bit - 1)

timearray = (0:(408226-1))/audio@samp.rate

plot(timearray, s1, type = 'l', col = 'blue')

#OR

plot(audio@left[10:408226], type ='l', 
     col = 'seagreen', xlab = 'Elements / Time', ylab = 'Freq', main = 'Audio Frequency Wave')


########################################################################################
 # MFCC
 
 # https://www.rdocumentation.org/packages/tuneR/versions/1.3.3/topics/melfcc
 
 sr = audio@samp.rate
 
 
 mfcc.m = melfcc(audio, sr = sr,
              wintime = 0.015,        # STAGE 2 Window length
              hoptime = 0.005,        # STAGE 2 Successive windown inbetween
              # numcep = 3,           # By default it will be 12 features -  # Create equal number of frames - Equal array for every diffrent length audio file
              sumpower = TRUE,        # frequence scale transformation based on powerspectrum
              nbands = 40,            # Number of spectra bands, filter banks
              bwidth = 1,             # Width of spectral bands
              preemph = 0.95,         # STAGE 1 pre Emphasis
              # frames_in_rows = TRUE
               ) 
 
 # here we assume FFT as 512 as default set


image(mfcc.m)

image(as.matrix(audio@left))


############################################################################


# Determine duration

dur = length(mfcc.m)/audio@samp.rate
dur # in seconds


# d=Determine sample rate

fs = audio@samp.rate
fs # in Hz

## Spectrogram parameters

nfft = 512    # Fast Fourier Transformation size can be 512 (default), 1024 or 2048.

window = 1500

overlap = 500


# Creater a function fot Spectrogram

spectrogram = function(a) {



# Define Parameters

spec = specgram(x = a,
                n = nfft,
                Fs = fs,
                window = window,
                overlap = overlap)

# Structure of 'spec'

str(spec)


P = abs(spec$S)

# Normalize
P = P/max(P)     # If we do without abs(*) it will creat NA

# Convert to dB
P = 10*log10(P)

# config time axis
t = spec$t


# plot spectrogram

imagep(x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = 'Frequency in Hz',
       xlab = 'Time in Sec',
       main = 'Spectrogram',
       drawPalette = T,
       decimate = F)

}


# Spectrogram without MFCC

without.mfcc = spectrogram(as.matrix(audio@left))


# Spectrogram with MFCC

with.mfcc = spectrogram(mfcc.m)


 
