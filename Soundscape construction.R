###Code to run the soundscape construction for Not silent yet: the shifting soundscapes of spring

###Simon Butler 2021

###SOUNDSCAPE CONSTRUCTION###

###Set working directory - this needs folder containing .wav sound files for each species and folder containing .txt count data files for each site###

###Load Packages###
library(seewave)
library(tuneR)
library(soundecology)
library(audio)

###Set number of iterations to run for each soundscape###
nreps <- 5

###Create 5 min silent mono 44100 wav file called silence.wav###
system("sox -n -r 44100 -c 1 silence.wav trim 0 300")

####Create 3s tone to act as calibration called tone.wav###
system("sox -n -r 44100 tone.wav synth 3 sine 500 norm -6")

####Concatenate tone and silence files into initially empty soundscape file baseline.wav###
system("sox tone.wav silence.wav baseline.wav")

###Bring in and process count data. Count data files have three columns [species; count; year]###

###list sites with count data###
txtfiles <- list.files()[grep(".txt",list.files())]

###loop through each site in turn###
for(i in 1:length(txtfiles))
{
  sitetab <- read.table(txtfiles[i], header=F,stringsAsFactors = F)
  
  ###determine years in which site surveyed###    
  nyears<-unique(sitetab[,3])
  
  ###loop through each year in turn###  
  for(v in 1:length(nyears))
  {
    
    siteyeartab<-sitetab[sitetab[,3]==nyears[v],]
    
    ###loop through site-year soundscape construction for specified number of iterations###    
    for(j in 1:nreps)
    {
      system("cp baseline.wav soundscape.wav")
      
      ###loop through each species recorded on site-year survey in turn###            
      for(k in 1:nrow(siteyeartab)) 
        
      {
        species <- siteyeartab[k,1]
        count <- siteyeartab[k,2]
        
        ###loop through for number of individuals of each species recorded on site-year survey in turn###        
        for(m in 1:count)
          
        {
          ###randomly assign insertion point###
          start <- sample(c(3:278),1)
          
          ###randomly assign playback volume                    
          vol <- sample(seq(0.01,1,0.01),1)
          
          ###randomly select sound file for species; specify playback volume and insert at randomly assigned point in soundscape audio file###                    
          system(paste0("sox $(gshuf -n1 -e ",species,"*.wav) -p pad ",start," 0 | sox -v ",vol," - -m soundscape.wav -t wavpcm soundscapea.wav norm -6"))
          system("sox soundscapea.wav -t wavpcm soundscape.wav")
        }
      }
      
      ###trim off three second calibration tone at start of soundscape file to produce final soundscape for analysis###      
      system("sox soundscape.wav -t wavpcm soundscape_final.wav trim 3")
      ACTIVESITE<-readWave("soundscape_final.wav")
      
      ###calculate acoustic indices on soundscape###      
      output <- data.frame(sitename = substr(txtfiles[i],1,nchar(txtfiles[i])-4),
                           year = nyears[v],
                           rep = j,
                           aei = acoustic_evenness(ACTIVESITE)[1],
                           bi = bioacoustic_index(ACTIVESITE, min_freq=2000, max_freq=22050)[1],
                           adi = acoustic_diversity(ACTIVESITE)[1],
                           H = H(ACTIVESITE)[1])
      
      if(i == 1 & j == 1 & v == 1)
      {
        finalout <- output
      } else
      {
        finalout <- rbind(finalout,output)
      }
    } ###end loop for iterations###
    
    ###save soundscape produced for final iteration for site-year count file###    
    system(paste0("mv soundscape_final.wav ",substr(txtfiles[i],1,nchar(txtfiles[i])-4),"_",nyears[v],".wav"))  
    
  } ###end loop for years###
  
} ###end loop for sites###

write.table(finalout, "OUTPUT.txt", row.names=F)
