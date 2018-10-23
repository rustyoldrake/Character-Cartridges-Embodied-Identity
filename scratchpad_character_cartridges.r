###################################################### 
### Character Cartridges - Experimental Code and Scratch Pad
### Focus: PERSONALITY
### https://dreamtolearn.com/ryan/cognitivewingman/18/en
### https://github.com/rustyoldrake/Character-Cartridges-Embodied-Identity 
######################################################

  
library(RCurl) # install.packages("RCurl"xx) # if the package is not already installed
library(httr)
library(data.table)
library(dplyr)
library(reshape2)
library(stringr)
library(splitstackshape)
library(tidyr)
library(XML)
library(ggplot2)
library(png)
library(jpeg)
library(grid)

######### Housekeeping And Authentication 
setwd("/Users/ryan/Code/Character_Cartridges") # Set working Directory
getwd()
source("keys.r") ## KEYS has acutal username:password for each IBM service. Seperate R file looks sort of like below


# Load in the CSV of the nearly 200 traits and types 
human_traits_types <- read.csv("anthropomorphic_model_v4.csv",header=TRUE)
head(human_traits_types)

###################################################### 
######################################################


## Base URLs for IBM Watson APIs
base_url_NLC = "https://gateway.watsonplatform.net/natural-language-classifier/api/v1/classifiers/"
getURL(base_url_NLC,userpwd = username_password_NLC )  # non essential , but checks if working /authenticated


##### NLC  ##### NLC  ##### NLC  ##### NLC  ##### NLC  
source("nlc_functions.r")
watson.NLC.listallclassifiers()  # TEST

### TONE
source("tone_functions.r")
process_data_to_tone("radness is greatness") # TEST

# LOAD UP THREE CHARACTER CARTRIDGES ### DATA SOURCE
characters <- read.csv2("characters_input_basic_traits.csv", header = TRUE, sep = ",")
characters
names <-  characters[ (characters$trait == "name"), ] 
names
characters <- characters[ (characters$value %in% c(1:100)), ] # kills rows with non numeric values
characters
characters$value <- as.numeric(paste(characters$value))
characters
# sum(characters$value)


# Improt 3 images 
img1<-readPNG("image_muskstark.png")
img1 <- rasterGrob(img1, interpolate=TRUE)

img2<-readPNG("image_jkoprah.png")
img2 <- rasterGrob(img2, interpolate=TRUE)

img3<-readPNG("image_halkitt.png")
img3 <- rasterGrob(img3, interpolate=TRUE)

#plot(img3)
#rasterImage(img3,0,0,5,100)
 
## Set the Table
plot.new()
#layout(matrix(1:9,3,3))
layout(matrix(1:4,2,2))
layout.show(1) # one at a time


#plot(img3)
#layout.show(9)
#plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', ylab = '', xlab = '')
#barplot(img3)

## Colors for our emotions
identity_colors <<- c("violet", "orange", "yellow", "white", "deepskyblue","lawngreen", "blue")
emotion_colors <<- c("red", "violet", "yellow", "green", "blue","orange", "orange", "yellow", "lawngreen", "deepskyblue","violet","forestgreen","deeppink1")

## Set the Table
plot.new()
layout(matrix(1:9,3,3))
layout.show(9) # one at a time


for (i in 1:max(characters$id))
{
  print(i)
  data <- characters[ which(characters$id == i), ]
  #print(newdata)
  
  ## IDENTITY PLOT ##
  #plot(i, type='n', main=paste("Agent ",i,"- State"), xlab="x", ylab="y")
  barplot(data$value, col = identity_colors,
          main=paste("ID ",i," ",names$value[i],"- State"),
          ylim=c(0,100),
          xlab="", 
          names.arg=data$trait,
          border="black",
          las=2, srt=45)
  
 
  
  # generic for now - but tune for identity later (sensitivity)
  response_tone <- response_raw[1:13,]
  response_tone
  response_tone$signal <- response_tone$signal*100
  
  ## OBSERVATION PLOT ##
  barplot(response_tone$signal, col = emotion_colors,
          main=paste("ID ",i," ",names$value[i],"- Observation"),
          ylim=c(0,100),
          xlab="", 
          names.arg=response_tone$trait,
          border="black",
          las=2, srt=45)
  
  barplot(0)
  #plot(i, type='n', main=paste("Agent ",i,"- Observations"), xlab="x", ylab="y")
  
}


  # CHARACTER STATE - IDENTITY 
personality_state <- response_raw[1:13,]
personality_state
response_tone$signal <- response_tone$signal*100
barplot(response_tone$signal, col = emotion_colors,
        main=paste("Agent ",i,"- State"),
        xlab="", 
        names.arg=response_tone$trait,
        border="black",
        las=2, srt=45)





## Change this from sample to sample to see the characters observing change
text <- "I'm so happy and totally confident it's gonna be rad"
response_raw <- process_data_to_tone(text)
response_raw


# CHARACTER STATE - IDENTITY 
personality_state <- response_raw[1:13,]
personality_state
response_tone$signal <- response_tone$signal*100
barplot(response_tone$signal, col = emotion_colors,
        main="1 - Current Emotional State",
        xlab="", 
        names.arg=response_tone$trait,
        border="black",
        las=2, srt=45)


# SPOTLIGHT - Tone, Language and Social Big 5
response_tone <- response_raw[1:13,]
response_tone
response_tone$signal <- response_tone$signal*100
barplot(response_tone$signal, col = emotion_colors,
        main="1 - Current Emotional State",
        xlab="", 
        names.arg=response_tone$trait,
        border="black",
        las=2, srt=45)


