# Spotify data analysis
# Brandon Le 20200531
rm(list=ls())
library(rjson)
library(tidyverse)
options(stringsAsFactors=FALSE)

### read .json Spotify streaming history data
# data 0 spans 2019-05-27 to 2019-11-03
data0 <- fromJSON(paste(readLines("/Users/biznaga/Documents/Spotify_MyData_200531/StreamingHistory0.json"), collapse=""))
# data 1
data1 <- fromJSON(paste(readLines("/Users/biznaga/Documents/Spotify_MyData_200531/StreamingHistory1.json"), collapse=""))
# data 2
data2 <- fromJSON(paste(readLines("/Users/biznaga/Documents/Spotify_MyData_200531/StreamingHistory2.json"), collapse=""))

data <- c(data0,data1,data2)  # combine lists into single object
data <- data.frame(matrix(unlist(data), nrow=length(data), byrow=T))  #convert to dataframe
colnames(data) <- c("timestamp","artist","tracktitle","msPlayed")
data$year <- NA
data$month <- NA
data$day <- NA
data$hour <- NA
# strsplit the timestamps on "-" produces 3 elementsyear mo (day hour)
data$year <- sapply(strsplit(data$timestamp,"-"),`[`,1)
data$month <- sapply(strsplit(data$timestamp,"-"),`[`,2)
data$day <- sapply(strsplit(data$timestamp," "),`[`,1)
data$hour <- sapply(strsplit(data$timestamp," "),`[`,2)
data$year.mo <- paste0(data$year,data$month) # create a year.mo string column
data$msPlayed <- as.numeric(data$msPlayed)
data$year.mo <- as.numeric(data$year.mo)
save(data,file="/Users/biznaga/Documents/Spotify_MyData_200531/streamHistory_20190527_20200530.RData")  # save data frame as .Rdata for future use

data$msPlayed <- as.numeric(data$msPlayed)
### Functions
## Unique Artists and Tracks
uniqArtistsTracks <- function(data){
  u.artists <- length(unique(data$artist))
  u.tracks <- length(unique(data$tracktitle))
  return(cat("Unique Artists: ", u.artists,
             "\n Unique Tracks: ", u.tracks))
}
## Total Playtime
totalplaytime <- function(data){
  ms <- sum(data$msPlayed)
  hours <- ms/(1000*60*60)
  days <- hours/24
  return(cat("Dataset Total Playtime: \n",
         ms, " milliseconds \n",
         hours, " hours \n",
         days, " days \n"))
  #return(length(data))
}

## Windowed Playtime
# start and end should be "year.mo" strings
start <- 201909
end <- 201911
data[which(data$year.mo>=201909&data$year.mo<=201911),]

slice.playtime <- function(data,start,end){
  temp <- data[which(data$year.mo>=start&data$year.mo<=end),"msPlayed"]
  ms <- sum(temp)
  hours <- ms/(1000*60*60)
  days <- hours/24
  return(cat("Playtime from: ",start, " ", end, "\n",
             ms, " milliseconds \n",
             hours, " hours \n",
             days, " days \n"))
}


### Apply Functions
uniqArtistsTracks(data)
totalplaytime(data)
slice.playtime(data,201909,201911)
slice.playtime(data,202003,202005)

### Summarizing data


### Plots
data$year.mo <- paste0(data$year,data$month)
#plot <- ggplot(data, aes(data$year.mo,msPlayed)) +
ggplot(data, aes(year.mo,msPlayed/(1000*60*60))) + geom_violin()
  
# myfunction <- function(arg1, arg2, ... ){
#   statements
#   return(object)
# }

