
#business <- data.frame()
#tip <- data.frame()
taggedText <- c()
finalOrder <- data.frame()
outfile <- c()

initializeData <- function(.progress='text'){
  setwd("D:/DevelopmentProjects/YelpProject/yelp_dataset_challenge_academic_dataset/")
  business <- read.csv("yelp_academic_dataset_business.csv")
  assign('business',business,envir=.GlobalEnv)
  tip <- read.csv("yelp_academic_dataset_tip.csv")
  assign('tip',tip,envir=.GlobalEnv)
}

process <- function(x){
  library(tm) # Framework for text mining.
  library(SnowballC) # Provides wordStem() for stemming.
  library(qdap) # Quantitative discourse analysis of transcripts.
  library(qdapDictionaries)
  library(dplyr) # Data preparation and pipes %>%.
  library(RColorBrewer) # Generate palette of colours for plots.
  library(ggplot2) # Plot word frequencies.
  library(scales) # Include commas in numbers.
  library(Rgraphviz) # Correlation plots.
  
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  
  
  source('D:/UTDallasStudy/RWorkingDirectory/myNLPFunctions.R')
  
  
  buss <- business[which((business$name) == (x)),]
  print(buss)
  ids<- (buss$business_id)
  print(ids)
  #levels(tip$business_id) <- factor(business$business_id)
  
  max1Tip <- tip[tip$business_id %in% factor(ids), "text"]
  maxTip <- toupper(max1Tip)
  print(maxTip)
  doc <- Corpus(VectorSource(maxTip))
  total <- length(maxTip)
  pb <- txtProgressBar(min = 0, max = 100, style = 3)
  
  for(i in seq_along(maxTip)){
    
    taggedText <- c(taggedText, concatenate(extractNouns(tagPOS(maxTip[i])$POStagged)[[1]]))
    setTxtProgressBar(pb, round((i/total)*100, 0))
    gc()
  }
  close(pb)
  finalOrder <- data.frame()
  finalFrame <- as.data.frame(table(taggedText))
  print(finalFrame)
  colnames(finalFrame) <- c("words", "frequency")
  finalOrder <- finalFrame[ order(-finalFrame$frequency),]
  assign('finalOrder',finalOrder,envir=.GlobalEnv)
  
  png("one.png", width=400, height=300)
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(finalOrder$words,finalOrder$frequency, scale=c(1,.4),min.freq=1,max.words=100, random.order=F, rot.per=.15, colors=pal2)    
  dev.off()
  list(finalOrder, buss[, c("name", "city", "full_address","state", "open",  "review_count",	"stars"	)], as.data.frame(maxTip))
}

getImage <- function(x){
  png("one.png", width=400, height=300)
  pal2 <- brewer.pal(8,"Dark2")
  wordcloud(x$words,x$frequency, scale=c(2,.6),min.freq=1,max.words=100, random.order=F, rot.per=.15, colors=pal2)    
  dev.off()
}