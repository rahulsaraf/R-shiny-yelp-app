
allNounChars1 <- c()

SplitTags<- function(y){
  newchars1 <- strsplit(y, "/")
  lapply(newchars1, checkNouns)
}

extractNouns <- function(x){
  mychars<- strsplit(x," ")
  lapply(mychars, SplitTags)
}

checkNouns <- function(z){
  z
  flag <- (z[2] == "NN" || z[2] == "NNP" || z[2] == "NNPS" || z[2] == "NN")
  if(flag){
    allNounChars1 <- c(allNounChars1, z[1])
  }
  allNounChars1
}

concatenate <- function(w){
  allw <- c()
  for(i in seq_along(w)){
    #print(i)
    #print(w[[i]][1])
    allw <- c(allw, w[[i]][1])
  }
  allw
}

str <- "IF/IN THEY/NNP HAVE/NNP THE/DT INGREDIENTS/NNS THEY/IN WILL/NNP MAKE/NNP ANY/NNP DRINK/NNP YOU/PRP WANT/VBD SO/JJ LONG/NNP AS/NNP YOU/PRP KNOW/NNP WHAT/WDT 'S/VBZ IN/IN IT.CHEERS/NNP !/."

#concatenate(extractNouns(str)[[1]])

library(NLP)
library(openNLP)

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  #list(POStagged = POStagged, POStags = POStags)
  list(POStagged = POStagged)
}
