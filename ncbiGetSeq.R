#THIS DOES NOT GET ALL GENES FOR chr12 ONLY 750000 to 55700000
call2 <- paste("http://api.genome.ucsc.edu/getData/track?genome=hg19;track=ncbiRefSeq;chrom=chr12;start=750000;end=55700000")
call2
get_Seq2 <- fromJSON(call2)
Seqdf <- data.frame(get_Seq2$ncbiRefSeq)
names(Seqdf)
head(Seqdf)
str(Seqdf)

#Creates the exonNum attribute
temp=0

for(i in 1:length(unique(Seqdf$name))){
  temp[i] = list(seq(from=1, to=Seqdf$exonCount[i]))
}

#Creates the gexon dataframe

gexons2 <- data.frame(name=rep(Seqdf$name, Seqdf$exonCount), name=rep(Seqdf$name2, Seqdf$exonCount),exonNum=unlist(temp), exonStarts = as.numeric(unlist(strsplit(Seqdf$exonStarts, ","))), exonEnds = as.numeric(unlist(strsplit(Seqdf$exonEnds,",")))) %>% mutate(BPSZ = exonStarts-100)

#LOOK AT GEXONS2HEAD,50 - how can we remove duplicates?
 
ben <- 0
cls <- 0

for(i in 1:10){
  caller <- paste("https://api.genome.ucsc.edu/getData/sequence?genome=hg38;chrom=chr12;start=",gexons2$BPSZ[i],";end=",gexons2$exonStarts[i], sep="")
  seQ <- fromJSON(call)
  ben[i] = unlist(seQ)[7]
  cls[i] <- caller
}

ben
cls
