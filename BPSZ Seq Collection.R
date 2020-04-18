#Collecting BPSZ sequences

call <- paste("https://api.genome.ucsc.edu/getData/sequence?genome=hg38;chrom=chr12;start=",bfa2$exonStarts[3],";end=",bfa2$exonEnds[3]+2,sep="")
call
get_Seq <- fromJSON(call)
unlist(get_Seq)[7]

seqbfa <- head(bfa2,10)
seqbfa

ben <- 0

for(i in 1:10){
  caller <- paste("https://api.genome.ucsc.edu/getData/sequence?genome=hg38;chrom=chr12;start=",seqbfa$BPSZ[i],";end=",seqbfa$exonEnds[i],sep="")
  seQ <- fromJSON(call)
  ben[i] = unlist(seQ)[7]
}

ben