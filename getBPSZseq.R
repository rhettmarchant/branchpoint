#Gathers the last 100 nt of each intron
#Do not use with exon 1 (for obvious reasons)
#Still poorly tested with reverse strands
#THIS IS WORKING NOWWW!!!!!!! But takes approx 1s/seq
#DO NOT USE WITH MORE THAN 60 seqs at a time
ben <- 0
cls <- 0
system.time(
    for(i in 1:10){
        caller <- paste("https://api.genome.ucsc.edu/getData/sequence?genome=hg38;chrom=chr12;start=",alluniqueExons$BPSZ[i],";end=",alluniqueExons$exonStarts[i], sep="")
        seQ <- fromJSON(caller)
        ben[i] = unlist(seQ)[7]
        cls[i] <- caller
    }
)

ben
cls
