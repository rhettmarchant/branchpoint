#Gathers the last 100 nt of each intron
#Do not use with exon 1 (for obvious reasons)
#Still poorly tested with reverse strands
#THIS IS WORKING NOWWW!!!!!!! But takes approx 1s/seq
#DO NOT USE WITH MORE THAN 60 seqs at a time

get_Seqs <- function(x){
    #create a dataframe for the results
    sequences <- data.frame(name=c(""),exon=c(""), sequence= c(""), stringsAsFactors = F)
    #processes the gene name and exon number
    requests <- strsplit(x,"#")
    #creates an empty list for the sequences
    APIseq <- 0
    for(i in seq_along(x)){
        #collects the gene name and exon number
        request <- c(requests[[i]][1], requests[[i]][2])
        #check that the gene name is valid and within alluniqueExons
        if(request[1] %in% alluniqueExons$name){
            #selects the BPSZ for the request
            APIrequest <- unlist(alluniqueExons %>% filter(name== request[1] & exonNum==request[2]) %>% select(BPSZ, exonStarts))
            #makes the call to the API
            APIcall <- paste("https://api.genome.ucsc.edu/getData/sequence?genome=hg38;chrom=chr12;start=",APIrequest[1],";end=",APIrequest[2], sep="")
            APIresult <- fromJSON(APIcall)
            APIseq[i] <- unlist(APIresult)[7]
        }else{
           APIseq[i] <- paste(request[1],"INVALID CALL",sep="") 
        }
        #adds the results of the request ot the sequences dataframe
        sequences[i,] <- c(request[1],request[2],APIseq[i])
    }
sequences
}

set.seed(42)
test <- sample(alluniqueExons$exonname, 300, replace = F)
system.time(testSeqs <- get_Seqs(test))
AGEZ(testSeqs)
