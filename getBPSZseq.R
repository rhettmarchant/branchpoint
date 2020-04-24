#Now collects the correct BPSZ for reverse strand exons


revComplement <- function(x){
    seqList <- unlist(str_split(toupper(x),""))
    bases <- c("A","G","C","T")
    names(bases) <- c("T","C","G","A")
    bases
    revSeq <- 0
    for(i in seq_along(seqList)){
        revSeq[i] <- bases[seqList[i]]
        
    }
    paste(revSeq, sep="", collapse= "")
}

get_Seqs <- function(x){
    #create a dataframe for the results
    sequences <- data.frame(name=c(""),exon=c(""), strand=c(""), sequence= c(""), stringsAsFactors = F)
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
            APIrequest <- unlist(alluniqueExons %>% filter(name== request[1] & exonNum==request[2]) %>% select(BPSZ, exonStarts, exonEnds, strand))
            #makes the call to the API
            if(APIrequest[4] == 2){
                APIcall <- paste("https://api.genome.ucsc.edu/getData/sequence?genome=hg38;chrom=chr12;start=",APIrequest[1],";end=",APIrequest[2], sep="")
                APIresult <- fromJSON(APIcall)
                APIseq[i] <- unlist(APIresult)[7]
            }
            if(APIrequest[4] == 1){
                APIcall <- paste("https://api.genome.ucsc.edu/getData/sequence?genome=hg38;chrom=chr12;start=",APIrequest[3],";end=",APIrequest[1], sep="")
                APIresult <- fromJSON(APIcall)
                APIseq[i] <- revComplement(stri_reverse(unlist(APIresult)[7]))
            }
            
            
        }else{
            APIseq[i] <- paste(request[1],"INVALID CALL",sep="") 
        }
        #adds the results of the request ot the sequences dataframe
        sequences[i,] <- c(request[1],request[2],APIrequest[4],APIseq[i])
    }
    sequences
}

set.seed(3)
test <- sample(alluniqueExons$exonname, 100, replace = F)
system.time(testSeqs <- get_Seqs(test))
browseURL('https://www.youtube.com/watch?v=WhPvJOnHotE')
AGEZ(testSeqs)

