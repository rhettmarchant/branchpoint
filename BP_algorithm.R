#branchpoint candidates

gencandidates <- function(y){
    g <- str_split(toupper(y$sequence),"")
    biglist <- data.frame(name = c(0), cannum=c(0), position=c(0), FiveSS=c(T), sequence=c(""), hdist=c(0), stringsAsFactors = FALSE)
    rc <- 0
    
    #for each sequence in dataframe
    for(x in g){
        rc <- rc +1
        counter <- 0
        
        #for each base in the sequence
        for(i in seq(from=1,to=(length(x)-2), by=1)){
            bplist <- 0
            if(x[i]=="T" & x[i+2]=="A" & i>2 & i < (length(x)-4)){
                counter <- counter + 1
                endpos <- if(i<=(length(x)/2)){TRUE}else{FALSE}
                BPseq <-paste(x[(i-2):(i+4)],sep="",collapse="")
                bplist <- c(y$name[rc],counter,(i-length(x)),endpos,BPseq,hdistance(BPseq))
                biglist <-rbind(biglist,bplist,stringsAsFactors=FALSE)

            }else{
                bplist <-c(y$name[rc],0,NA,"NO BP CANDIDATE FOUND")
            }
        }
    }
    tbl_df(biglist) %>% filter(name != "0")
}

#hammingdistance function
hdistance <- function(x){
    if(class(x)=="character")
        x <- unlist(str_split(x,""))
    U2 <- unlist(str_split("ACTAACAC",""))
    counter <- 0
    for(i in seq_along(x)){
        if(x[i] != U2[i]){
            counter <- counter + 1
        }
            
    }
    counter
}
