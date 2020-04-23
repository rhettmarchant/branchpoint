#Returns the AG exclusion zone for sequences in get_Seqs tables
#CURRENTLY RETURNING:
#Error in if (Seq[i] %in% c("g", "G") & Seq[i - 1] %in% c("a", "A") & i <  : argument is of length zero

AGEZ <- function(y){
    y=testSeqs
    AGEZdf <- data.frame(name=y$name,AG3SS=rep("0",nrow(y)),length=rep(0,nrow(y)),sequence=rep("0",nrow(y)), stringsAsFactors = FALSE)
    g <- y$sequence
    for(x in seq_along(g)){
        SeqL <- str_length(g[x])
        Seq <- strsplit(g[x],"")[[1]]
        #print(c(Seq[1:10], SeqL))
        errors <- 0
    
        if(Seq[SeqL] %in% c("g","G") & Seq[SeqL-1] %in% c("a","A")){
        
            for(i in seq(SeqL,1,-1)){
                if(i < SeqL & Seq[i] %in% c("g","G") & Seq[i-1] %in% c("a","A")){
                    if(i>13){
                        m <- i-13
                    }
                    AGEZSeq <- Seq[seq(m,SeqL,1)]
                    AGEZdf$sequence[x] <- paste(AGEZSeq, sep="", collapse="")
                    AGEZdf$length[x] <- str_length(AGEZdf$sequence[x])
                    AGEZdf$AG3SS[x] <- TRUE
                    break
                }else if(i==1){
                    break
                }    
        
            }
        }else{
            AGEZdf$AG3SS[x] <- FALSE
            AGEZdf$sequence[x] <- "ERROR INVALID 3'SS: No AG"
            AGEZdf$length[x] <- NA
        }
    }
    tbl_df(AGEZdf %>% filter(AG3SS=="TRUE"))
}