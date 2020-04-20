#Returns the AG exclusion zone for sequences in get_Seqs tables

AGEZ <- function(y){
    for(x in y$sequence){
    SeqL <- str_length(x)
    Seq <- strsplit(x,"")[[1]]
    counter <- 0
    
    if("G" %in% Seq[SeqL] & "A" %in% Seq[SeqL-1]){
        print("Confirmed: AG 3'SS")
        print(paste(Seq[(SeqL-1):SeqL],sep="",collapse=""))
        for(i in seq(SeqL,1,-1)){
            if(Seq[i] == "G" & Seq[i-1] == "A" & i != SeqL){
                #            counter <- counter + 1
                #           if(counter == 2){
                AGEZ <- Seq[seq(i-13,SeqL,1)]
                print(paste(AGEZ, sep="", collapse=""))
                break
                #            }
            }    
        }
        
    }else{
        print("INVALID: No AG 3'SS")
    }
    

    }
}
    
