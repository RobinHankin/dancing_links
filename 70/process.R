# This (R) file defines process(), which reads the file which is
# written by 70.R or 70t.R or 70k.R, then turns it into a matrix of 0s
# and 1s.  This is the format needed by sage's implementation of
# dancing links.  Function writepython takes the output of process()
# and writes a python-readable file that looks like
# "M=[[1,0,0],[1,1,1],[0,0,1]]", and is suitable for inputting to
# sage.


#NB functions process() and writepython() each take SEVERAL MINUTES to
#run when executed with file 70.txt [which is produced by 70.R]

# NBB: look at process2.R which writes a file with entries in a
# different format which might be more efficient.



require(stringr)

getv <- function(s){str_split(s,pattern=' ')[[1]]}

process <- function(infile){   #returns a matrix of 0s and 1s; takes several minutes to run
    a <- readLines(infile)
    x <- getv(a[1])
    out <- matrix(0L,length(a)-1,length(x))    # -1 because first line of 'infile' is a header
    colnames(out) <- x
    for(i in 2:length(a)){
        p <- getv(a[i])
        out[i-1,x %in% p] <- 1L
    }
    return(out)
}

writepython <- function(a,filename){  # writes matrix 'a' in pythonstyle to filename
    out <- file(filename,"w")
    writeLines("M = [",out)   # initial line 
    for(i in seq_len(nrow(a)-1)){
        writeLines(paste("[",paste(a[i      ,],collapse=","),"],",sep=""),out) # note comma
    }
    writeLines    (paste("[",paste(a[nrow(a),],collapse=","),"]" ,sep=""),out) # note absence of comma

    writeLines("]",out)  # write closing close square bracket to match that on initial line
    close(out)
    return(paste("written to ",filename))   # side-effect is writing to outfile
}
           




# can now say writepython(process("jj.txt"),"pyfile.txt")

# this takes a few minutes to run.
    
