# This (R) file defines process(), which reads the file which is
# written by 70.R or 70t.R or 70k.R, then turns it into a matrix his
# is the format needed by sage's implementation of dancing links.
# Function process() writes a python-readable file that looks like
# "ones += [[2, [1,3]]]" 

#NB function process() takes OVER AN HOUR to run when executed with
#file 70.txt [which is produced by 70.R]


require(stringr)

getv <- function(s){str_split(s,pattern=' ')[[1]]}

process <- function(infile,outfile){  
    a <- readLines(infile)
    out <- file(outfile,"w")
    header <- getv(a[1])
    jj <- paste(which(getv(a[1]) %in% getv(a[2])),collapse=",")
    writeLines(paste("ones = [[1,[",jj, "]]]",sep=""),out)

    for(i in seq(from=3,  by=1,to=length(a))){    # start at third line; first is header, second done above
        jj <- paste(which(getv(a[1]) %in% getv(a[i])),collapse=",")
        writeLines(paste("ones += [[",i-1,",[", jj, "]]]",sep=""),out)
        print(i)
    }
    close(out)
    return(paste("written to ",outfile))   # side-effect is writing to outfile
}
