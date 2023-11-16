# This file is '70k.R'.  The "k" means "klein": it is a modification
# of 70t.R but with a different global topology.


filename <- "fkk.txt"  # ftt.txt The p01_OK device is used
big_square_size <- 70   # ie a 70x70 big square.
little_square_size <- 24 # little squares are the pieces, 1x1, 2x2, ..., 24x24


require(magic)  # need the process() function



# kleinmapper and isrev() are for continuous spaces (ie unit square)
kleinmapper <- function(V){
 x <- V[1]
 y <- V[2]
 x <- x%%2
 y <- y%%1
 if(x<1){return(c(x,y))
 } else {
   return(c(x-1,1-y))
 }
}


isrev <- function(V){    ifelse(V[1]<1,TRUE,FALSE)  }


# km() and ir() map a pair of integers to the (1:n) x (1:n) sauare.

km <- function(V,n){
  if(is.matrix(V)){return(t(apply(V,1,km,n=n)))}
  x <- V[1]
  y <- V[2]
  x <- process(x,2*n)
  if(x <= n){
    return(c(x,process(y,n)))
  } else {  # x>n
    return(c(x-n,process(n-y,n)))
  }
}

isrev <- function(V){  ifelse(V[1]<1,TRUE,FALSE)  }


"padder" <- function(i){# pad with a leading zero if necessary
  if(i<10){  
    jj <- "0"
  } else {
    jj <- ""
  }
  paste(jj,i,sep="")
}


# write first line to <filename>:

locm <- as.matrix(expand.grid(seq_len(big_square_size),seq_len(big_square_size)))
locsx <- sapply(locm[,1],padder)
locsy <- sapply(locm[,2],padder)

locs <- paste(locsx,locsy,sep="_")
pieces <- paste(paste("p",sapply(seq_len(little_square_size),padder),sep=""),collapse=" ")

write(paste(c(pieces,locs,"p01_OK"),collapse=" "),
      file=filename,append=FALSE)



# that's the first line done, now for the other lines:

write("p01 01_01 p01_OK",file=filename,append=TRUE) # this forces the
                                                     # algorithm to
                                                     # put p01 [ie the
                                                     # 1x1 square] in
                                                     # location 01_01,
                                                     # because it has
                                                     # to match
                                                     # "p01_OK".

for(ss in seq_len(little_square_size)){   # 'ss'= 'square size'
  print(paste("ss=",ss))
  ss_string <- paste("p",padder(ss),sep="")
  for(ix in seq_len(big_square_size)){# pad (ix,iy)= coords of lower left corner
    for(iy in seq_len(big_square_size)){
      jj <- as.matrix(expand.grid(seq_len(ss),seq_len(ss)))
      jj <- sweep(jj,2,c(ix,iy)-1,"+")
      jj <- km(jj,big_square_size)
      jj1 <- sapply(jj[,1],padder)
      jj2 <- sapply(jj[,2],padder)
      places <- paste(paste(jj1,jj2,sep="_"),collapse=" ") # ie places occupied by square
      textstring <- c(paste(ss_string,places))
      write(textstring,file=filename,append=TRUE)
    }
  }
}
