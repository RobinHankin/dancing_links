## makes a file for use with sagemath.  The first 26 columns are
## letters a-z and column 27 is a proxy for "single letter".

#a <- readLines("words5_including_stupid_words")
a <- readLines("wordle_weeded.txt")

## open parentheses:
cat("M = Matrix([",file="data.py",sep="\n",append=FALSE)


for(o in a){
  x <- strsplit(o,"")[[1]]

  jj <- rep(0,26)
  jj[which(letters %in% x)] <- 1
  if(sum(jj) ==5){   # check for repeated letters, eg "adder"
    jj <- paste("[",paste(jj,collapse=","),",0],",collapse="")
    cat(jj,file="data.py",sep="\n",append=TRUE)
  }
}

## now single letters:
m <- cbind(diag(26),1)
for(i in seq_len(25)){  # sic, line 26 dealt with separately
  jj <- paste("[",paste(m[i,],collapse=","),"],",collapse="")
  cat(jj,file="data.py",sep="\n",append=TRUE)
}


## last line, no comma:
jj <- paste("[",paste(m[26,],collapse=","),"]",collapse="")
cat(jj,file="data.py",sep="\n",append=TRUE)


## close parentheses:
cat("])",file="data.py",sep="\n",append=TRUE)
