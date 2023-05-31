require(stringr)
## The 5x5x5 cube is 0-124, numbers base 5.

blockdims <- c(5,5,5)  # the "block" is five by five by fice.

coords_to_number <- function(coords){
1+    coords[1] + coords[2]*5 + coords[3]*25   # note the one; the numbers go from 1 to 125 (not 0 to 124)
}

wherecanitgo <- function(piece_dims,blockdims){  # returns places occupied.
    stopifnot(all(piece_dims>0))

    places_for_corner <- as.matrix(expand.grid(
        seq(from=0,to=blockdims[1]-piece_dims[1]),
        seq(from=0,to=blockdims[2]-piece_dims[2]),
        seq(from=0,to=blockdims[3]-piece_dims[3])
        ))

    offset <- as.matrix(expand.grid(
        seq(from=0,to=piece_dims[1]-1),
        seq(from=0,to=piece_dims[2]-1),
        seq(from=0,to=piece_dims[3]-1)
        ))

    out <- list()
    for(i in seq_len(nrow(places_for_corner))){
        jj <- sweep(offset,2,places_for_corner[i,],"+")
        out <- c(out,list(jj))
    }
    return(lapply(out,function(x){apply(x,1,coords_to_number)}))
}


# first do cube and square:
index <- matrix(c(
    2,2,2,126,    # cube = piece number 126
    1,2,2,127,    # square = piece number 127 (three orientations)
    2,1,2,127,    # square
    2,2,1,127    # square
),byrow=TRUE,ncol=4)



# rods are harder because there are three of them:
index_rods <-
    matrix(c(
        1,1,3,0,    # rod orientation A
        1,3,1,0,    # rod orientation B
        3,1,1,0     # rod orientation C
        ),byrow=TRUE,ncol=4)


number_of_rods <- 3
for(i in seq_len(number_of_rods)){
    jj <- index_rods
    jj[,4] <- 127 + i
    index <- rbind(index,jj)
}
# the three rods are piece number 128,129,130



    index_planks <- matrix(c(
        1,2,4,0,    # plank
        1,4,2,0,    # plank
        2,1,4,0,    # plank
        2,4,1,0,    # plank
        4,1,2,0,    # plank
        4,2,1,0     # plank
        ),byrow=TRUE,ncol=4)

number_of_planks <- 13
for(i in seq_len(number_of_planks)){
    jj <- index_planks
    jj[,4] <- 130 + i  # off by one: the first plank is number 131
    index <- rbind(index,jj)
}

# the planks are pieces 131 through 143

ans <- list()

for(i in seq_len(nrow(index))){
    jj <- wherecanitgo(index[i,1:3],blockdims)
    jj <- lapply(jj,function(x){c(index[i,4],x)})
    ans <- c(ans, jj)
}

oneswriter <- function(l,outfile){   #  'l' is a list; use oneswriter(ans,"~/f.txt")
    out <- file(outfile,"w")
    jj <- paste(l[[1]],collapse=",")
    header <- paste("ones =  [[1,[",jj,"]]]",sep="")
    writeLines(header,out)

    for(i in seq(from=2,to=length(l))){
        jj <- paste(l[[i]],collapse=",")
        body <- paste("ones += [[",i,",[",jj,"]]]",sep="")
        writeLines(body,out)
    }

    close(out)
}

f <- function(o,comma=TRUE){
    jj <- rep(0,143)
    jj[(1:143) %in% o] <- 1
    jj <- paste(jj,collapse=",")
    if(comma){
        return( paste("[",jj,"],",sep=""))
    } else {
        return( paste("[",jj,"]",sep=""))
    }
}


Mwriter <- function(l,outfile){
    out <- file(outfile,"w")
    header <- "M =  Matrix(["
    writeLines(header,out)

    for(i in seq(from=2,to=length(l))-1){
        writeLines(f(l[[i]]),out)
    }


    # final line has no comma:
    writeLines(f(l[[length(l)]],comma=FALSE),out)
    writeLines("])",out)
    
    close(out)
}


oneswriter(ans,"~/f.sage")
Mwriter(ans,"~/g.sage")
