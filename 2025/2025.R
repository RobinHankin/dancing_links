n <- 3
filename <- "~/rstudio/dancing_links/2025/f.txt"
N <- sum(seq_len(n)^3) # n=9 -> N=2025
r <- sum(seq_len(n))   # n=9 -> r=45


## puzzle has 285 squares, 1 of 1x1, 2 of 2x2, 3 of 3x3, ..., 9 of 9x9
## Fit all of these into a  45-by-45 frame of 2025 places.

o <- 
        lapply(
            sapply(
                seq_len(n), function(x){rep(paste(x,x,sep="x"),x)}
            ),
            function(x){paste("p",seq_along(x),x,sep="_")}
        ) |> unlist()

o <- paste(sub("p_", "p", o), collapse = " ")

xlines <- NULL
one_2_N <- sprintf("%03d",seq_len(N))
for(i in 2:n){
    ## "i" means we are considering a i-by-i square (of which there
    ## are i); there are 5 5x5 squares.

    
  xlines <- c(xlines, paste("X_", i, i+1, "_", one_2_N, sep = ""))
}


o <- paste(c(o, sprintf("%03d", seq_len(N)), " | ", xlines), collapse = " ")

## Write first line of filename:
write(o, file = filename, append = FALSE)


## First try a single size square, will try s=5 first.  There are five
## squares of this size: p1_5x5, p2_5x5, p3_5x5, p4_5x5, p5_5x5.  We
## will ensure that the p1<p2<p3<p4<p5 [that is, the top left square
## of p1 is less than that of p2 which is less than that of p3, etc].


M <- matrix(seq_len(N), r, r)

## we want lines like p1_5x5 
