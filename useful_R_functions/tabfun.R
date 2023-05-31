"tabs" <- function(n){
  if(n<0){return(!rev(Recall(-n)))}
       if(n==1){return(c(F,T,F,T,F,T,T))}
  else if(n==2){return(c(F,F,T,T,T,T,F))}
  else if(n==3){return(c(F,T,T,T,F,T,F))}
  else if(n==4){return(c(T,F,T,T,T,F,F))}
  else if(n==5){return(c(F,F,T,T,T,F,T))}
  else if(n==6){return(c(T,T,F,T,F,F,T))}
  else if(n==7){return(c(T,T,F,T,F,F,T))}
  else {stop('argument n not allowed')}
}

# Function nums() takes an integer 1-19; a hexagon position, an
# orientation (1-6); and a side type (-6 to -1 or 1 to 6).  It retunrs
# numbers of
  
