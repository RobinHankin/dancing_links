## This file sifts through file wordle_words and removes anagrams and
## words with repeated letters.



a <- readLines("wordle_words")
out <- "www.txt" # www == "wordle_words_weeded"
cat(NULL,file=out,append=FALSE)

for(o in a){
  jj <- strsplit(o,"")[[1]]
  if(all(table(jj)==1)){ cat(paste(sort(jj),collapse=""),file=out,sep="\n",append=TRUE) }
}

# now take file www and "cat www|sort|uniq > wordle_weeded.txt"

