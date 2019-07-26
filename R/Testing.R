library(collidr) #One dependancy
library(seqinr)
source("R/functions.R")

fname <- "R/Testing.R" ## Set to current file name
packagesToInstall <- AutoPackagR(fname) ## Call package installer

### Go call something, including some stuff you don't have installed
read.alignment("somefile.fasta",format="fasta")
entropy("ABC")
phylogeny("some")
abc(123)

sum(1,2,3)


AddThenSquare <- function(values){
  added <- sum(values)
  squared <- added ^ 2
  return(squared)
}


read.alignment(file = "somefile.fasta",format = "fasta",forceToLower = FALSE)
