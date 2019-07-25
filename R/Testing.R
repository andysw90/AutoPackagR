library(collidr) #One dependancy
source("R/functions.R")

fname <- "R/Testing.R" ## Set to current file name
packagesToInstall <- AutoPackagR(fname) ## Call package installer

### Go call something, including some stuff you don't have installed
read.alignment("somefile.fasta",format="fasta")
entropy("ABC")
phylogeny("some")
abc(123)

