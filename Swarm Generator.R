n <- 100
n2 <- 100
name <- "ThreeSimsFullData.swarm"
x <- ""
for (i in 1:n){
  x <- paste(x,"module load R; Rscript ThreeSimsFullData.R ",i,";\n", sep = "")
  # for (j in 1:n2){
  #   x <- paste(x,"module load R; Rscript ThreeSimsFullData.R ",i," ",j,";\n", sep = "")
  # }
}

writeLines(x, name)