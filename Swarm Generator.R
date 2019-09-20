n <- 2500
name <- "EM18Boot2.swarm"
x <- ""
for (i in 2001:n){
  x <- paste(x,"module load R; Rscript EM18Boot.R ",i,";\n", sep = "")
}

writeLines(x, name)