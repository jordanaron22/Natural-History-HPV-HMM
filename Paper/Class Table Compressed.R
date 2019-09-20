setwd("EM Estimates 18 Norm /")
states <- 18
list_of_names <- list.files()
reps <- length(list_of_names)
getwd()

init_init_array <- array(0,dim = c(1,states,reps))
tran_init_array <- array(0,dim = c(states,states,reps))
class_init_array <- array(0,dim = c(states,states,reps))

init_est_array <- array(0,dim = c(1,states,reps))
tran_est_array <- array(0,dim = c(states,states,reps))
class_est_array <- array(0,dim = c(states,states,reps))

likelihood_array <- numeric(reps)
pi_init_array <- numeric(reps)
pi_est_array <- numeric(reps)
time_array <- numeric(reps)

init_init_list <- vector("list", length = reps)
tran_init_list <- vector("list", length = reps)
class_init_list <- vector("list", length = reps)

init_est_list <- vector("list", length = reps)
tran_est_list <- vector("list", length = reps)
class_est_list <- vector("list", length = reps)

count <- 1

for (name in list_of_names){
  load(name)
  
  init_parameters <- to_save[[1]]
  est_parameters <- to_save[[2]]
  likelihood_array[count] <- to_save[[3]]
  # time_array[count] <- to_save[[4]] / (60 * 60 * 24)
  
  pi_init_array[count] <- init_parameters[[4]]
  pi_est_array[count] <- est_parameters[[4]]
  
  init_init_list[[count]] <- init_parameters[[1]]
  tran_init_list[[count]] <- init_parameters[[2]]
  class_init_list[[count]] <- init_parameters[[3]]
  
  init_est_list[[count]] <- est_parameters[[1]]
  tran_est_list[[count]] <- est_parameters[[2]]
  class_est_list[[count]] <- est_parameters[[3]]
  
  
  
  
  for (i in 1:states){
    init_init_array[1,i,count] <- init_init_list[[count]][i]
    init_est_array[1,i,count] <- init_est_list[[count]][i]
    
    for (j in 1:states){
      tran_init_array[i,j,count] <- tran_init_list[[count]][i,j]
      tran_est_array[i,j,count] <- tran_est_list[[count]][i,j]
      
      class_init_array[i,j,count] <- class_init_list[[count]][i,j]
      class_est_array[i,j,count] <- class_est_list[[count]][i,j]
    }
  }
  
  count <- count + 1
}


#############################################

first <- which.max(likelihood_array)

est_init <- init_est_array[1,,first]
est_tran <- tran_est_array[,,first]
est_class <- class_est_array[,,first]
est_pi <- pi_est_array[first]

#############################################

setwd("..")
setwd("Bootstrap 18")
count <- 1
list_of_names <- list.files()
reps <- length(list_of_names)

init_sd <- numeric(states)
tran_sd <- matrix(0,states,states)
class_sd <- matrix(0,states,states)
pi_sd <- 0

init_est_array <- array(0,dim = c(1,states,reps))
tran_est_array <- array(0,dim = c(states,states,reps))
class_est_array <- array(0,dim = c(states,states,reps))
pi_est_array <- numeric(reps)

for (name in list_of_names){
  load(name)
  est_parameters <- to_save[[2]]
  
  init_est_list[[count]] <- est_parameters[[1]]
  tran_est_list[[count]] <- est_parameters[[2]]
  class_est_list[[count]] <- est_parameters[[3]]
  pi_est_array[count] <- est_parameters[[4]]
  
  for (i in 1:states){
    init_est_array[1,i,count] <- init_est_list[[count]][i]
    
    for (j in 1:states){
      tran_est_array[i,j,count] <- tran_est_list[[count]][i,j]
      class_est_array[i,j,count] <- class_est_list[[count]][i,j]
    }
  }
  count <- count + 1
}

boot_init <- numeric(states)
boot_tran <- matrix(0,states,states)
boot_class <- matrix(0,states,states)
for (i in 1:states){
  boot_init[i] <- mean(init_est_array[1,i,])
  for (j in 1:states){
    boot_tran[i,j] <- mean(tran_est_array[i,j,])
    boot_class[i,j] <- mean(class_est_array[i,j,])
  }
}

for (i in 1:reps){
  init_sd <- init_sd + (init_est_array[1,,i] - boot_init)^2
  tran_sd <- tran_sd + (tran_est_array[,,i] - boot_tran)^2
  class_sd <-class_sd + (class_est_array[,,i] - boot_class)^2
  pi_sd <- pi_sd + (pi_est_array[i] - mean(pi_est_array))^2
}

init_sd <- sqrt(init_sd / (reps - 1))
tran_sd <- sqrt(tran_sd / (reps - 1))
class_sd <- sqrt(class_sd / (reps - 1))
pi_sd <- sqrt(pi_sd / (reps - 1))

setwd("..")

GetState <- function(j){
  if(j == 1){adj_state <- "(0,0,0)"} 
  if(j == 2){adj_state <- "(0,1,0)"}
  if(j == 3){adj_state <- "(0,2,0)"}
  if(j == 4){adj_state <- "(1,0,0)"}
  if(j == 5){adj_state <- "(1,1,0)"}
  if(j == 6){adj_state <- "(1,2,0)"}
  if(j == 7){adj_state <- "(2,0,0)"}
  if(j == 8){adj_state <- "(2,1,0)"}
  if(j == 9){adj_state <- "(2,2,0)"}
  if(j == 10){adj_state <- "(0,0,1)"}
  if(j == 11){adj_state <- "(0,1,1)"}
  if(j == 12){adj_state <- "(0,2,1)"}
  if(j == 13){adj_state <- "(1,0,1)"}
  if(j == 14){adj_state <- "(1,1,1)"}
  if(j == 15){adj_state <- "(1,2,1)"}
  if(j == 16){adj_state <- "(2,0,1)"}
  if(j == 17){adj_state <- "(2,1,1)"}
  if(j == 18){adj_state <- "(2,2,1)"}
  return(adj_state)
}


table <- ""
library(xtable)
new_order <- c(1,3,5,7,9,11,13,15,17,2,4,6,8,10,12,14,16,18)
sig_figs <- 3
siig__fiigs <- 4

true_states       <- c(3,3,6,9)
tranified_states <-  c(1,2,5,7)

for (i in 1:length(true_states)){
  table <- paste0(table, GetState(true_states[i]), " & \\multicolumn{1}{c|}{", GetState(tranified_states[i]), "} & ")
  
  table <- paste0(table, round(est_class[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  table <- paste0(table, " & ", round(class_sd[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  
  
  table <- paste0(table, " \\\\ \n")
}
table <- paste(table, "\\bottomrule")
