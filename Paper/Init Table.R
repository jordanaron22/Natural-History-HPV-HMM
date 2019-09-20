setwd("EM Estimates 18 Norm /")
states <- 18
list_of_names <- list.files()
reps <- length(list_of_names)

init_init_array <- array(0,dim = c(1,states,reps))
init_est_array <- array(0,dim = c(1,states,reps))
likelihood_array <- numeric(reps)
pi_init_array <- numeric(reps)
pi_est_array <- numeric(reps)

init_est_list <- vector("list", length = reps)
count <- 1

for (name in list_of_names){
  load(name)
  est_parameters <- to_save[[2]]
  likelihood_array[count] <- to_save[[3]]
  pi_est_array[count] <- est_parameters[[4]]
  init_est_list[[count]] <- est_parameters[[1]]
  
  for (i in 1:states){
    init_est_array[1,i,count] <- init_est_list[[count]][i]
  }
  
  count <- count + 1
}

###

first <- which.max(likelihood_array)
est_init <- init_est_array[1,,first]
est_pi <- pi_est_array[first]

###

setwd("..")
setwd("Bootstrap 18/")

count <- 1
list_of_names <- list.files()
reps <- length(list_of_names)

init_sd <- numeric(states)
pi_sd <- 0

init_est_array <- array(0,dim = c(1,states,reps))
tran_est_array <- array(0,dim = c(states,states,reps))
class_est_array <- array(0,dim = c(states,states,reps))
pi_est_array <- numeric(reps)

for (name in list_of_names){
  load(name)
  est_parameters <- to_save[[2]]

  init_est_list[[count]] <- est_parameters[[1]]
  pi_est_array[count] <- est_parameters[[4]]

  for (i in 1:states){
    init_est_array[1,i,count] <- init_est_list[[count]][i]
  }

  count <- count + 1
}


boot_init <- numeric(states)
for (i in 1:states){
  boot_init[i] <- mean(init_est_array[1,i,])
}

for (i in 1:reps){
  init_sd <- init_sd + (init_est_array[1,,i] - boot_init)^2
  pi_sd <- pi_sd + (pi_est_array[i] - mean(pi_est_array))^2
}

init_sd <- sqrt(init_sd / (reps - 1))
pi_sd <- sqrt(pi_sd / (reps - 1))

setwd("..")

###############################

library(xtable)
new_order <- c(1,3,5,7,9,11,13,15,17,2,4,6,8,10,12,14,16,18)
sig_figs <- 3
table <-""

est_init <- round(est_init,sig_figs)[new_order]
init_sd <- round(init_sd,sig_figs)[new_order]

est_pi <- round(est_pi,sig_figs)
pi_sd <- round(pi_sd, sig_figs)


table <- paste0(table, "\\multicolumn{1}{c|}{Stayer} & & & \\multicolumn{1}{c|}{} & " , est_pi, " & ",pi_sd)
table <- paste0(table, " \\\\ \n")
table <- paste0(table, "\\multicolumn{1}{c|}{Mover} & & & \\multicolumn{1}{c|}{} & " , (1 - est_pi), " & ",pi_sd)
table <- paste0(table, " \\\\ \n")

adj_state <- "000"

for (i in 1:states){
  
  if(i == 2){adj_state <-"010" }
  else if(i == 3){adj_state <- "020"}
  else if(i == 4){adj_state <- "100"}
  else if(i == 5){adj_state <- "110"}
  else if(i == 6){adj_state <- "120"}
  else if(i == 7){adj_state <- "200"}
  else if(i == 8){adj_state <- "210"}
  else if(i == 9){adj_state <- "220"}
  else if(i == 10){adj_state <- "001"}
  else if(i == 11){adj_state <- "011"}
  else if(i == 12){adj_state <- "021"}
  else if(i == 13){adj_state <- "101"}
  else if(i == 14){adj_state <- "111"}
  else if(i == 15){adj_state <- "121"}
  else if(i == 16){adj_state <- "201"}
  else if(i == 17){adj_state <- "211"}
  else if(i == 18){adj_state <- "221"}
  
  table <- paste0(table, "\\multicolumn{1}{c|}{} & ", substr(adj_state, 1,1), " & ", substr(adj_state, 2,2), " & \\multicolumn{1}{c|}{", substr(adj_state, 3,3), "} & ")
  table <- paste0(table, est_init[i], " & ", init_sd[i])
  table <- paste0(table, " \\\\ \n")
}
