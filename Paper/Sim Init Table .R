states <- 18

setwd("Sim PM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

init_array_pm <- array(0,dim = c(1,states,reps))
init_true_array_pm <- array(0,dim = c(1,states,reps))
init_list_pm <- vector("list", length = reps)
init_true_list_pm <- vector("list", length = reps)
pi_array_pm <- array(0,dim = c(1,1,reps))
pi_true_array_pm <- array(0,dim = c(1,1,reps))
init_true_count_array_pm <- array(0,dim = c(1,states,reps))
init_true_count_list_pm <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_pm <- to_save[[1]]
  true_parameters_pm <- to_save[[2]]
  count_parameters_pm <- to_save[[3]]

  init_list_pm[[count]] <- obs_parameters_pm[[1]]
  init_true_list_pm[[count]] <- true_parameters_pm[[1]]
  pi_array_pm[1,1,count] <- obs_parameters_pm[[4]]
  pi_true_array_pm[1,1,count] <- true_parameters_pm[[4]]
  init_true_count_list_pm[[count]] <- count_parameters_pm[[1]]
  for (i in 1:states){
    init_array_pm[1,i,count] <- init_list_pm[[count]][i]
    init_true_array_pm[1,i,count] <- init_true_list_pm[[count]][i]
    init_true_count_array_pm[1,i,count] <- init_true_count_list_pm[[count]][i]
  }
  count <- count + 1
}

init_avg_pm <- numeric(states)
init_true_avg_pm <- numeric(states)
init_true_count_avg_pm <- numeric(states)
for (i in 1:states){
  init_avg_pm[i] <- mean(init_array_pm[1,i,])
  init_true_avg_pm[i] <- mean(init_true_array_pm[1,i,])
  init_true_count_avg_pm[i] <- mean(init_true_count_array_pm[1,i,])
}
pi_avg_pm <- mean(pi_array_pm[1,1,])
pi_true_avg_pm <- mean(pi_true_array_pm[1,1,])

init_mse_pm <- numeric(18)
for (i in 1:states){
  init_mse_pm[i] <- sum((init_true_array_pm - init_array_pm)[1,i,]^2) / dim(init_array_pm)[3]
}
pi_mse_pm <- sum((pi_true_array_pm - pi_array_pm)^2) / dim(pi_true_array_pm)[3]

setwd("..")
setwd("Sim CM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

init_array_cm <- array(0,dim = c(1,states,reps))
init_true_array_cm <- array(0,dim = c(1,states,reps))
init_list_cm <- vector("list", length = reps)
init_true_list_cm <- vector("list", length = reps)
pi_array_cm <- array(0,dim = c(1,1,reps))
pi_true_array_cm <- array(0,dim = c(1,1,reps))

for (name in list_of_names){
  load(name)
  obs_parameters_cm <- to_save[[1]]
  true_parameters_cm <- to_save[[2]]
  init_list_cm[[count]] <- obs_parameters_cm[[1]]
  init_true_list_cm[[count]] <- true_parameters_cm[[1]]
  pi_array_cm[1,1,count] <- obs_parameters_cm[[4]]
  pi_true_array_cm[1,1,count] <- true_parameters_cm[[4]]
  for (i in 1:states){
    init_array_cm[1,i,count] <- init_list_cm[[count]][i]
    init_true_array_cm[1,i,count] <- init_true_list_cm[[count]][i]
  }
  count <- count + 1
}

init_avg_cm <- numeric(states)
init_true_avg_cm <- numeric(states)
for (i in 1:states){
  init_avg_cm[i] <- mean(init_array_cm[1,i,])
  init_true_avg_cm[i] <- mean(init_true_array_cm[1,i,])
}
pi_avg_cm <- mean(pi_array_cm[1,1,])
pi_true_avg_cm <- mean(pi_true_array_cm[1,1,])

init_mse_cm <- numeric(18)
for (i in 1:states){
  init_mse_cm[i] <- sum((init_true_array_cm - init_array_cm)[1,i,]^2) / dim(init_array_cm)[3]
}
pi_mse_cm <- sum((pi_true_array_cm - pi_array_cm)^2) / dim(pi_true_array_cm)[3]

setwd("..")
setwd("Sim NM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

init_array_nm <- array(0,dim = c(1,states,reps))
init_true_array_nm <- array(0,dim = c(1,states,reps))
init_list_nm <- vector("list", length = reps)
init_true_list_nm <- vector("list", length = reps)
pi_array_nm <- array(0,dim = c(1,1,reps))
pi_true_array_nm <- array(0,dim = c(1,1,reps))

for (name in list_of_names){
  load(name)
  obs_parameters_nm <- to_save[[1]]
  true_parameters_nm <- to_save[[2]]
  init_list_nm[[count]] <- obs_parameters_nm[[1]]
  init_true_list_nm[[count]] <- true_parameters_nm[[1]]
  pi_array_nm[1,1,count] <- obs_parameters_nm[[4]]
  pi_true_array_nm[1,1,count] <- true_parameters_nm[[4]]
  for (i in 1:states){
    init_array_nm[1,i,count] <- init_list_nm[[count]][i]
    init_true_array_nm[1,i,count] <- init_true_list_nm[[count]][i]
  }
   count <- count + 1
}

init_avg_nm <- numeric(states)
init_true_avg_nm <- numeric(states)
for (i in 1:states){
  init_avg_nm[i] <- mean(init_array_nm[1,i,])
  init_true_avg_nm[i] <- mean(init_true_array_nm[1,i,])
}
pi_avg_nm <- mean(pi_array_nm[1,1,])
pi_true_avg_nm <- mean(pi_true_array_nm[1,1,])

init_mse_nm <- numeric(18)
for (i in 1:states){
  init_mse_nm[i] <- sum((init_true_array_nm - init_array_nm)[1,i,]^2) / dim(init_array_nm)[3]
}

pi_mse_nm <- sum((pi_true_array_nm - pi_array_nm)^2) / dim(pi_true_array_nm)[3]

setwd("..")
setwd("Sim MC 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

init_array_mc <- array(0,dim = c(1,states,reps))
init_true_array_mc <- array(0,dim = c(1,states,reps))
init_list_mc <- vector("list", length = reps)
init_true_list_mc <- vector("list", length = reps)
pi_array_mc <- array(0,dim = c(1,1,reps))
pi_true_array_mc <- array(0,dim = c(1,1,reps))

for (name in list_of_names){
  load(name)
  obs_parameters_mc <- to_save[[1]]
  true_parameters_mc <- to_save[[2]]
  init_list_mc[[count]] <- obs_parameters_mc[[1]]
  init_true_list_mc[[count]] <- true_parameters_mc[[1]]
  pi_array_mc[1,1,count] <- obs_parameters_mc[[4]]
  pi_true_array_mc[1,1,count] <- true_parameters_mc[[4]]
  for (i in 1:states){
    init_array_mc[1,i,count] <- init_list_mc[[count]][i]
    init_true_array_mc[1,i,count] <- init_true_list_mc[[count]][i]
  }
  count <- count + 1
}

init_avg_mc <- numeric(states)
init_true_avg_mc <- numeric(states)
for (i in 1:states){
  init_avg_mc[i] <- mean(init_array_mc[1,i,])
  init_true_avg_mc[i] <- mean(init_true_array_mc[1,i,])
}
pi_avg_mc <- mean(pi_array_mc[1,1,])
pi_true_avg_mc <- mean(pi_true_array_mc[1,1,])


init_sd_cm <- numeric(states)
init_sd_pm <- numeric(states)
init_sd_nm <- numeric(states)
init_sd_mc <- numeric(states)
for (i in 1:states){
  for (j in 1:states){
    init_sd_cm[i] <- sd(init_array_cm[1,i,])
    init_sd_pm[i] <- sd(init_array_pm[1,i,])
    init_sd_nm[i] <- sd(init_array_nm[1,i,])
    init_sd_mc[i] <- sd(init_array_mc[1,i,])
  }
}

init_mse_mc <- numeric(18)
for (i in 1:states){
  init_mse_mc[i] <- sum((init_true_array_mc - init_array_mc)[1,i,]^2) / dim(init_array_mc)[3]
}
pi_mse_mc <- sum((pi_true_array_mc - pi_array_mc)^2) / dim(pi_true_array_mc)[3]

setwd("..")

#################################################

library(xtable)
new_order <- c(1,3,5,7,9,11,13,15,17,2,4,6,8,10,12,14,16,18)
sig_figs <- 3
table <-""

#####

init_avg_marg_hpv_cm <- numeric(3)
init_avg_marg_hpv_cm[1] <- round(sum(init_avg_cm[c(1,2,3,4,5,6)]),sig_figs)
init_avg_marg_hpv_cm[2] <- round(sum(init_avg_cm[c(7,8,9,10,11,12)]),sig_figs)
init_avg_marg_hpv_cm[3] <- round(sum(init_avg_cm[c(13,14,15,16,17,18)]),sig_figs)

init_avg_marg_hpv_pm <- numeric(3)
init_avg_marg_hpv_pm[1] <- round(sum(init_avg_pm[c(1,2,3,4,5,6)]),sig_figs)
init_avg_marg_hpv_pm[2] <- round(sum(init_avg_pm[c(7,8,9,10,11,12)]),sig_figs)
init_avg_marg_hpv_pm[3] <- round(sum(init_avg_pm[c(13,14,15,16,17,18)]),sig_figs)

init_avg_marg_hpv_nm <- numeric(3)
init_avg_marg_hpv_nm[1] <- round(sum(init_avg_nm[c(1,2,3,4,5,6)]),sig_figs)
init_avg_marg_hpv_nm[2] <- round(sum(init_avg_nm[c(7,8,9,10,11,12)]),sig_figs)
init_avg_marg_hpv_nm[3] <- round(sum(init_avg_nm[c(13,14,15,16,17,18)]),sig_figs)

init_avg_marg_hpv_mc <- numeric(3)
init_avg_marg_hpv_mc[1] <- round(sum(init_avg_mc[c(1,2,3,4,5,6)]),sig_figs)
init_avg_marg_hpv_mc[2] <- round(sum(init_avg_mc[c(7,8,9,10,11,12)]),sig_figs)
init_avg_marg_hpv_mc[3] <- round(sum(init_avg_mc[c(13,14,15,16,17,18)]),sig_figs)

##

init_true_avg_marg_hpv_cm <- numeric(3)
init_true_avg_marg_hpv_cm[1] <- round(sum(init_true_avg_cm[c(1,2,3,4,5,6)]),sig_figs)
init_true_avg_marg_hpv_cm[2] <- round(sum(init_true_avg_cm[c(7,8,9,10,11,12)]),sig_figs)
init_true_avg_marg_hpv_cm[3] <- round(sum(init_true_avg_cm[c(13,14,15,16,17,18)]),sig_figs)

init_true_avg_marg_hpv_pm <- numeric(3)
init_true_avg_marg_hpv_pm[1] <- round(sum(init_true_avg_pm[c(1,2,3,4,5,6)]),sig_figs)
init_true_avg_marg_hpv_pm[2] <- round(sum(init_true_avg_pm[c(7,8,9,10,11,12)]),sig_figs)
init_true_avg_marg_hpv_pm[3] <- round(sum(init_true_avg_pm[c(13,14,15,16,17,18)]),sig_figs)

init_true_avg_marg_hpv_nm <- numeric(3)
init_true_avg_marg_hpv_nm[1] <- round(sum(init_true_avg_nm[c(1,2,3,4,5,6)]),sig_figs)
init_true_avg_marg_hpv_nm[2] <- round(sum(init_true_avg_nm[c(7,8,9,10,11,12)]),sig_figs)
init_true_avg_marg_hpv_nm[3] <- round(sum(init_true_avg_nm[c(13,14,15,16,17,18)]),sig_figs)

init_true_avg_marg_hpv_mc <- numeric(3)
init_true_avg_marg_hpv_mc[1] <- round(sum(init_true_avg_mc[c(1,2,3,4,5,6)]),sig_figs)
init_true_avg_marg_hpv_mc[2] <- round(sum(init_true_avg_mc[c(7,8,9,10,11,12)]),sig_figs)
init_true_avg_marg_hpv_mc[3] <- round(sum(init_true_avg_mc[c(13,14,15,16,17,18)]),sig_figs)

#####

init_avg_marg_cyt_cm <- numeric(3)
init_avg_marg_cyt_cm[1] <- round(sum(init_avg_cm[c(1,2,7,8,13,14)]),sig_figs)
init_avg_marg_cyt_cm[2] <- round(sum(init_avg_cm[c(3,4,9,10,15,16)]),sig_figs)
init_avg_marg_cyt_cm[3] <- round(sum(init_avg_cm[c(5,6,11,12,17,18)]),sig_figs)

init_avg_marg_cyt_pm <- numeric(3)
init_avg_marg_cyt_pm[1] <- round(sum(init_avg_pm[c(1,2,7,8,13,14)]),sig_figs)
init_avg_marg_cyt_pm[2] <- round(sum(init_avg_pm[c(3,4,9,10,15,16)]),sig_figs)
init_avg_marg_cyt_pm[3] <- round(sum(init_avg_pm[c(5,6,11,12,17,18)]),sig_figs)

init_avg_marg_cyt_nm <- numeric(3)
init_avg_marg_cyt_nm[1] <- round(sum(init_avg_nm[c(1,2,7,8,13,14)]),sig_figs)
init_avg_marg_cyt_nm[2] <- round(sum(init_avg_nm[c(3,4,9,10,15,16)]),sig_figs)
init_avg_marg_cyt_nm[3] <- round(sum(init_avg_nm[c(5,6,11,12,17,18)]),sig_figs)

init_avg_marg_cyt_mc <- numeric(3)
init_avg_marg_cyt_mc[1] <- round(sum(init_avg_mc[c(1,2,7,8,13,14)]),sig_figs)
init_avg_marg_cyt_mc[2] <- round(sum(init_avg_mc[c(3,4,9,10,15,16)]),sig_figs)
init_avg_marg_cyt_mc[3] <- round(sum(init_avg_mc[c(5,6,11,12,17,18)]),sig_figs)

## 

init_true_avg_marg_cyt_cm <- numeric(3)
init_true_avg_marg_cyt_cm[1] <- round(sum(init_true_avg_cm[c(1,2,7,8,13,14)]),sig_figs)
init_true_avg_marg_cyt_cm[2] <- round(sum(init_true_avg_cm[c(3,4,9,10,15,16)]),sig_figs)
init_true_avg_marg_cyt_cm[3] <- round(sum(init_true_avg_cm[c(5,6,11,12,17,18)]),sig_figs)

init_true_avg_marg_cyt_pm <- numeric(3)
init_true_avg_marg_cyt_pm[1] <- round(sum(init_true_avg_pm[c(1,2,7,8,13,14)]),sig_figs)
init_true_avg_marg_cyt_pm[2] <- round(sum(init_true_avg_pm[c(3,4,9,10,15,16)]),sig_figs)
init_true_avg_marg_cyt_pm[3] <- round(sum(init_true_avg_pm[c(5,6,11,12,17,18)]),sig_figs)

init_true_avg_marg_cyt_nm <- numeric(3)
init_true_avg_marg_cyt_nm[1] <- round(sum(init_true_avg_nm[c(1,2,7,8,13,14)]),sig_figs)
init_true_avg_marg_cyt_nm[2] <- round(sum(init_true_avg_nm[c(3,4,9,10,15,16)]),sig_figs)
init_true_avg_marg_cyt_nm[3] <- round(sum(init_true_avg_nm[c(5,6,11,12,17,18)]),sig_figs)

init_true_avg_marg_cyt_mc <- numeric(3)
init_true_avg_marg_cyt_mc[1] <- round(sum(init_true_avg_mc[c(1,2,7,8,13,14)]),sig_figs)
init_true_avg_marg_cyt_mc[2] <- round(sum(init_true_avg_mc[c(3,4,9,10,15,16)]),sig_figs)
init_true_avg_marg_cyt_mc[3] <- round(sum(init_true_avg_mc[c(5,6,11,12,17,18)]),sig_figs)

#####

init_avg_marg_cop_cm <- numeric(2)
init_avg_marg_cop_cm[1] <- round(sum(init_avg_cm[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_avg_marg_cop_cm[2] <- round(sum(init_avg_cm[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

init_avg_marg_cop_pm <- numeric(2)
init_avg_marg_cop_pm[1] <- round(sum(init_avg_pm[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_avg_marg_cop_pm[2] <- round(sum(init_avg_pm[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

init_avg_marg_cop_nm <- numeric(2)
init_avg_marg_cop_nm[1] <- round(sum(init_avg_nm[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_avg_marg_cop_nm[2] <- round(sum(init_avg_nm[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

init_avg_marg_cop_mc <- numeric(2)
init_avg_marg_cop_mc[1] <- round(sum(init_avg_mc[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_avg_marg_cop_mc[2] <- round(sum(init_avg_mc[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

##

init_true_avg_marg_cop_cm <- numeric(2)
init_true_avg_marg_cop_cm[1] <- round(sum(init_true_avg_cm[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_true_avg_marg_cop_cm[2] <- round(sum(init_true_avg_cm[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

init_true_avg_marg_cop_pm <- numeric(2)
init_true_avg_marg_cop_pm[1] <- round(sum(init_true_avg_pm[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_true_avg_marg_cop_pm[2] <- round(sum(init_true_avg_pm[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

init_true_avg_marg_cop_nm <- numeric(2)
init_true_avg_marg_cop_nm[1] <- round(sum(init_true_avg_nm[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_true_avg_marg_cop_nm[2] <- round(sum(init_true_avg_nm[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

init_true_avg_marg_cop_mc <- numeric(2)
init_true_avg_marg_cop_mc[1] <- round(sum(init_true_avg_mc[c(1,3,5,7,9,11,13,15,17)]),sig_figs)
init_true_avg_marg_cop_mc[2] <- round(sum(init_true_avg_mc[c(2,4,6,8,10,12,14,16,18)]),sig_figs)

#####

##########

init_true_count_avg <- round(init_true_count_avg_pm[new_order])

init_avg_cm <- round(init_avg_cm,sig_figs)[new_order]
init_avg_pm <- round(init_avg_pm,sig_figs)[new_order]
init_avg_nm <- round(init_avg_nm,sig_figs)[new_order]
init_avg_mc <- round(init_avg_mc,sig_figs)[new_order]

init_true_avg_cm <- round(init_true_avg_cm,sig_figs)[new_order]
init_true_avg_pm <- round(init_true_avg_pm,sig_figs)[new_order]
init_true_avg_nm <- round(init_true_avg_nm,sig_figs)[new_order]
init_true_avg_mc <- round(init_true_avg_mc,sig_figs)[new_order]

init_sd_cm <- round(init_sd_cm,sig_figs)[new_order]
init_sd_pm <- round(init_sd_pm,sig_figs)[new_order]
init_sd_nm <- round(init_sd_nm,sig_figs)[new_order]
init_sd_mc <- round(init_sd_mc,sig_figs)[new_order]

pi_avg_cm <- round(pi_avg_cm,sig_figs)
pi_avg_pm <- round(pi_avg_pm,sig_figs)
pi_avg_nm <- round(pi_avg_nm,sig_figs)
pi_avg_mc <- round(pi_avg_mc,sig_figs)

pi_true_avg_cm <- round(pi_true_avg_cm,sig_figs)
pi_true_avg_pm <- round(pi_true_avg_pm,sig_figs)
pi_true_avg_nm <- round(pi_true_avg_nm,sig_figs)
pi_true_avg_mc <- round(pi_true_avg_mc,sig_figs)

pi_sd_cm <- round(sd(pi_array_cm),sig_figs)
pi_sd_pm <- round(sd(pi_array_pm),sig_figs) 
pi_sd_nm <- round(sd(pi_array_nm),sig_figs)
pi_sd_mc <- round(sd(pi_array_mc),sig_figs)

init_mse_pm <- round(init_mse_pm,4)[new_order]
init_mse_cm <- round(init_mse_cm,4)[new_order]
init_mse_nm <- round(init_mse_nm,4)[new_order]
init_mse_mc <- round(init_mse_mc,4)[new_order]

pi_mse_pm <- round(pi_mse_pm,4)
pi_mse_cm <- round(pi_mse_cm,4)
pi_mse_nm <- round(pi_mse_nm,4)
pi_mse_mc <- round(pi_mse_mc,4)

table <- ""

table <- paste0(table, "\\multicolumn{1}{c|}{Stayer} & & & \\multicolumn{1}{c|}{} & " , pi_true_avg_nm, " & \\multicolumn{1}{c|}{" ,round(pi_true_avg_nm * 3488), "} & ")
table <- paste0(table, round(pi_avg_cm - pi_true_avg_cm,3), " & ", pi_sd_cm, " & \\multicolumn{1}{c|}{", pi_mse_cm, "} & ")
table <- paste0(table, round(pi_avg_mc - pi_true_avg_mc,3), " & ", pi_sd_mc, " & \\multicolumn{1}{c|}{", pi_mse_mc, "} & ")
table <- paste0(table, round(pi_avg_nm - pi_true_avg_nm,3), " & ", pi_sd_nm, " & ", pi_mse_nm)
table <- paste0(table, " \\\\ \n")

table <- paste0(table, "\\multicolumn{1}{c|}{Mover} & & & \\multicolumn{1}{c|}{} & " , (1 - pi_true_avg_nm), " & \\multicolumn{1}{c|}{" ,3488 - round(pi_true_avg_nm * 3488), "} & ")
table <- paste0(table, round(-pi_avg_cm + pi_true_avg_cm,3), " & ", pi_sd_cm," & \\multicolumn{1}{c|}{", pi_mse_cm, "} & ")
table <- paste0(table, round(-pi_avg_mc + pi_true_avg_mc,3), " & ", pi_sd_mc," & \\multicolumn{1}{c|}{", pi_mse_mc, "} & ")
table <- paste0(table, round(-pi_avg_nm + pi_true_avg_nm,3), " & ", pi_sd_nm," & ", pi_mse_nm)
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
  table <- paste0(table, round(init_true_avg_nm[i],sig_figs), "& \\multicolumn{1}{c|}{", init_true_count_avg[i], "} & ")
  
  table <- paste0(table, round(init_avg_cm[i] - init_true_avg_cm[i],3))
  if (abs(init_avg_cm[i] - init_true_avg_cm[i]) > 1.96 * init_sd_cm[i]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", init_sd_cm[i])
  table <- paste0(table, "& \\multicolumn{1}{c|}{", init_mse_cm[i] , "} & ")
  
  table <- paste0(table, round(init_avg_mc[i] - init_true_avg_mc[i],3))
  if (abs(init_avg_mc[i] - init_true_avg_mc[i]) > 1.96 * init_sd_mc[i]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", init_sd_mc[i])
  table <- paste0(table, "& \\multicolumn{1}{c|}{", init_mse_mc[i], "} & ")
  
  table <- paste0(table, round(init_avg_nm[i] - init_true_avg_nm[i],3))
  if (abs(init_avg_nm[i] - init_true_avg_nm[i]) > 1.96 * init_sd_nm[i]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", init_sd_nm[i])
  table <- paste0(table, " & ", init_mse_nm[i])
  
  
  table <- paste0(table, " \\\\ \n")
}

table <- paste0(table, "\\bottomrule")