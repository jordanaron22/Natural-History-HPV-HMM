states <- 18

setwd("Sim PM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

tran_array_pm <- array(0,dim = c(states,states,reps))
tran_true_array_pm <- array(0,dim = c(states,states,reps))
tran_list_pm <- vector("list", length = reps)
tran_true_list_pm <- vector("list", length = reps)
tran_true_count_array_pm <- array(0,dim = c(states,states,reps))
tran_true_count_list_pm <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_pm <- to_save[[1]]
  true_parameters_pm <- to_save[[2]]
  count_parameters_pm <- to_save[[3]]
  
  tran_list_pm[[count]] <- obs_parameters_pm[[2]]
  tran_true_list_pm[[count]] <- true_parameters_pm[[2]]
  tran_true_count_list_pm[[count]] <- count_parameters_pm[[2]]
  for (i in 1:states){
    for (j in 1:states){
      tran_array_pm[i,j,count] <- tran_list_pm[[count]][i,j]
      tran_true_array_pm[i,j,count] <- tran_true_list_pm[[count]][i,j]
      tran_true_count_array_pm[i,j,count] <- tran_true_count_list_pm[[count]][i,j]
    }
  }
  count <- count + 1
}

tran_avg_pm <- matrix(0,states,states)
tran_true_avg_pm <- matrix(0,states,states)
tran_true_count_avg_pm <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    tran_avg_pm[i,j] <- mean(tran_array_pm[i,j,])
    tran_true_avg_pm[i,j] <- mean(tran_true_array_pm[i,j,])
    tran_true_count_avg_pm[i,j] <- mean(tran_true_count_array_pm[i,j,])
  }
}

setwd("..")
setwd("Sim CM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

tran_array_cm <- array(0,dim = c(states,states,reps))
tran_true_array_cm <- array(0,dim = c(states,states,reps))
tran_list_cm <- vector("list", length = reps)
tran_true_list_cm <- vector("list", length = reps)
tran_true_count_array_cm <- array(0,dim = c(states,states,reps))
tran_true_count_list_cm <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_cm <- to_save[[1]]
  true_parameters_cm <- to_save[[2]]
  count_parameters_cm <- to_save[[3]]
  
  tran_list_cm[[count]] <- obs_parameters_cm[[2]]
  tran_true_list_cm[[count]] <- true_parameters_cm[[2]]
  tran_true_count_list_cm[[count]] <- count_parameters_cm[[2]]
  for (i in 1:states){
    for (j in 1:states){
      tran_array_cm[i,j,count] <- tran_list_cm[[count]][i,j]
      tran_true_array_cm[i,j,count] <- tran_true_list_cm[[count]][i,j]
      tran_true_count_array_cm[i,j,count] <- tran_true_count_list_cm[[count]][i,j]
    }
  }
  count <- count + 1
}

tran_avg_cm <- matrix(0,states,states)
tran_true_avg_cm <- matrix(0,states,states)
tran_true_count_avg_cm <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    tran_avg_cm[i,j] <- mean(tran_array_cm[i,j,])
    tran_true_avg_cm[i,j] <- mean(tran_true_array_cm[i,j,])
    tran_true_count_avg_cm[i,j] <- mean(tran_true_count_array_cm[i,j,])
  }
}

setwd("..")
setwd("Sim NM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

tran_array_nm <- array(0,dim = c(states,states,reps))
tran_true_array_nm <- array(0,dim = c(states,states,reps))
tran_list_nm <- vector("list", length = reps)
tran_true_list_nm <- vector("list", length = reps)
tran_true_count_array_nm <- array(0,dim = c(states,states,reps))
tran_true_count_list_nm <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_nm <- to_save[[1]]
  true_parameters_nm <- to_save[[2]]
  count_parameters_nm <- to_save[[3]]
  
  tran_list_nm[[count]] <- obs_parameters_nm[[2]]
  tran_true_list_nm[[count]] <- true_parameters_nm[[2]]
  tran_true_count_list_nm[[count]] <- count_parameters_nm[[2]]
  for (i in 1:states){
    for (j in 1:states){
      tran_array_nm[i,j,count] <- tran_list_nm[[count]][i,j]
      tran_true_array_nm[i,j,count] <- tran_true_list_nm[[count]][i,j]
      tran_true_count_array_nm[i,j,count] <- tran_true_count_list_nm[[count]][i,j]
    }
  }
  count <- count + 1
}

tran_avg_nm <- matrix(0,states,states)
tran_true_avg_nm <- matrix(0,states,states)
tran_true_count_avg_nm <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    tran_avg_nm[i,j] <- mean(tran_array_nm[i,j,])
    tran_true_avg_nm[i,j] <- mean(tran_true_array_nm[i,j,])
    tran_true_count_avg_nm[i,j] <- mean(tran_true_count_array_nm[i,j,])
  }
}

setwd("..")
setwd("Sim MC 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

tran_array_mc <- array(0,dim = c(states,states,reps))
tran_true_array_mc <- array(0,dim = c(states,states,reps))
tran_list_mc <- vector("list", length = reps)
tran_true_list_mc <- vector("list", length = reps)
tran_true_count_array_mc <- array(0,dim = c(states,states,reps))
tran_true_count_list_mc <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_mc <- to_save[[1]]
  true_parameters_mc <- to_save[[2]]
  count_parameters_mc <- to_save[[3]]
  
  tran_list_mc[[count]] <- obs_parameters_mc[[2]]
  tran_true_list_mc[[count]] <- true_parameters_mc[[2]]
  tran_true_count_list_mc[[count]] <- count_parameters_mc[[2]]
  for (i in 1:states){
    for (j in 1:states){
      tran_array_mc[i,j,count] <- tran_list_mc[[count]][i,j]
      tran_true_array_mc[i,j,count] <- tran_true_list_mc[[count]][i,j]
      tran_true_count_array_mc[i,j,count] <- tran_true_count_list_mc[[count]][i,j]
    }
  }
  count <- count + 1
}

tran_avg_mc <- matrix(0,states,states)
tran_true_avg_mc <- matrix(0,states,states)
tran_true_count_avg_mc <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    tran_avg_mc[i,j] <- mean(tran_array_mc[i,j,])
    tran_true_avg_mc[i,j] <- mean(tran_true_array_mc[i,j,])
    tran_true_count_avg_mc[i,j] <- mean(tran_true_count_array_mc[i,j,])
  }
}

setwd("..")

tran_sd_cm <- matrix(0,states,states)
tran_sd_pm <- matrix(0,states,states)
tran_sd_nm <- matrix(0,states,states)
tran_sd_mc <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    tran_sd_cm[i,j] <- sd(tran_array_cm[i,j,])
    tran_sd_pm[i,j] <- sd(tran_array_pm[i,j,])
    tran_sd_nm[i,j] <- sd(tran_array_nm[i,j,])
    tran_sd_mc[i,j] <- sd(tran_array_mc[i,j,])
  }
}

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



tran_mse_cm <- matrix(0,states,states)
tran_mse_pm <- matrix(0,states,states)
tran_mse_nm <- matrix(0,states,states)
tran_mse_mc <- matrix(0,states,states)

for (i in 1:states){
  for (j in 1:states){
    tran_mse_cm[i,j] <- sum((tran_true_array_cm - tran_array_cm)[i,j,]^2) / dim(tran_array_cm)[3]
    tran_mse_pm[i,j] <- sum((tran_true_array_pm - tran_array_pm)[i,j,]^2) / dim(tran_array_pm)[3]
    tran_mse_nm[i,j] <- sum((tran_true_array_nm - tran_array_nm)[i,j,]^2) / dim(tran_array_nm)[3]
    tran_mse_mc[i,j] <- sum((tran_true_array_mc - tran_array_mc)[i,j,]^2) / dim(tran_array_mc)[3]
  }
}

####

table <- ""
library(xtable)
new_order <- c(1,3,5,7,9,11,13,15,17,2,4,6,8,10,12,14,16,18)
sig_figs <- 3
siig__fiigs <- 4

true_states       <- c(1,1,2, 2,3, 3,4, 4,5,5,6, 6,7,7,8, 8,9, 9)
tranified_states <-  c(1,4,2,11,3,11,7,11,2,9,2,16,1,7,8,11,8,17)

for (i in 1:length(true_states)){
  table <- paste0(table, GetState(true_states[i]), " & \\multicolumn{1}{c|}{", GetState(tranified_states[i]), "} & ")
  table <- paste0(table, round(tran_true_avg_nm[new_order,new_order][true_states[i],tranified_states[i]],sig_figs), " & \\multicolumn{1}{c|}{", round(tran_true_count_avg_nm[new_order,new_order][true_states[i],tranified_states[i]]), "} & ")
  
  table <- paste0(table, round(tran_true_avg_cm[new_order,new_order][true_states[i],tranified_states[i]] - tran_avg_cm[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  if (abs(tran_true_avg_cm[new_order,new_order][true_states[i],tranified_states[i]] - tran_avg_cm[new_order,new_order][true_states[i],tranified_states[i]]) > 1.96 * tran_sd_cm[new_order,new_order][true_states[i],tranified_states[i]]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", round(tran_sd_cm[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  table <- paste0(table, " & \\multicolumn{1}{c|}{", round(tran_mse_cm[new_order,new_order][true_states[i],tranified_states[i]],siig__fiigs) ,"} & ")
  
  table <- paste0(table, round(tran_true_avg_mc[new_order,new_order][true_states[i],tranified_states[i]] - tran_avg_mc[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  if (abs(tran_true_avg_mc[new_order,new_order][true_states[i],tranified_states[i]] - tran_avg_mc[new_order,new_order][true_states[i],tranified_states[i]]) > 1.96 * tran_sd_mc[new_order,new_order][true_states[i],tranified_states[i]]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", round(tran_sd_mc[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  table <- paste0(table, " & \\multicolumn{1}{c|}{", round(tran_mse_mc[new_order,new_order][true_states[i],tranified_states[i]],siig__fiigs) ,"} & ")
  
  table <- paste0(table, round(tran_true_avg_nm[new_order,new_order][true_states[i],tranified_states[i]] - tran_avg_nm[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  if (abs(tran_true_avg_nm[new_order,new_order][true_states[i],tranified_states[i]] - tran_avg_nm[new_order,new_order][true_states[i],tranified_states[i]]) > 1.96 * tran_sd_nm[new_order,new_order][true_states[i],tranified_states[i]]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", round(tran_sd_nm[new_order,new_order][true_states[i],tranified_states[i]],sig_figs))
  table <- paste0(table, " & ", round(tran_mse_nm[new_order,new_order][true_states[i],tranified_states[i]],siig__fiigs))
  
  
  table <- paste0(table, " \\\\ \n")
}
table <- paste(table, "\\bottomrule")


