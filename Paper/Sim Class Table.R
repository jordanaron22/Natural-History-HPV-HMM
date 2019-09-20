states <- 18

setwd("Sim PM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

class_array_pm <- array(0,dim = c(states,states,reps))
class_true_array_pm <- array(0,dim = c(states,states,reps))
class_list_pm <- vector("list", length = reps)
class_true_list_pm <- vector("list", length = reps)
class_true_count_array_pm <- array(0,dim = c(states,states,reps))
class_true_count_list_pm <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_pm <- to_save[[1]]
  true_parameters_pm <- to_save[[2]]
  count_parameters_pm <- to_save[[3]]
  
  class_list_pm[[count]] <- obs_parameters_pm[[3]]
  class_true_list_pm[[count]] <- true_parameters_pm[[3]]
  class_true_count_list_pm[[count]] <- count_parameters_pm[[3]]
  for (i in 1:states){
    for (j in 1:states){
      class_array_pm[i,j,count] <- class_list_pm[[count]][i,j]
      class_true_array_pm[i,j,count] <- class_true_list_pm[[count]][i,j]
      class_true_count_array_pm[i,j,count] <- class_true_count_list_pm[[count]][i,j]
    }
  }
  count <- count + 1
}

class_avg_pm <- matrix(0,states,states)
class_true_avg_pm <- matrix(0,states,states)
class_true_count_avg_pm <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    class_avg_pm[i,j] <- mean(class_array_pm[i,j,])
    class_true_avg_pm[i,j] <- mean(class_true_array_pm[i,j,])
    class_true_count_avg_pm[i,j] <- mean(class_true_count_array_pm[i,j,])
  }
}

setwd("..")
setwd("Sim CM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

class_array_cm <- array(0,dim = c(states,states,reps))
class_true_array_cm <- array(0,dim = c(states,states,reps))
class_list_cm <- vector("list", length = reps)
class_true_list_cm <- vector("list", length = reps)
class_true_count_array_cm <- array(0,dim = c(states,states,reps))
class_true_count_list_cm <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_cm <- to_save[[1]]
  true_parameters_cm <- to_save[[2]]
  count_parameters_cm <- to_save[[3]]
  
  class_list_cm[[count]] <- obs_parameters_cm[[3]]
  class_true_list_cm[[count]] <- true_parameters_cm[[3]]
  class_true_count_list_cm[[count]] <- count_parameters_cm[[3]]
  for (i in 1:states){
    for (j in 1:states){
      class_array_cm[i,j,count] <- class_list_cm[[count]][i,j]
      class_true_array_cm[i,j,count] <- class_true_list_cm[[count]][i,j]
      class_true_count_array_cm[i,j,count] <- class_true_count_list_cm[[count]][i,j]
    }
  }
  count <- count + 1
}

class_avg_cm <- matrix(0,states,states)
class_true_avg_cm <- matrix(0,states,states)
class_true_count_avg_cm <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    class_avg_cm[i,j] <- mean(class_array_cm[i,j,])
    class_true_avg_cm[i,j] <- mean(class_true_array_cm[i,j,])
    class_true_count_avg_cm[i,j] <- mean(class_true_count_array_cm[i,j,])
  }
}

setwd("..")
setwd("Sim NM 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

class_array_nm <- array(0,dim = c(states,states,reps))
class_true_array_nm <- array(0,dim = c(states,states,reps))
class_list_nm <- vector("list", length = reps)
class_true_list_nm <- vector("list", length = reps)
class_true_count_array_nm <- array(0,dim = c(states,states,reps))
class_true_count_list_nm <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_nm <- to_save[[1]]
  true_parameters_nm <- to_save[[2]]
  count_parameters_nm <- to_save[[3]]
  
  class_list_nm[[count]] <- obs_parameters_nm[[3]]
  class_true_list_nm[[count]] <- true_parameters_nm[[3]]
  class_true_count_list_nm[[count]] <- count_parameters_nm[[3]]
  for (i in 1:states){
    for (j in 1:states){
      class_array_nm[i,j,count] <- class_list_nm[[count]][i,j]
      class_true_array_nm[i,j,count] <- class_true_list_nm[[count]][i,j]
      class_true_count_array_nm[i,j,count] <- class_true_count_list_nm[[count]][i,j]
    }
  }
  count <- count + 1
}

class_avg_nm <- matrix(0,states,states)
class_true_avg_nm <- matrix(0,states,states)
class_true_count_avg_nm <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    class_avg_nm[i,j] <- mean(class_array_nm[i,j,])
    class_true_avg_nm[i,j] <- mean(class_true_array_nm[i,j,])
    class_true_count_avg_nm[i,j] <- mean(class_true_count_array_nm[i,j,])
  }
}

setwd("..")
setwd("Sim MC 18/")
list_of_names <- list.files()
reps <- length(list_of_names)
count <- 1

class_array_mc <- array(0,dim = c(states,states,reps))
class_true_array_mc <- array(0,dim = c(states,states,reps))
class_list_mc <- vector("list", length = reps)
class_true_list_mc <- vector("list", length = reps)
class_true_count_array_mc <- array(0,dim = c(states,states,reps))
class_true_count_list_mc <- vector("list", length = reps)

for (name in list_of_names){
  load(name)
  obs_parameters_mc <- to_save[[1]]
  true_parameters_mc <- to_save[[2]]
  count_parameters_mc <- to_save[[3]]
  
  class_list_mc[[count]] <- obs_parameters_mc[[3]]
  class_true_list_mc[[count]] <- true_parameters_mc[[3]]
  class_true_count_list_mc[[count]] <- count_parameters_mc[[3]]
  for (i in 1:states){
    for (j in 1:states){
      class_array_mc[i,j,count] <- class_list_mc[[count]][i,j]
      class_true_array_mc[i,j,count] <- class_true_list_mc[[count]][i,j]
      class_true_count_array_mc[i,j,count] <- class_true_count_list_mc[[count]][i,j]
    }
  }
  count <- count + 1
}

class_avg_mc <- matrix(0,states,states)
class_true_avg_mc <- matrix(0,states,states)
class_true_count_avg_mc <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    class_avg_mc[i,j] <- mean(class_array_mc[i,j,])
    class_true_avg_mc[i,j] <- mean(class_true_array_mc[i,j,])
    class_true_count_avg_mc[i,j] <- mean(class_true_count_array_mc[i,j,])
  }
}

setwd("..")

class_sd_cm <- matrix(0,states,states)
class_sd_pm <- matrix(0,states,states)
class_sd_nm <- matrix(0,states,states)
class_sd_mc <- matrix(0,states,states)
for (i in 1:states){
  for (j in 1:states){
    class_sd_cm[i,j] <- sd(class_array_cm[i,j,])
    class_sd_pm[i,j] <- sd(class_array_pm[i,j,])
    class_sd_nm[i,j] <- sd(class_array_nm[i,j,])
    class_sd_mc[i,j] <- sd(class_array_mc[i,j,])
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



class_mse_cm <- matrix(0,states,states)
class_mse_pm <- matrix(0,states,states)
class_mse_nm <- matrix(0,states,states)
class_mse_mc <- matrix(0,states,states)

for (i in 1:states){
  for (j in 1:states){
    class_mse_cm[i,j] <- sum((class_true_array_cm - class_array_cm)[i,j,]^2) / dim(class_array_cm)[3]
    class_mse_pm[i,j] <- sum((class_true_array_pm - class_array_pm)[i,j,]^2) / dim(class_array_pm)[3]
    class_mse_nm[i,j] <- sum((class_true_array_nm - class_array_nm)[i,j,]^2) / dim(class_array_nm)[3]
    class_mse_mc[i,j] <- sum((class_true_array_mc - class_array_mc)[i,j,]^2) / dim(class_array_mc)[3]
  }
}

####

table <- ""
library(xtable)
new_order <- c(1,3,5,7,9,11,13,15,17,2,4,6,8,10,12,14,16,18)
sig_figs <- 3
siig__fiigs <- 4

true_states       <- c(1,1,3,4,5,6,7,7,8,9,9,10,10,11,12,12,13,13,15,16,16,17,17,18)
classified_states <- c(1,2,2,4,5,6,7,8,8,7,9, 1, 2, 1, 3,11, 4, 5,15, 7,16, 8,17,18)

for (i in 1:length(true_states)){
  table <- paste0(table, GetState(true_states[i]), " & \\multicolumn{1}{c|}{", GetState(classified_states[i]), "} & ")
  table <- paste0(table, round(class_true_avg_nm[new_order,new_order][true_states[i],classified_states[i]],sig_figs), " & \\multicolumn{1}{c|}{", round(class_true_count_avg_nm[new_order,new_order][true_states[i],classified_states[i]]), "} & ")
  
  table <- paste0(table, round(class_true_avg_cm[new_order,new_order][true_states[i],classified_states[i]] - class_avg_cm[new_order,new_order][true_states[i],classified_states[i]],sig_figs))
  if (abs(class_true_avg_cm[new_order,new_order][true_states[i],classified_states[i]] - class_avg_cm[new_order,new_order][true_states[i],classified_states[i]]) > 1.96 * class_sd_cm[new_order,new_order][true_states[i],classified_states[i]]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", round(class_sd_cm[new_order,new_order][true_states[i],classified_states[i]],sig_figs))
  table <- paste0(table, " & \\multicolumn{1}{c|}{", round(class_mse_cm[new_order,new_order][true_states[i],classified_states[i]],siig__fiigs) ,"} & ")
  
  table <- paste0(table, round(class_true_avg_mc[new_order,new_order][true_states[i],classified_states[i]] - class_avg_mc[new_order,new_order][true_states[i],classified_states[i]],sig_figs))
  if (abs(class_true_avg_mc[new_order,new_order][true_states[i],classified_states[i]] - class_avg_mc[new_order,new_order][true_states[i],classified_states[i]]) > 1.96 * class_sd_mc[new_order,new_order][true_states[i],classified_states[i]]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", round(class_sd_mc[new_order,new_order][true_states[i],classified_states[i]],sig_figs))
  table <- paste0(table, " & \\multicolumn{1}{c|}{", round(class_mse_mc[new_order,new_order][true_states[i],classified_states[i]],siig__fiigs) ,"} & ")
  
  table <- paste0(table, round(class_true_avg_nm[new_order,new_order][true_states[i],classified_states[i]] - class_avg_nm[new_order,new_order][true_states[i],classified_states[i]],sig_figs))
  if (abs(class_true_avg_nm[new_order,new_order][true_states[i],classified_states[i]] - class_avg_nm[new_order,new_order][true_states[i],classified_states[i]]) > 1.96 * class_sd_nm[new_order,new_order][true_states[i],classified_states[i]]){
    table <- paste0(table, "^* ")
  }
  table <- paste0(table, " & ", round(class_sd_nm[new_order,new_order][true_states[i],classified_states[i]],sig_figs))
  table <- paste0(table, " & ", round(class_mse_nm[new_order,new_order][true_states[i],classified_states[i]],siig__fiigs))
  
  table <- paste0(table, " \\\\ \n")
}
table <- paste(table, "\\bottomrule")


