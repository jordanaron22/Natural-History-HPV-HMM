StateCalculator <- function(val1, val2, val3){
  if (!is.na(val1) & !is.na(val2) & !is.na(val3)){
    return(c(4 * val1 + 2 * val2 + val3))
  } else if (!is.na(val1) & !is.na(val2) & is.na(val3)){
    if(val1 == 0 & val2 == 0){
      return(0)
    }
    return(c(4 * val1 + 2 * val2, 4 * val1 + 2 * val2 + 1))
  } else if (!is.na(val1) & is.na(val2) & !is.na(val3)){
    return(c(4 * val1 + val3, 4 * val1 + 2 + val3))
  } else if (is.na(val1) & !is.na(val2) & !is.na(val3)){
    return(c(2 * val2 + val3, 4 + 2 * val2 + val3))
  } else if (!is.na(val1) & is.na(val2) & is.na(val3)){
    return(c(4 * val1, 4 * val1 + 1, 4 * val1 + 2, 4 * val1 + 3))
  }else if (is.na(val1) & !is.na(val2) & is.na(val3)){
    return(c(2 * val2, 2 * val2 + 1, 2 * val2 + 4, 2 * val2 + 5))
  }else if (is.na(val1) & is.na(val2) & !is.na(val3)){
    return(c(val3, val3 + 2, val3 + 4, val3 + 6))
  } else {
    return(NA)
  }
}


ALTS_og <- read.csv(file = "lsil.risk.file.y2018m10d17.csv")
ALTS_og <- ALTS_og[,c(1,6,7,8,9)]
for (i in 1:dim(ALTS_og)[1]){
  
  if (!is.na(ALTS_og[i,3])){
    if (ALTS_og[i,3] > 0){
      ALTS_og[i,3] <- 1
    } else if (ALTS_og[i,3] < 0){
      ALTS_og[i,3] <- NA
    }
  }
  
  if (!is.na(ALTS_og[i,4])){
    if (ALTS_og[i,4] < 0){
      ALTS_og[i,4] <- NA
    } else if (ALTS_og[i,4] < 5){
      ALTS_og[i,4] <- 0
    } else {
      ALTS_og[i,4] <- 1
    }
  }
}

n <- length(unique(ALTS_og[,1]))
max_time <- 5

ALTS <- array(NA, dim = c(n,max_time,4))
real_i <- 0
ind <- ""
Censoring <- F
for (i in 1:dim(ALTS_og)[1]){
  
  if (ind != ALTS_og[i,1]){
    ind <- ALTS_og[i,1]
    real_i <- real_i + 1
    Censoring <- F
  }
  
  real_time <- (ALTS_og[i,2] %/% 6) + 1 
  
  possible_vals <- StateCalculator(ALTS_og[i,3], ALTS_og[i,4], ALTS_og[i,5])
  
  
  if(Censoring == T){
    possible_vals <- NA
  }
  
  for (j in 1:length(possible_vals)){
    ALTS[real_i, real_time, j] <- possible_vals[j]
  }
  
  if (!is.na(ALTS_og[i,5])){
    if (ALTS_og[i,5] == 1){
      Censoring <- T
    }
  }
}


Normalize <- function(data){
  for (i in 1:dim(data)[1]){
    if (sum(data[i,],na.rm = TRUE) != 0){
      data[i,] <- data[i,]/sum(data[i,])
    }
  }
  return(data)
}



Classification <- function(x_val,y_val,class_matrix){
  if (is.na(x_val)){
    return(1)
  }
  return(class_matrix[[y_val+1,x_val+1]])
}



ClassificationRatio <- function(x_val, x_vals, y_val, class_matrix){
  num <- Classification(x_val, y_val, class_matrix)
  denom <- 0
  for (val in x_vals){
    denom <- denom + Classification(val, y_val, class_matrix)
  }
  if (denom == 0){return(0)}
  return(num/denom)
}



numOfStates <- function(data,type){
  if (missing(type)){
    return(c(0:7))
  }
  else if (type == 1){
    return (c(0:3))
  } else if (type == 2){
    return(c(0:2))
  } else if (type == 3){
    return (c(0:1))
  }else if (type == 4){
    return(c(0:23))
  }else if (missing(type)){
    return (c(min(data, na.rm = T):max(data, na.rm = T)))
  }
  
}



GetPatterns <- function(states){
  library(plyr)
  options(scipen = 999)
  vals <- rep(NaN,dim(states)[1])
  for (i in 1:dim(states)[1]){
    val <- ""
    for(j in 1:dim(states)[2]){
      for(k in 1:dim(states)[3]){
        if (!is.na(states[i,j,k])){
          states_val <- states[i,j,k] 
        } else {
          states_val <- 8
        }
        val <- paste0(val, states_val)
      }
    }
    vals[i] <- val
  }
  return(vals)
}



Pattern2Data <- function(unique_patterns){
  time_length <- 5
  n <- length(unique_patterns[[1]])
  partial_len <- 4
  freq_vec <- numeric(n)
  state_array <- array(0, dim = c(n,time_length,partial_len))
  for (i in 1:n){
    freq_vec[i] <- unique_patterns$Freq[[i]]
    pattern <- unique_patterns$all_patterns[[i]]
    
    for (time in 1:time_length){
      for (partial in 1:partial_len){
        new_val <- as.integer(substr(pattern, (time - 1) * partial_len + partial,(time - 1) * partial_len + partial))
        if (new_val == 8){
          new_val <- NaN
        }
        state_array[i,time, partial] <- new_val
      }
    }
  }
  
  return(list(state_array, freq_vec))
}




InitialProb <- function(data, freq_vec, stayer_vec = rep(0,length(freq_vec))){
  init <- numeric(length(numOfStates(data)))
  for (i in 1:dim(data)[1]){
    if (stayer_vec[i] == 0){
      num_of_parts <- sum(!is.na(data[i,1,]))
      if (!is.na(data[i,1,1])){
        for (j in data[i,1,]){
          init[j+1] <- init[j+1] + freq_vec[i]/num_of_parts
        }
      }
    }
  }
  return(init)
}



TransitionProb <- function(data, freq_vec, stayer_vec = rep(0,length(freq_vec))){
  k <- length(numOfStates(data))
  tran <- matrix(0,k,k)
  for (i in 1:dim(data)[1]){
    if (stayer_vec[i] == 0){
      for (j in 2:dim(data)[2]){
        if (!is.na(data[i,j-1,1]) & !is.na(data[i,j,1])){
          num_of_parts <- sum(!is.na(data[i,j-1,])) * sum(!is.na(data[i,j,]))
          for (k1 in data[i,j-1,]){
            for (k2 in data[i,j,]){
              tran[k1+1,k2+1] <- tran[k1+1,k2+1] + freq_vec[i]/num_of_parts
            }
          }
        }
      }
    }
  }
  return(tran)
}



GetAllPossibilitiesForw <- function(data, time, individual){
  
  for (curr_time in 1:time){
    
    if (curr_time == 1){
      possibilities <- data[individual,1,]
      if (sum(!is.na(possibilities)) == 0){
        possibilities <- NA
      } else {
        possibilities <- possibilities[!is.na(possibilities)]
      }
      possibilities <- t(as.matrix(possibilities))
      
    } else {
      current_possibilities <- data[individual,curr_time,]
      if (sum(!is.na(current_possibilities)) == 0){
        possibilities <- rbind(possibilities, NA)
        
      } else {
        current_possibilities <- current_possibilities[!is.na(current_possibilities)]
        running_possibilities <- c()
        current_possibilities <- rev(current_possibilities)
        for (i in 1:length(current_possibilities)){
          running_possibilities <- cbind(rbind(possibilities,current_possibilities[i]),running_possibilities)
        }
        possibilities <- running_possibilities
      }
    }
  }
  return(possibilities)
  
}



GetAllPossibilitiesBackw <- function(data, time, individual){
  max_time <- dim(data)[2]
  for (curr_time in time:max_time){
    
    if (curr_time == time){
      possibilities <- data[individual,curr_time,]
      if (sum(!is.na(possibilities)) == 0){
        possibilities <- NA
      } else {
        possibilities <- possibilities[!is.na(possibilities)]
      }
      possibilities <- t(as.matrix(possibilities))
      
    } else {
      current_possibilities <- data[individual,curr_time,]
      if (sum(!is.na(current_possibilities)) == 0){
        possibilities <- rbind(possibilities, NA)
        
      } else {
        current_possibilities <- current_possibilities[!is.na(current_possibilities)]
        running_possibilities <- c()
        current_possibilities <- rev(current_possibilities)
        for (i in 1:length(current_possibilities)){
          running_possibilities <- cbind(rbind(possibilities,current_possibilities[i]),running_possibilities)
        }
        possibilities <- running_possibilities
      }
    }
  }
  if (time == 4){
    possibilities <- possibilities[,order(possibilities[1,], possibilities[2,])]
  } else if (time == 3){
    possibilities <- possibilities[,order(possibilities[1,], possibilities[2,], possibilities[3,])]
  } else if (time == 2){
    possibilities <- possibilities[,order(possibilities[1,], possibilities[2,], possibilities[3,], possibilities[4,])]
  } else if (time == 1){
    possibilities <- possibilities[,order(possibilities[1,], possibilities[2,], possibilities[3,], possibilities[4,], possibilities[5,])]
  }
  return(as.matrix(possibilities))
  
}



#JASA paper method 
ForwardIterFull <- function(data,time,individual,initial_probabilities,transition,class_matrix){
  k <- numOfStates(data)
  all_pos_obs <- GetAllPossibilitiesForw(data,time, individual)
  alpha_matrix_pos <- vector("list", dim(all_pos_obs)[2])
  for(pos in 1:dim(all_pos_obs)[2]){
    pos_ob <- all_pos_obs[,pos]
    
    alpha_matrix<-vector("list",time)
    alpha_i <- numeric(length(k))
    
    for (i in 1:time){
      alpha_matrix[[i]] <- alpha_i
    }
    
    
    for (i in 1:length(k)){
      if (!is.na(pos_ob[1])){
        alpha_matrix[[1]][i] <- initial_probabilities[i] * Classification(pos_ob[1],k[i],class_matrix)
      } else {
        alpha_matrix[[1]][i] <- initial_probabilities[i]
      }
    }
    
    if (time > 1){
      for (i in 2:time){
        for (j in 1:length(k)){
          if (!is.na(pos_ob[i])){
            alpha_matrix[[i]][j] <- (alpha_matrix[[i-1]] %*% transition[,j]) *  Classification(pos_ob[i],k[j],class_matrix)
          } else {
            alpha_matrix[[i]][j] <- (alpha_matrix[[i-1]] %*% transition[,j])
          }
        }
      }
    }
    
    alpha_matrix_pos[[pos]] <- alpha_matrix
    
  }
  
  return(alpha_matrix_pos)
}



#JASA method 
BackwardIterFull <- function(data,time,individual,transition,class_matrix){
  k <- numOfStates(data)
  length <- dim(data)[2]
  
  #Works since Beta only uses X_t+1
  if (time == 5){
    adj_time <- time
  } else{
    adj_time <- time + 1
  }
  
  all_pos_obs <- GetAllPossibilitiesBackw(data,adj_time,individual)
  beta_matrix_pos <- vector("list", dim(all_pos_obs)[2])
  
  
  for(pos in 1:dim(all_pos_obs)[2]){
    pos_ob <- all_pos_obs[,pos]
    #Want backward length to always be 5 as it makes future indexing easier
    #Add dummy -1 to inflate length
    pos_ob <- append(pos_ob,rep(-1,adj_time-1),0)
    
    beta_i <- numeric(length(k))
    misclass_vector <- numeric(length(k))
    beta_matrix<-vector("list",length)
    
    for (i in 1:length){
      beta_matrix[[i]] <- beta_i
    }
    
    for (i in 1:length(k)){
      beta_matrix[[length]][i] <- 1
    }
    if (time != length) {
      for (i in (length-1):time){
        if (!is.na(pos_ob[i+1])){
          for (l in 1:length(k)){
            misclass_vector[l] <- Classification(pos_ob[i+1],k[l],class_matrix)
          }
          for (j in 1:length(k)){
            beta_matrix[[i]][j] <- sum(beta_matrix[[i+1]] * transition[j,] * misclass_vector,na.rm = TRUE)
          }
        } else {
          for (j in 1:length(k)){
            if (sum(beta_matrix[[i+1]]) == length(k)){
              beta_matrix[[i]][j] <- 1
            } else {
              beta_matrix[[i]][j] <- sum(beta_matrix[[i+1]] * transition[j,]) 
            }
          }
        }
      }
    }
    
    beta_matrix_pos[[pos]] <- beta_matrix
  }
  
  return(beta_matrix_pos)
}



CalcIndLikelihoodFast <- function(forward, backward, time){
  #If only forward should be time = 5
  
  if(missing(time)){
    time <- 1
  }
  
  
  if (missing(backward) | time == 5){
    forward_sum <- 0
    for (i in 1:length(forward)){
      forward_sum <- forward_sum + sum(forward[[i]][[length(forward[[i]])]])
    }
    return(forward_sum)
  } 
  
  else {
    k <- length(forward[[1]][[1]])
    forward_sum <- numeric(k)
    backward_sum <- numeric(k)
    
    for (i in 1:length(forward)){
      forward_sum <- forward_sum + forward[[i]][[time]]
    }
    for (i in 1:length(backward)){
      backward_sum <- backward_sum + backward[[i]][[time]]
    }
    
    return((backward_sum %*% forward_sum)[[1]])
    
  }
  
}



CalcIndLikelihood <- function(data, individual, initial_probabilities,transition,class_matrix,pi){
  
  forward <- ForwardIterFull(data,dim(data)[2],individual,initial_probabilities,transition,class_matrix)
  
  forward_sum <- 0
  for (i in 1:length(forward)){
    forward_sum <- forward_sum + sum(forward[[i]][[dim(data)[2]]])
  }
  
  likelihood <- (pi * CalcProdInd(data,individual,class)) + ((1 - pi) * forward_sum)
  return(likelihood)
}



CalcProdInd <- function(data,individual,class_matrix){
  prod_total <- 0
  partial_data_full <- GetAllPossibilitiesForw(data,dim(data)[2],individual)
  for (chain in 1:dim(partial_data_full)[2]){
    prod <- 1
    partial_data <- partial_data_full[,chain]
    for (j in 1:length(partial_data)){  
      if (!is.na(partial_data[j])){
        prod <- prod * Classification(partial_data[j], 0, class_matrix)
      }
    }
    prod_total <- prod_total + prod
  }
  return(prod_total)
}
 


CalcProdIndAlt <- function(data,class_matrix){
  prod <- 1
  for (j in 1:length(data)){
    if (!is.na(data[j])){
      prod <- prod * Classification(data[j], 0, class_matrix)
    }
  }
  return(prod)
}




CalcStayer <- function(data,init,tran,class,freq_vec,pi_0){
  stayer_vec <- numeric(dim(data)[1])
  for (i in 1:dim(data)[1]){
    stayer_vec[i] <- (pi_0 * CalcProdInd(data,i,class) / CalcIndLikelihood(data,i,init,tran,class,pi_0))
  }
  
  pi_0 <- (stayer_vec %*% freq_vec)/sum(freq_vec)
  return(pi_0[1])
}



CalcLikelihoodPattern <- function(data, initial_probabilities,transition,class_matrix, freq_vec, pi_0){
  likelihood_sum <- 0
  for (i in 1:dim(data)[1]){
    likelihood <- log(CalcIndLikelihood(data,i,initial_probabilities,transition,class_matrix, pi_0)) * freq_vec[i]
    likelihood_sum <- likelihood_sum + likelihood
  }
  return(likelihood_sum)
}



CalcInitialPattern <- function(data, initial_probabilities, transition, class_matrix, freq_vec, pi_0){
  # THE PROBLEM IS THE PLACEMENT OF FREQ VEC 
  #DONT KNOW WHY NEED TO DO MORE TESTING
  k <- numOfStates(data)
  prob_list <- numeric(length(k))
  workaround <- rep(-2,length(k))
  
  for (i in 1:dim(data)[1]){
    forward <- ForwardIterFull(data,1,i,initial_probabilities, transition,class_matrix)
    backward <- BackwardIterFull(data,1,i,transition,class_matrix)
    
    forward_sum <- numeric(length(k))
    backward_sum <- numeric(length(k))
    
    for (k in 1:length(forward)){
      forward_sum <- forward_sum + forward[[k]][[1]] 
    }
    for (k in 1:length(backward)){
      backward_sum <- backward_sum + backward[[k]][[1]] 
    }
    
    mover <- forward_sum * backward_sum * (1 - pi_0) / CalcIndLikelihood(data,i,initial_probabilities,transition,class_matrix,pi_0)
    workaround <- rbind(workaround,mover)
    
  }
  workaround <- workaround[-1,]
  # return(workaround)
  
  for (i in 1:length(freq_vec)){
    prob_list <- prob_list + (workaround[i,] * freq_vec[i])
  }
  
  return(prob_list/sum(prob_list))
} 



CalcTransitionPattern <- function(data, initial, transition,class,freq_vec, pi_0){
  k <- numOfStates(data)
  tran_matrix <- matrix(0L, nrow = length(k), ncol = length(k))
  
  for (ind in 1:dim(data)[1]){
    forward <- ForwardIterFull(data, dim(data)[2], ind, initial, transition, class)
    backward <- BackwardIterFull(data, 1, ind, transition, class)
    for (time in 2:dim(data)[2]){
      tran_matrix_current <- matrix(0L, nrow = length(k), ncol = length(k))
      possible_x <- unique(GetAllPossibilitiesBackw(data, time, ind)[1,])
      
      if (time != 5){current_backw_length <- dim(GetAllPossibilitiesBackw(data,time+1,ind))[2]} 
      else {current_backw_length <- 1}
      
      current_forw_length <- dim(GetAllPossibilitiesForw(data,time-1,ind))[2]
      denom <- CalcIndLikelihood(data,ind,initial,transition,class, pi_0)
      
      
      forward_sum <- numeric(length(k))
      for (u in 1:current_forw_length){
        forward_sum <- forward_sum + forward[[u]][[time-1]]
      }
      
      backward_sum <- numeric(length(k))
      for (v in 1:current_backw_length){
        backward_sum <- backward_sum + backward[[v]][[time]]
      }
      
      if (time == dim(data)[2]){
        backward_sum <- rep(1, length(k))
      }
      
      for(gX in possible_x){
        for (initial_state in 1:length(k)){
          for(new_state in 1:length(k)){
            
            num <- forward_sum[[initial_state]] * backward_sum[[new_state]] * transition[initial_state,new_state] * Classification(gX,new_state-1,class) * (1 - pi_0)
            if(denom != 0){
              tran_matrix_current[initial_state,new_state] <- tran_matrix_current[initial_state,new_state] + (num/denom)
            } 
          }
        }
      }
      tran_matrix <- tran_matrix + ((tran_matrix_current) * freq_vec[ind])
    }
  }
  
  for (i in 1:dim(tran_matrix)[1]){
    if (i %% 2 == 0){
      for (j in 1:dim(tran_matrix)[2]){
        if (i == j){
          tran_matrix[i,j] <- 1
        } else {
          tran_matrix[i,j] <- 0
        }
      }
    }
  }
  
  return(Normalize(tran_matrix))
}



CalcClassificationPattern <- function(data,init,tran,class, freq_vec, pi_0){
  k <- numOfStates(data)
  num_matrix_full <- matrix(0L, nrow = length(k), ncol = length(k))
  
  for (ind in 1:dim(data)[1]){
    
    for(time in 1:dim(data)[2]){
      num_matrix <- matrix(0L, nrow = length(k), ncol = length(k))
      
      forward <- ForwardIterFull(data,time,ind,init, tran,class)
      backward <- BackwardIterFull(data,time,ind,tran,class)
      individual_likelihood <- CalcIndLikelihoodFast(forward,backward,time)
      beggining_forw <- GetAllPossibilitiesForw(data,time,ind)
      if (time != dim(data)[2]){
        ending_backw <- GetAllPossibilitiesBackw(data,(time+1),ind)
      }
      x_vals <- unique(beggining_forw[time,])
      
      for (u in 1:length(forward)){
        x_val <- beggining_forw[time,u]
        for (v in 1:length(backward)){
          if (!is.na(x_val)){
            
            if (time != dim(data)[2]){
              chain <- c(beggining_forw[,u], as.vector(ending_backw[,v]))
            } else {
              chain <- beggining_forw[,u]
            }
            
            mover <- (forward[[u]][[time]]*backward[[v]][[time]] * (1 - pi_0) / individual_likelihood)
            
            stayer <- (pi_0 * CalcProdIndAlt(chain,class)) / individual_likelihood
            
            mover_stayer <- mover
            mover_stayer[1] <- mover_stayer[1] + stayer
            
            for (y_state in 1:length(k)){   
              num_matrix[y_state,x_val + 1] <- num_matrix[y_state, x_val+1] + mover_stayer[y_state]
            }
          }
        }
      }
      
      
      #Is this where the normalization should be
      if (sum(num_matrix) != 0){
        num_matrix <- num_matrix/sum(num_matrix)
      }
      num_matrix_full <- num_matrix_full + (num_matrix * freq_vec[ind])
      
    }
  }
  
  for (i in 1:dim(num_matrix_full)[1]){
    for (j in 1:dim(num_matrix_full)[2]){
      if(i %% 2 == 1 & j %% 2 == 0){
        num_matrix_full[i,j] <- 0
      }
    }
  }
  
  return(Normalize(num_matrix_full))
}



all_patterns <- GetPatterns(ALTS)
unique_patterns <- as.data.frame(table(all_patterns))
ALTS_pattern_list <- Pattern2Data(unique_patterns)

ALTS_pattern <- ALTS_pattern_list[[1]]
freq_vec <- ALTS_pattern_list[[2]]

load("EMSlow.rda")

init_count<- InitialProb(ALTS_pattern,freq_vec)
tran_count <- TransitionProb(ALTS_pattern,freq_vec)
init <- obs_parameters[[1]]
tran <- obs_parameters[[2]]
class <- obs_parameters[[3]]
pi_0 <- obs_parameters[[4]]

old_likelihood <- CalcLikelihoodPattern(ALTS_pattern, init, tran, class, freq_vec,pi_0)
pi_0 <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)
init <- CalcInitialPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
tran <- CalcTransitionPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
class <- CalcClassificationPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
new_likelihood <- CalcLikelihoodPattern(ALTS_pattern, init, tran, class, freq_vec,pi_0)

while(abs(new_likelihood - old_likelihood) > .00001){
  old_likelihood <- new_likelihood
  pi_0 <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  init <- CalcInitialPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  tran <- CalcTransitionPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  class <- CalcClassificationPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  new_likelihood <- CalcLikelihoodPattern(ALTS_pattern, init, tran, class, freq_vec,pi_0)
  print(new_likelihood - old_likelihood)
}

obs_parameters <- list(init,tran,class, pi_0)
name <- paste("EMSlowQuick.rda")
save(obs_parameters, file = name)

while(abs(new_likelihood - old_likelihood) > .000001){
  old_likelihood <- new_likelihood
  pi_0 <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  init <- CalcInitialPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  tran <- CalcTransitionPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  class <- CalcClassificationPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  new_likelihood <- CalcLikelihoodPattern(ALTS_pattern, init, tran, class, freq_vec,pi_0)
  print(new_likelihood - old_likelihood)
}

obs_parameters <- list(init,tran,class, pi_0)
name <- paste("EMSlowNormal.rda")
save(obs_parameters, file = name)


while(abs(new_likelihood - old_likelihood) > .0000001){
  old_likelihood <- new_likelihood
  pi_0 <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  init <- CalcInitialPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  tran <- CalcTransitionPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  class <- CalcClassificationPattern(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  new_likelihood <- CalcLikelihoodPattern(ALTS_pattern, init, tran, class, freq_vec,pi_0)
  print(new_likelihood - old_likelihood)
}

obs_parameters <- list(init,tran,class, pi_0)
name <- paste("EMSlowSlow.rda")
save(obs_parameters, file = name)





