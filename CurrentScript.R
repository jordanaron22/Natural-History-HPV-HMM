
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
    return(c(2 * val2 + val3, 4 + 2 * val2 + val3, 8 + 2 * val2 + val3))
  } else if (!is.na(val1) & is.na(val2) & is.na(val3)){
    return(c(4 * val1, 4 * val1 + 1, 4 * val1 + 2, 4 * val1 + 3))
  }else if (is.na(val1) & !is.na(val2) & is.na(val3)){
    return(c(2 * val2, 2 * val2 + 1, 2 * val2 + 4, 2 * val2 + 5, 2 * val2 + 8, 2 * val2 + 9))
  }else if (is.na(val1) & is.na(val2) & !is.na(val3)){
    return(c(val3, val3 + 2, val3 + 4, val3 + 6, val3 + 8, val3 + 10))
  } else {
    return(NA)
  }
}




#3 -> (1,2)
IntroducePersistence <- function(hpv){
  for (i in 1:dim(hpv)[1]){
    for (j in 1:dim(hpv)[2]){
      if (!is.na(hpv[i,j])){
        if (j == 1){hpv[i,j] <- persitenceHelper(hpv[i,j], NA, NA)}
        else {hpv[i,j] <- persitenceHelper(hpv[i,j], hpv[i,j-1], NA)}
      }
    }
  }
  
  hpv_pers <- array(NA, dim = c(dim(hpv)[1], dim(hpv)[2], 3))
  for (i in 1:dim(hpv)[1]){
    for (j in 1:dim(hpv)[2]){
      if (!is.na(hpv[i,j])){
        if (hpv[i,j] < 3){hpv_pers[i,j,1] <- hpv[i,j]}
        else if (hpv[i,j] == 3){
          hpv_pers[i,j,1] <- 1
          hpv_pers[i,j,2] <- 2
        }
      }
    }
  }
  return(list(hpv_pers,hpv))
}



persitenceHelper <- function(hpv_val, one_back, two_back){
  if (hpv_val == 0){return(0)}
  
  else if (!is.na(one_back)){
    if (one_back == 0){return(1)}
    else if (one_back == 1){return(2)} 
    else if (one_back == 2){return(2)} 
    else if (one_back == 3){return(2)}
  }
  else{return(3)}
  
}



Combiner <- function(hpv, cyt, copa){
  full_data <- array(NA, dim = c(dim(hpv)[1], dim(hpv)[2], 8))
  for (i in 1:dim(hpv)[1]){
    for (j in 1:dim(hpv)[2]){
      vals <- c()
      num_of_hpv <- sum(!is.na(hpv[i,j,]))
      
      if (num_of_hpv == 0 | num_of_hpv == 1){
        vals <- StateCalculator(hpv[i,j,1], cyt[i,j], copa[i,j])
      } else {
        for (val_index in 1:num_of_hpv){
          vals <- c(vals, StateCalculator(hpv[i,j,val_index], cyt[i,j], copa[i,j]))
          vals <- sort(unique(vals))
        }
      }
      
      for (part_val in 1:length(vals)){
        full_data[i,j,part_val] <- vals[part_val]
      }
    }
  }
  return(full_data)
}




# setwd("Data/")
ALTS_og <- read.csv("lsil.risk.file.y2018m10d17.csv")
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

hpv <- matrix(NA,n,max_time)
cyt <- matrix(NA,n,max_time)
copa <- matrix(NA,n,max_time)
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
  
  cyt_val <- ALTS_og[i,3]
  copa_val <- ALTS_og[i,4]
  hpv_val <- ALTS_og[i,5]
  
  if(Censoring == T){
    hpv_val <- NA
    cyt_val <- NA
    copa_val <- NA
  }
  
  hpv[real_i,real_time] <- hpv_val
  cyt[real_i,real_time] <- cyt_val
  copa[real_i,real_time] <- copa_val
  
  if (!is.na(ALTS_og[i,4])){
    if (ALTS_og[i,4] == 1){
      Censoring <- T
    }
  }
}

hpv_og <- hpv
hpvs <- IntroducePersistence(hpv)
hpv_pers <- hpvs[[1]]
hpv <- hpvs[[2]]
ALTS <- Combiner(hpv_pers,cyt,copa)



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
    return(c(2 * val2 + val3, 4 + 2 * val2 + val3, 8 + 2 * val2 + val3))
  } else if (!is.na(val1) & is.na(val2) & is.na(val3)){
    return(c(4 * val1, 4 * val1 + 1, 4 * val1 + 2, 4 * val1 + 3))
  }else if (is.na(val1) & !is.na(val2) & is.na(val3)){
    return(c(2 * val2, 2 * val2 + 1, 2 * val2 + 4, 2 * val2 + 5, 2 * val2 + 8, 2 * val2 + 9))
  }else if (is.na(val1) & is.na(val2) & !is.na(val3)){
    return(c(val3, val3 + 2, val3 + 4, val3 + 6, val3 + 8, val3 + 10))
  } else {
    return(NA)
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



Classification <- function(x_val,y_val,class){
  if (is.na(x_val)){
    return(1)
  }
  return(class[[y_val+1,x_val+1]])
}



numOfStates <- function(data,type){
  if (missing(type)){
    return(c(0:11))
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
          states_val <- NA
        }
        val <- paste0(val, states_val,",")
      }
    }
    val <- substr(val, 1, nchar(val)-1)
    vals[i] <- val
  }
  return(vals)
}



Pattern2Data <- function(unique_patterns){
  time_length <- 5
  n <- length(unique_patterns[[1]])
  partial_len <- 8
  freq_vec <- numeric(n)
  state_array <- array(0, dim = c(n,time_length,partial_len))
  for (i in 1:n){
    freq_vec[i] <- unique_patterns$Freq[[i]]
    pattern <- unique_patterns$all_patterns[[i]]
    
    pattern <- strsplit(pattern, split = ",")[[1]]
    for (time in 1:time_length){
      state_array[i,time,] <- suppressWarnings(as.integer(pattern[(((time-1)*partial_len)+1):(time*partial_len)]))
    }
  }
  
  return(list(state_array, freq_vec))
}




InitialProbTrue <- function(data, stayer_vec){
  init <- numeric(length(numOfStates(data)))
  for (i in 1:dim(data)[1]){
    if (stayer_vec[i] == 0 & !is.na(data[i,1])){
      init[data[i,1]+1] <- init[data[i,1]+1] + 1
    }
  }
  return(init)
}



InitialProb <- function(data, freq_vec, stayer_vec = rep(0,length(freq_vec))){
  init <- numeric(length(numOfStates(data)))
  for (i in 1:dim(data)[1]){
    if (stayer_vec[i] == 0){
      num_of_parts <- sum(!is.na(data[i,1,]))
      if (!is.na(data[i,1,1])){
        for (j in data[i,1,]){
          if (!is.na(j)){
            init[j+1] <- init[j+1] + freq_vec[i]/num_of_parts
          }
          
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
              if (!is.na(k1) & !is.na(k2)){
                tran[k1+1,k2+1] <- tran[k1+1,k2+1] + freq_vec[i]/num_of_parts
              }
              
            }
          }
        }
      }
    }
  }
  for (i in 1:dim(tran)[1]){
    if (i %% 2 == 0){
      for (j in 1:dim(tran)[2]){
        if (j %% 2 == 1){
          tran[i,j] <- 0
        }
      }
    }
  }
  
  tran[1:4,9:12] <- 0
  tran[5:12,5:8] <- 0
  return(tran)
}



TransitionProbTrue <- function(data,stayer_vec){
  k <- length(numOfStates(data))
  tran <- matrix(0,k,k)
  for (i in 1:dim(data)[1]){
    if (stayer_vec[i] == 0){
      for (j in 2:dim(data)[2]){
        if (!is.na(data[i,j-1]) & !is.na(data[i,j])){
          tran[data[i,j-1]+1,data[i,j]+1] <- tran[data[i,j-1]+1,data[i,j]+1] + 1
        }
      }
    }
  }
  return(tran)
}



GetAllPossibilitiesForw <- function(data, time){
  
  for (curr_time in 1:time){
    
    if (curr_time == 1){
      possibilities <- data[1,]
      if (sum(!is.na(possibilities)) == 0){
        possibilities <- NA
      } else {
        possibilities <- possibilities[!is.na(possibilities)]
      }
      possibilities <- t(as.matrix(possibilities))
      
    } else {
      current_possibilities <- data[curr_time,]
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



GetAllPossibilitiesBackw <- function(data, time){
  max_time <- dim(data)[1]
  for (curr_time in time:max_time){
    if (curr_time == time){
      possibilities <- data[curr_time,]
      if (sum(!is.na(possibilities)) == 0){
        possibilities <- NA
      } else {
        possibilities <- possibilities[!is.na(possibilities)]
      }
      possibilities <- t(as.matrix(possibilities))
      
    } else {
      current_possibilities <- data[curr_time,]
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
ForwardIterFull <- function(data,time,init,tran,class){
  k <- numOfStates()
  all_pos_obs <- GetAllPossibilitiesForw(data,time)
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
        alpha_matrix[[1]][i] <- init[i] * Classification(pos_ob[1],k[i],class)
      } else {
        alpha_matrix[[1]][i] <- init[i]
      }
    }
    
    if (time > 1){
      for (i in 2:time){
        for (j in 1:length(k)){
          if (!is.na(pos_ob[i])){
            alpha_matrix[[i]][j] <- (alpha_matrix[[i-1]] %*% tran[,j]) *  Classification(pos_ob[i],k[j],class)
          } else {
            alpha_matrix[[i]][j] <- (alpha_matrix[[i-1]] %*% tran[,j])
          }
        }
      }
    }
    
    alpha_matrix_pos[[pos]] <- alpha_matrix
    
  }
  
  return(alpha_matrix_pos)
}



BackwardIterFull <- function(data,time,tran,class){
  k <- numOfStates()
  length <- dim(data)[1]
  
  #Works since Beta only uses X_t+1
  if (time == 5){
    adj_time <- time
  } else{
    adj_time <- time + 1
  }
  
  all_pos_obs <- GetAllPossibilitiesBackw(data,adj_time)
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
            misclass_vector[l] <- Classification(pos_ob[i+1],k[l],class)
          }
          for (j in 1:length(k)){
            beta_matrix[[i]][j] <- sum(beta_matrix[[i+1]] * tran[j,] * misclass_vector,na.rm = TRUE)
          }
        } else {
          for (j in 1:length(k)){
            if (sum(beta_matrix[[i+1]]) == length(k)){
              beta_matrix[[i]][j] <- 1
            } else {
              beta_matrix[[i]][j] <- sum(beta_matrix[[i+1]] * tran[j,]) 
            }
          }
        }
      }
    }
    
    beta_matrix_pos[[pos]] <- beta_matrix
  }
  
  return(beta_matrix_pos)
}



ForwardIterFullInd <- function(data, ind, time, init, tran, class){
  return(ForwardIterFull(data[ind,,], time, init, tran, class))
}



BackwardIterFullInd <- function(data, ind, time, tran, class){
  return(BackwardIterFull(data[ind,,], time, tran, class))
}



CalcProdInd <- function(data,class){
  prod_total <- 0
  partial_data_full <- GetAllPossibilitiesForw(data,dim(data)[1])
  for (chain in 1:dim(partial_data_full)[2]){
    prod <- 1
    partial_data <- partial_data_full[,chain]
    for (j in 1:length(partial_data)){  
      if (!is.na(partial_data[j])){
        prod <- prod * Classification(partial_data[j], 0, class)
      }
    }
    prod_total <- prod_total + prod
  }
  return(prod_total)
}
 


CalcProdIndAlt <- function(data,class){
  prod <- 1
  for (j in 1:length(data)){
    if (!is.na(data[j])){
      prod <- prod * Classification(data[j], 0, class)
    }
  }
  return(prod)
}



CalcIndLikelihood <- function(data, init,tran,class,pi){
  
  forward <- ForwardIterFull(data,2,init,tran,class)
  backward <- BackwardIterFull(data,2,tran,class)
  
  
  
  forward_sum <- numeric(length(numOfStates()))
  for (u in 1:length(forward)){
    forward_sum <- forward_sum + forward[[u]][[2]]
  }
  
  backward_sum <- numeric(length(numOfStates()))
  for (v in 1:length(backward)){
    backward_sum <- backward_sum + backward[[v]][[2]]
  }
  
  
  individual_likelihood <- (forward_sum %*% backward_sum)[[1]]
  
  likelihood <- (pi * CalcProdInd(data,class)) + ((1 - pi) * individual_likelihood)
  # if (likelihood == 0){
  #   print(individual)
  # }
  return(likelihood)
}



CalcLikelihoodPattern <- function(data, init,tran,class, freq_vec, pi_0){
  
  CalcIndLikelihoodHelper <- function(data) {
    return(log(CalcIndLikelihood(data,init,tran,class,pi_0)))
  }
  
  return((apply(data,1,FUN = CalcIndLikelihoodHelper) %*% freq_vec_sim)[[1]])
  
  
}



CalcStayer <- function(data,init,tran,class,freq_vec,pi_0){
  stayer_vec <- numeric(dim(data)[1])
  
  CalcStayerHelper <- function(data){
    return((pi_0 * CalcProdInd(data,class) / CalcIndLikelihood(data,init,tran,class,pi_0)))
  }
  
  pi_0 <- (apply(data,1,FUN = CalcStayerHelper) %*% freq_vec)/sum(freq_vec)
  return(pi_0[1])
}



CalcInitialPattern <- function(data,freq_vec, pi_0, forw, backw, likelihoods){
  k <- numOfStates(data)
  prob_list <- numeric(length(k))
  
  # forw <- apply(data,1,ForwardIterFull, time = 1, init,tran,class)
  # backw <- apply(data,1,FUN = BackwardIterFull, time = 1, tran, class)
  # likelihoods <- apply(data,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)
  
  
  
  for (i in 1:dim(data)[1]){
    forward <- forw[[i]]
    backward <- backw[[i]]
    ind_likelihood <- likelihoods[[i]]
    
    forward_sum <- numeric(length(k))
    backward_sum <- numeric(length(k))
    
    forw_length <- dim(GetAllPossibilitiesForw(data[i,,],1))[2]
    
    for (k in 1:forw_length){
      forward_sum <- forward_sum + forward[[k]][[1]] 
    }
    for (k in 1:length(backward)){
      backward_sum <- backward_sum + backward[[k]][[1]] 
    }
    
    mover <- forward_sum * backward_sum * (1 - pi_0) / ind_likelihood
    prob_list <- prob_list + (mover * freq_vec[i])
  }
  return(prob_list/sum(prob_list))
} 



CalcTransitionPattern <- function(data, tran,class,freq_vec, pi_0, forw, backw, likelihoods){
  
  # forw <- apply(data,1,ForwardIterFull, time = 4, init,tran,class)
  # backw <- apply(data,1,FUN = BackwardIterFull, time = 2, tran, class)
  # likelihoods <- apply(data,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)
  
  k <- numOfStates(data)
  tran_matrix <- matrix(0L, nrow = length(k), ncol = length(k))
  possible_initial_states <- c(1,3,5,7,9,11)
  possible_initial_states_copa <- c(2,4,6,8,10,12)
  
  for (ind in 1:dim(data)[1]){
    forward <- forw[[ind]]
    backward <- backw[[ind]]
    denom <- likelihoods[[ind]]
    for (time in 2:dim(data)[2]){
      tran_matrix_current <- matrix(0L, nrow = length(k), ncol = length(k))
      possible_x <- unique(GetAllPossibilitiesBackw(data[ind,,], time)[1,])
      
      if (time != 5){current_backw_length <- dim(GetAllPossibilitiesBackw(data[ind,,],time+1))[2]} 
      else {current_backw_length <- 1}
      
      current_forw_length <- dim(GetAllPossibilitiesForw(data[ind,,],time-1))[2]
      
      
      forward_sum <- numeric(length(k))
      backward_sum <- numeric(length(k))
      
      
      for (u in 1:current_forw_length){
        forward_sum <- forward_sum + forward[[u]][[time-1]]
      }
      
      
      if (time == dim(data)[2]){
        backward_sum <- rep(1, length(k))
      } else {
        for (v in 1:current_backw_length){
          backward_sum <- backward_sum + backward[[v]][[time]]
        }
      }
      
      for(gX in possible_x){
        
        for (initial_state in possible_initial_states){
          if (initial_state < 4){possible_new_states <- c(1:8)}
          else {possible_new_states <- c(1:4,9:12)}
          
          for(new_state in possible_new_states){
            
            num <- forward_sum[[initial_state]] * backward_sum[[new_state]] * tran[initial_state,new_state] * Classification(gX,new_state-1,class) * (1 - pi_0)
            if(denom != 0){
              tran_matrix_current[initial_state,new_state] <- tran_matrix_current[initial_state,new_state] + (num/denom)
            } 
          }
        }
        
        for (initial_state in possible_initial_states_copa){
          if (initial_state < 5){possible_new_states <- c(2,4,6,8)}
          else {possible_new_states <- c(2,4,10,12)}
          
          for(new_state in possible_new_states){
            
            num <- forward_sum[[initial_state]] * backward_sum[[new_state]] * tran[initial_state,new_state] * Classification(gX,new_state-1,class) * (1 - pi_0)
            if(denom != 0){
              tran_matrix_current[initial_state,new_state] <- tran_matrix_current[initial_state,new_state] + (num/denom)
            } 
          }
        }
        
        
      }
      tran_matrix <- tran_matrix + ((tran_matrix_current) * freq_vec[ind])
    }
  }
  return(Normalize(tran_matrix))
}



CalcClassificationPattern <- function(data,class, freq_vec, forw, backw, likelihoods, pi_0){
  k <- numOfStates(data)
  num_matrix_full <- matrix(0L, nrow = length(k), ncol = length(k))
  # forw <- apply(data,1,ForwardIterFull, time = 5, init,tran,class)
  # backw <- apply(data,1,FUN = BackwardIterFull, time = 1, tran, class)
  # likelihoods <- apply(data,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)
  for (ind in 1:dim(data)[1]){
    
    forward <- forw[[ind]]
    backward <- backw[[ind]]
    individual_likelihood <- likelihoods[[ind]]
    
    for(time in 1:dim(data)[2]){
      num_matrix <- matrix(0L, nrow = length(k), ncol = length(k))
      
      if (time != dim(data)[2]){current_backw_length <- dim(GetAllPossibilitiesBackw(data[ind,,],time+1))[2]} 
      else {current_backw_length <- 1}
      current_forw_length <- dim(GetAllPossibilitiesForw(data[ind,,],time))[2]
      
      
      
      beggining_forw <- GetAllPossibilitiesForw(data[ind,,],time)
      if (time != dim(data)[2]){
        ending_backw <- GetAllPossibilitiesBackw(data[ind,,],(time+1))
      }
      x_vals <- unique(beggining_forw[time,])
      
      for (u in 1:current_forw_length){
        x_val <- beggining_forw[time,u]
        for (v in 1:current_backw_length){
          if (!is.na(x_val)){
            
            if (time != dim(data)[2]){
              chain <- c(beggining_forw[,u], as.vector(ending_backw[,v]))
            } else {
              chain <- beggining_forw[,u]
            }
            
            mover <- (forward[[u]][[time]]*backward[[v]][[time]] * (1 - pi_0) / individual_likelihood)
            if (individual_likelihood == 0){
              print(ind)
            }
            
            stayer <- (pi_0 * CalcProdIndAlt(chain,class)) / individual_likelihood
            
            mover_stayer <- mover
            mover_stayer[1] <- mover_stayer[1] + stayer
            
            for (y_state in 1:length(k)){   
              num_matrix[y_state,x_val + 1] <- num_matrix[y_state, x_val+1] + mover_stayer[y_state]
            }
          }
        }
      }
      
      
      #Normalization for partial
      if (sum(num_matrix) != 0){
        num_matrix <- num_matrix/sum(num_matrix)
      }
      num_matrix_full <- num_matrix_full + (num_matrix * freq_vec[ind])
      
    }
  }
  
  return(Normalize(num_matrix_full))
}

ClassHC <- function(){
  class_hc <- matrix(0,12,12)
  for (i in 1:12){
    if ((i-1) %% 2 == 0){
      class_hc[i,i] <- .95
      if ((((i-1) %% 4) %/% 2) == 0){
        class_hc[i,i+2] <- 1 - class_hc[i,i]
      }  else {
        class_hc[i,i-2] <- 1 - class_hc[i,i]
      }
    }else {
      class_hc[i,i] <- .95
      if ((((i-1) %% 4) %/% 2) == 0){
        class_hc[i,i-1] <- (1 - class_hc[i,i]) / 3
        class_hc[i,i+1] <- (1 - class_hc[i,i]) / 3
        class_hc[i,i+2] <- (1 - class_hc[i,i]) / 3
      } else {
        class_hc[i,i-1] <- (1 - class_hc[i,i]) / 3
        class_hc[i,i-2] <- (1 - class_hc[i,i]) / 3
        class_hc[i,i-3] <- (1 - class_hc[i,i]) / 3
      }
    }
  }
  return(class_hc)
}



IntroduceNoise <- function(parameter, epsilon){
  if (is.vector(parameter)){
    for (i in 1:length(parameter)){
      new_val <- runif(1,parameter[i] - epsilon, parameter[i] + epsilon)
      if (new_val < .01){new_val <- .01}
      else if (new_val > 1){new_val <- 1}
      parameter[i] <- new_val
    }
  } else {
    for (i in 1:dim(parameter)[1]){
      for (j in 1:dim(parameter)[2]){
        if (parameter[i,j] != 0 & parameter[i,j] != 1){
          new_val <- runif(1,parameter[i,j] - epsilon, parameter[i,j] + epsilon)
          if (new_val < .01){new_val <- .01}
          else if (new_val > 1){new_val <- 1}
          parameter[i,j] <- new_val
        }
      }
    }
    
  }
  
  return(parameter)
}



all_patterns <- GetPatterns(ALTS)
unique_patterns <- as.data.frame(table(all_patterns), stringsAsFactors = F)
ALTS_pattern_list <- Pattern2Data(unique_patterns)

ALTS_pattern <- ALTS_pattern_list[[1]]
freq_vec <- ALTS_pattern_list[[2]]


load("parameters.rda")

init_count <- InitialProb(ALTS_pattern,freq_vec)
init <- init_count/(sum(init_count))
init <- IntroduceNoise(init,.02) 
init <- init/sum(init)
init <- parameters[[1]]

tran_count <- TransitionProb(ALTS_pattern,freq_vec)
tran <- Normalize(tran_count)
tran <- IntroduceNoise(tran,.02) 
tran <- Normalize(tran)
tran <- parameters[[2]]

class <- ClassHC()
rand1 <- runif(1,-.2,.2)
rand2 <- runif(1,-.2,.2)
class[1,1] <- class[1,1] - .2 + rand1
class[1,3] <- class[1,3] + .2 - rand1
class[3,3] <- class[3,3] - .2 + rand2
class[3,1] <- class[3,1] + .2 - rand2
class <- parameters[[3]]
class <- IntroduceNoise(class,.02)
class_og <- class
class <- Normalize(class)
class_new <- class

pi_0_og <- runif(1,0,.05)
pi_0_og <- parameters[[4]] + runif(1,-.03,.03)
pi_0 <- pi_0_og

init_og <- init
tran_og <- tran



rand_name <- do.call(paste0, replicate(25, sample(LETTERS, 1, TRUE), FALSE))

library(parallel)
library(foreach)
library(tictoc)
library(doParallel)

tic()
tic()
no_cores <- 4
cl <- makeCluster(no_cores)

clusterExport(cl, c("numOfStates", "GetAllPossibilitiesForw", "Classification"))
clusterExport(cl, c("GetAllPossibilitiesBackw"))
clusterExport(cl, c("ForwardIterFull", "BackwardIterFull", "CalcProdInd"))

forw <- parApply(cl, ALTS_pattern,1,ForwardIterFull, time = 5, init,tran,class)
backw <- parApply(cl, ALTS_pattern,1,FUN = BackwardIterFull, time = 1, tran, class)
likelihoods <- parApply(cl, ALTS_pattern,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)


old_likelihood <- (log(likelihoods) %*% freq_vec)[1]

init_new <- CalcInitialPattern(ALTS_pattern,freq_vec,pi_0,forw,backw,likelihoods)
tran_new <- CalcTransitionPattern(ALTS_pattern,tran,class,freq_vec,pi_0, forw, backw, likelihoods)
# class_new <- CalcClassificationPatternNoStayer(ALTS_pattern,class,freq_vec, forw, backw, likelihoods)

# class_new <- CalcClassificationPattern(ALTS_pattern,class,freq_vec, forw, backw, likelihoods,pi_0)
pi_0_new <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)


init <- init_new
tran <- tran_new
class <- class_new
pi_0 <- pi_0_new


likelihoods_new <- parApply(cl, ALTS_pattern,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)

new_likelihood <- (log(likelihoods_new) %*% freq_vec)[1]
print(new_likelihood - old_likelihood)

while ((new_likelihood - old_likelihood) > .000001){
  forw <- parApply(cl, ALTS_pattern,1,ForwardIterFull, time = 5, init,tran,class)
  backw <- parApply(cl, ALTS_pattern,1,FUN = BackwardIterFull, time = 1, tran, class)
  likelihoods <- likelihoods_new
  
  old_likelihood <- new_likelihood
  
  init_new <- CalcInitialPattern(ALTS_pattern,freq_vec,pi_0,forw,backw,likelihoods)
  tran_new <- CalcTransitionPattern(ALTS_pattern,tran,class,freq_vec,pi_0, forw, backw, likelihoods)
  # class_new <- CalcClassificationPatternNoStayer(ALTS_pattern,class,freq_vec, forw, backw, likelihoods)
  
  # class_new <- CalcClassificationPattern(ALTS_pattern,class,freq_vec, forw, backw, likelihoods,pi_0)
  pi_0_new <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  
  
  init <- init_new
  tran <- tran_new
  class <- class_new
  pi_0 <- pi_0_new
  
  likelihoods_new <- parApply(cl, ALTS_pattern,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)
  
  new_likelihood <- (log(likelihoods_new) %*% freq_vec)[1]
  print(new_likelihood - old_likelihood)
}
first_time <- toc()
first_elapsed <- first_time$toc - first_time$tic
initial_parameters <- list(init_og,tran_og, class_og, pi_0_og)
estimated_parameters <- list(init, tran, class, pi_0)
to_save <- list(initial_parameters,estimated_parameters, new_likelihood, first_elapsed)
name <- paste("AAAASmallVarNoClassFromParameters",rand_name,".rda",sep="")
save(to_save, file = name)

forw <- parApply(cl, ALTS_pattern,1,ForwardIterFull, time = 5, init,tran,class)
backw <- parApply(cl, ALTS_pattern,1,FUN = BackwardIterFull, time = 1, tran, class)
likelihoods <- likelihoods_new

old_likelihood <- new_likelihood

init_new <- CalcInitialPattern(ALTS_pattern,freq_vec,pi_0,forw,backw,likelihoods)
tran_new <- CalcTransitionPattern(ALTS_pattern,tran,class,freq_vec,pi_0, forw, backw, likelihoods)
# class_new <- CalcClassificationPatternNoStayer(ALTS_pattern,class,freq_vec, forw, backw, likelihoods)

class_new <- CalcClassificationPattern(ALTS_pattern,class,freq_vec, forw, backw, likelihoods,pi_0)
pi_0_new <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)


init <- init_new
tran <- tran_new
class <- class_new
pi_0 <- pi_0_new

likelihoods_new <- parApply(cl, ALTS_pattern,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)

new_likelihood <- (log(likelihoods_new) %*% freq_vec)[1]
print(new_likelihood - old_likelihood)

while ((new_likelihood - old_likelihood) > .000001){
  forw <- parApply(cl, ALTS_pattern,1,ForwardIterFull, time = 5, init,tran,class)
  backw <- parApply(cl, ALTS_pattern,1,FUN = BackwardIterFull, time = 1, tran, class)
  likelihoods <- likelihoods_new
  
  old_likelihood <- new_likelihood
  
  init_new <- CalcInitialPattern(ALTS_pattern,freq_vec,pi_0,forw,backw,likelihoods)
  tran_new <- CalcTransitionPattern(ALTS_pattern,tran,class,freq_vec,pi_0, forw, backw, likelihoods)
  # class_new <- CalcClassificationPatternNoStayer(ALTS_pattern,class,freq_vec, forw, backw, likelihoods)
  
  class_new <- CalcClassificationPattern(ALTS_pattern,class,freq_vec, forw, backw, likelihoods,pi_0)
  pi_0_new <- CalcStayer(ALTS_pattern,init,tran,class,freq_vec,pi_0)
  
  
  init <- init_new
  tran <- tran_new
  class <- class_new
  pi_0 <- pi_0_new
  
  likelihoods_new <- parApply(cl, ALTS_pattern,1,FUN = CalcIndLikelihood, init, tran, class,pi_0)
  
  new_likelihood <- (log(likelihoods_new) %*% freq_vec)[1]
  print(new_likelihood - old_likelihood)
}

second_time <- toc()
second_elapsed <- second_time$toc - second_time$tic

initial_parameters <- list(init_og,tran_og, class_og, pi_0_og)
estimated_parameters <- list(init, tran, class, pi_0)
to_save <- list(initial_parameters,estimated_parameters, new_likelihood, second_elapsed)
name <- paste("FFFFSmallVarEM",rand_name,".rda",sep="")
save(to_save, file = name)



stopCluster(cl)
