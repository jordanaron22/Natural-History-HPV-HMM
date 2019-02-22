
numOfStates <- function(data,type){
  ###
  if (missing(type)){
    return (c(min(data, na.rm = T):max(data, na.rm = T)))
  }
  else if (type == 1){
    return (c(0:3))
  } else if (type == 2){
    return(c(0:2))
  } else if (type == 3){
    return (c(0:1))
  }else if (type == 4){
    return(c(0:23))
  }
  
}



Classification <- function(x_val,y_val,class_matrix){
  if (is.na(x_val)){
    return(1)
  }
  return(class_matrix[[y_val+1,x_val+1]])
}



Normalize <- function(data){
  for (i in 1:dim(data)[1]){
    if (sum(data[i,],na.rm = TRUE) != 0){
      data[i,] <- data[i,]/sum(data[i,])
    }
  }
  return(data)
}



#takes in 3 classification matrix and outputs a combined classification matrix 
FullClassificationMatrix <- function(HPV,Pap,Copa){
  l1 <- dim(HPV)[1]
  l2 <- dim(Pap)[1]
  l3 <- dim(Copa)[1]
  row_index <- 0
  col_index <- 0
  
  full_matrix <- matrix(0L, nrow = l1*l2*l3, ncol = l1*l2*l3)
  
  for (i in 1:dim(HPV)[1]){
    for (j in 1:dim(Pap)[1]){
      for (k in 1:dim(Copa)[1]){
        for (i2 in 1:dim(HPV)[2]){
          for (j2 in 1:dim(Pap)[2]){
            for (k2 in 1:dim(Copa)[2]){
              row_index <- ((i-1)*l2*l3) + ((j-1)*l3) + k
              col_index <- ((i2-1)*l2*l3) + ((j2-1)*l3) + k2
              
              full_matrix[row_index,col_index] <-full_matrix[row_index,col_index] + (HPV[i,i2] * Pap[j,j2] * Copa[k,k2])
            }
          }
        }
      }
    }
  }
  return(full_matrix)
}



GetPatterns <- function(HPVX,PapX,CopaX, stayer_vec){
  library(plyr)
  options(scipen = 999)
  vals <- rep(NaN,dim(HPVX)[1])
  for (i in 1:dim(HPVX)[1]){
    val <- ""
    for(j in 1:dim(HPVX)[2]){
      
      if (is.na(HPVX[i,j])){
        HPVX_val <- 8
      } else {
        HPVX_val <- HPVX[i,j] 
      }
      
      if (is.na(PapX[i,j])){
        PapX_val <- 3
      } else {
        PapX_val <- PapX[i,j] 
      }
      
      if (is.na(CopaX[i,j])){
        CopaX_val <- 2
      } else {
        CopaX_val <- CopaX[i,j] 
      }
      triplet <- paste0(HPVX_val,PapX_val,CopaX_val)
      val <- paste0(val, triplet)
    }
    
    if (!missing(stayer_vec)){
      val <- paste0(val,stayer_vec[i])
    }
    
    vals[i] <- val
  }
  return(vals)
}



alterPap <- function(data){
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      if (!is.na(data[i,j])){
        if (data[i,j] == 2) {data[i,j] <- 1}
        if (data[i,j] > 2) {data[i,j] <- 2}
      }
    }
  }
  return(data)
}



alterCopa <- function(data){
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      if (!is.na(data[i,j])){
        if (data[i,j] < 2) {data[i,j] <- 0}
        if (data[i,j] > 1) {data[i,j] <- 1}
      }
    }
  }
  return(data)
}



# 0 -> 0
# 1 -> 1
# 2 -> 2 
# 3 -> 3
# 4 -> 1,2,3
# 5 -> 2,3
# 6 -> 1,2
# 7 -> 1,3
# 8 -> NaN
alterHPV <- function(data){
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      if (!is.na(data[i,j])){
        if (data[i,j] == 1){
          
          if (j == 1){
            data[i,j] <- 4
          }
          
          else if ( (j - 1) > 0 & !is.na(data[i,j-1])){
            if (data[i,j-1] == 0){data[i,j] <- 1}
            if (data[i,j-1] == 1){data[i,j] <- 2}
            if (data[i,j-1] == 2){data[i,j] <- 3}
            if (data[i,j-1] == 3){data[i,j] <- 3}
            if (data[i,j-1] == 4){data[i,j] <- 5}
            if (data[i,j-1] == 5){data[i,j] <- 3}
            if (data[i,j-1] == 6){data[i,j] <- 5}
            if (data[i,j-1] == 7){data[i,j] <- 5}
          }
          
          else if (j == 2){
            data[i,j] <- 4
          }
          
          else if ( (j - 2) > 0 & !is.na(data[i,j-2])){
            if (data[i,j-2] == 0){data[i,j] <- 6}
            if (data[i,j-2] == 1){data[i,j] <- 7}
            if (data[i,j-2] == 2){data[i,j] <- 7}
            if (data[i,j-2] == 3){data[i,j] <- 7}
            if (data[i,j-2] == 4){data[i,j] <- 7}
            if (data[i,j-2] == 5){data[i,j] <- 7}
            if (data[i,j-2] == 6){data[i,j] <- 7}
            if (data[i,j-2] == 7){data[i,j] <- 7}
          }
          
          else {
            data[i,j] <- 4
          }
        }
      }
    }
  }
  return(data)
}



ReduceStateSpace <- function(data){
  HPV <- alterHPV(data[,8:12])
  Pap <- alterPap(data[,13:17])
  Copa <- alterCopa(data[,18:22])
  
  return(list(HPV,Pap,Copa))
}



createSubData <- function(rows, data_2429, all){
  library(dplyr)
  if (missing(all)){
    # data <- vector("list", length = 2)
    # data <- split(data_2429,data_2429$GL_MED_BMI>25)
    # data_2429 <- data[[2]]
    data_2429_altered <- sample_n(data_2429,rows)
  } else {
    data_2429_altered <- data_2429
  }
  
  altered_data <- ReduceStateSpace(data_2429_altered)
  
  
  HPV <- altered_data[[1]]
  Pap <- altered_data[[2]]
  Copa <- altered_data[[3]]
  
  for (i in 1:dim(Copa)[1]){
    for (j in 1:dim(Copa)[2]){
      if (!is.na(Copa[i,j]) & Copa[i,j] == 1){
        
        if (j < dim(Copa)[2]){
          Copa[i,(j+1):dim(Copa)[2]] <- NaN
          Pap[i,(j+1):dim(Copa)[2]] <- NaN
          HPV[i,(j+1):dim(Copa)[2]] <- NaN
          
        }
      }
    }
  }
  
  return(list(HPV,Pap,Copa))
}



CombinerHelper <- function(HPV,Pap,Copa,k1,k2,k3,dimen){
  state_space = rep(NaN,dimen)
  if (is.na(HPV)){HPV_miss <- 1}
  else {HPV_miss <- 0}
  
  if (is.na(Pap)){Pap_miss <- 1}
  else {Pap_miss <- 0}
  
  if (is.na(Copa)){Copa_miss <- 1}
  else {Copa_miss <- 0}
  
  if (HPV_miss == 1 & Pap_miss == 0 & Copa_miss == 0){
    base_val <- Pap*length(k3) + Copa
    for (i in 1:length(k1)){
      state_space[i] <- base_val + (length(k2) * length(k3) * k1[i])
    }
  }
  
  else if (HPV_miss == 0 & Pap_miss == 1 & Copa_miss == 0){
    base_val <- HPV*length(k2)*length(k3) + Copa
    for (i in 1:length(k2)){
      state_space[i] <- base_val + (length(k3) * k2[i])
    }
  }
  
  else if (HPV_miss == 0 & Pap_miss == 0 & Copa_miss == 1){
    if(HPV == 0  & Pap == 0){
      state_space[1] <- 0
    } else if(HPV == 0 & Pap == 1){
      state_space[1] <- 2
    } else {
      base_val <- HPV*length(k2)*length(k3) + Pap*length(k3)
      for (i in 1:length(k3)){
        state_space[i] <- base_val + (k3[i])
      }
    }
  }
  
  else if (HPV_miss == 1 & Pap_miss == 1 & Copa_miss == 0){
    base_val <- Copa
    for (i in 1:length(k1)){
      for (j in 1:length(k2)){
        state_space[((i-1)*length(k2) + j)] <- base_val + (length(k2) * length(k3) * k1[i]) + (length(k3) * k2[j])
      }
    }
  }
  
  else if (HPV_miss == 1 & Pap_miss == 0 & Copa_miss == 1){
    base_val <- Pap * length(k3)
    for (i in 1:length(k1)){
      for (j in 1:length(k3)){
        state_space[((i-1)*length(k3) + j)] <- base_val + (length(k2) * length(k3) * k1[i]) + (k3[j])
      }
    }
  }
  
  else if (HPV_miss == 0 & Pap_miss == 1 & Copa_miss == 1){
    
    base_val <- HPV * length(k2) * length(k3)
    for (i in 1:length(k2)){
      for (j in 1:length(k3)){
        state_space[((i-1)*length(k3) + j)] <- base_val + (length(k3) * k2[i]) + (k3[j])
      }
    }
  }
  
  #No missing
  else {
    state_space[1] <- (HPV * length(k2) * length(k3)) + (Pap * length(k3)) + Copa
  }
  
  #deals with HPV 4,5,6,7
  if (HPV_miss == 0){
    state_space <- CombinerHelperHPV(HPV, state_space)
  }
  
  return(state_space)
}



#Deals with HPV 4,5,6,7
CombinerHelperHPV <- function(HPV_val, state_space){
  new_ss <- state_space
  state_len <- length(state_space)
  base <- state_space[!is.na(state_space)]
  base_len <- length(base)
  if (HPV_val == 4){
    base1 <- base - 18
    base2 <- base - 12
    base3 <- base - 6
    new_ss <- c(base1,base2,base3, rep(NaN, state_len - (3*base_len)))
  }
  
  if (HPV_val == 5){
    base1 <- base - 18
    base2 <- base - 12
    new_ss <- c(base1,base2, rep(NaN, state_len - (2*base_len)))
  }
  
  if (HPV_val == 6){
    base1 <- base - 30
    base2 <- base - 24
    new_ss <- c(base1,base2, rep(NaN, state_len - (2*base_len)))
  }
  
  if (HPV_val == 7){
    base1 <- base - 36
    base2 <- base - 24
    new_ss <- c(base1,base2, rep(NaN, state_len - (2*base_len)))
  }
  
  return (new_ss)
}




Combiner <- function(HPV,Pap,Copa){
  l1 <- length(numOfStates(HPV,1))[[1]]
  k1 <- numOfStates(HPV,1)
  l2 <- length(numOfStates(Pap,2))
  k2 <- numOfStates(Pap,2)
  l3 <- length(numOfStates(Copa, 3))
  k3 <- numOfStates(Copa, 3)
  dimen <- max(l1*l2,l1*l3,l2,l3)
  dimen <- 18
  full_matrix <- array(0,c(dim(HPV)[1],dim(HPV)[2],dimen))
  
  
  for (i in 1:dim(HPV)[1]){
    for (j in 1:dim(HPV)[2]){
      
      if (!is.na(HPV)[i,j] | !is.na(Pap)[i,j] | !is.na(Copa)[i,j]){
        state_space <- CombinerHelper(HPV[i,j],Pap[i,j],Copa[i,j],k1,k2,k3,dimen)
      } else {
        state_space <- rep(NaN,dimen)
      }
      
      for (k in 1:length(state_space)){
        full_matrix[i,j,k] <- state_space[k]
      }
      
    }
  }
  return(full_matrix)
}



Pattern2Data <- function(unique_patterns, time_length){
  if (missing(time_length)){time_length <- 5}
  n <- dim(unique_patterns)[1]
  data_array <- array(0, dim=c(n,time_length,18))
  freq_vec <- numeric(n)
  stayer_vec <- numeric(n)
  for (i in 1:n){
    freq_vec[i] <- unique_patterns$Freq[[i]]
    pattern <- unique_patterns$all_patterns[[i]]
    stayer_vec[i] <- as.integer(substr(pattern, 16,16))
    
    
    HPV_mat <- matrix(0, ncol=time_length,nrow=1)
    Pap_mat <- matrix(0, ncol=time_length,nrow=1)
    Copa_mat <- matrix(0, ncol=time_length,nrow=1)
    
    for (time in 1:time_length){
      visit <- substr(pattern, 3*time -2, 3*time)
      HPV_val <- substr(visit,1,1)
      Pap_val <- substr(visit,2,2)
      Copa_val <- substr(visit,3,3)
      
      if (HPV_val == 8){
        HPV_val <- NaN
      }
      if (Pap_val == 3){
        Pap_val <- NaN
      }
      if (Copa_val == 2){
        Copa_val <- NaN
      }
      HPV_mat[1,time] <- as.integer(HPV_val)
      Pap_mat[1,time] <- as.integer(Pap_val)
      Copa_mat[1,time] <- as.integer(Copa_val)
    }
    num_visit <- Combiner(HPV_mat,Pap_mat,Copa_mat)
    data_array[i,,] <- num_visit
  }
  return(list(data_array, freq_vec,stayer_vec))
}




InitialProbFull <- function(data, freq_vec,stayer_vec){
  k <- numOfStates(data)
  init_prob <- numeric(length(k))
  
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[3]){
      if (!is.na(data[i,1,j])){
        if (!missing(stayer_vec)){
          init_prob[data[i,1,j] + 1] <- (init_prob[data[i,1,j]+1] + freq_vec[i]) * (1 - stayer_vec[[i]])
        } else {
          init_prob[data[i,1,j] + 1] <- (init_prob[data[i,1,j]+1] + freq_vec[i])
        }
      }
    }
  }
  
  return(init_prob/sum(init_prob))
}



createTransitionFullPattern <- function(data, freq_vec,stayer_vec){
  
  n <- dim(data)[1]
  length <- dim(data)[2]
  k <- numOfStates(data)
  transition <- matrix(0L, nrow = length(k), ncol = length(k))
  
  for (j in 1:(length - 1)){
    for (i in 1:n){
      for (k1 in 1:length(data[i,j,])){
        for (k2 in 1:length(data[i,j+1,])){
          
          if (!is.na(data[i,j,k1]) & !is.na(data[i,j+1,k2])){
            if (!missing(stayer_vec)){
              transition[which(data[i,j,k1] == k),which(data[i,j+1,k2] == k)] = transition[which(data[i,j,k1] == k),which(data[i,j+1,k2] == k)] + (freq_vec[i] * (1 - stayer_vec[i]))
            } else {
              transition[which(data[i,j,k1] == k),which(data[i,j+1,k2] == k)] = (transition[which(data[i,j,k1] == k),which(data[i,j+1,k2] == k)] + freq_vec[i])
            }
            
          }
          
        }
      }
    }
  }
  
  return(transition)
}




ClassificationFull <- function(x_vals,y_val,class_matrix){
  bool_vals <- !is.na(x_vals)
  count <- 0
  for (i in 1:length(x_vals)){
    if (bool_vals[i]){
      count <- i
    }
  }
  return_val <- 0
  for(i in 1:count){
    return_val <- return_val + Classification(x_vals[i],y_val,class_matrix)
  }
  
  if (return_val == 0){
    return(1)
  }
  return(return_val)
}



#JASA paper method 
ForwardIterFull <- function(data,time,individual,initial_probabilities,transition,class_matrix){
  k <- numOfStates(data)
  alpha_matrix<-vector("list",time)
  alpha_i <- numeric(length(k))
  
  for (i in 1:time){
    alpha_matrix[[i]] <- alpha_i
  }
  
  for (i in 1:length(k)){
    if (!is.na(data[individual,1,1])){
      alpha_matrix[[1]][i] <- initial_probabilities[i] * ClassificationFull(data[individual,1,],k[i],class_matrix)
    } else {
      alpha_matrix[[1]][i] <- initial_probabilities[i]
    }
  }
  if (time == 1){
    return(alpha_matrix)
  }
  
  for (i in 2:time){
    for (j in 1:length(k)){
      if (!is.na(data[individual,i,1])){
        alpha_matrix[[i]][j] <- (alpha_matrix[[i-1]] %*% transition[,j]) *  ClassificationFull(data[individual,i,],k[j],class_matrix)
      } else {
        alpha_matrix[[i]][j] <- (alpha_matrix[[i-1]] %*% transition[,j])
      }
    }
  }
  return(alpha_matrix)
}






#JASA method 
BackwardIterFull <- function(data,time,individual,transition,class_matrix){
  k <- numOfStates(data)
  length <- dim(data)[2]
  beta_i <- numeric(length(k))
  misclass_vector <- numeric(length(k))
  beta_matrix<-vector("list",length)
  
  for (i in 1:length){
    beta_matrix[[i]] <- beta_i
  }
  
  for (i in 1:length(k)){
    beta_matrix[[length]][i] <- 1
  }
  if (time == length){
    return(beta_matrix)
  }
  for (i in (length-1):time){
    if (!is.na(data[individual,i+1,1])){
      for (l in 1:length(k)){
        misclass_vector[l] <- ClassificationFull(data[individual,i+1,],k[l],class_matrix)
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
  
  return(beta_matrix)
}





YgivenXi <- function(data, time, individual, initial_probabilities, transition, class_matrix,stayer_vec){
  max_time <- dim(data)[2]
  forward <- ForwardIterFull(data,max_time,individual,initial_probabilities, transition,class_matrix)
  backward <- BackwardIterFull(data,time,individual,transition,class_matrix)
  
  num <- forward[[time]] * backward[[time]] * (1 - stayer_vec[individual])
  denom <- CalcIndLikelihoodFast(data,individual,class_matrix,forward,stayer_vec)
  
  frac <- num/as.vector(denom)
  return(frac)
}




YgivenXiFast <- function(data,time,forward,backward,individual, class_matix,stayer_vec, freq_vec){
  num <- forward[[time]] * backward[[time]] * (1 - stayer_vec[individual]) * freq_vec[individual]
  denom <- CalcIndLikelihoodFast(data,individual,class_matrix,forward,stayer_vec)
  frac <- num/as.vector(denom)
  return(frac)
}



CalcYiPattern <- function(data,time, initial_probabilities, transition, class_matrix, freq_vec,stayer_vec){
  stayer_vec <- rep(0,length(freq_vec))
  k <- numOfStates(data)
  prob_list <- numeric(length(k))
  
  for (i in 1:dim(data)[1]){
    forward <- ForwardIterFull(data,5,i,initial_probabilities, transition,class_matrix)
    backward <- BackwardIterFull(data,1,i,transition,class_matrix)
    
    
    mover <- forward[[time]] * backward[[time]] * (1 - stayer_vec[i])  / CalcIndLikelihoodFast(data,i,class_matrix,forward,stayer_vec)
    
    
    
    for (j in k){
      prob_list[j+1] <- prob_list[j+1] + (mover[j+1] * freq_vec[i])
    }
  }
  return(prob_list/sum(prob_list))
} 






ExpectedTransitionFast <- function(data,time,individual,transition,initial_state,new_state,forward,backward,class_matrix,stayer_vec){
  HPV_init <- initial_state %/% 6
  HPV_new <- new_state %/% 6
  
  if ((initial_state) %% 2 == 1 & (new_state) %% 2 == 0){
    return(0)
  }
  if (HPV_init == 0 & HPV_new > 1){
    return(0)
  } 
  if (HPV_init == 1 & (HPV_new == 3|HPV_new == 1)){
    return(0)
  }
  if (HPV_init == 2 & (HPV_new == 1|HPV_new == 2)){
    return(0)
  }
  if(HPV_init == 3 & (HPV_new == 1|HPV_new == 2)){
    return(0)
  }
  
  num <- forward[[time-1]][initial_state+1] * backward[[time]][new_state+1] * (1 - stayer_vec[individual])
  if (!is.na(data[individual,time,1])){
    num <- num * transition[initial_state+1,new_state+1] * ClassificationFull(data[individual,time,],new_state,class_matrix)
  } else {
    num <- num * transition[initial_state+1,new_state+1]
  }
  denom <- CalcIndLikelihoodFast(data,individual,class_matrix,forward,stayer_vec)
  return(num/denom)
}



CalcTransitionPattern <- function(data,initial_probabilities,transition,class_matrix, freq_vec, stayer_vec){
  k <- numOfStates(data)
  num_matrix <- matrix(0L, nrow = length(k), ncol = length(k))
  
  for (i in 1:dim(data)[1]){
    forward <- ForwardIterFull(data,dim(data)[2],i,initial_probabilities, transition,class_matrix)
    backward <- BackwardIterFull(data,1,i,transition,class_matrix)
    for (j in 2:dim(data)[2]){
      for (l in 1:length(k)){
        for (m in 1:length(k)){
          if (is.na(ExpectedTransitionFast(data,j,i,transition,k[l],k[m],forward,backward,class_matrix,stayer_vec))){
          }
          num_matrix[l,m] <- num_matrix[l,m] + (ExpectedTransitionFast(data,j,i,transition,k[l],k[m],forward,backward,class_matrix,stayer_vec) * freq_vec[i])
        }
      }
    }
  }
  
  
  
  
  return(Normalize(num_matrix))
}




CalcClassificationPattern <- function(data, initial_probabilities, transition, class_matrix, freq_vec, stayer_vec){
  k <- c(0:23)
  num_matrix <- matrix(0L, nrow = length(k), ncol = length(k))
  
  for (i in 1:dim(data)[1]){
    forward <- ForwardIterFull(data,dim(data)[2],i,initial_probabilities, transition,class_matrix)
    backward <- BackwardIterFull(data,1,i,transition,class_matrix)
    individual_likelihood <- CalcIndLikelihoodFast(data,i,class_matrix,forward,stayer_vec)
    
    for(j in 1:dim(data)[2]){
      mover <- (forward[[j]] * backward[[j]] * (1 - stayer_vec[i]))  / individual_likelihood
      stayer <- (stayer_vec[i] * CalcStayerInd(data,i,class_matrix)) / individual_likelihood 
      mover_stayer <- mover
      mover_stayer[1] <- mover_stayer[1] + stayer
      probabilities <- mover_stayer * freq_vec[i]
      
      for(y_state in 1:length(k)){
        for (l in 1:length(data[i,j,])){
          if (!is.na(data[i,j,l])){
            num_matrix[y_state,data[i,j,l]+1] <- num_matrix[y_state,data[i,j,l]+1] + probabilities[y_state]
          }
        }
      }
    }
  }
  
  for (i in 1:dim(num_matrix)[1]){
    for(j in 1:dim(num_matrix)[2]){
      if (i%%2 == 1 & j%%2 == 0){
        num_matrix[i,j] <- 0
      }
    }
  }
  
  return(Normalize(num_matrix))
}



CalcLikelihoodPattern <- function(data, initial_probabilities,transition,class_matrix, freq_vec, stayer_vec){
  likelihood_sum <- 0
  for (i in 1:dim(data)[1]){
    likelihood <- log(CalcIndLikelihood(data,i,initial_probabilities,transition,class_matrix,stayer_vec)) * freq_vec[i]
    likelihood_sum <- likelihood_sum + likelihood
  }
  return(likelihood_sum)
}




CalcIndLikelihood <- function(data, individual, initial_probabilities,transition,class_matrix, stayer_vec){
  stay_lik <- CalcStayerInd(data,individual,class_matrix)
  if (stay_lik == 0){
    if(stayer_vec[individual] > .95){
      print("Propensity to stay but 0 likelihood of staying")
    }
  }
  
  
  forward <- ForwardIterFull(data,dim(data)[2],individual,initial_probabilities,transition,class_matrix)
  forward_sum <- sum(forward[[dim(data)[2]]])
  
  
  
  likelihood <- ((stayer_vec[individual] * stay_lik) + ((1 - stayer_vec[individual]) * forward_sum)) 
  return(likelihood)
}




CalcIndLikelihoodFast <- function(data, individual,class_matrix,forward, stayer_vec){
  stay_lik <- CalcStayerInd(data,individual,class_matrix)
  
  forward_sum <- sum(forward[[dim(data)[2]]])
  # if(stayer_vec[individual] == 0 & forward_sum == 0){
  #   print("Uh Oh")
  # }
  
  likelihood <- ((stayer_vec[individual] * stay_lik) + ((1 - stayer_vec[individual]) * forward_sum)) 
  return(likelihood)
}



CalcStayerInd <- function(data,individual,class_matrix){
  prod <- 1
  for (j in 1:dim(data)[2]){  
    if (!is.na(data[individual,j,1])){
      prod <- prod * ClassificationFull(data[individual,j,], 0, class_matrix)
    }
  }
  
  return(prod)
}



CalcStayer <- function(data, initial_pronbabilities, transition,class_matrix, stayer_vec){
  for (i in 1:length(stayer_vec)){
    new_stayer <- CalcStayerInd(data,i,class_matrix)
    likelihood <- CalcIndLikelihood(data,i,initial_pronbabilities,transition,class_matrix,stayer_vec)
    
    stayer_vec[i] <- stayer_vec[i] * new_stayer / likelihood
  }
  return(stayer_vec)
}

##########################################


Normalize <- function(data){
  for (i in 1:dim(data)[1]){
    if (sum(data[i,],na.rm = TRUE) != 0){
      data[i,] <- data[i,]/sum(data[i,])
    }
  }
  return(data)
}



numOfStates <- function(data,type){
  if (missing(type)){
    return(c(0:2))
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



GenerateData <- function(n,t,p){
  data_mat <- matrix(0L, nrow = n, ncol = t)
  stayer_vec <- rbinom(n,1,p)
  
  for (i in 1:n){
    if (stayer_vec[i] == 0){
      data_mat[i,1] <- which(rmultinom(1,1,init_hc) == 1) - 1
      for (j in 2:t){
        p_current <- tran_hc[data_mat[i,j-1]+1,]
        data_mat[i,j] <- which(rmultinom(1,1,p_current) == 1) - 1
      }
    }
  }
  return(list(data_mat, stayer_vec))
}



GenerateObs <- function(data_true){
  data_obs <- matrix(0L, nrow = dim(data_true)[1], ncol = dim(data_true)[2])
  for (i in 1:dim(data_true)[1]){
    for (j in 1:dim(data_true)[2]){
      if(!is.na(data_true)[i,j]){
        p_current <- class_hc[data_true[i,j]+1,]
        new_val <- which(rmultinom(1,1,p_current) == 1) - 1
        data_obs[i,j] <- new_val
      }
    }
  }
  return(data_obs)
}



GenerateObsPartial <- function(data_true, data_obs){
  for (i in 1:dim(data_true)[1]){
    for (j in 1:dim(data_true)[2]){
      if (!is.na(data_obs[i,j]) & !is.na(data_true[i,j])){
        if (rbinom(1,1,3/10) == 1){
          if (data_true[i,j] == 0){
            rand <- runif(1)
            if (rand < (class_hc[1,1] + class_hc[1,2])/2){
              data_obs[i,j] <- 3
            } else if (rand < (class_hc[1,1] + class_hc[1,2] + class_hc[1,1] + class_hc[1,3])/2) {
              data_obs[i,j] <- 4
            } else {
              data_obs[i,j] <- 5
            }
          } else if (data_true[i,j] == 1){
            rand <- runif(1)
            if (rand < (class_hc[2,1] + class_hc[2,2])/2) {
              data_obs[i,j] <- 3
            } else if(rand < (class_hc[2,1] + class_hc[2,2] + class_hc[2,2] + class_hc[2,3])/2) {
              data_obs[i,j] <- 5
            } else {
              data_obs[i,j] <- 4
            }
          } else {
            rand <- runif(1)
            if (rand < (class_hc[3,2] + class_hc[3,3])/2) {
              data_obs[i,j] <- 5
            } else if(rand < (class_hc[3,2] + class_hc[3,3] + class_hc[3,3] + class_hc[3,1])/2) {
              data_obs[i,j] <- 4
            } else {
              data_obs[i,j] <- 3
            }
          }
        }
      }
    }
  }
  return(data_obs)
}




IntroNA <- function(data, obs, class, data_t){
  
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      
      if (!is.na(data[i,j])){
        if (obs == TRUE){
          #Probability to be missing
          if (rbinom(1,1,0) == 1){
            data[i,j] <- NaN
          }
        } 
      }
      
      if(!is.na(data[i,j])){
        if (data[i,j] == 2){
          if (j < dim(data)[2]){
            data[i,(j+1):dim(data)[2]] <- NaN
          }
        }
      }
    }
  }
  return(data)
}



GetPatterns <- function(states){
  library(plyr)
  options(scipen = 999)
  vals <- rep(NaN,dim(states)[1])
  for (i in 1:dim(states)[1]){
    val <- ""
    for(j in 1:dim(states)[2]){
      if (!is.na(states[i,j])){
        states_val <- states[i,j] 
      } else {
        states_val <- 6
      }
      val <- paste0(val, states_val)
    }
    
    vals[i] <- val
  }
  return(vals)
}



Pattern2Data <- function(unique_patterns){
  time_length <- 5
  n <- dim(unique_patterns)[1]
  freq_vec <- numeric(n)
  state_mat <- matrix(0,n,time_length)
  for (i in 1:n){
    freq_vec[i] <- unique_patterns$Freq[[i]]
    pattern <- unique_patterns$all_patterns[[i]]
    
    for (time in 1:time_length){
      new_val <- as.integer(substr(pattern, time,time))
      if (new_val == 6){
        new_val <- NaN
      }
      state_mat[i,time] <- new_val
    }
  }
  
  return(list(state_mat, freq_vec))
}




Data2Array <- function(data){
  data_array <- array(NaN, dim = c(dim(data)[1],dim(data)[2],2))
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      
      if (!is.na(data[i,j])){
        
        if (data[i,j] == 0 | data[i,j] == 1 | data[i,j] == 2){
          data_array[i,j,1] <- data[i,j]
        }
        else if (data[i,j] == 3){
          data_array[i,j,1] <- 0
          data_array[i,j,2] <- 1
        }
        else if (data[i,j] == 4){
          data_array[i,j,1] <- 0
          data_array[i,j,2] <- 2
        }
        else if (data[i,j] == 5){
          data_array[i,j,1] <- 1
          data_array[i,j,2] <- 2
        }
        
      } 
      
    }
  }
  return(data_array)
}



CalcTrueMisclassNoNA <- function(data,true_data){
  misclass_mat <- matrix(0,3,3)
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      if (!is.na(data[i,j]) & !is.na(true_data[i,j])){
        if (data[i,j] < 3){
          misclass_mat[true_data[i,j]+1,data[i,j]+1] <- misclass_mat[true_data[i,j]+1,data[i,j]+1] + 1
        } else if (data[i,j] == 3){
          misclass_mat[true_data[i,j]+1,1] <- misclass_mat[true_data[i,j]+1,1] + 1/2
          misclass_mat[true_data[i,j]+1,2] <- misclass_mat[true_data[i,j]+1,2] + 1/2
        } else if (data[i,j] == 4){
          misclass_mat[true_data[i,j]+1,1] <- misclass_mat[true_data[i,j]+1,1] + 1/2
          misclass_mat[true_data[i,j]+1,3] <- misclass_mat[true_data[i,j]+1,3] + 1/2
        } else if (data[i,j] == 5){
          misclass_mat[true_data[i,j]+1,2] <- misclass_mat[true_data[i,j]+1,2] + 1/2
          misclass_mat[true_data[i,j]+1,3] <- misclass_mat[true_data[i,j]+1,3] + 1/2
        }
        
      } 
    }
  }
  return(misclass_mat)
}



NoPattern <- function(data){
  data_array <- array(NaN,dim = c(dim(data)[1], dim(data)[2], 2))
  
  for (i in 1:dim(data)[1]){
    for (j in 1:dim(data)[2]){
      if (!is.na(data[i,j])){
        if (data[i,j] < 3){
          data_array[i,j,1] <- data[i,j]
        } else if (data[i,j] == 3){
          data_array[i,j,1] <- 0
          data_array[i,j,2] <- 1
        } else if (data[i,j] == 4){
          data_array[i,j,1] <- 0
          data_array[i,j,2] <- 2
        } else if (data[i,j] == 5){
          data_array[i,j,1] <- 1
          data_array[i,j,2] <- 2
        } 
      }
    }
  }
  return(data_array)
}



GetAllPossibilitiesForw <- function(data, time, individual, only_one){
  
  if (!missing(only_one)){
    return(matrix(data[individual,1:time]))
  }
  possibilities <- matrix(data[individual,1:time,1], nrow = time)
  for (i in 2:dim(data)[3]){
    for (j in 1:time){
      if (!is.na(data[individual,j,i])){
        to_add <- possibilities
        to_add[j,] <- data[individual,j,i]
        possibilities <- cbind(possibilities,to_add)
      }
    }
  }
  # rownames(possibilities) <- c(1:time)
  return(possibilities)
  
}




#JASA paper method 
ForwardIterFull <- function(data,time,individual,initial_probabilities,transition,class_matrix, only_one){
  k <- numOfStates(data)
  all_pos_obs <- GetAllPossibilitiesForw(data,time, individual,only_one)
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



GetAllPossibilitiesBackw <- function(data, time, individual){
  
  real_time <- time
  possibilities <- matrix(data[individual,time:dim(data)[2],1], nrow = (dim(data)[2] - time + 1))
  for (i in 2:dim(data)[3]){
    
    for (j in time:dim(data)[2]){
      if (!is.na(data[individual,j,i])){
        to_add <- possibilities
        to_add[(j - time + 1),] <- data[individual,j,i]
        possibilities <- cbind(possibilities,to_add)
      }
    }
  }
  rownames(possibilities) <- c(real_time:dim(data)[2])
  return(possibilities)
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



CalcLikelihoodPattern <- function(data, initial_probabilities,transition,class_matrix, freq_vec, pi_0){
  likelihood_sum <- 0
  for (i in 1:dim(data)[1]){
    likelihood <- log(CalcIndLikelihood(data,i,initial_probabilities,transition,class_matrix, pi_0)) * freq_vec[i]
    likelihood_sum <- likelihood_sum + likelihood
  }
  return(likelihood_sum)
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
  partial_data_full <- GetAllPossibilitiesForw(pattern_data,dim(data)[2],individual)
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
  
  pi_0 <- (stayer_vec %*% freq_vec)/n
  return(pi_0[1])
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
    
    mover <- forward_sum * backward_sum * (1 - pi_0) / CalcIndLikelihood(pattern_data,i,initial_probabilities,transition,class_matrix,pi_0)
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
  data2 <- data[1:2,,] 
  data2[,,] <- NaN
  k <- numOfStates(data)
  tran_matrix <- matrix(0L, nrow = length(k), ncol = length(k))
  
  for (ind in 1:dim(data)[1]){
    for (time in 2:dim(data)[2]){
      forward <- ForwardIterFull(data, time - 1, ind, initial, transition, class)
      backward <- BackwardIterFull(data, time, ind, transition, class)
      possible_x <- unique(GetAllPossibilitiesBackw(data, time, ind)[1,])
      
      for (u in 1:length(forward)){
        for(v in 1:length(backward)){
          for(gX in possible_x){
            
            denom <- CalcIndLikelihood(data,ind,initial,transition,class, pi_0)
            for (initial_state in 1:length(k)){
              for(new_state in 1:length(k)){
                
                num <- forward[[u]][[time-1]][[initial_state]] * backward[[v]][[time]][[new_state]] * transition[initial_state,new_state] * Classification(gX,new_state-1,class) * (1 - pi_0)
                if(denom == 0){
                  frac <- 0
                } else {
                  frac <- (num/denom)
                }
                tran_matrix[initial_state,new_state] <- tran_matrix[initial_state,new_state] + (frac * freq_vec[ind])
              }
            }
          }
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
      individual_likelihood <- CalcIndLikelihood(data,ind,init,tran,class,pi_0)
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
            
            if (u == length(forward) & v == length(backward)){
              # print(num_matrix)
              if (sum(num_matrix) != 0){
                num_matrix <- num_matrix/sum(num_matrix)
              }
              num_matrix_full <- num_matrix_full + (num_matrix * freq_vec[ind])
            }
            
            
            
          }
        }
      }
    }
  }
  return(Normalize(num_matrix_full))
}




init_hc <- c(.75,.25,0)

tran_hc <- t(matrix(c(.7,.25,.05,.2,.7,.1,0,0,1),3,3))

class_hc <- t(matrix(c(.95,0.04,.01,.05,.95,.01,0,.05,.95),3,3))


n <- 3000
t <- 5
p <- .6
data_true_full <- GenerateData(n,t,p)
data_true <- data_true_full[[1]]
stayer_vec_true <- data_true_full[[2]]
data_obs <- GenerateObs(data_true)

class_true_counts <- CalcTrueMisclassNoNA(data_obs,data_true)
class_true <- Normalize(class_true_counts)

data_obs <- GenerateObsPartial(data_true, data_obs)

class_true_counts_partial <- CalcTrueMisclassNoNA(data_obs,data_true)
class_true_partial <- Normalize(class_true_counts_partial)

data_obs_NA <- IntroNA(data_obs,T, class_hc, data_true)

data_true_NA <- IntroNA(data_true,F, class_hc)

all_patterns <- GetPatterns(data_obs_NA)
unique_patterns <- as.data.frame(table(all_patterns))

all_patterns_true <- GetPatterns(data_true_NA)
unique_patterns_true <- as.data.frame(table(all_patterns_true))

pattern_list <- Pattern2Data(unique_patterns)
pattern_data <- pattern_list[[1]]
pattern_data <- Data2Array(pattern_data)
freq_vec <- pattern_list[[2]]

pattern_list_true <- Pattern2Data(unique_patterns_true)
pattern_data_true <- pattern_list_true[[1]]
pattern_data_true <- Data2Array(pattern_data_true)
freq_vec_true <- pattern_list_true[[2]]
freq_vec_true[1] <- (freq_vec_true[1] - sum(stayer_vec_true))



init_true <- InitialProbFull(pattern_data_true, freq_vec_true)
tran_true_counts <- createTransitionFullPattern(pattern_data_true, freq_vec_true)
tran_true <- Normalize(tran_true_counts)
tran_true[3,3] <- 1

pi_0_true <- sum(stayer_vec_true)/n




init_init <- jitter(init_true, amount = .15)
init_init[init_init < 0] <- 0
init_init <- init_init/sum(init_init)
# 
tran_init <- jitter(tran_true, amount = .15 )
tran_init[tran_init < 0] <- 0
tran_init <- Normalize(tran_init)
# 
class_init <- jitter(class_true, amount = .15 )
class_init[class_init < 0] <- 0
class_init <- Normalize(class_init)




init <- init_init
tran <- tran_init
class <- class_init

pi_0 <- jitter(p, amount = .15)

old_likelihood <- CalcLikelihoodPattern(pattern_data,init,tran,class,freq_vec, pi_0)
init <- CalcInitialPattern(pattern_data,init,tran,class,freq_vec, pi_0)
tran <- CalcTransitionPattern(pattern_data,init,tran,class,freq_vec, pi_0)
class <- CalcClassificationPattern(pattern_data,init,tran,class, freq_vec, pi_0)
pi_0 <- CalcStayer(pattern_data, init, tran, class,freq_vec,pi_0)
new_likelihood <- CalcLikelihoodPattern(pattern_data,init,tran,class,freq_vec, pi_0)
print(new_likelihood - old_likelihood)

while((new_likelihood - old_likelihood ) > .0001){
  old_likelihood <- new_likelihood
  init <- CalcInitialPattern(pattern_data,init,tran,class,freq_vec, pi_0)
  tran <- CalcTransitionPattern(pattern_data,init,tran,class,freq_vec, pi_0)
  class <- CalcClassificationPattern(pattern_data,init,tran,class, freq_vec, pi_0)
  pi_0 <- CalcStayer(pattern_data, init, tran, class,freq_vec,pi_0)
  new_likelihood <- CalcLikelihoodPattern(pattern_data,init,tran,class,freq_vec, pi_0)
  print(new_likelihood - old_likelihood)
}


##########################################




obs_parameters <- list(init,tran,class, pi_0)
true_parameters <- list(init_true,tran_true,class_true,class_true_counts,pi_0_true)
datas <- list(pattern_data, pattern_data_true)
to_save <- list(obs_parameters, true_parameters,datas)
name <- paste("PartiallyMissing",do.call(paste0, replicate(15, sample(LETTERS, 1, TRUE), FALSE)),".rda",sep="")
save(to_save, file = name)