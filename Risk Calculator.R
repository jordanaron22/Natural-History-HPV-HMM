RiskCalcInd <- function(init,tran,class,pi,HPV, cyt){
  p_latent <- (init %*% tran)
  p_latent <- (1 - pi) * p_latent
  p_latent[1,1] <- p_latent[1,1] + pi
  hist_pos <- c(2,4,6,8,10,12,14,16,18)
  
  
  possible_states <- c(HPV*6 + cyt*2 + 1, HPV*6 + cyt*2 + 2)
  running_num <- 0
  for (hist_state in hist_pos){
    running_class <- 0
    for (pos_state in possible_states){
      running_num <- running_num + p_latent[hist_state] * class[hist_state,pos_state]
    }
  }
  
  running_denom <- 0
  for (i in 1:18){
    running_class <- 0
    for (pos_state in possible_states){
      running_denom <- running_denom + p_latent[i] * class[i,pos_state]
    }
  }
  return(running_num/running_denom)
}

RiskCalc <- function(init, tran, class, pi){
  risk <- numeric(9)
  risk[1] <- RiskCalcInd(init,tran,class,pi,0,0)
  risk[2] <- RiskCalcInd(init,tran,class,pi,0,1)
  risk[3] <- RiskCalcInd(init,tran,class,pi,0,2)
  risk[4] <- RiskCalcInd(init,tran,class,pi,1,0)
  risk[5] <- RiskCalcInd(init,tran,class,pi,1,1)
  risk[6] <- RiskCalcInd(init,tran,class,pi,1,2)
  risk[7] <- RiskCalcInd(init,tran,class,pi,2,0)
  risk[8] <- RiskCalcInd(init,tran,class,pi,2,1)
  risk[9] <- RiskCalcInd(init,tran,class,pi,2,2)
  return(risk)
}

RiskCalc(est_init, est_tran, est_class, est_pi)