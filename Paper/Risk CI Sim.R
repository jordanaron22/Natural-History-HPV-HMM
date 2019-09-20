list_of_names <- list.files()
load(list_of_names[1])
states <- length(to_save[[1]][[1]])

RiskCalculatorLatent <- function(est_tran){
  est_risk_latent <- numeric(9)
  for (i in 1:9){
    est_risk_latent[i] <- sum((c(rep(0,((i-1) * 2)),1,rep(0,(17 - ((i-1) * 2)))) %*% est_tran %*% est_tran %*% est_tran %*% est_tran)[seq(2,18,2)])
  }
  return(est_risk_latent)
}

reps <- length(list_of_names)
count <- 1
risk_matrix_latent <- matrix(0,reps,states/2)
tran_list <- vector("list", length = reps)
tran_array <- array(0,dim = c(states,states,reps))

for (name in list_of_names){
  load(name)
  obs_parameters <- to_save[[1]]
  tran_list[[count]] <- obs_parameters[[2]]
  risk_matrix_latent[count,] <- RiskCalculatorLatent(tran_list[[count]])
  
  for (i in 1:states){
    for (j in 1:states){
      tran_array[i,j,count] <- tran_list[[count]][i,j]
    }
  }
  count <- count + 1
}

tran_avg <- matrix(0,states,states)

for (i in 1:states){
  for (j in 1:states){
    tran_avg[i,j] <- mean(tran_array[i,j,])
  }
}




setwd("..")
load("parameters18.rda")
true_risk_latent <- RiskCalculatorLatent(to_save[[2]][[2]])
est_risk_latent <- RiskCalculatorLatent(tran_avg)

risk_sd_latent <- numeric(states/2)
for (i in 1:(states/2)){
  risk_sd_latent[i] <- sqrt(var(risk_matrix_latent[,i]))
}


risk_ci_lb_latent <- est_risk_latent - (1.96 * risk_sd_latent)
risk_ci_ub_latent <- est_risk_latent + (1.96 * risk_sd_latent)

risk_ci_lb_latent[risk_ci_lb_latent < 0] <- 0
risk_ci_ub_latent[risk_ci_ub_latent > 1] <- 1

round(risk_ci_lb_latent,3)
round(true_risk_latent,3)
round(risk_ci_ub_latent,3)

(true_risk_latent > risk_ci_lb_latent) & (true_risk_latent < risk_ci_ub_latent)