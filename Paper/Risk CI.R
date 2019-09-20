# setwd("../Data/Bootstrap 12/")
# setwd("../Data/Bootstrap 18/")
list_of_names <- list.files()
load(list_of_names[1])
states <- length(to_save[[1]][[1]])

# RiskCalculator <- function(init, tran, class, pi, latent_states){
#   if (missing(latent_states)){
#     p_latent <- (init + init %*% tran + init %*% tran %*% tran + init %*% tran %*% tran %*% tran + init %*% tran %*% tran %*% tran %*% tran)
#     latent_states <- p_latent * 3488 
#     latent_states <- latent_states
#   }
#   risk <- numeric(states/2)
#   latent_states_colpo <- latent_states
#   latent_states_colpo[seq(1,states,2)] <- 0
#   obs_states <-  latent_states %*% class
#   obs_states_colpo <- latent_states_colpo %*% class
#   
#   for (i in 1:states/2){
#     risk[i] <-(obs_states_colpo[2*i - 1] + obs_states_colpo[2*i])/(obs_states[2*i - 1] + obs_states[2*i])
#   }
#   
#   return(risk)
#   
# }

RiskCalculatorLatent <- function(est_tran){
  est_risk_latent <- numeric(9)
  for (i in 1:9){
    est_risk_latent[i] <- sum((c(rep(0,((i-1) * 2)),1,rep(0,(17 - ((i-1) * 2)))) %*% est_tran %*% est_tran %*% est_tran %*% est_tran)[seq(2,18,2)])
  }
  return(est_risk_latent)
}



reps <- length(list_of_names)

count <- 1
# risk_matrix <- matrix(0,reps,states/2)
risk_matrix_latent <- matrix(0,reps,states/2)

for (name in list_of_names){
  
  
  load(name)
  parameters <- to_save[[2]]
  # est_init <- parameters[[1]]
  est_tran <- parameters[[2]]
  # est_class <- parameters[[3]]
  # est_pi <- parameters[[4]]
  # 
  # p_latent <- (est_init + est_init %*% est_tran + est_init %*% est_tran %*% est_tran + est_init %*% est_tran %*% est_tran %*% est_tran + est_init %*% est_tran %*% est_tran %*% est_tran %*% est_tran)
  # latent_states <- p_latent * 3488 
  # latent_states <- latent_states
  # 
  # risk_matrix[count,] <- RiskCalculator(est_init,est_tran,est_class,est_pi, latent_states)
  

  risk_matrix_latent[count,] <- RiskCalculatorLatent(est_tran)
  count <- count + 1
}
# if (states == 12){
#   setwd("..")
#   load("parameters12.rda")
#   est_risk <- RiskCalculator(parameters12[[2]][[1]],parameters12[[2]][[2]],parameters12[[2]][[3]],parameters12[[2]][[4]])
# } else if (states == 18){
  setwd("..")
  load("parameters18.rda")
  # est_risk <- RiskCalculator(to_save[[2]][[1]],to_save[[2]][[2]],to_save[[2]][[3]],to_save[[2]][[4]])
  est_risk_latent <- RiskCalculatorLatent(to_save[[2]][[2]])
# }


# risk_sd <- numeric(states/2)
risk_sd_latent <- numeric(states/2)
for (i in 1:(states/2)){
  # risk_sd[i] <- sqrt(var(risk_matrix[,i]))
  risk_sd_latent[i] <- sqrt(var(risk_matrix_latent[,i]))
}
##Observed
# risk_ci_lb <- est_risk - (1.96 * risk_sd)
# risk_ci_ub <- est_risk + (1.96 * risk_sd)
# 
# risk_ci_lb[risk_ci_lb < 0] <- 0
# risk_ci_ub[risk_ci_ub > 1] <- 1
# 
# round(risk_ci_lb,3)
# round(risk_ci_ub,3)


risk_ci_lb_latent <- est_risk_latent - (1.96 * risk_sd_latent)
risk_ci_ub_latent <- est_risk_latent + (1.96 * risk_sd_latent)

risk_ci_lb_latent[risk_ci_lb_latent < 0] <- 0
risk_ci_ub_latent[risk_ci_ub_latent > 1] <- 1

round(risk_ci_lb_latent,3)
round(est_risk_latent,3)
round(risk_ci_ub_latent,3)

(est_risk_latent > risk_ci_lb_latent) & (est_risk_latent < risk_ci_ub_latent)
