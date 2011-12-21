
sens_int_bounds <- function(treat, control, G){
  return(
    function(beta){
      effect_diffs <- (treat-beta) - control;
      abs_ranks <- rank(abs(effect_diffs))
      pos_diffs <- effect_diffs > 0
      neg_diffs <- effect_diffs < 0
      test_stat <- sum(pos_diffs * abs_ranks)
      rank_sum <- sum((effect_diffs != 0) * abs_ranks)
      E_plus <- rank_sum * G / (1 + G)
      E_minus <- rank_sum / (1 + G)
      SD <- sqrt(sum((effect_diffs != 0) * abs_ranks^2) * G / (1 + G)^2)
      c((test_stat - E_plus) / SD, (test_stat - E_minus) / SD)
    }
  )
}

alpha_ci <- function(betagrid, treat, control, Gamma, alpha, n_tests){
  D_plus_minus <- sapply(betagrid, sens_int_bounds(treat, control, Gamma))
  alpha_corrected <- alpha / 2 / n_tests
  z <- qnorm(alpha_corrected)
  upper <- max(betagrid[D_plus_minus[2,]>=z])
  lower <- min(betagrid[D_plus_minus[1,]<=-z])
  c(lower,upper)
}

betagrid <- seq(-abs(avg_diff), abs(avg_diff), 2*abs(avg_diff)/1000)
Gammas <- seq(1,5,0.01)

Gamma_min <- 1 #Smallest Gamma for which we have no effect

for (G in Gammas){
  CI <- alpha_ci(betagrid, Xm_trt_matched[, response_col], Xm_control_matched[, response_col], G, 0.05, num_comparisons)
  if (VERBOSE_GAMMA_COMPUTATIONS){
  	cat(sprintf("%f   SI for Gamma = %f: (%f, %f)\n", 95, G, CI[1], CI[2]))
  }
  if (0 >= CI[1] * CI[2] || (CI[1] == min(betagrid) && CI[2] == max(betagrid)) ){
    Gamma_min <- G
    break
  }  
}



#mult_comp_adj_gamma = 1
#print sensitivity result to screen
cat(paste("Sensitivity Analysis, Gamma =\n  ", Gamma_min, "\n\n\n", sep = ""))