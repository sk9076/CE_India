pacman::p_load(dplyr, tidyverse, ggplot2, here, rio, ggthemr, EnvStats)

calc_ppv <- function(sen, spec, prev){
  return((sen*prev)/((sen*prev)+(1-spec)*(1-prev)))
}

calc_npv <- function(sen, spec, prev){
  return((spec*(1-prev))/((spec*(1-prev)+(1-sen)*prev)))  
}


myrbeta<- function(n = 1, mean, lower, upper){
  sd = (upper-lower)/(2*1.96)
  
  alpha = (1-mean)/(sd^2)-mean
  beta = alpha*(1-mean)/mean
  print(sprintf("alpha = %.2f, beta = %.2f", alpha, beta))
  
  rbeta(n, alpha, beta)
}

myrgamma <- function(n = 1, mean, sd){
  #sd = (upper-lower)/(2*1.96)
  
  shape = (mean/sd)^2
  rate = mean/(sd^2)
  print(sprintf("shape = %.2f, rate = %.2f", shape, rate))
  rgamma(n, shape, rate)
}

### Arm 1 # of people in each end node

# Malaria
malaria_branch <- function(
    n_pop, # cohort size
    p_true_pos, # malaria prevalence
    prev,
    sen, # sensitivity
    spec, # specificity
    p_trt_eff, # treatment effectiveness
    p_sev_no_trt, # proportion of severe cases when not treated
    p_sev_trt,
    p_mort, # all-cause mortality
    p_neuroseq, # probability of neurosequelae among severe cases
    p_sev_mort){ # severe malaria case mortality
  
  
  p_ppv <- (sen*p_true_pos) / ((sen*p_true_pos) + (1-spec)*(1-prev))
    ## positive malaria, treated, uncomplicated, recover
    br1  <- n_pop*p_true_pos*p_ppv*(1-p_trt_eff)*(1-p_sev_no_trt)*(1-p_mort)
    ## positive malaria,treated, uncomplicated, die
    br2  <- n_pop*p_true_pos*p_ppv*(1-p_trt_eff)*(1-p_sev_no_trt)*p_mort
    ## positive malaria, treated, severe, neurological sequelae
    br3  <- n_pop*p_true_pos*p_ppv*(1-p_trt_eff)*p_sev_no_trt*p_neuroseq
    ## positive malaria, treated, severe, die
    br4  <- n_pop*p_true_pos*p_ppv*(1-p_trt_eff)*p_sev_no_trt*p_sev_mort
    ## positive malaria, treated, severe, recover
    br5  <- n_pop*p_true_pos*p_ppv*(1-p_trt_eff)*p_sev_no_trt*(1-p_neuroseq - p_sev_mort)
    ## positive malaria, treated, uncomplicated, recover
    br6  <- n_pop*p_true_pos*p_ppv*p_trt_eff*(1-p_sev_trt)*(1-p_mort)
    ## positive malaria, treated, uncomplicated, die
    br7  <- n_pop*p_true_pos*p_ppv*p_trt_eff*(1-p_sev_trt)*p_mort
    ## positive malaria, treated, severe, neuro sequelae
    br8  <- n_pop*p_true_pos*p_ppv*p_trt_eff*p_sev_trt*p_neuroseq
    ## positive malaria, treated, severe, die
    br9  <- n_pop*p_true_pos*p_ppv*p_trt_eff*p_sev_trt*p_sev_mort
    ## positive malaria, treated, severe, recover
    br10 <- n_pop*p_true_pos*p_ppv*p_trt_eff*p_sev_trt*(1-p_neuroseq - p_sev_mort)
    ## positive malaria, not treated, uncomplicated, die
    br11 <- n_pop*p_true_pos*(1-p_ppv)*(1-p_sev_no_trt)*p_mort
    ## positive malaria, not treated, uncomplicated, recover
    br12 <- n_pop*p_true_pos*(1-p_ppv)*(1-p_sev_no_trt)*(1-p_mort)
    ## positive malaria, not treated, severe, neurosequelae
    br13 <- n_pop*p_true_pos*(1-p_ppv)*p_sev_no_trt*p_neuroseq
    ## positive malaria, not treated, severe, die
    br14 <- n_pop*p_true_pos*(1-p_ppv)*p_sev_no_trt*p_sev_mort
    ## positive malaria, not treated, severe, recover
    br15 <- n_pop*p_true_pos*(1-p_ppv)*p_sev_no_trt*(1-p_sev_mort - p_neuroseq)

    return(
      data.frame(br1, br2, br3, br4, br5, br6, br7, br8, br9, br10, br11, br12, br13, br14, br15)
    )
}

# No malaria
no_malaria_branch <- function(
    n_pop, # cohort size
    p_true_pos, # malaria prevalence
    sen, # sensitivity
    spec, # specificity
    p_mort # all-cause mortality
    ){
  
  p_npv <- spec*(1-p_true_pos)/(spec*(1-p_true_pos) + (1-sen)*p_true_pos)
  
    ## not malaria, treated, die
    br16 <- n_pop*(1-p_true_pos)*(1-p_npv)*p_mort
    ## not malaria, treated, live
    br17 <- n_pop*(1-p_true_pos)*(1-p_npv)*(1-p_mort)
    ## not malaria, not treated, die
    br18 <- n_pop*(1-p_true_pos)*p_npv*p_mort
    ## not malaria, not treated, live
    br19 <- n_pop*(1-p_true_pos)*p_npv*(1-p_mort)

    return(
      data.frame(br16, br17, br18, br19)
    )
}

#sum(br1, br2, br3, br4, br5, br6, br7, br8, br9, br10, 
#    br11, br12, br13, br14, br15, br16, br17, br18, br19) == n_pop



run_tree <- function(n_pop,
                     prev,
                     p_pf,
                     p_pv,
                     p_co,
                     p_hrp2_deletion,
                     
                     arm_sen_pf,
                     arm_spec_pf,
                     arm_sen_pf_del,
                     arm_spec_pf_del,
                     arm_sen_pv,
                     arm_spec_pv,
                     arm_sen_co,
                     arm_spec_co,
                     p_trt_eff,
                     p_sev_no_trt,
                     p_sev_trt,
                     p_mort,
                     p_neuroseq,
                     p_sev_mort
){
  
  # pf hrp2+
  arm_mal_pf <- malaria_branch(
    n_pop = n_pop,
    p_true_pos = prev*p_pf*(1-p_hrp2_deletion),
    prev = prev,
    sen = arm_sen_pf,
    spec = arm_spec_pf,
    p_trt_eff = p_trt_eff, # treatment effectiveness
    p_sev_no_trt = p_sev_no_trt, # proportion of severe cases when not treated
    p_sev_trt = p_sev_trt,
    p_mort = p_mort, # all-cause mortality
    p_neuroseq = p_neuroseq, # probability of neurosequelae among severe cases
    p_sev_mort = p_sev_mort
  )
  
  
  # pf hrp2-
  arm_mal_pf_hrp2_del <- malaria_branch(
    n_pop = n_pop,
    p_true_pos = prev*p_pf*p_hrp2_deletion,
    prev = prev,
    
    sen = arm_sen_pf_del,
    spec = arm_spec_pf_del,
    p_trt_eff = p_trt_eff, # treatment effectiveness
    p_sev_no_trt = p_sev_no_trt, # proportion of severe cases when not treated
    p_sev_trt = p_sev_trt,
    
    p_mort = p_mort, # all-cause mortality
    p_neuroseq = p_neuroseq, # probability of neurosequelae among severe cases
    p_sev_mort = p_sev_mort
  )
  
 
  # pv
  arm_mal_pv <- malaria_branch(
    n_pop = n_pop,
    p_true_pos = prev*p_pv,
    prev = prev,
    
    sen = arm_sen_pv,
    spec = arm_spec_pv,
    p_trt_eff = p_trt_eff, # treatment effectiveness
    p_sev_no_trt = p_sev_no_trt, # proportion of severe cases when not treated
    p_sev_trt = p_sev_trt,
    p_mort = p_mort, # all-cause mortality
    p_neuroseq = p_neuroseq, # probability of neurosequelae among severe cases
    p_sev_mort = p_sev_mort
  )
  
  
  # coinfection hrp2+
  arm_mal_co <- malaria_branch(
    n_pop = n_pop,
    p_true_pos = prev*p_co*(1-p_hrp2_deletion),
    prev = prev,
    
    sen = arm_sen_co,
    spec = arm_spec_co,
    p_trt_eff = p_trt_eff, # treatment effectiveness
    p_sev_no_trt = p_sev_no_trt, # proportion of severe cases when not treated
    p_sev_trt = p_sev_trt,
    
    p_mort = p_mort, # all-cause mortality
    p_neuroseq = p_neuroseq, # probability of neurosequelae among severe cases
    p_sev_mort = p_sev_mort
  )
  
  
  
  # coinfection hrp2-
  arm_mal_co_hrp2_del <- malaria_branch(
    n_pop = n_pop,
    p_true_pos = prev*p_co*p_hrp2_deletion,
    prev = prev,
    
    sen = arm_sen_pf_del,
    spec = arm_spec_pf_del,
    p_trt_eff = p_trt_eff, # treatment effectiveness
    p_sev_no_trt = p_sev_no_trt, # proportion of severe cases when not treated
    p_sev_trt = p_sev_trt,
    
    p_mort = p_mort, # all-cause mortality
    p_neuroseq = p_neuroseq, # probability of neurosequelae among severe cases
    p_sev_mort = p_sev_mort
  )
 
  # no malaria
  arm_no_mal <- no_malaria_branch(
    n_pop = n_pop, # cohort size
    p_true_pos = prev, # malaria prevalence
    
    sen = arm_sen_pf, # sensitivity
    spec = arm_spec_pf, # specificity
    p_mort = p_mort# all-cause mortality
  )

  # Join all the results
  arm_total <- bind_rows(arm_mal_pf,
                        arm_mal_pf_hrp2_del,
                        arm_mal_pv,
                        arm_mal_co,
                        arm_mal_co_hrp2_del,
                        arm_no_mal)
  
  return(colSums(arm_total, na.rm=T))
}

calc_total_cost <- function(
    init_cost, cost_vec, index_mat, arm_total){
  cost_per_node <- colSums(cost_vec*index_mat[names(cost_vec),])
  cost_total <- sum(init_cost + sum(arm_total*cost_per_node))
}

calc_total_daly <- function(daly_vec, index_mat, arm_total, discount){
  
  life_loss <- (1/discount)*(1-exp(-discount*daly_vec["life_loss"]))
  neuroseq <- daly_vec[c("daly_neuroseq")]*(1/discount)*(1-exp(-discount*(life_exp-avg_age)))
  
  daly_vec_new <- daly_vec
  daly_vec_new["life_loss"] <- life_loss
  daly_vec_new["daly_neuroseq"] <- neuroseq
  
  daly_per_node <- colSums(daly_vec_new * index_mat[names(daly_vec_new),], na.rm=T)
  total_daly <- sum(arm_total*daly_per_node)
  return(total_daly)
}


run_psa <- function(
    n_pop,
    prev,
    p_pf,
    p_pv,
    p_co,
    p_hrp2_deletion,
    arm1_sen_pf,
    arm1_spec_pf,
    arm1_sen_pf_del,
    arm1_spec_pf_del,
    arm1_sen_pv,
    arm1_spec_pv,
    arm1_sen_co,
    arm1_spec_co,
    arm2_sen_pf,
    arm2_spec_pf,
    arm2_sen_pf_del,
    arm2_spec_pf_del,
    arm2_sen_pv,
    arm2_spec_pv,
    arm2_sen_co,
    arm2_spec_co, 
    arm3_sen_pf,
    arm3_spec_pf,
    arm3_sen_pf_del,
    arm3_spec_pf_del,
    arm3_sen_pv,
    arm3_spec_pv,
    arm3_sen_co,
    arm3_spec_co,
    p_trt_eff,
    p_sev_no_trt,
    p_sev_trt,
    p_mort,
    p_neuroseq,
    p_sev_mort,
    c_screen_arm1,
    c_screen_arm2,
    c_screen_arm3,
    c_init_arm1,
    c_init_arm2,
    c_init_arm3,
    c_trt,
    c_trt_sev,
    c_personnel,
    c_msat,
    c_hh_mal,
    c_hh_sev,
    c_hh_neuroseq,
    c_indirect,
    daly_mal,
    daly_neuroseq,
    daly_sev,
    life_exp,
    discount,
    avg_age
    ){
  res_psa <- data.frame(
    arm1_total_cost = rep(NA, n),
    arm1_total_daly = rep(NA, n),
    arm2_total_cost = rep(NA, n),
    arm2_total_daly = rep(NA, n),
    arm3_total_cost = rep(NA, n),
    arm3_total_daly = rep(NA, n),
    arm2_icer = rep(NA, n),
    arm3_icer = rep(NA, n)
  )
  
  for(i in 1:n){
    
    
    arm1_total <- run_tree(
      n_pop = n_pop[i],
      prev = prev[i],
      p_pf = p_pf[i],
      p_pv = p_pv[i],
      p_co = p_co[i],
      p_hrp2_deletion = p_hrp2_deletion[i],
      arm_sen_pf = arm1_sen_pf[i],
      arm_spec_pf = arm1_spec_pf[i],
      arm_sen_pf_del = arm1_sen_pf_del[i],
      arm_spec_pf_del = arm1_spec_pf_del[i],
      arm_sen_pv = arm1_sen_pv[i],
      arm_spec_pv = arm1_spec_pv[i],
      arm_sen_co = arm1_sen_co[i],
      arm_spec_co = arm1_spec_co[i],
      p_trt_eff = p_trt_eff[i],
      p_sev_no_trt = p_sev_no_trt[i],
      p_sev_trt = p_sev_trt[i],
      p_mort = p_mort[i],
      p_neuroseq = p_neuroseq[i],
      p_sev_mort = p_sev_mort[i]# mortality among severe malaria cases
    )
    
    ## Arm 2
    
    arm2_total <- run_tree(
      n_pop[i], # total population
      prev[i], # malaria prevalence
      p_pf[i], # % of falciparum
      p_pv[i],
      p_co[i],
      p_hrp2_deletion[i], # % of hrp2 deletion among falciparum
      arm2_sen_pf[i], # Pf sensitivity
      arm2_spec_pf[i], # Pf specificity
      arm2_sen_pf_del[i], # Pf (hrp2-) sensitivity
      arm2_spec_pf_del[i], # Pf (hrp2) specificity
      arm2_sen_pv[i], # Pv sensitivity
      arm2_spec_pv[i], # Pv specificity
      arm2_sen_co[i], # co-infection sensitivity
      arm2_spec_co[i], # co-infection specificity
      p_trt_eff[i], # antimalarial treatment effectiveness
      p_sev_no_trt[i], # % of severe cases when not treated
      p_sev_trt[i],
      
      p_mort[i], # general mortality
      p_neuroseq[i], # probability of neurosequelae
      p_sev_mort[i] # mortality among severe malaria cases
    )
    
    
    ## Arm 3
    
    arm3_total <- run_tree(
      n_pop[i], # total population
      prev[i], # malaria prevalence
      p_pf[i], # % of falciparum
      p_pv[i],
      p_co[i],
      p_hrp2_deletion[i], # % of hrp2 deletion among falciparum
      arm3_sen_pf[i], # Pf sensitivity
      arm3_spec_pf[i], # Pf specificity
      arm3_sen_pf_del[i], # Pf (hrp2-) sensitivity
      arm3_spec_pf_del[i], # Pf (hrp2) specificity
      arm3_sen_pv[i], # Pv sensitivity
      arm3_spec_pv[i], # Pv specificity
      arm3_sen_co[i], # co-infection sensitivity
      arm3_spec_co[i], # co-infection specificity
      p_trt_eff[i], # antimalarial treatment effectiveness
      p_sev_no_trt[i], # % of severe cases when not treated
      p_sev_trt[i],
      
      p_mort[i], # general mortality
      p_neuroseq[i], # probability of neurosequelae
      p_sev_mort[i] # mortality among severe malaria cases
    )
    
    arm1_cost_vec <-c(
      c_screen = c_screen_arm1[i],
      c_trt = c_trt[i],
      c_trt_sev = c_trt_sev[i],
      c_personnel = c_personnel[i],
      c_msat = c_msat[i],
      c_hh_mal = c_hh_mal[i],
      c_hh_sev = c_hh_sev[i],
      c_hh_neuroseq = c_hh_neuroseq[i],
      c_indirect = c_indirect[i]
    )
    
    arm2_cost_vec <-c(
      c_screen = c_screen_arm2[i],
      c_trt = c_trt[i],
      c_trt_sev = c_trt_sev[i],
      c_personnel = c_personnel[i],
      c_msat = c_msat[i],
      c_hh_mal = c_hh_mal[i],
      c_hh_sev = c_hh_sev[i],
      c_hh_neuroseq = c_hh_neuroseq[i],
      c_indirect = c_indirect[i]
    )
    
    arm3_cost_vec <-c(
      c_screen = c_screen_arm3[i],
      c_trt = c_trt[i],
      c_trt_sev = c_trt_sev[i],
      c_personnel = c_personnel[i],
      c_msat = c_msat[i],
      c_hh_mal = c_hh_mal[i],
      c_hh_sev = c_hh_sev[i],
      c_hh_neuroseq = c_hh_neuroseq[i],
      c_indirect = c_indirect[i]
    )
    
    
    arm1_total_cost <- calc_total_cost(c_init_arm1[i],
                                       arm1_cost_vec,
                                       index_mat,
                                       arm1_total)
    
    arm2_total_cost <- calc_total_cost(c_init_arm2[i],
                                       arm2_cost_vec,
                                       index_mat,
                                       arm2_total)
    
    arm3_total_cost <- calc_total_cost(c_init_arm3[i],
                                       arm3_cost_vec,
                                       index_mat,
                                       arm3_total)
    
    
    ## Calculate DALY
    
    daly_vec <- c(
      daly_mal = daly_mal[i],
      daly_sev = daly_sev[i],
      life_loss = life_exp[i]-avg_age[i],
      daly_neuroseq = daly_neuroseq[i]
    )
    
    
    arm1_total_daly <- calc_total_daly(daly_vec, 
                                       index_mat, 
                                       arm1_total,
                                       
                                       discount[i])
    
    arm2_total_daly <- calc_total_daly(daly_vec, 
                                       index_mat, 
                                       arm2_total,
                                       
                                       discount[i])
    
    arm3_total_daly <- calc_total_daly(daly_vec, 
                                       index_mat, 
                                       arm3_total,
                                       
                                       discount[i])
    
    icer_arm2 <- (arm2_total_cost - arm1_total_cost)/-(arm2_total_daly - arm1_total_daly)
    icer_arm3 <- (arm3_total_cost - arm1_total_cost)/-(arm3_total_daly - arm1_total_daly)
    
    res_psa[i,] <- c(arm1_total_cost,
                     arm1_total_daly,
                     arm2_total_cost,
                     arm2_total_daly,
                     arm3_total_cost,
                     arm3_total_daly,
                     icer_arm2,
                     icer_arm3)
    
  }
  
  res_psa %<>% mutate(
    arm2_inc_cost = arm2_total_cost - arm1_total_cost,
    arm3_inc_cost = arm3_total_cost - arm1_total_cost,
    arm2_daly_avt = arm1_total_daly - arm2_total_daly,
    arm3_daly_avt = arm1_total_daly - arm3_total_daly
  )
  
  apply(res_psa[,c("arm2_inc_cost", "arm2_daly_avt", "arm2_icer", 
                   "arm3_inc_cost", "arm3_daly_avt", "arm3_icer")],
        2,
        function(x){
          return(data.frame(
                  mean = median(x),
                  low = quantile(x, 0.025),
                  high = quantile(x, 0.975)))
        })
}




run_det <- function(
    n_pop,
    prev,
    p_pf,
    p_pv,
    p_co,
    p_hrp2_deletion,
    arm1_sen_pf,
    arm1_spec_pf,
    arm1_sen_pf_del,
    arm1_spec_pf_del,
    arm1_sen_pv,
    arm1_spec_pv,
    arm1_sen_co,
    arm1_spec_co,
    arm2_sen_pf,
    arm2_spec_pf,
    arm2_sen_pf_del,
    arm2_spec_pf_del,
    arm2_sen_pv,
    arm2_spec_pv,
    arm2_sen_co,
    arm2_spec_co, 
    arm3_sen_pf,
    arm3_spec_pf,
    arm3_sen_pf_del,
    arm3_spec_pf_del,
    arm3_sen_pv,
    arm3_spec_pv,
    arm3_sen_co,
    arm3_spec_co,
    p_trt_eff,
    p_sev_no_trt,
    p_sev_trt,
    p_mort,
    p_neuroseq,
    p_sev_mort,
    c_screen_arm1,
    c_screen_arm2,
    c_screen_arm3,
    c_init_arm1,
    c_init_arm2,
    c_init_arm3,
    c_trt,
    c_trt_sev,
    c_personnel,
    c_msat,
    c_hh_mal,
    c_hh_sev,
    c_hh_neuroseq,
    c_indirect,
    daly_mal,
    daly_neuroseq,
    daly_sev,
    life_exp,
    discount,
    avg_age
){
  
    
    arm1_total <- run_tree(
      n_pop = n_pop,
      prev = prev,
      p_pf = p_pf,
      p_pv = p_pv,
      p_co = p_co,
      p_hrp2_deletion = p_hrp2_deletion,
      arm_sen_pf = arm1_sen_pf,
      arm_spec_pf = arm1_spec_pf,
      arm_sen_pf_del = arm1_sen_pf_del,
      arm_spec_pf_del = arm1_spec_pf_del,
      arm_sen_pv = arm1_sen_pv,
      arm_spec_pv = arm1_spec_pv,
      arm_sen_co = arm1_sen_co,
      arm_spec_co = arm1_spec_co,
      p_trt_eff = p_trt_eff,
      p_sev_no_trt = p_sev_no_trt,
      p_sev_trt = p_sev_trt,
      p_mort = p_mort,
      p_neuroseq = p_neuroseq,
      p_sev_mort = p_sev_mort# mortality among severe malaria cases
    )
    
    ## Arm 2
    
    arm2_total <- run_tree(
      n_pop, # total population
      prev, # malaria prevalence
      p_pf, # % of falciparum
      p_pv,
      p_co,
      p_hrp2_deletion, # % of hrp2 deletion among falciparum
      arm2_sen_pf, # Pf sensitivity
      arm2_spec_pf, # Pf specificity
      arm2_sen_pf_del, # Pf (hrp2-) sensitivity
      arm2_spec_pf_del, # Pf (hrp2) specificity
      arm2_sen_pv, # Pv sensitivity
      arm2_spec_pv, # Pv specificity
      arm2_sen_co, # co-infection sensitivity
      arm2_spec_co, # co-infection specificity
      p_trt_eff, # antimalarial treatment effectiveness
      p_sev_no_trt, # % of severe cases when not treated
      p_sev_trt,
      
      p_mort, # general mortality
      p_neuroseq, # probability of neurosequelae
      p_sev_mort # mortality among severe malaria cases

    )
    
    
    ## Arm 3
    
    arm3_total <- run_tree(
      n_pop, # total population
      prev, # malaria prevalence
      p_pf, # % of falciparum
      p_pv,
      p_co,
      p_hrp2_deletion, # % of hrp2 deletion among falciparum
      arm3_sen_pf, # Pf sensitivity
      arm3_spec_pf, # Pf specificity
      arm3_sen_pf_del, # Pf (hrp2-) sensitivity
      arm3_spec_pf_del, # Pf (hrp2) specificity
      arm3_sen_pv, # Pv sensitivity
      arm3_spec_pv, # Pv specificity
      arm3_sen_co, # co-infection sensitivity
      arm3_spec_co, # co-infection specificity
      p_trt_eff, # antimalarial treatment effectiveness
      p_sev_no_trt, # % of severe cases when not treated
      p_sev_trt,
      
      p_mort, # general mortality
      p_neuroseq, # probability of neurosequelae
      p_sev_mort # mortality among severe malaria cases
    )
    
    arm1_cost_vec_soc <-c(
      c_screen = c_screen_arm1,
      c_trt = c_trt,
      c_trt_sev = c_trt_sev,
      c_personnel = c_personnel,
      c_msat = c_msat,
      c_hh_mal = c_hh_mal,
      c_hh_sev = c_hh_sev,
      c_hh_neuroseq = c_hh_neuroseq,
      c_indirect = c_indirect
    )
    arm2_cost_vec_soc <-c(
      c_screen = c_screen_arm2,
      c_trt = c_trt,
      c_trt_sev = c_trt_sev,
      c_personnel = c_personnel,
      c_msat = c_msat,
      c_hh_mal = c_hh_mal,
      c_hh_sev = c_hh_sev,
      c_hh_neuroseq = c_hh_neuroseq,
      c_indirect = c_indirect
    )
    
    arm3_cost_vec_soc <-c(
      c_screen = c_screen_arm3,
      c_trt = c_trt,
      c_trt_sev = c_trt_sev,
      c_personnel = c_personnel,
      c_msat = c_msat,
      c_hh_mal = c_hh_mal,
      c_hh_sev = c_hh_sev,
      c_hh_neuroseq = c_hh_neuroseq,
      c_indirect = c_indirect
    )
    
    arm1_cost_vec_pp <-c(
      c_screen = c_screen_arm1,
      c_trt = c_trt,
      c_trt_sev = c_trt_sev,
      c_personnel = c_personnel,
      c_msat = c_msat,
      c_hh_mal = 0,
      c_hh_sev = 0,
      c_hh_neuroseq = 0,
      c_indirect = 0
    )
    
    arm2_cost_vec_pp <-c(
      c_screen = c_screen_arm2,
      c_trt = c_trt,
      c_trt_sev = c_trt_sev,
      c_personnel = c_personnel,
      c_msat = c_msat,
      c_hh_mal = 0,
      c_hh_sev = 0,
      c_hh_neuroseq = 0,
      c_indirect = 0
    )
    
    arm3_cost_vec_pp <-c(
      c_screen = c_screen_arm3,
      c_trt = c_trt,
      c_trt_sev = c_trt_sev,
      c_personnel = c_personnel,
      c_msat = c_msat,
      c_hh_mal = 0,
      c_hh_sev = 0,
      c_hh_neuroseq = 0,
      c_indirect = 0
    )
    
    names(arm1_cost_vec_soc) <-
      names(arm1_cost_vec_pp) <-
      names(arm2_cost_vec_soc) <-
      names(arm2_cost_vec_pp) <-
      names(arm3_cost_vec_soc) <-
      names(arm3_cost_vec_pp) <-
      c("c_screen", "c_trt", "c_trt_sev", "c_personnel", "c_msat", "c_hh_mal", "c_hh_sev", "c_hh_neuroseq", "c_indirect")
    
    arm1_total_cost_soc <- calc_total_cost(c_init_arm1,
                                       arm1_cost_vec_soc,
                                       index_mat,
                                       arm1_total)
    arm2_total_cost_soc <- calc_total_cost(c_init_arm2,
                                       arm2_cost_vec_soc,
                                       index_mat,
                                       arm2_total)
    
    arm3_total_cost_soc <- calc_total_cost(c_init_arm3,
                                       arm3_cost_vec_soc,
                                       index_mat,
                                       arm3_total)
    
    
    
    arm1_total_cost_pp <- calc_total_cost(c_init_arm1,
                                           arm1_cost_vec_pp,
                                           index_mat,
                                           arm1_total)
    
    arm2_total_cost_pp <- calc_total_cost(c_init_arm2,
                                           arm2_cost_vec_pp,
                                           index_mat,
                                           arm2_total)
    
    arm3_total_cost_pp <- calc_total_cost(c_init_arm3,
                                           arm3_cost_vec_pp,
                                           index_mat,
                                           arm3_total)
    
    ## Calculate DALY
    
    daly_vec <- c(
      daly_mal = daly_mal,
      daly_sev = daly_sev,
      life_loss = life_exp-avg_age,
      daly_neuroseq = daly_neuroseq
    )
    names(daly_vec) <- c("daly_mal", "daly_sev", "life_loss", "daly_neuroseq")

    arm1_total_daly <- calc_total_daly(daly_vec, 
                                       index_mat, 
                                       arm1_total,
                                       discount)

    arm2_total_daly <- calc_total_daly(daly_vec, 
                                       index_mat, 
                                       arm2_total,
                                       
                                       discount)
    
    arm3_total_daly <- calc_total_daly(daly_vec, 
                                       index_mat, 
                                       arm3_total,
                                       
                                       discount)
    
    icer_arm2_soc <- (arm2_total_cost_soc - arm1_total_cost_soc)/-(arm2_total_daly - arm1_total_daly)
    icer_arm3_soc <- (arm3_total_cost_soc - arm1_total_cost_soc)/-(arm3_total_daly - arm1_total_daly)
    icer_arm2_pp <- (arm2_total_cost_pp - arm1_total_cost_pp)/-(arm2_total_daly - arm1_total_daly)
    icer_arm3_pp <- (arm3_total_cost_pp - arm1_total_cost_pp)/-(arm3_total_daly - arm1_total_daly)
    
    res <- data.frame(arm1_total_cost_soc,
             arm1_total_cost_pp,
            arm1_total_daly,
            arm2_total_cost_soc,
            arm2_total_cost_pp,
            arm2_total_daly,
            arm3_total_cost_soc,
            arm3_total_cost_pp,
            arm3_total_daly,
            icer_arm2_soc, icer_arm2_pp,
            icer_arm3_soc, icer_arm3_pp)
    
  return(res)
}

run_tornado <- function(parm_tab){
  n_pop = parm_tab["n_pop"]
  prev = parm_tab["prev"]
  p_trt_eff = parm_tab["p_trt_eff"]
  p_sev_no_trt = parm_tab["p_sev_no_trt"]
  p_mort = parm_tab["p_mort"]
  p_neuroseq = parm_tab["p_neuroseq"]
  p_sev_mort = parm_tab["p_sev_mort"]
  p_pf = parm_tab["p_pf"]
  p_pv = parm_tab["p_pv"]
  p_hrp2_deletion = parm_tab["p_hrp2_deletion"]
  dec_perf = parm_tab["dec_perf"]
  arm1_sen_pf = parm_tab["arm1_sen_pf"]
  arm1_spec_pf = parm_tab["arm1_spec_pf"]
  arm2_sen_pf = parm_tab["arm2_sen_pf"]
  arm2_spec_pf = parm_tab["arm2_spec_pf"]
  arm3_sen_pf = parm_tab["arm3_sen_pf"]
  arm3_spec_pf = parm_tab["arm3_spec_pf"]
  c_init_arm1 = parm_tab["c_init_arm1"]
  c_screen_arm1 = parm_tab["c_screen_arm1"]
  c_init_arm2 = parm_tab["c_init_arm2"]
  c_screen_arm2 = parm_tab["c_screen_arm2"]
  c_init_arm3 = parm_tab["c_init_arm3"]
  c_screen_arm3 = parm_tab["c_screen_arm3"]
  c_trt = parm_tab["c_trt"]
  c_personnel = parm_tab["c_personnel"]
  c_msat = parm_tab["c_msat"]
  c_hh_mal = parm_tab["c_hh_mal"]
  c_hh_sev = parm_tab["c_hh_sev"]
  c_indirect = parm_tab["c_indirect"]
  
  ### Utility
  daly_mal = parm_tab["daly_mal"]
  daly_sev = parm_tab["daly_sev"]
  daly_neuroseq = parm_tab["daly_neuroseq"]
  
  life_exp = parm_tab["life_exp"]
  avg_age = parm_tab["avg_age"]
  discount = parm_tab["discount"]
  
  # dependent parameters
  p_sev_trt <- p_sev_no_trt*(1-p_trt_eff)
  p_co <- 1-p_pf-p_pv
  arm1_sen_pf_del <- arm1_sen_pf*dec_perf
  arm1_spec_pf_del <- arm1_spec_pf
  arm1_sen_pv <- arm1_sen_pf
  arm1_spec_pv <- arm1_spec_pf
  arm1_sen_co <- arm1_sen_pf
  arm1_spec_co <- arm1_spec_pf
  arm1_sen_co_del <- arm1_sen_pf_del
  arm1_spec_co_del <- arm1_spec_pf_del
  arm2_sen_pf_del <- arm2_sen_pf
  arm2_spec_pf_del <- arm2_spec_pf
  arm2_sen_pv <- arm2_sen_pf
  arm2_spec_pv <- arm2_spec_pf
  arm2_sen_co <- arm2_sen_pf
  arm2_spec_co <- arm2_spec_pf
  arm2_sen_co_del <- arm2_sen_pf_del
  arm2_spec_co_del <- arm2_spec_pf_del
  arm3_sen_pf_del <- arm3_sen_pf
  arm3_spec_pf_del <- arm3_spec_pf
  arm3_sen_pv <- arm3_sen_pf
  arm3_spec_pv <- arm3_spec_pf
  arm3_sen_co <- arm3_sen_pf
  arm3_spec_co <- arm3_spec_pf
  arm3_sen_co_del <- arm3_sen_pf_del
  arm3_spec_co_del <- arm3_spec_pf_del
  c_hh_neuroseq <- c_hh_sev
  c_trt_sev <- c_trt*mean(c_hh_sev)/mean(c_hh_mal)
  
  
  # run
  run_det(
    n_pop,
    prev,
    p_pf,
    p_pv,
    p_co,
    p_hrp2_deletion,
    arm1_sen_pf,
    arm1_spec_pf,
    arm1_sen_pf_del,
    arm1_spec_pf_del,
    arm1_sen_pv,
    arm1_spec_pv,
    arm1_sen_co,
    arm1_spec_co,
    arm2_sen_pf,
    arm2_spec_pf,
    arm2_sen_pf_del,
    arm2_spec_pf_del,
    arm2_sen_pv,
    arm2_spec_pv,
    arm2_sen_co,
    arm2_spec_co, 
    arm3_sen_pf,
    arm3_spec_pf,
    arm3_sen_pf_del,
    arm3_spec_pf_del,
    arm3_sen_pv,
    arm3_spec_pv,
    arm3_sen_co,
    arm3_spec_co,
    p_trt_eff,
    p_sev_no_trt,
    p_sev_trt,
    p_mort,
    p_neuroseq,
    p_sev_mort,
    c_screen_arm1,
    c_screen_arm2,
    c_screen_arm3,
    c_init_arm1,
    c_init_arm2,
    c_init_arm3,
    c_trt,
    c_trt_sev,
    c_personnel,
    c_msat,
    c_hh_mal,
    c_hh_sev,
    c_hh_neuroseq,
    c_indirect,
    daly_mal,
    daly_neuroseq,
    daly_sev,
    life_exp,
    discount,
    avg_age
  )
}
