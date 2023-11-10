### RUN PSA
## Make sure you set up the parameter at script #3 before this
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
