## Calculate cost
index_mat <- rio::import(here::here("data", "cost_daly_matrix.xlsx"))
rownames(index_mat) <- index_mat$varname
index_mat %<>% select(-varname, -description)

arm1_cost_vec <-c(
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

arm2_cost_vec <-c(
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

arm3_cost_vec <-c(
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


arm1_total_cost <- calc_total_cost(c_init_arm1,
                                    arm1_cost_vec,
                                    index_mat,
                                    arm1_total)

arm2_total_cost <- calc_total_cost(c_init_arm2,
                                   arm2_cost_vec,
                                   index_mat,
                                   arm2_total)

arm3_total_cost <- calc_total_cost(c_init_arm3,
                                   arm3_cost_vec,
                                   index_mat,
                                   arm3_total)


## Calculate DALY

daly_vec <- c(
  daly_mal = daly_mal,
  daly_sev = daly_sev,
  life_loss = life_exp-avg_age,
  daly_neuroseq = daly_neuroseq
)


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

icer_arm2 <- (arm2_total_cost - arm1_total_cost)/-(arm2_total_daly - arm1_total_daly)
icer_arm3 <- (arm3_total_cost - arm1_total_cost)/-(arm3_total_daly - arm1_total_daly)

