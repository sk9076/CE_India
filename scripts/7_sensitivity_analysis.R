# Sensitivity analysis

# Tornado diagram

# Changing the malaria prevalence
prev_org <- prev
factor <- c(0.25, 0.5, 1.5, 2)
for(i in 1:4){
  prev <- prev_org*factor[i]
  temp <- run_psa( n_pop,
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
                   avg_age)
  if(i==1){
    res_sen_prev <- do.call(cbind, temp)
  }else{
    res_sen_prev %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_prev) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_prev$prev = factor*mean(prev_org)

prev <- prev_org

# Changing the HRP2 deletion prevalence
p_hrp2_deletion_org <- p_hrp2_deletion
factor <- c((0.819317/0.9)^-1,
            (0.819317/0.6)^-1,
            (0.819317/0.4)^-1,
            (0.819317/0.2)^-1)
for(i in 1:4){
  p_hrp2_deletion <- p_hrp2_deletion_org*factor[i]
  temp <- run_psa( n_pop,
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
                   avg_age)
  if(i==1){
    res_sen_hrp2 <- do.call(cbind, temp)
  }else{
    res_sen_hrp2 %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_hrp2) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_hrp2$deletion = c(0.9, 0.6, 0.4, 0.2)

p_hrp2_deletion <- p_hrp2_deletion_org

# Changing the cost of LAMP
c_init_arm3_org <- c_init_arm3
factor <- c(0.5, 2, 3, 4)
for(i in 1:4){
  c_init_arm3 <- c_init_arm3_org*factor[i]
  temp <- run_psa( n_pop,
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
                   avg_age)
  if(i==1){
    res_sen_cost <- do.call(cbind, temp)
  }else{
    res_sen_cost %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_cost) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_cost$init_arm3_cost = factor

c_init_arm3 <- c_init_arm3_org

## Changing the prevalence and HRP-2 deletion status together
prev_org <- prev
factor_prev <- c(0.25, 0.5, 1.5, 2)

p_hrp2_deletion_org <- p_hrp2_deletion
factor_hrp2 <- c((0.819317/0.9)^-1,
            (0.819317/0.6)^-1,
            (0.819317/0.4)^-1,
            (0.819317/0.2)^-1)

for(i in 1:4){
  for(j in 1:4){
    
    prev <- prev_org*factor_prev[i]
    p_hrp2_deletion <- p_hrp2_deletion_org*factor_hrp2[j]
    
    temp <- run_psa( n_pop,
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
                     avg_age)
    if(i==1 & j==1){
      res_sen_prev_hrp2 <- do.call(cbind, temp)
    }else{
      res_sen_prev_hrp2[(i-1)*4+j, 1:18] <-do.call(cbind, temp)
    }
    res_sen_prev_hrp2$prev[(i-1)*4+j] <- mean(prev)
    res_sen_prev_hrp2$hrp2[(i-1)*4+j] <- mean(p_hrp2_deletion)
  }
}
#colnames(res_sen_prev_hrp2)[1:6] %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))

prev <- prev_org
p_hrp2_deletion <- p_hrp2_deletion_org


## Changing the unit cost of Arm 2
c_screen_arm2_org <- c_screen_arm2
factor <- c(0.5, 2, 3, 4)
for(i in 1:4){
  c_screen_arm2 <- c_screen_arm2_org*factor[i]
  temp <- run_psa( n_pop,
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
                   avg_age)
  if(i==1){
    res_sen_cost2 <- do.call(cbind, temp)
  }else{
    res_sen_cost2 %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_cost) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_cost2$per_screen_cost_arm2 = factor

c_screen_arm2 <- c_screen_arm2_org




## Save
pacman::p_load(openxlsx)
wb <- createWorkbook()

addWorksheet(wb, "prevalence")
writeData(wb, "prevalence", res_sen_prev)

addWorksheet(wb, "hrp2_deletion")
writeData(wb, "hrp2_deletion", res_sen_hrp2)

addWorksheet(wb, "arm3_init_cost")
writeData(wb, "arm3_init_cost", res_sen_cost)

addWorksheet(wb, "prev_hrp2")
writeData(wb, "prev_hrp2", res_sen_prev_hrp2)

addWorksheet(wb, "arm2_unit_cost")
writeData(wb, "arm2_unit_cost", res_sen_cost2)

saveWorkbook(wb, file = here::here("results", "Sensitivity analysis_soc.xlsx"), overwrite = TRUE)
