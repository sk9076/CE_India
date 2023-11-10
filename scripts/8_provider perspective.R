## Program provider perspective
### RUN PSA
## Make sure you set up the parameter at script #3 before this
res_psa_pp <- data.frame(
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
    c_hh_mal = 0,
    c_hh_sev = 0,
    c_hh_neuroseq = 0,
    c_indirect = 0
  )
  
  arm2_cost_vec <-c(
    c_screen = c_screen_arm2[i],
    c_trt = c_trt[i],
    c_trt_sev = c_trt_sev[i],
    c_personnel = c_personnel[i],
    c_msat = c_msat[i],
    c_hh_mal = 0,
    c_hh_sev = 0,
    c_hh_neuroseq = 0,
    c_indirect = 0
  )
  
  arm3_cost_vec <-c(
    c_screen = c_screen_arm3[i],
    c_trt = c_trt[i],
    c_trt_sev = c_trt_sev[i],
    c_personnel = c_personnel[i],
    c_msat = c_msat[i],
    c_hh_mal = 0,
    c_hh_sev = 0,
    c_hh_neuroseq = 0,
    c_indirect = 0
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
  
  res_psa_pp[i,] <- c(arm1_total_cost,
                   arm1_total_daly,
                   arm2_total_cost,
                   arm2_total_daly,
                   arm3_total_cost,
                   arm3_total_daly,
                   icer_arm2,
                   icer_arm3)
  
}

# ICER with 95% Credible interval
res_psa_pp %<>% mutate(
  arm2_inc_cost = arm2_total_cost - arm1_total_cost,
  arm3_inc_cost = arm3_total_cost - arm1_total_cost,
  arm2_daly_avt = -1*(arm2_total_daly - arm1_total_daly),
  arm3_daly_avt = -1*(arm3_total_daly - arm1_total_daly)
)

apply(res_psa_pp[,c("arm2_inc_cost", "arm3_inc_cost")],
      2,
      function(x){
        sprintf("Incremental cost= %.2f, 95%% CrI [%.2f, %.2f]",
                median(x),
                quantile(x, 0.025),
                quantile(x, 0.975))
      })

apply(res_psa_pp[,c("arm2_daly_avt", "arm3_daly_avt")],
      2,
      function(x){
        sprintf("DALY averted= %.2f, 95%% CrI [%.2f, %.2f]",
                median(x),
                quantile(x, 0.025),
                quantile(x, 0.975))
      })

apply(res_psa_pp[,c("arm2_icer", "arm3_icer")],
      2,
      function(x){
        sprintf("ICER = %.2f, 95%% CrI [%.2f, %.2f]",
                median(x),
                quantile(x, 0.025),
                quantile(x, 0.975))
      })

# CE Plane 

## CE threshold: sciencedirect.com/science/article/pii/S1098301521017319
## India perspective: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5201057/
## Suggestion: https://www.thelancet.com/pdfs/journals/langlo/PIIS2214-109X(23)00162-6.pdf (0.12 - 0.30 of GDP per capita)
#gdp_per_cap <- 2389
#he_per_cap <- 57
#cet_low <- 0.12*gdp_per_cap
#cet_high <- 0.30*gdp_per_cap


ggthemr::ggthemr("fresh")

# Size 750*500
CE_pane_pp <- ggplot(res_psa_pp) +
  geom_hline(yintercept = 0, color = "black")+
  geom_vline(xintercept = 0, color = "black")+
  geom_point(aes(arm2_daly_avt, arm2_inc_cost, color = "Arm2"), size = 0.2) +
  geom_point(aes(arm3_daly_avt,arm3_inc_cost,  color = "Arm3"), size = 0.2) +
  geom_abline(intercept = 0, slope = cet_low, linetype = "dotted", color = "red") +
  annotate("text", x =110, y = 10500, label = "CET = $287/DALY averted", color = "red")+
  geom_abline(intercept = 0, slope = cet_high, linetype = "dotted", color = "red") +
  annotate("text", x = 320, y = 13500, label = "CET = $57/DALY averted", color = "red")+
  geom_abline(intercept = 0, slope = he_per_cap, linetype = "dotted", color = "red") +
  annotate("text", x = -60, y = 13500, label = "CET = $717/DALY averted", color = "red")+
  scale_y_continuous(limits = c(-5000, 22000))+
  ylab("Incremental cost") +
  xlab("DALY averted") +
  scale_color_discrete(
    guide = guide_legend(nrow = 1,
                         override.aes = list(size = 1))
  )+
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank())

CE_pane_pp
ggsave(plot = CE_pane_pp,
       filename = here::here("results", "CE pane_pp.png"),
       width = 8,
       height = 5)

sprintf("%.3f%% of simulation for Arm 2 were cost-saving.",
        sum(res_psa_pp$arm2_icer<=0 & res_psa_pp$arm2_inc_cost<0)/nrow(res_psa_pp)*100)

sprintf("%.3f%% of simulation for Arm 2 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa_pp$arm2_icer<=cet_low)/nrow(res_psa_pp)*100,
        cet_low)
sprintf("%.3f%% of simulation for Arm 2 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa_pp$arm2_icer<=cet_high)/nrow(res_psa_pp)*100,
        cet_high)
sprintf("%.3f%% of simulation for Arm 2 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa_pp$arm2_icer<=he_per_cap)/nrow(res_psa_pp)*100,
        he_per_cap)

sprintf("%.3f%% of simulation for Arm 3 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa_pp$arm3_icer<=cet_low)/nrow(res_psa_pp)*100,
        cet_low)
sprintf("%.3f%% of simulation for Arm 3 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa_pp$arm3_icer<=cet_high)/nrow(res_psa_pp)*100,
        cet_high)
sprintf("%.3f%% of simulation for Arm 3 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa_pp$arm3_icer<=he_per_cap)/nrow(res_psa_pp)*100,
        he_per_cap)

sprintf("%.3f%% of simulation for Arm 3 ended up in incremental cost while not averting more DALY than Arm 1.",
        sum(res_psa_pp$arm3_icer<=0 & res_psa_pp$arm3_daly_avt<0)/nrow(res_psa_pp)*100)

# Acceptability curve

#wtp <- seq(0, 2500, by = 1)
dat_wtp_pp <- data.frame(
  wtp = wtp,
  arm2_p_ce = NA,
  arm3_p_ce = NA
)

for(i in 0:2500){
  dat_wtp_pp$arm2_p_ce[i] <- sum(res_psa$arm2_icer <wtp[i])/nrow(res_psa)
  dat_wtp_pp$arm3_p_ce[i] <- sum(res_psa$arm3_icer <wtp[i])/nrow(res_psa)
}

arm2_dom <- min(dat_wtp_pp$wtp[which(dat_wtp$arm2_p_ce>0.5)])
arm3_dom <- min(dat_wtp_pp$wtp[which(dat_wtp$arm3_p_ce>0.5)])
arm2_sat <- min(dat_wtp_pp$wtp[which(dat_wtp$arm2_p_ce==1)])
arm3_sat <- min(dat_wtp_pp$wtp[which(dat_wtp$arm3_p_ce==1)])

accept_curve_arm2_pp <- 
  ggplot(dat_wtp_pp) + geom_path(aes(wtp, arm2_p_ce, color = "Arm 2")) +
  geom_path(aes(wtp, 1-arm2_p_ce, color = "Arm1")) +
  scale_x_continuous(limits = c(0, 25)) +
  geom_vline(xintercept = 0.35, color = "red", linetype = "dotted")+
  geom_vline(xintercept = arm2_sat, color = "blue", linetype = "dotted") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by=0.2), labels = paste0(seq(0, 100, by=20), " %")) +
  ylab("Acceptability") +
  xlab("Willingness-to-pay ($)") +
  theme(legend.title = element_blank())

accept_curve_arm3_pp <-
  ggplot(dat_wtp_pp) + geom_path(aes(wtp, arm3_p_ce, color = "Arm 3")) +
  geom_path(aes(wtp, 1-arm3_p_ce, color = "Arm1")) +
  scale_x_continuous(limits = c(0, 1000))+
  geom_vline(xintercept = arm3_dom, color = "red", linetype = "dotted")+
  #geom_vline(xintercept = arm3_sat, color = "blue")
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by=0.2), labels = paste0(seq(0, 100, by=20), " %")) +
  ylab("Acceptability") +
  xlab("Willingness-to-pay ($)") +
  theme(legend.title = element_blank())

ggsave(plot  = ggpubr::ggarrange(accept_curve_arm2_pp,
                                 accept_curve_arm3_pp,
                                 nrow = 1),
       filename = here::here('results', 'acceptability curve_pp.png'),
       width = 8,
       height = 2.5)
saveRDS(res_psa_pp, here::here("results", "res_psa_pp.RDS"))


######### Sensitivity analysis

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
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   daly_mal,
                   daly_neuroseq,
                   daly_sev,
                   life_exp,
                   discount,
                   avg_age)
  if(i==1){
    res_sen_prev_pp <- do.call(cbind, temp)
  }else{
    res_sen_prev_pp %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_prev_pp) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_prev_pp$prev = factor*mean(prev_org)

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
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   daly_mal,
                   daly_neuroseq,
                   daly_sev,
                   life_exp,
                   discount,
                   avg_age)
  if(i==1){
    res_sen_hrp2_pp <- do.call(cbind, temp)
  }else{
    res_sen_hrp2_pp %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_hrp2_pp) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_hrp2_pp$deletion = c(0.9, 0.6, 0.4, 0.2)

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
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   daly_mal,
                   daly_neuroseq,
                   daly_sev,
                   life_exp,
                   discount,
                   avg_age)
  if(i==1){
    res_sen_cost_pp <- do.call(cbind, temp)
  }else{
    res_sen_cost_pp %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_cost_pp) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_cost_pp$init_arm3_cost = factor

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
                     rep(0, n),
                     rep(0, n),
                     rep(0, n),
                     rep(0, n),
                     daly_mal,
                     daly_neuroseq,
                     daly_sev,
                     life_exp,
                     discount,
                     avg_age)
    if(i==1 & j==1){
      res_sen_prev_hrp2_pp <- do.call(cbind, temp)
    }else{
      res_sen_prev_hrp2_pp[(i-1)*4+j, 1:18] <-do.call(cbind, temp)
    }
    res_sen_prev_hrp2_pp$prev[(i-1)*4+j] <- mean(prev)
    res_sen_prev_hrp2_pp$hrp2[(i-1)*4+j] <- mean(p_hrp2_deletion)
  }
}
#colnames(res_sen_prev_hrp2_pp)[1:6] %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))

prev <- prev_org
p_hrp2_deletion <- p_hrp2_deletion_org


## Unit cost of arm 2
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
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   rep(0, n),
                   daly_mal,
                   daly_neuroseq,
                   daly_sev,
                   life_exp,
                   discount,
                   avg_age)
  if(i==1){
    res_sen_cost2_pp <- do.call(cbind, temp)
  }else{
    res_sen_cost2_pp %<>% rbind(do.call(cbind, temp))
  }
}
#colnames(res_sen_cost) %<>% paste0(c(rep("_arm2", 3), rep("_arm3", 3)))
res_sen_cost2_pp$per_screen_cost_arm2 = factor

c_screen_arm2 <- c_screen_arm2_org


## Save
pacman::p_load(openxlsx)
wb <- createWorkbook()

addWorksheet(wb, "prevalence")
writeData(wb, "prevalence", res_sen_prev_pp)

addWorksheet(wb, "hrp2_deletion")
writeData(wb, "hrp2_deletion", res_sen_hrp2_pp)

addWorksheet(wb, "arm3_init_cost")
writeData(wb, "arm3_init_cost", res_sen_cost_pp)

addWorksheet(wb, "prev_hrp2")
writeData(wb, "prev_hrp2", res_sen_prev_hrp2_pp)

addWorksheet(wb, "arm2_unit_cost")
writeData(wb, "arm2_unit_cost", res_sen_cost2_pp)

saveWorkbook(wb, file = here::here("results", "Sensitivity analysis_pp.xlsx"), overwrite = TRUE)

