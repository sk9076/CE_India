## Arm 1

arm1_total <- run_tree(
  n_pop, # total population
  prev, # malaria prevalence
  p_pf, # % of falciparum
  p_pv,
  p_co,
  p_hrp2_deletion, # % of hrp2 deletion among falciparum
  arm1_sen_pf, # Pf sensitivity
  arm1_spec_pf, # Pf specificity
  arm1_sen_pf_del, # Pf (hrp2-) sensitivity
  arm1_spec_pf_del, # Pf (hrp2) specificity
  arm1_sen_pv, # Pv sensitivity
  arm1_spec_pv, # Pv specificity
  arm1_sen_co, # co-infection sensitivity
  arm1_spec_co, # co-infection specificity
  p_trt_eff, # antimalarial treatment effectiveness
  p_sev_no_trt, # % of severe cases when not treated
  p_sev_trt,
  p_mort, # general mortality
  p_neuroseq, # probability of neurosequelae
  p_sev_mort # mortality among severe malaria cases
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
