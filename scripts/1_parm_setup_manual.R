### Common parms
n_pop <- 2525

prev <- 0.1028689
p_trt_eff <- 0.955
p_sev_no_trt <- 0.52
p_sev_trt <- p_sev_no_trt*(1-p_trt_eff)
p_mort <- 0.0067531
p_neuroseq <- 0.015
p_sev_mort <- 0.1
p_pf <- 0.97076023
p_pv <- 0.01754386
p_co <- 1-p_pf-p_pv
p_hrp2_deletion <- 0.8193717

### Adjusting factors
dec_perf <- 0.0682902
rdt_ref_sen <- 0.842

### Arm specific parms
arm1_sen_pf <- 0.90*rdt_ref_sen
arm1_spec_pf <- 0.95
arm1_sen_pf_del <- arm1_sen_pf*dec_perf
arm1_spec_pf_del <- arm1_spec_pf
arm1_sen_pv <- arm1_sen_pf
arm1_spec_pv <- arm1_spec_pf
arm1_sen_co <- arm1_sen_pf
arm1_spec_co <- arm1_spec_pf
arm1_sen_co_del <- arm1_sen_pf_del
arm1_spec_co_del <- arm1_spec_pf_del


arm2_sen_pf <- 0.92*rdt_ref_sen
arm2_spec_pf <- 0.98
arm2_sen_pf_del <- arm2_sen_pf
arm2_spec_pf_del <- arm2_spec_pf
arm2_sen_pv <- arm2_sen_pf
arm2_spec_pv <- arm2_spec_pf
arm2_sen_co <- arm2_sen_pf
arm2_spec_co <- arm2_spec_pf
arm2_sen_co_del <- arm2_sen_pf_del
arm2_spec_co_del <- arm2_spec_pf_del

arm3_sen_pf <- 0.971
arm3_spec_pf <- 0.956
arm3_sen_pf_del <- arm3_sen_pf
arm3_spec_pf_del <- arm3_spec_pf
arm3_sen_pv <- arm3_sen_pf
arm3_spec_pv <- arm3_spec_pf
arm3_sen_co <- arm3_sen_pf
arm3_spec_co <- arm3_spec_pf
arm3_sen_co_del <- arm3_sen_pf_del
arm3_spec_co_del <- arm3_spec_pf_del

### Cost
c_init_arm1 <- 0
c_screen_arm1 <- 0.274058506
c_init_arm2 <- 128.4649247
c_screen_arm2 <- 0.274058506
c_init_arm3 <- 757.944034
c_screen_arm3 <- 6.46

c_trt <- 0.357210348
c_personnel <- 0.118062454
c_msat <- 3.20305879
c_hh_mal <- 1.157897188
c_hh_sev <- 2.925574553
c_hh_neuroseq <- c_hh_sev
c_indirect <- 30.73394859
c_trt_sev <- c_trt*c_hh_sev/c_hh_mal

### Utility
daly_mal <- 0.051
daly_sev <- 0.542
daly_neuroseq <- 0.133

life_exp <- 70.15
avg_age <- 25.68
discount <- 0.03
