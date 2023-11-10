# Load index matrix
index_mat <- rio::import(here::here("data", "cost_daly_matrix.xlsx"))
rownames(index_mat) <- index_mat$varname
index_mat %<>% select(-varname, -description)

# number of iterations
n <- 10000

### Fixed parms
n_pop <- rep(2525, n)
life_exp <- rep(70.15, n)
avg_age <- rep(25.68, n)
discount <- rep(0.03, n)

### Common parms
prev <- rtri(n, min= 0.07715168, mode = 0.1028689,max= 0.12858612)
p_trt_eff <- myrbeta(n, mean = 0.955, lower = 0.82, upper = 1.00)
p_sev_no_trt <- myrbeta(n, 0.52, 0.27, 0.64)
p_sev_trt <- p_sev_no_trt*(1-p_trt_eff) #### THIS ONE NEEEDS TO BE DISCUSSED!
p_mort <- myrbeta(n, 0.0067531, 0.0060615, 0.0074966)
p_neuroseq <- myrbeta(n, 0.015, 0.01, 0.02)
p_sev_mort <- myrbeta(n, 0.1, 0.05, 0.15)
p_pf <- rtri(n, min = 0.97076023*0.75, max = 1, mode = 0.97076023)
p_pv <- NA
for(i in 1:n){
  temp <- rtri(1, min = 0.01754386*0.75, max = 0.01754386*1.25, mode = 0.01754386)
  while(temp+p_pf[i]>1){
    temp <- rtri(1, min = 0, max = 0.01754386*1.25, mode = 0.01754386)
  }
  p_pv[i] <- temp
}

p_co <- 1-p_pf-p_pv
p_hrp2_deletion <- rtri(n, min = 0.8193717*0.75, max = 1, mode = 0.8193717)

# Adjusting factors
dec_perf <- rtri(n, 0.0682902*0.75, 0.0682902*1.25, 0.0682902) # degree to which hrp2 deletion affects the sensitivity of detection (arbitrary)
rdt_ref_sen <- myrbeta(n, 0.842, 0.798, 0.88) # converting the ref gold standard from microscopy to PCR

### Arm specific parms ----> This has to be fixed!!!!!
arm1_sen_pf <- myrbeta(n, 0.90, 0.81, 0.95)*rdt_ref_sen
arm1_spec_pf <- myrbeta(n, 0.95, 0.86, 0.99)
arm1_sen_pf_del <- arm1_sen_pf*dec_perf
arm1_spec_pf_del <- arm1_spec_pf 
arm1_sen_pv <- arm1_sen_pf
arm1_spec_pv <- arm1_spec_pf
arm1_sen_co <- arm1_sen_pf
arm1_spec_co <- arm1_spec_pf
arm1_sen_co_del <- arm1_sen_pf_del
arm1_spec_co_del <- arm1_spec_pf


arm2_sen_pf <- myrbeta (n,0.92, 0.85, 0.96)*rdt_ref_sen
arm2_spec_pf <- myrbeta(n, 0.98, 0.96, 0.99) 
arm2_sen_pf_del <- arm2_sen_pf
arm2_spec_pf_del <- arm2_spec_pf
arm2_sen_pv <- arm2_sen_pf
arm2_spec_pv <- arm2_spec_pf
arm2_sen_co <- arm2_sen_pf
arm2_spec_co <- arm2_spec_pf
arm2_sen_co_del <- arm2_sen_pf_del
arm2_spec_co_del <- arm2_spec_pf_del

arm3_sen_pf <- myrbeta(n, 0.971, 0.957, 0.98)
arm3_spec_pf <- myrbeta(n, 0.956, 0.938, 0.968)
arm3_sen_pf_del <- arm3_sen_pf
arm3_spec_pf_del <- arm3_spec_pf
arm3_sen_pv <- arm3_sen_pf
arm3_spec_pv <- arm3_spec_pf
arm3_sen_co <- arm3_sen_pf
arm3_spec_co <- arm3_spec_pf
arm3_sen_co_del <- arm3_sen_pf_del
arm3_spec_co_del <- arm3_spec_pf_del

### Cost
c_init_arm1 <- rep(0, n)
c_screen_arm1 <- rtri(n, 0.27405851*0.75, 0.27405851*1.25, 0.27405851) 
c_init_arm2 <- rtri(n, 128.464925*0.75, 128.464925*1.25, 128.464925) ### Need to be discussed
c_screen_arm2 <- c_screen_arm1 ### Assumed to be same as the existing RDT?
c_init_arm3 <- c_init_arm2 + rtri(n, 629.4791091*0.75, 629.4791091*1.25, 629.4791091) #+equipment cost
c_screen_arm3 <- rtri(n, 6.46*0.75, 6.46*1.25, 6.46) 

c_trt <- rtri(n, 0.357210348*0.75, 0.357210348*1.25, 0.357210348)
c_personnel <- rtri(n, 0.118062454*0.75, 0.118062454*1.25, 0.118062454)
c_msat <- rtri(n, 3.20305879*0.75, 3.20305879*1.25, 3.20305879)
c_hh_mal <- myrgamma(n, 1.157897188, 1.978359841)
c_hh_sev <- myrgamma(n, 2.925574553, 2.536754047)
c_hh_neuroseq <- c_hh_sev
c_indirect <- myrgamma(n, 30.73394859, 21.28920733)
c_trt_sev <- c_trt*mean(c_hh_sev)/mean(c_hh_mal)

### Utility
daly_mal <- myrbeta(n, 0.051, 0.032, 0.074)
daly_sev <- myrbeta(n, 0.542, 0.374, 0.702)
daly_neuroseq <- myrbeta(n, 0.133, 0.088, 0.190) 


