pacman::p_load(magrittr, dplyr, tidyverse, ggplot2, ggthemr, here, rio)

dat <- readRDS(here::here("data", "master_dat_20230118.rds"))
str(dat)
dat %<>% mutate(
  outcome_pcr_pos = ifelse(pcr_res_pf==1 | pcr_res_pv==1, 1, 
                           ifelse(is.na(pcr_res_pf) & is.na(pcr_res_pv), NA, 0))
)

table(dat$outcome_rdt_pos, dat$fu, useNA='ifany')
table(dat$pcr_res_pf, dat$pcr_res_pv, useNA='ifany')
table(dat$pcr_res_pf, dat$fu, useNA='ifany')
table(dat$pcr_res_pv, dat$fu, useNA='ifany')

table(dat$outcome_rdt_pos, dat$outcome_pcr_pos, useNA='ifany')

dat_baseline <- dat %>% filter(fu==1)
table(dat_baseline$outcome_pcr_pos, dat_baseline$outcome_rdt_pos, useNA='ifany') # Row sum is pcr pos, col sum is rdt pos


#################
a <- 45 #TP
b <- 206 #FN
c <- 5 #FP
d <- 2184 #TN

npv <- d/(c+d)
#p_deletion <- 0.5
sen <- a/(a+b) # % of true malaria cases testing positive
spec <- d/(d+c)

sen
spec

a_p <- a
c_p <- a*(1-spec)/spec
d_p <- c_p*npv/(1-npv)
b_p <- d_p*(1-sen)/sen
  
a_pp <- a-a_p
b_pp <- b-b_p
c_pp <- c-c_p
d_pp <- d-d_p
  
new_spec <- d_pp/(d_pp+c_pp) # % of true malaria cases testing positive
new_sen <- a_pp/(a_pp+b_pp) # % of true negative cases testing negative

p_deletion <- (a_pp+b_pp)/(a_p+b_p+a_pp+b_pp)
print(sprintf("percentage deletion = %.3f", p_deletion))
print(new_sen)
print(new_spec)


### Pooling sensitivity and specifciity

pacman::p_load(mada, meta)

## Falcivax
test_dat <- rio::import(here::here("data", "20230818-malaria_diagnostics_performance.xlsx"), which = "FalciVax")
colnames(test_dat)[5] <- "type"
test_dat %<>% select(study_name_short, type, TP, FP, FN, TN)
test_dat %<>% filter(type == "FalciVax")
mada::forest(madad(test_dat), type = "sens")
mada::forest(madad(test_dat), type = "spec")

test_dat %<>% mutate(
  n_sen = TP+FP,
  n_spec = TN + FN
)
# Sensitivity
forest(metaprop(
  event = TP,
  n = n_sen,
  studlab = study_name_short,
  data = test_dat
))

# Specificity
forest(metaprop(
  event = TN,
  n = n_spec,
  studlab = study_name_short,
  data = test_dat
))

### LDH-based
test_dat <- rio::import(here::here("data", "20230818-malaria_diagnostics_performance.xlsx"), which = "LDH-based RDT")
test_dat %<>% select(study_name_short, TP, FP, FN, TN)
test_dat %<>% .[c(1:5),]
mada::forest(madad(test_dat), type = "sens")
mada::forest(madad(test_dat), type = "spec")

test_dat %<>% mutate(
  n_sen = TP+FP,
  n_spec = TN + FN
)
# Sensitivity
forest(metaprop(
  event = TP,
  n = n_sen,
  studlab = study_name_short,
  data = test_dat
))

# Specificity
forest(metaprop(
  event = TN,
  n = n_spec,
  studlab = study_name_short,
  data = test_dat
))
