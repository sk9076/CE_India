## Tornado diagram
pacman::p_load(gridExtra, grid, ggtext)

### Set parms for tornado diagram
parm_tab <- c(
n_pop = 2525,

prev = 0.1028689,
p_trt_eff = 0.955,
p_sev_no_trt = 0.52,

p_mort = 0.0067531,
p_neuroseq = 0.015,
p_sev_mort = 0.1,
p_pf = 0.97076023,
p_pv = 0.01754386,
p_hrp2_deletion = 0.8193717,

### Adjusting factors
dec_perf = 0.0682902,
rdt_ref_sen = 0.842,

### Arm specific parms
arm1_sen_pf = 0.90*0.842,
arm1_spec_pf = 0.95,

arm2_sen_pf = 0.92*0.842,
arm2_spec_pf = 0.98,

arm3_sen_pf = 0.971,
arm3_spec_pf = 0.956,

### Cost
c_init_arm1 = 0,
c_screen_arm1 = 0.274058506,
c_init_arm2 = 128.4649247,
c_screen_arm2 = 0.274058506,
c_init_arm3 = 757.944034,
c_screen_arm3 = 6.46,

c_trt = 0.357210348,
c_personnel = 0.118062454,
c_msat = 3.20305879,
c_hh_mal = 1.157897188,
c_hh_sev = 2.925574553,
c_indirect = 30.73394859,

### Utility
daly_mal = 0.051,
daly_sev = 0.542,
daly_neuroseq = 0.133,

life_exp =70.15,
avg_age = 25.68,
discount = 0.03
)

## Parms to change

dat_tor <- data.frame(
  parm_name = names(parm_tab),
  parm_val = parm_tab
  )

dat_tor %<>% mutate(
  parm_low = parm_val*0.75,
  parm_high = parm_val*1.25
) %>% mutate(
  parm_high = ifelse((str_detect(parm_name, "p_")|
                        str_detect(parm_name, "daly_")|
                        str_detect(parm_name, "_spec_")|
                        str_detect(parm_name, "_sen")) &
                       parm_high>1, 1, parm_high)
)

dat_tor$parm_high[which(dat_tor$parm_name=="p_pf")] <- 1-dat_tor$parm_val[which(dat_tor$parm_name=="p_pv")]
dat_tor

for(i in 1:nrow(dat_tor)){
  # set up a temporary parm_tab
  parm_tab_low <- parm_tab_high <- dat_tor$parm_val
  names(parm_tab_low) <- names(parm_tab_high) <- dat_tor$parm_name
  
  # update one parameter
  parm_tab_high[dat_tor$parm_name[i]] <- dat_tor$parm_high[i]
  parm_tab_low[dat_tor$parm_name[i]] <- dat_tor$parm_low[i]
  
  # run an analysis
  res_high <- run_tornado(parm_tab_high)
  res_low <- run_tornado(parm_tab_low)
  
  res_temp_tornado <- data.frame(
    parm_name = dat_tor$parm_name[i],
    arm2_icer_pp_high = res_high$icer_arm2_pp,
    arm2_icer_soc_high = res_high$icer_arm2_soc,
    arm3_icer_pp_high = res_high$icer_arm3_pp,
    arm3_icer_soc_high = res_high$icer_arm3_soc,
    arm2_icer_pp_low = res_low$icer_arm2_pp,
    arm2_icer_soc_low = res_low$icer_arm2_soc,
    arm3_icer_pp_low = res_low$icer_arm3_pp,
    arm3_icer_soc_low = res_low$icer_arm3_soc,
    arm2_inc_cost_pp_high = res_high$arm2_total_cost_pp - res_high$arm1_total_cost_pp,
    arm2_inc_cost_soc_high = res_high$arm2_total_cost_soc - res_high$arm1_total_cost_soc,
    arm2_inc_cost_pp_low = res_low$arm2_total_cost_pp - res_low$arm1_total_cost_pp,
    arm2_inc_cost_soc_low = res_low$arm2_total_cost_soc - res_low$arm1_total_cost_soc,
    arm3_inc_cost_pp_high = res_high$arm3_total_cost_pp - res_high$arm1_total_cost_pp,
    arm3_inc_cost_soc_high = res_high$arm3_total_cost_soc - res_high$arm1_total_cost_soc,
    arm3_inc_cost_pp_low = res_low$arm3_total_cost_pp - res_low$arm1_total_cost_pp,
    arm3_inc_cost_soc_low = res_low$arm3_total_cost_soc - res_low$arm1_total_cost_soc,
    arm2_inc_daly_high = res_high$arm1_total_daly - res_high$arm2_total_daly,
    arm2_inc_daly_low = res_low$arm1_total_daly - res_low$arm2_total_daly,
    arm3_inc_daly_high = res_high$arm1_total_daly - res_high$arm3_total_daly,
    arm3_inc_daly_low = res_low$arm1_total_daly - res_low$arm3_total_daly
  )
  # save the results
  if(i==1){
    res_tornado <- res_temp_tornado
  }else{
    res_tornado %<>% rbind(res_temp_tornado)
  }
}

rio::export(res_tornado, here::here('results', 'res_tornado.xlsx'))

## Draw the plot
base_icer_arm2_pp <- 0.98
base_icer_arm2_soc <- 0.40
base_icer_arm3_pp <- 114.05
base_icer_arm3_soc <- 113.43

## Provider perspective arm 2
ggplot(res_tornado, aes(x=reorder(parm_name, abs(arm2_icer_pp_low-base_icer_arm2_pp)))) +
  geom_bar(aes(y=arm2_icer_pp_high-base_icer_arm2_pp, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm2_icer_pp_low-base_icer_arm2_pp, fill = "-25%"), stat = "identity") +
  coord_flip()

## Societal perspective arm 2
ggplot(res_tornado, aes(x=reorder(parm_name, abs(arm2_icer_soc_low-base_icer_arm2_soc)))) +
  geom_bar(aes(y=arm2_icer_soc_high-base_icer_arm2_soc, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm2_icer_soc_low-base_icer_arm2_soc, fill = "-25%"), stat = "identity") +
  coord_flip()

## Provider perspective arm 3
ggplot(res_tornado, aes(x=reorder(parm_name, abs(arm3_icer_pp_low-base_icer_arm3_pp)))) +
  geom_bar(aes(y=arm3_icer_pp_high-base_icer_arm3_pp, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm3_icer_pp_low-base_icer_arm3_pp, fill = "-25%"), stat = "identity") +
  coord_flip()

## Societal perspective arm 3
ggplot(res_tornado, aes(x=reorder(parm_name, abs(arm3_icer_soc_low-base_icer_arm3_soc)))) +
  geom_bar(aes(y=arm3_icer_soc_high-base_icer_arm3_soc, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm3_icer_soc_low-base_icer_arm3_soc, fill = "-25%"), stat = "identity") +
  coord_flip()


### Percentage plot
res_tornado_p <- res_tornado %>%
  mutate(
    arm2_p_change_low_soc = (arm2_icer_soc_low-base_icer_arm2_soc)/base_icer_arm2_soc*100,
    arm2_p_change_high_soc = (arm2_icer_soc_high-base_icer_arm2_soc)/base_icer_arm2_soc*100,
    arm2_p_change_low_pp= (arm2_icer_pp_low-base_icer_arm2_pp)/base_icer_arm2_pp*100,
    arm2_p_change_high_pp = (arm2_icer_pp_high-base_icer_arm2_pp)/base_icer_arm2_pp*100,
    arm3_p_change_low_soc = (arm3_icer_soc_low-base_icer_arm3_soc)/base_icer_arm3_soc*100,
    arm3_p_change_high_soc = (arm3_icer_soc_high-base_icer_arm3_soc)/base_icer_arm3_soc*100,
    arm3_p_change_low_pp = (arm3_icer_pp_low-base_icer_arm3_pp)/base_icer_arm3_pp*100,
    arm3_p_change_high_pp = (arm3_icer_pp_high-base_icer_arm3_pp)/base_icer_arm3_pp*100
  )

long_var_name <- c(
  "n_pop"    = "Target population size",     
  "prev"     = "Malaria prevalence in the target population",
  "p_trt_eff"      = "Antimalarial treatment effectiveness",
  "p_sev_no_trt"   = "% of severe cases among untreated malaria",
  "p_mort"          =  "all-cause mortality",
  "p_neuroseq"     = "% of neurosequalae among severe cases",
  "p_sev_mort"     = "Mortality among severe cases",
  "p_pf"           = "% of P.falciparum infection among total malaria cases",
  "p_pv"           = "% of P.vivax infection among total malaria cases",
  "p_hrp2_deletion" = "% of pfhrp2 deletion among P.falciparum infection",
  "dec_perf"       = "Diagnostic performance decrease factor due to pfhrp2 deletion",
  "arm1_sen_pf"    = "HRP-2-based RDT sensitivity",
  "arm1_spec_pf"   = "HRP-2-based RDT specificity",
  "arm2_sen_pf"     = "LDH-based RDT sensitivity",
  "arm2_spec_pf" = "LDH-based RDT specificity",
  "arm3_sen_pf" = "Molecular diagnostic sensitivity",
  "arm3_spec_pf" = "Molecular diagnostic specificity",
  "c_init_arm1" = "Arm 1 initial set-up cost",
  "c_screen_arm1" = "Per-screening unit cost of HRP-2-based RDT",
  "c_init_arm2"    = "Arm 2 initial set-up cost",
  "c_screen_arm2"   ="Per-screening unit cost of LDH-based RDT",
  "c_init_arm3"    = "Arm 2 initial set-up cost",
  "c_screen_arm3"  ="Per-screening unit cost of molecular diagnosis",
  "c_trt"         = "Cost of treatment for uncomplicated malaria",
  "c_personnel"   = "Personnel cost of MSAT per screening",
  "c_msat"          = "MSAT operational cost per screening",
  "c_hh_mal"       = "Household out-of-pocket cost for uncomplicated malaria",
  "c_hh_sev"       = "Household out-of-pocket cost for severe malaria",
 "c_indirect"     = "Total indirect cost of malaria",
 "daly_mal"        = "Disability weight of uncomplicated malaria",
"daly_sev"       = "Disability weight of severe malaria",
 "daly_neuroseq"  = "Disability weight of neurological sequelae",
 "life_exp"       = "Life expectancy of target population",
 "avg_age"        = "Average age of target population",
"discount" = "Annual discount rate",
"rdt_ref_sen" = "Microscopy sensitivity (vs. RDT)"
)

df_long_var_name <- data.frame(
  parm_name = names(long_var_name),
  long_name = long_var_name
)

res_tornado_p %<>% left_join(df_long_var_name, by = "parm_name") %>% 
  filter(!parm_name %in% c("c_init_arm1", "rdt_ref_sen") )
res_tornado_p %<>% mutate(
  rank_arm2_pp = ifelse(abs(arm2_p_change_high_pp)>abs(arm2_p_change_low_pp), abs(arm2_p_change_high_pp),abs(arm2_p_change_low_pp)),
  rank_arm2_soc = ifelse(abs(arm2_p_change_high_soc)>abs(arm2_p_change_low_soc), abs(arm2_p_change_high_soc),abs(arm2_p_change_low_soc)),
  rank_arm3_pp = ifelse(abs(arm3_p_change_high_pp)>abs(arm3_p_change_low_pp), abs(arm3_p_change_high_pp),abs(arm3_p_change_low_pp)),
  rank_arm3_soc = ifelse(abs(arm3_p_change_high_soc)>abs(arm3_p_change_low_soc), abs(arm3_p_change_high_soc),abs(arm3_p_change_low_soc))
)

tornado_arm2_pp <- 
  ggplot(res_tornado_p %>% filter(!parm_name %in% c("arm3_sen_pf", "arm3_spec_pf", "c_init_arm3", "c_screen_arm3",
                                                    "c_hh_mal", "c_indirect", "c_hh_sev")), 
         aes(x=reorder(long_name, rank_arm2_pp))) +
  geom_bar(aes(y=arm2_p_change_high_pp, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm2_p_change_low_pp, fill = "-25%"), stat = "identity") +
  scale_fill_manual(values = c("+25%" = "#c763a2",
                               "-25%" = "#4a8575"))+
  scale_y_continuous(label = scales::label_number(suffix = " %"))+
  xlab("Input parameter") + 
  ylab("% change in incremental cost-effectiveness ratio (ICER)")+
  #ggtitle("Healthcare Provider Perspective") +
  theme(legend.position = "none",
        axis.text = ggtext::element_markdown())+
  coord_flip() 

tornado_arm2_soc <- 
  ggplot(res_tornado_p%>% filter(!parm_name %in% c("arm3_sen_pf", "arm3_spec_pf", "c_init_arm3", "c_screen_arm3"
                                                   
                                                   )), 
         aes(x=reorder(long_name, rank_arm2_soc))) +
  geom_bar(aes(y=arm2_p_change_high_soc, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm2_p_change_low_soc, fill = "-25%"), stat = "identity") +
  scale_fill_manual(values = c("+25%" = "#c763a2",
                               "-25%" = "#4a8575"))+
  scale_y_continuous(label = scales::label_number(suffix = " %"))+
  xlab("") + 
  ylab("% change in incremental cost-effectiveness ratio (ICER)")+
  #ggtitle("Societal Perspective") +
  theme(legend.title = element_blank(),
        axis.text = ggtext::element_markdown())+
  coord_flip() 

tornado_comb_arm2 <- grid.arrange(
  textGrob("(A) Healthcare Provider Perspective", gp = gpar(fontsize = 13, fontface = 'bold')),
  textGrob("(B) Societal Perspective", gp = gpar(fontsize = 13, fontface = 'bold')),
  tornado_arm2_pp,
  tornado_arm2_soc,
  heights = c(0.1, 1),
  widths = c(1, 1.2),
  nrow= 2
)

ggsave(plot = tornado_comb_arm2,
       filename = here::here('results', 'tornado_comb_arm2.png'),
       width = 14,
       height = 6)


## ARm 3 combined plot
tornado_arm3_pp <- 
  ggplot(res_tornado_p%>% filter(!parm_name %in% c("arm2_sen_pf", "arm2_spec_pf", "c_init_arm2", "c_screen_arm2",
                                                   "c_hh_mal", "c_indirect", "c_hh_sev"
                                                   )), 
         aes(x=reorder(long_name, rank_arm3_pp))) +
  geom_bar(aes(y=arm3_p_change_high_pp, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm3_p_change_low_pp, fill = "-25%"), stat = "identity") +
  scale_fill_manual(values = c("+25%" = "#c763a2",
                               "-25%" = "#4a8575"))+
  scale_y_continuous(label = scales::label_number(suffix = " %"))+
  xlab("Input parameter") + 
  ylab("% change in incremental cost-effectiveness ratio (ICER)")+
  #ggtitle("Healthcare Provider Perspective") +
  theme(legend.position = "none")+
  coord_flip() 

tornado_arm3_soc <- 
  ggplot(res_tornado_p%>% filter(!parm_name %in% c("arm2_sen_pf", "arm2_spec_pf", "c_init_arm2", "c_screen_arm2"
                                                   )), 
         aes(x=reorder(long_name, rank_arm3_soc))) +
  geom_bar(aes(y=arm3_p_change_high_soc, fill = "+25%"), stat = "identity") +
  geom_bar(aes(y=arm3_p_change_low_soc, fill = "-25%"), stat = "identity") +
  scale_fill_manual(values = c("+25%" = "#c763a2",
                               "-25%" = "#4a8575"))+
  scale_y_continuous(label = scales::label_number(suffix = " %"))+
  xlab("") + 
  ylab("% change in incremental cost-effectiveness ratio (ICER)")+
  #ggtitle("Societal Perspective") +
  theme(legend.title = element_blank())+
  coord_flip() 

tornado_comb_arm3 <- grid.arrange(
  textGrob("(A) Healthcare Provider Perspective", gp = gpar(fontsize = 13, fontface = 'bold')),
  textGrob("(B) Societal Perspective", gp = gpar(fontsize = 13, fontface = 'bold')),
  tornado_arm3_pp,
  tornado_arm3_soc,
  heights = c(0.1, 1),
  widths = c(1, 1.2),
  nrow= 2
)

ggsave(plot = tornado_comb_arm3,
       filename = here::here('results', 'tornado_comb_arm3.png'),
       width = 14,
       height = 6)

