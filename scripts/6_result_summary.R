# ICER with 95% Credible interval
res_psa %<>% mutate(
  arm2_inc_cost = arm2_total_cost - arm1_total_cost,
  arm3_inc_cost = arm3_total_cost - arm1_total_cost,
  arm2_daly_avt = -1*(arm2_total_daly - arm1_total_daly),
  arm3_daly_avt = -1*(arm3_total_daly - arm1_total_daly)
)

apply(res_psa[,c("arm2_inc_cost", "arm3_inc_cost")],
      2,
      function(x){
        sprintf("Incremental cost= %.2f, 95%% CrI [%.2f, %.2f]",
               median(x),
               quantile(x, 0.025),
               quantile(x, 0.975))
      })

apply(res_psa[,c("arm2_daly_avt", "arm3_daly_avt")],
      2,
      function(x){
        sprintf("DALY averted= %.2f, 95%% CrI [%.2f, %.2f]",
                median(x),
                quantile(x, 0.025),
                quantile(x, 0.975))
      })

apply(res_psa[,c("arm2_icer", "arm3_icer")],
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
gdp_per_cap <- 2389
he_per_cap <- 57
cet_low <- 0.12*gdp_per_cap
cet_high <- 0.30*gdp_per_cap


ggthemr::ggthemr("fresh")


CE_pane_soc <- 
  ggplot(res_psa) +
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
  scale_y_continuous(limits = c(-5000, 20000))+
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

CE_pane_soc
ggsave(plot = CE_pane_soc,
       filename = here::here("results", "CE pane_soc.png"),
       width = 8,
       height = 5)

sprintf("%.3f%% of simulation for Arm 2 were cost-saving.",
        sum(res_psa$arm2_icer<=0 & res_psa$arm2_inc_cost<0)/nrow(res_psa)*100)

sprintf("%.3f%% of simulation for Arm 2 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa$arm2_icer<=cet_low)/nrow(res_psa)*100,
        cet_low)
sprintf("%.3f%% of simulation for Arm 2 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa$arm2_icer<=cet_high)/nrow(res_psa)*100,
        cet_high)
sprintf("%.3f%% of simulation for Arm 2 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa$arm2_icer<=he_per_cap)/nrow(res_psa)*100,
        he_per_cap)

sprintf("%.3f%% of simulation for Arm 3 were cost-saving.",
        sum(res_psa$arm3_icer<=0 & res_psa$arm3_inc_cost<0)/nrow(res_psa)*100)

sprintf("%.3f%% of simulation for Arm 3 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa$arm3_icer<=cet_low)/nrow(res_psa)*100,
        cet_low)
sprintf("%.3f%% of simulation for Arm 3 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa$arm3_icer<=cet_high)/nrow(res_psa)*100,
        cet_high)
sprintf("%.3f%% of simulation for Arm 3 falls under the cost-effectiveness threshold of $%.1f.",
        sum(res_psa$arm3_icer<=he_per_cap)/nrow(res_psa)*100,
        he_per_cap)

sprintf("%.3f%% of simulation for Arm 3 ended up in incremental cost while not averting more DALY than Arm 1.",
        sum(res_psa$arm3_icer<=0 & res_psa$arm3_daly_avt<0)/nrow(res_psa)*100)

# Acceptability curve

wtp <- seq(0, 2500, by = 1)
dat_wtp <- data.frame(
  wtp = wtp,
  arm2_p_ce = NA,
  arm3_p_ce = NA
)

for(i in 0:2500){
  dat_wtp$arm2_p_ce[i] <- sum(res_psa$arm2_icer <wtp[i])/nrow(res_psa)
  dat_wtp$arm3_p_ce[i] <- sum(res_psa$arm3_icer <wtp[i])/nrow(res_psa)
}

arm2_dom <- min(dat_wtp$wtp[which(dat_wtp$arm2_p_ce>0.5)])
arm3_dom <- min(dat_wtp$wtp[which(dat_wtp$arm3_p_ce>0.5)])
arm2_sat <- min(dat_wtp$wtp[which(dat_wtp$arm2_p_ce==1)])
arm3_sat <- min(dat_wtp$wtp[which(dat_wtp$arm3_p_ce==1)])

accept_curve_arm2_soc <-
  ggplot(dat_wtp) + geom_path(aes(wtp, arm2_p_ce, color = "Arm 2")) +
  geom_path(aes(wtp, 1-arm2_p_ce, color = "Arm1")) +
  scale_x_continuous(limits = c(0, 25)) +
  geom_vline(xintercept = 0.35, color = "red", linetype = "dotted")+
  geom_vline(xintercept = arm2_sat, color = "blue", linetype = "dotted") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by=0.2), labels = paste0(seq(0, 100, by=20), " %")) +
  ylab("Acceptability") +
  xlab("Willingness-to-pay ($)") +
  theme(legend.title = element_blank())

accept_curve_arm3_soc <-
  ggplot(dat_wtp) + geom_path(aes(wtp, arm3_p_ce, color = "Arm 3")) +
  geom_path(aes(wtp, 1-arm3_p_ce, color = "Arm1")) +
  scale_x_continuous(limits = c(0, 1000))+
  geom_vline(xintercept = arm3_dom, color = "red", linetype = "dotted")+
  #geom_vline(xintercept = arm3_sat, color = "blue")
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, by=0.2), labels = paste0(seq(0, 100, by=20), " %")) +
  ylab("Acceptability") +
  xlab("Willingness-to-pay ($)") +
  theme(legend.title = element_blank())

ggsave(plot  = ggpubr::ggarrange(accept_curve_arm2_soc,
                  accept_curve_arm3_soc,
                  nrow = 1),
       filename = here::here('results', 'acceptability curve_soc.png'),
       width = 8,
       height = 2.5)
saveRDS(res_psa, here::here("results", "res_psa.RDS"))
  