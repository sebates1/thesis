########################################################
# Adding MoA to SPHR model: Psychological subgroups    #
########################################################

load("df_subgp_50.RData")
df_subgp<-df_subgp_50

load("data/PSAResults00.RData")
load("data/PSAResultsAA.RData")
load("data/PSAResultsBB.RData")
load("df_subgp_quar.RData")
df_subgp<-df_subgp_quar

# Matrix of outcomes (total costs and QALYs)
m_sg_total<-matrix(nrow = 9,ncol = 18, dimnames = list(c( "Total", "High habit strength", "Low habit strength", "High dietary restraint",
                                                            "Low dietary restraint", "High ADSR", "Low ADSR", "High all", "Low all"),
                                                         c("Total_costs_BI", "LCI_costs_BI", "HCI_costs_BI", "Total_QALYS_BI","LCI_QALYS_BI","LCI_QALYS_BI",
                                                           "Total_costs_12", "LCI_costs_12", "HCI_costs_12", "Total_QALYS_12","LCI_QALYS_12","LCI_QALYS_12",
                                                           "Total_costs_52", "LCI_costs_52", "HCI_costs_52", "Total_QALYS_52","LCI_QALYS_52","LCI_QALYS_52"
                                                         )))

# Populate matrix
for(i in 1:nrow(m_sg_total)){
  
  seq<-(c(2,7,12,17,22,27,32,37,42))
  p <- seq[[i]]
  
  m_sg_total[i, ]<-c(mean(PSAResults00[, p]/PSAResults00[,(p-1)]), sort(PSAResults00[, p]/PSAResults00[,(p-1)])[975], sort(PSAResults00[, p]/PSAResults00[,(p-1)])[25],
                       mean(PSAResults00[, (p+1)]/PSAResults00[,(p-1)]), sort(PSAResults00[, (p+1)]/PSAResults00[,(p-1)])[975], sort(PSAResults00[, (p+1)]/PSAResults00[,(p-1)])[25],
                       mean(PSAResultsAA[, p]/PSAResultsAA[,(p-1)]), sort(PSAResultsAA[, p]/PSAResultsAA[,(p-1)])[975], sort(PSAResultsAA[, p]/PSAResultsAA[,(p-1)])[25],
                       mean(PSAResultsAA[, (p+1)]/PSAResultsAA[,(p-1)]), sort(PSAResultsAA[,(p+1)]/PSAResultsAA[,(p-1)])[975], sort(PSAResultsAA[, (p+1)]/PSAResultsAA[,(p-1)])[25],
                       mean(PSAResultsBB[, p]/PSAResultsBB[,(p-1)]), sort(PSAResultsBB[, p]/PSAResultsBB[,(p-1)])[975], sort(PSAResultsBB[, p]/PSAResultsBB[,(p-1)])[25],
                       mean(PSAResultsBB[, (p+1)]/PSAResultsBB[,(p-1)]), sort(PSAResultsBB[, (p+1)]/PSAResultsBB[,(p-1)])[975], sort(PSAResultsBB[, (p+1)]/PSAResultsBB[,(p-1)])[25])
}

# Reformat and rearrange matrix for graphs
df_sg_total <- as.data.frame(m_sg_total)
df_sg_total$model_spec <- row.names(df_sg_total)

df_sg_total_cBI <- df_sg_total[,c(1,2,3,19)]
df_sg_total_qBI <- df_sg_total[,c(4,5,6,19)]

df_sg_total_c12 <- df_sg_total[,c(7,8,9,19)]
df_sg_total_q12 <- df_sg_total[,c(10,11,12,19)]

df_sg_total_c52 <- df_sg_total[,c(13,14,15,19)]
df_sg_total_q52 <- df_sg_total[,c(16,17,18,19)]

df_sg_total_cBI$TX<-df_sg_total_qBI$TX<-"Brief intervention"
df_sg_total_c12$TX<-df_sg_total_q12$TX<-"12-week intervention"
df_sg_total_c52$TX<-df_sg_total_q52$TX<-"52-week intervention"

colnames(df_sg_total_cBI)<-c("Total", "L_CI", "H_CI", "model_spec", "treatment")
colnames(df_sg_total_qBI)<-c("Total", "L_CI", "H_CI", "model_spec", "treatment")
colnames(df_sg_total_c12)<-c("Total", "L_CI", "H_CI", "model_spec", "treatment")
colnames(df_sg_total_q12)<-c("Total", "L_CI", "H_CI", "model_spec", "treatment")
colnames(df_sg_total_c52)<-c("Total", "L_CI", "H_CI", "model_spec", "treatment")
colnames(df_sg_total_q52)<-c("Total", "L_CI", "H_CI", "model_spec", "treatment")

df_sg_tot_clong<-rbind(df_sg_total_cBI,df_sg_total_c12,df_sg_total_c52)
df_sg_tot_qlong<-rbind(df_sg_total_qBI,df_sg_total_q12,df_sg_total_q52)

df_sg_tot_clong$model_spec<-factor(df_sg_tot_clong$model_spec, levels=unique(df_sg_tot_clong$model_spec))
df_sg_tot_qlong$model_spec<-factor(df_sg_tot_qlong$model_spec, levels=unique(df_sg_tot_qlong$model_spec))
df_sg_tot_clong$treatment<-factor(df_sg_tot_clong$treatment, levels=unique(df_sg_tot_clong$treatment))
df_sg_tot_qlong$treatment<-factor(df_sg_tot_clong$treatment, levels=unique(df_sg_tot_clong$treatment))

# Graph of costs accross subgroups
p_costs <- ggplot(df_sg_tot_clong, aes(fill=model_spec, y=Total, x=model_spec)) + 
  geom_bar(position="dodge", stat="identity", width=0.8, col="black", size=0.1, alpha = 0.75)+
  ylab("Costs per person")+
  xlab("Subgroup")+
  coord_cartesian(ylim=c(15000,50000))+
  scale_fill_manual(values = c("grey67", "indianred1", "indianred1", "darkseagreen3",  "darkseagreen3",
                                "lightsteelblue3", "lightsteelblue3", "lightgoldenrod2", "lightgoldenrod2"))+
  #  ylim(10000,50000)+
  geom_errorbar(aes(ymin=L_CI, ymax=H_CI), width=.2,position=position_dodge(.9)) +
  theme_minimal() +
  facet_wrap(~treatment) +
  theme(legend.position = "none", legend.title=element_blank(),text = element_text(size=25), 
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank() )

# Graph of QALYs accross subgroups
p_qlays <- ggplot(df_sg_tot_qlong, aes(fill=model_spec, y=Total, x=model_spec)) + 
  geom_bar(position="dodge", stat="identity", width=0.8, col="black", size=0.1, alpha = 0.75)+
  ylab("QALYs per person")+
  xlab("Subgroup")+
  coord_cartesian(ylim=c(8,11))+
  scale_fill_manual(values = c("grey67", "indianred1", "indianred1", "darkseagreen3",  "darkseagreen3",
                               "lightsteelblue3", "lightsteelblue3", "lightgoldenrod2", "lightgoldenrod2"))+
  #  ylim(10000,50000)+
  geom_errorbar(aes(ymin=L_CI, ymax=H_CI), width=.2,position=position_dodge(.9)) +
  theme_minimal() +
  facet_wrap(~treatment) +
  theme(legend.position = "none", legend.title=element_blank(),text = element_text(size=25), 
        axis.text.x=element_text(angle=45, hjust=1))

# Arrange plots together
full_plane_CE <- ggarrange(p_costs, p_qlays,
                           labels = c("a", "b"),
                           ncol = 1, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "none")

ggsave("p_costs_qalys.png", width = 20, height = 15)

# Matrix of outcomes (incremental)
m_sg_inc<-matrix(nrow = 9,ncol = 18, dimnames = list(c( "Total", "High habit strength", "Low habit strength", "High dietary restraint",
                                                          "Low dietary restraint", "High ADSR", "Low ADSR", "High all", "Low all"),
                                                       c("costs_BI12", "LCI_costs_BI12", "HCI_costs_BI12", "QALYs_BI12","LCI_QALYs_BI12","LCI_QALYs_BI12",
                                                         "NMB_BI12", "LCI_NMB_BI12", "HCI_NMB_BI12",
                                                         "costs_BI52", "LCI_costs_BI52", "HCI_costs_BI52", "QALYs_BI52","LCI_QALYs_BI52","LCI_QALYs_BI52",
                                                         "NMB_BI52", "LCI_NMB_BI52", "HCI_NMB_BI52")))


# Populate each row of matrix 
for(i in 1:nrow(m_sg_inc)){
  
  seq<-(c(2,7,12,17,22,27,32,37,42))
  p <- seq[[i]]
  
  inc_c_12 <- (PSAResultsAA[,p]-PSAResults00[,p])/PSAResults00[,(p-1)]
  inc_q_12 <- (PSAResultsAA[,(p+1)]-PSAResults00[,(p+1)])/PSAResults00[,(p-1)]
  nmb_12 <- inc_q_12*20000-inc_c_12
  
  inc_c_52 <- (PSAResultsBB[,p]-PSAResults00[,p])/PSAResults00[,(p-1)]
  inc_q_52 <- (PSAResultsBB[,(p+1)]-PSAResults00[,(p+1)])/PSAResults00[,(p-1)]
  nmb_52 <- inc_q_52*20000-inc_c_52
  
  m_sg_inc[i, ]<-c(mean(inc_c_12), sort(inc_c_12)[975], sort(inc_c_12)[25], 
                   mean(inc_q_12), sort(inc_q_12)[975], sort(inc_q_12)[25], 
                   mean(nmb_12), sort(nmb_12)[975], sort(nmb_12)[25], 
                   mean(inc_c_52), sort(inc_c_52)[975], sort(inc_c_52)[25], 
                   mean(inc_q_52), sort(inc_q_52)[975], sort(inc_q_52)[25], 
                   mean(nmb_52), sort(nmb_52)[975], sort(nmb_52)[25])

}
 
# Save outcome tables                    
write.table(m_sg_total, "total_sg.txt")
write.table(m_sg_inc, "m_sg_inc.txt")

# Create cost-effectiveness planes for each subgroup

# Ensure matrix format
df_subgp<-as.matrix(df_subgp)

# Cost-effectiveness planes for totals
p_CE12_total = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=df_subgp[,"total_12_Q"], y=df_subgp[,"total_12_C"]),size=1, show.legend = T, col="coral", alpha = 0.5) +
  geom_point(aes(x=mean(df_subgp[,"total_12_Q"], na.rm = T),y=mean(df_subgp[,"total_12_C"], na.rm=T)), size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.025),y=c(-500,500), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  scale_linetype()+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plots/p_CE12_total.png")  

p_CE52_total = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=df_subgp[,"total_52_Q"], y=df_subgp[,"total_52_C"]),size=1, show.legend = T, col="coral", alpha = 0.5) +
  geom_point(aes(x=mean(df_subgp[,"total_52_Q"], na.rm = T),y=mean(df_subgp[,"total_52_C"], na.rm=T)), size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.025),y=c(-500,500), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  scale_linetype()+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plots/p_CE52_total.png")  

# Cost-effectiveness plane for high vs low on each MoA
p_sg_habit12 <- plot_subgroup_ce(high_q=df_subgp[,"HH_12_Q"], low_q = df_subgp[,"LH_12_Q"], 
                               high_c = df_subgp[,"HH_12_C"], low_c = df_subgp[,"LH_12_C"])
p_sg_habit52 <- plot_subgroup_ce(high_q=df_subgp[,"HH_52_Q"], low_q = df_subgp[,"LH_52_Q"], 
                                 high_c = df_subgp[,"HH_52_C"], low_c = df_subgp[,"LH_52_C"])
p_sg_res12 <- plot_subgroup_ce(high_q=df_subgp[,"HR_12_Q"], low_q = df_subgp[,"LR_12_Q"], 
                                 high_c = df_subgp[,"HR_12_C"], low_c = df_subgp[,"LR_12_C"])
p_sg_res52 <- plot_subgroup_ce(high_q=df_subgp[,"HR_52_Q"], low_q = df_subgp[,"LR_52_Q"], 
                               high_c = df_subgp[,"HR_52_C"], low_c = df_subgp[,"LR_52_C"])
p_sg_adsr12 <- plot_subgroup_ce(high_q=df_subgp[,"HREG_12_Q"], low_q = df_subgp[,"LREG_12_Q"], 
                               high_c = df_subgp[,"HREG_12_C"], low_c = df_subgp[,"LREG_12_C"])
p_sg_adsr52 <- plot_subgroup_ce(high_q=df_subgp[,"HREG_52_Q"], low_q = df_subgp[,"LREG_52_Q"], 
                                high_c = df_subgp[,"HREG_52_C"], low_c = df_subgp[,"LREG_52_C"])
p_sg_all12 <- plot_subgroup_ce(high_q=df_subgp[,"HA_12_Q"], low_q = df_subgp[,"LA_12_Q"], 
                                high_c = df_subgp[,"HA_12_C"], low_c = df_subgp[,"LA_12_C"])
p_sg_all52 <- plot_subgroup_ce(high_q=df_subgp[,"HA_52_Q"], low_q = df_subgp[,"LA_52_Q"], 
                               high_c = df_subgp[,"HA_52_C"], low_c = df_subgp[,"LA_52_C"])
  
# Arrange and save plots
full_plane <- ggarrange(p_sg_habit12,p_sg_res12, p_sg_adsr12, p_sg_all12,
                       labels = c("a", "b", "c", "d"),
                       ncol = 2, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "top")
full_plane_fig12_psy <- annotate_figure(full_plane,
                                 bottom = text_grob("Incremental QALYs"),
                                 left = text_grob("Incremental Costs", rot=90))
ggsave("full_plane12.png") 

full_plane <- ggarrange(p_sg_habit52,p_sg_res52, p_sg_adsr52, p_sg_all52,
                        labels = c("a", "b", "c", "d"),
                        ncol = 2, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "top")
full_plane_fig52_psy <- annotate_figure(full_plane,
                                  bottom = text_grob("Incremental QALYs"),
                                  left = text_grob("Incremental Costs", rot=90))
ggsave("full_plane52.png") 

### 52 WEEK VS 12 WEEK (not in paper) ####################################

PSA_full<-as.matrix(PSA_full)

ce_plane_plot_H_5212 = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=PSA_full[,"HH_5212_Q"], y=PSA_full[,"HH_5212_C"],col="High"),size=1, show.legend = T) +
  geom_point(aes(x=PSA_full[,"LH_5212_Q"], y=PSA_full[,"LH_5212_C"],col="Low"),size=1, show.legend = T) +
  geom_point(aes(x=mean(PSA_full[,"HH_5212_Q"], na.rm = T),y=mean(PSA_full[,"HH_5212_C"], na.rm=T), shape="Mean high"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSA_full[,"LH_5212_Q"], na.rm = T),y=mean(PSA_full[,"LH_5212_C"], na.rm=T), shape="Mean low"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.5,0.5),y=c(-10000,10000), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "Habit")+
  scale_color_manual(labels = c("High", "Low"), values = c("cadetblue4", "coral"))+
  scale_shape_manual(labels = c("Mean high", "Mean low"), values = c(23, 25))+
  scale_linetype()+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("Habit_5212.png")  

ce_plane_plot_R_5212 = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=PSA_full[,"HR_5212_Q"], y=PSA_full[,"HR_5212_C"],col="High"),size=1, show.legend = T) +
  geom_point(aes(x=PSA_full[,"LR_5212_Q"], y=PSA_full[,"LR_5212_C"],col="Low"),size=1, show.legend = T) +
  geom_point(aes(x=mean(PSA_full[,"HR_5212_Q"], na.rm = T),y=mean(PSA_full[,"HR_5212_C"], na.rm=T), shape="Mean high"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSA_full[,"LR_5212_Q"], na.rm = T),y=mean(PSA_full[,"LR_5212_C"], na.rm=T), shape="Mean low"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.5,0.5),y=c(-10000,10000), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "Restraint")+
  scale_color_manual(labels = c("High", "Low"), values = c("cadetblue4", "coral"))+
  scale_shape_manual(labels = c("Mean high", "Mean low"), values = c(23, 25))+
  scale_linetype()+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("Restraint_5212.png")  

ce_plane_plot_S_5212 = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=PSA_full[,"HREG_5212_Q"], y=PSA_full[,"HREG_5212_C"],col="High"),size=1, show.legend = T) +
  geom_point(aes(x=PSA_full[,"LREG_5212_Q"], y=PSA_full[,"LREG_5212_C"],col="Low"),size=1, show.legend = T) +
  geom_point(aes(x=mean(PSA_full[,"HREG_5212_Q"], na.rm = T),y=mean(PSA_full[,"HREG_5212_C"], na.rm=T), shape="Mean high"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSA_full[,"LREG_5212_Q"], na.rm = T),y=mean(PSA_full[,"LREG_5212_C"], na.rm=T), shape="Mean low"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.5,0.5),y=c(-10000,10000), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "Autonomous Diet Self-regulation")+
  scale_color_manual(labels = c("High", "Low"), values = c("cadetblue4", "coral"))+
  scale_shape_manual(labels = c("Mean high", "Mean low"), values = c(23, 25))+
  scale_linetype()+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("SelfReg_5212.png") 

ce_plane_plot_ex_5212 = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=PSA_full[,"HA_5212_Q"], y=PSA_full[,"HA_5212_C"],col="High"),size=1, show.legend = T) +
  geom_point(aes(x=PSA_full[,"LA_5212_Q"], y=PSA_full[,"LA_5212_C"],col="Low"),size=1, show.legend = T) +
  geom_point(aes(x=mean(PSA_full[,"HA_5212_Q"], na.rm = T),y=mean(PSA_full[,"HA_5212_C"], na.rm=T), shape="Mean high"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSA_full[,"LA_5212_Q"], na.rm = T),y=mean(PSA_full[,"LA_5212_C"], na.rm=T), shape="Mean low"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-1,1),y=c(-20000,20000), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "All variables")+
  scale_color_manual(labels = c("High", "Low"), values = c("cadetblue4", "coral"))+
  scale_shape_manual(labels = c("Mean high", "Mean low"), values = c(23, 25))+
  scale_linetype()+
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("HighLow_5212.png") 

full_plane <- ggarrange(ce_plane_plot_H_5212,ce_plane_plot_R_5212, ce_plane_plot_S_5212, ce_plane_plot_ex_5212,
                        labels = c("a", "b", "c", "d"),
                        ncol = 2, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "top")
full_plane_fig5212 <- annotate_figure(full_plane,
                                    bottom = text_grob("Incremental QALYs"),
                                    left = text_grob("Incremental Costs", rot=90))
ggsave("full_plane5212.png") 




