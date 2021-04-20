########################################################
# Adding MoA to SPHR model: Sensitivity analysis       #
########################################################

# Load PSA results
load("data/PSAResults00_3.RData")
load("data/PSAResultsAA_3.RData")
load("data/PSAResultsAA_4.RData")
load("data/PSAResultsBB_5.RData")

load("data/df_scen1.RData")
load("data/df_scen2.RData")

df_scen1<-as.data.frame(df_scen1)
df_scen2<-as.data.frame(df_scen2)

PSAResultsBB_5 <- as.data.frame(PSAResultsBB_5)
##Costs and QALYs per person
m_SA<-matrix(nrow = 2,ncol = 18, dimnames = list(c( "Scenario 1", "Scenario 2"),
                                                        c("C_BI", "CLCI_BI", "CHCI_BI", "Q_BI", "QLCI_BI", "QHCI_BI",
                                                          "C_12", "CLCI_12", "CHCI_12", "Q_12", "QLCI_12", "QHCI_12",
                                                          "C_52", "CLCI_52", "CHCI_52", "Q_52", "QLCI_52", "QHCI_52"
                                                          )))


m_SA["Scenario 1",]<-c(mean(PSAResults00_3$TOTAL_COST),sort(PSAResults00_3$TOTAL_COST,T)[975], sort(PSAResults00_3$TOTAL_COST,T)[25],
                               mean(PSAResults00_3$QALY),sort(PSAResults00_3$QALY,T)[975], sort(PSAResults00_3$QALY,T)[25],
                               mean(PSAResultsAA_3$TOTAL_COST),sort(PSAResultsAA_3$TOTAL_COST,T)[975], sort(PSAResultsAA_3$TOTAL_COST,T)[25],
                               mean(PSAResultsAA_3$QALY),sort(PSAResultsAA_3$QALY,T)[975], sort(PSAResultsAA_3$QALY,T)[25],
                               mean(PSAResultsBB_5$C_52),sort(PSAResultsBB_5$C_52,T)[975], sort(PSAResultsBB_5$C_52,T)[25],
                               mean(PSAResultsBB_5$Q_52),sort(PSAResultsBB_5$Q_52,T)[975], sort(PSAResultsBB_5$Q_52,T)[25]
                               )
m_SA["Scenario 2",]<-c(mean(PSAResults00_3$TOTAL_COST),sort(PSAResults00_3$TOTAL_COST,T)[975], sort(PSAResults00_3$TOTAL_COST,T)[25],
                                mean(PSAResults00_3$QALY),sort(PSAResults00_3$QALY,T)[975], sort(PSAResults00_3$QALY,T)[25],
                                mean(PSAResultsAA_4$TOTAL_COST),sort(PSAResultsAA_4$TOTAL_COST,T)[975], sort(PSAResultsAA_4$TOTAL_COST,T)[25],
                                mean(PSAResultsAA_4$QALY),sort(PSAResultsAA_4$QALY,T)[975], sort(PSAResultsAA_4$QALY,T)[25],
                                mean(PSAResultsBB_5$C_52),sort(PSAResultsBB_5$C_52,T)[975], sort(PSAResultsBB_5$C_52,T)[25],
                                mean(PSAResultsBB_5$Q_52),sort(PSAResultsBB_5$Q_52,T)[975], sort(PSAResultsBB_5$Q_52,T)[25]
)

# scenario 1
(mean(PSAResultsAA_3$TOTAL_COST, na.rm=T)-mean(PSAResults00_3$TOTAL_COST))/5000
(mean(PSAResultsBB_5$C_52, na.rm=T)-mean(PSAResults00_3$TOTAL_COST, na.rm=T))/5000

(mean(PSAResultsAA_3$QALY, na.rm=T)-mean(PSAResults00_3$QALY, na.rm=T))/5000
(mean(PSAResultsBB_5$Q_52, na.rm=T)-mean(PSAResults00_3$QALY, na.rm=T))/5000

# scenario 2
(mean(PSAResultsAA_4$TOTAL_COST)-mean(PSAResults00_3$TOTAL_COST))/5000
(mean(PSAResultsBB_5$C_52)-mean(PSAResults00_3$TOTAL_COST))/5000

(mean(PSAResultsAA_4$QALY)-mean(PSAResults00_3$QALY))/5000
(mean(PSAResultsBB_5$Q_52)-mean(PSAResults00_3$QALY))/5000


m_SA <- m_SA/5000
PSAResults_p <- PSAResults_p/5000

pSA_12_BI = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=df_scen1[,"inc_q_12"], y=df_scen1[,"inc_c_12"],col="Scenario 1"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=df_scen2[,"inc_q_12"], y=df_scen2[,"inc_c_12"],col="Scenario 2"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=PSAResults_p[,"Q_12_BI"], y=PSAResults_p[,"C_12_BI"],col="Base case"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=mean(df_scen1[,"inc_q_12"], na.rm = T),y=mean(df_scen1[,"inc_c_12"], na.rm=T), shape="Mean scenario 1"),size=2,col="black", fill="black") +
  geom_point(aes(x=mean(df_scen2[,"inc_q_12"], na.rm = T),y=mean(df_scen2[,"inc_c_12"], na.rm=T), shape="Mean scenario 2"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_p[,"Q_12_BI"], na.rm = T),y=mean(PSAResults_p[,"C_12_BI"], na.rm=T), shape="Mean base case"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.01),y=c(-500,200), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "12-week programme vs. brief intervention")+
#  scale_color_manual(labels = c( "Scenario 1", "Scenario 2", "Base case"), values = c("cadetblue4", "coral", "gray80"))+
#  scale_shape_manual(labels = c("Mean scenario 1", "Mean scenario 2", "Mean base case"), values = c(23, 25, 21))+
  scale_linetype()+
  theme_minimal() +
  #xlim(-0.025, 0.075)+
  #ylim(-850, 200)+
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plane_12_BI.png")  

pSA_52_BI = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=df_scen1[,"inc_q_52"], y=df_scen1[,"inc_c_52"],col="Scenario 1"),size=1, show.legend = T, alpha = 0.75, position = position_nudge(y = -3)) +
  geom_point(aes(x=df_scen2[,"inc_q_52"], y=df_scen2[,"inc_c_52"],col="Scenario 2"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=PSAResults_p[,"Q_52_BI"], y=PSAResults_p[,"C_52_BI"],col="Base case"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=mean(df_scen1[,"inc_q_52"], na.rm = T),y=mean(df_scen1[,"inc_c_52"], na.rm=T), shape="Mean scenario 1"),size=2,col="black", fill="black", position = position_nudge(x = 0.001, y = -5)) +
  geom_point(aes(x=mean(df_scen2[,"inc_q_52"], na.rm = T),y=mean(df_scen2[,"inc_c_52"], na.rm=T), shape="Mean scenario 2"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_p[,"Q_52_BI"], na.rm = T),y=mean(PSAResults_p[,"C_52_BI"], na.rm=T), shape="Mean Mean base case"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.01),y=c(-500,200), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "52-week programme vs. brief intervention")+
  #  scale_color_manual(labels = c( "Scenario 1", "Scenario 2", "Base case"), values = c("cadetblue4", "coral", "gray80"))+
  #  scale_shape_manual(labels = c("Mean scenario 1", "Mean scenario 2", "Mean base case"), values = c(23, 25, 21))+
  scale_linetype()+
  theme_minimal() +
#  xlim(-0.025, 0.075)+
#  ylim(-850, 200)+
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plane_12_BI.png") 


pSA_52_12 = ggplot() +
  # plot iteration results and mean estimate
  geom_point(aes(x=df_scen1[,"inc_q_1252"], y=df_scen1[,"inc_c_1252"],col="Scenario 1"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=df_scen2[,"inc_q_1252"], y=df_scen2[,"inc_c_1252"],col="Scenario 2"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=PSAResults_p[,"Q_52_12"], y=PSAResults_p[,"C_52_12"],col="Base case"),size=1, show.legend = T, alpha = 0.75) +
  geom_point(aes(x=mean(df_scen1[,"inc_q_1252"], na.rm = T),y=mean(df_scen1[,"inc_c_1252"], na.rm=T), shape="Mean scenario 1"),size=2,col="black", fill="black") +
  geom_point(aes(x=mean(df_scen2[,"inc_q_1252"], na.rm = T),y=mean(df_scen2[,"inc_c_1252"], na.rm=T), shape="Mean scenario 2"), size=2,col="black", fill="black") +
  geom_point(aes(x=mean(PSAResults_p[,"Q_52_12"], na.rm = T),y=mean(PSAResults_p[,"C_52_12"], na.rm=T), shape="Mean Mean base case"),size=2,col="black", fill="black") +
  # coordinate systems
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)    +
  geom_line(aes(x=c(-0.025,0.01),y=c(-500,200), lty = "WTP Threshold £20,000")) +
  # labels
  xlab("Incremental QALY") +
  ylab("Incremental Costs") +
  labs(title = "12-week programme vs. brief intervention")+
  #  scale_color_manual(labels = c( "Scenario 1", "Scenario 2", "Base case"), values = c("cadetblue4", "coral", "gray80"))+
  #  scale_shape_manual(labels = c("Mean scenario 1", "Mean scenario 2", "Mean base case"), values = c(23, 25, 21))+
  scale_linetype()+
  theme_minimal() +
  #xlim(-0.025, 0.075)+
  #ylim(-850, 200)+
  theme(legend.position = "bottom", legend.title=element_blank(),text = element_text(size=10), axis.title = element_blank())+
  guides(colour = guide_legend(override.aes = list(size=3)))
ggsave("plane_12_BI.png")

# Arrange plots together
full_plane_SACE <- ggarrange(pSA_12_BI, pSA_52_BI,
                           labels = c("a", "b"),
                           ncol = 1, nrow = 2, heights = 5, widths = 5, common.legend = T, legend = "right")

# Add axis lables
full_plane_SACE <- annotate_figure(full_plane_SACE,
                                 bottom = text_grob("Incremental QALYs"),
                                 left = text_grob("Incremental Costs", rot=90))
ggsave("p_SA.png") 


PSAResults_p<-as.data.frame(PSAResults_p)

# Set matrices for incremental costs, QALYs and net monetry benefit
inc_costs<-inc_qalys<-inc_nmb<-matrix(nrow = 3,ncol = 9, dimnames = list(c( "Base case", "Scenario 1", "Scenario 2"),
                                                              c("BI_12", "LCI_BI_12", "HCI_BI_12", 
                                                                "BI_52", "LCI_52_BI", "HCI_52_BI",
                                                                "12_52", "LCI_12_52", "HCI_12_52")))

inc_costs["Base case",]<-c(mean(PSAResults_p[,"C_12_BI"]),sort(PSAResults_p[,"C_12_BI"],T)[975], sort(PSAResults_p[,"C_12_BI"],T)[25],
                   mean(PSAResults_p[,"C_52_BI"]),sort(PSAResults_p[,"C_52_BI"],T)[975], sort(PSAResults_p[,"C_52_BI"],T)[25],
                   mean(PSAResults_p[,"C_52_12"]),sort(PSAResults_p[,"C_52_12"],T)[975], sort(PSAResults_p[,"C_52_12"],T)[25]
                   )
  
inc_qalys["Base case",]<-c(mean(PSAResults_p[,"Q_12_BI"]),sort(PSAResults_p[,"Q_12_BI"],T)[975], sort(PSAResults_p[,"Q_12_BI"],T)[25],
                   mean(PSAResults_p[,"Q_52_BI"]),sort(PSAResults_p[,"Q_52_BI"],T)[975], sort(PSAResults_p[,"Q_52_BI"],T)[25],
                   mean(PSAResults_p[,"Q_52_12"]),sort(PSAResults_p[,"Q_52_12"],T)[975], sort(PSAResults_p[,"Q_52_12"],T)[25]
                   )

inc_nmb["Base case",]<-c(mean(PSAResults_p[,"NMB_12_BI"]),sort(PSAResults_p[,"NMB_12_BI"],T)[975], sort(PSAResults_p[,"NMB_12_BI"],T)[25],
               mean(PSAResults_p[,"NMB_52_BI"]),sort(PSAResults_p[,"NMB_52_BI"],T)[975], sort(PSAResults_p[,"NMB_52_BI"],T)[25],
               mean(PSAResults_p[,"NMB_52_12"]),sort(PSAResults_p[,"NMB_52_12"],T)[975], sort(PSAResults_p[,"NMB_52_12"],T)[25]
)

inc_costs["Scenario 1",]<-c(mean(df_scen1[,"inc_c_12"]),sort(df_scen1[,"inc_c_12"],T)[975], sort(df_scen1[,"inc_c_12"],T)[25],
                           mean(df_scen1[,"inc_c_52"]),sort(df_scen1[,"inc_c_52"],T)[975], sort(df_scen1[,"inc_c_52"],T)[25],
                           mean(df_scen1[,"inc_c_1252"]),sort(df_scen1[,"inc_c_1252"],T)[975], sort(df_scen1[,"inc_c_1252"],T)[25]
)

inc_qalys["Scenario 1",]<-c(mean(df_scen1[,"inc_q_12"]),sort(df_scen1[,"inc_q_12"],T)[975], sort(df_scen1[,"inc_q_12"],T)[25],
                           mean(df_scen1[,"inc_q_52"]),sort(df_scen1[,"inc_q_52"],T)[975], sort(df_scen1[,"inc_q_52"],T)[25],
                           mean(df_scen1[,"inc_q_52"]),sort(df_scen1[,"inc_q_1252"],T)[975], sort(df_scen1[,"inc_q_1252"],T)[25]
)

inc_nmb["Scenario 1",]<-c(mean(df_scen1[,"NMB_12"]),sort(df_scen1[,"NMB_12"],T)[975], sort(df_scen1[,"NMB_12"],T)[25],
                         mean(df_scen1[,"NMB_52"]),sort(df_scen1[,"NMB_52"],T)[975], sort(df_scen1[,"NMB_52"],T)[25],
                         mean(df_scen1[,"NMB_1252"]),sort(df_scen1[,"NMB_1252"],T)[975], sort(df_scen1[,"NMB_1252"],T)[25]
)
  
inc_costs["Scenario 2",]<-c(mean(df_scen2[,"inc_c_12"]),sort(df_scen2[,"inc_c_12"],T)[975], sort(df_scen2[,"inc_c_12"],T)[25],
                            mean(df_scen2[,"inc_c_52"]),sort(df_scen2[,"inc_c_52"],T)[975], sort(df_scen2[,"inc_c_52"],T)[25],
                            mean(df_scen2[,"inc_c_1252"]),sort(df_scen2[,"inc_c_1252"],T)[975], sort(df_scen2[,"inc_c_1252"],T)[25]
)

inc_qalys["Scenario 2",]<-c(mean(df_scen2[,"inc_q_12"]),sort(df_scen2[,"inc_q_12"],T)[975], sort(df_scen2[,"inc_q_12"],T)[25],
                            mean(df_scen2[,"inc_q_52"]),sort(df_scen2[,"inc_q_52"],T)[975], sort(df_scen2[,"inc_q_52"],T)[25],
                            mean(df_scen2[,"inc_q_52"]),sort(df_scen2[,"inc_q_1252"],T)[975], sort(df_scen2[,"inc_q_1252"],T)[25]
)

inc_nmb["Scenario 2",]<-c(mean(df_scen2[,"NMB_12"]),sort(df_scen2[,"NMB_12"],T)[975], sort(df_scen2[,"NMB_12"],T)[25],
                          mean(df_scen2[,"NMB_52"]),sort(df_scen2[,"NMB_52"],T)[975], sort(df_scen2[,"NMB_52"],T)[25],
                          mean(df_scen2[,"NMB_1252"]),sort(df_scen2[,"NMB_1252"],T)[975], sort(df_scen2[,"NMB_1252"],T)[25]
)
  
 






